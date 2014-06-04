(* Sdmmc_analyzer

   Copyright (C) 2014 Xiphos Systems Corporation

   Author: Berke Durak <obd@xiphos.com>

 *)

exception Unknown_command

let show_data = ref false
let clock = ref 1e6
let file = ref None

module Opt = struct
  open Arg

  let spec = align
  [
    "-show-data",   Set show_data,    " Dump data blocks";
    "-clock",   Set_float clock,  " Set clock rate";
  ]

  let anonymous u =
  match !file with
  | None -> file := Some u
  | Some _ -> raise (Bad "Only one file can be specified")
end


let _ =
  Arg.parse Opt.spec Opt.anonymous
    "I have the displeaasure to announce you that your arguments are bad.";
  let ic =
    match !file with
    | None -> stdin
    | Some fn -> open_in_bin fn
  in
  let byte = ref 0xff in
  let command = ref 0
  and mosi = ref false
  and word = ref 0l
  and words = ref []
  and word_bits = ref 0
  and crc = ref 0
  and q = ref `idle
  and q_data = ref `idle
  and index = ref 0
  and start_index = ref 0
  and data_start_index = ref 0
  and last_clk = ref true
  and computed_crc = ref 0
  and crc_bits = ref 0
  and response_type = ref `R0
  and content_length = ref 0
  and application_specific = ref false
  and unknown_command = ref false
  and has_crc = ref false
  and data_buffer = Buffer.create 512
  and block_length = ref 512
  and received_crc16s = [|0;0;0;0|]
  and computed_crc16s = [|0;0;0;0|]
  and data_enabled = ref false (* Data transmission enabled? *)
  and data_block_mode = ref false
  in
  let tick_crc bit =
    incr crc_bits;
    let c = !computed_crc in
    let z = (c lsr 6) lxor bit in
    let c' = ((c lsl 1) land 0x7f) lxor z lxor (z lsl 3) in
    (*Printf.printf "T (%d)->0x%02x->(%d)->0x%02x (%d)\n"
        bit c z c' !crc_bits;*)
    computed_crc := c'
  in
  let clear_crc () =
    crc_bits := 0;
    computed_crc := 0
  in
  let crc16_add_bit c bit =
    let z = bit lxor (c lsr 15) in
    let x = z lor (z lsl 5) lor (z lsl 12) in
    let c' = ((c lsl 1) land 0xffff) lxor x in
    c'
  in
  let crc16_add_nibble d =
    for i = 0 to 3 do
      computed_crc16s.(i) <-
        crc16_add_bit computed_crc16s.(i) ((d lsr i) land 1)
    done
  in
  let clear_crc16s () =
    for i = 0 to 3 do
      received_crc16s.(i) <- 0;
      computed_crc16s.(i) <- 0;
    done
  in
  let clear_words () =
    word := 0l;
    words := [];
    word_bits := 0;
  in
  let get_words () =
    if !word_bits > 0 then begin
        let r = Some(!word_bits, !word), List.rev !words in
        word := 0l;
        word_bits := 0;
        r
      end
    else
      None, List.rev !words
  in
  let print_list oc (last, rest) =
    let rec loop = function
      | [] ->
          (
            match last with
            | None -> ()
            | Some(k, w) ->
                if k mod 4 = 0 then
                  let rec loop2 i =
                    if i = k then
                      ()
                    else
                      (
                        Printf.fprintf oc "%lx"
                          (Int32.shift_right_logical w (k - i - 4));
                        loop2 (i + 4)
                      )
                  in
                  loop2 0
                else
                  (
                    Printf.fprintf oc "[";
                    let rec loop3 i =
                      if i = k then
                        ()
                      else
                        (
                          Printf.fprintf oc "%ld"
                            (Int32.logand 1l (Int32.shift_right w (k - i - 1)));
                          loop3 (i + 1)
                        )
                    in
                    loop3 0;
                    Printf.fprintf oc "]"
                  )
            )
      | w :: rest ->
        Printf.fprintf oc "%08lx" w;
        loop rest;
    in
    Printf.fprintf oc "{";
    loop rest;
    Printf.fprintf oc " }";
  in
  let get_and_print_words oc () = print_list oc (get_words ()) in
  let push_word_bit bit =
    word :=
      Int32.logor
        (Int32.shift_left !word 1)
        (if bit = 0 then 0l else 0x1l);
    incr word_bits;
    if !word_bits = 32 then begin
      words := !word :: !words;
      word := 0l;
      word_bits := 0;
    end
  in
  let type_of_response cmd =
    if !application_specific then
      match cmd with
      | 18 | 25 | 26 | 38 | 43 | 49 -> `R0
      |  6 | 22 | 23 | 42 | 51 -> `R1
      | 41 -> `R3
      | _ -> raise Unknown_command
    else
      match cmd with
      |  2 |  9 -> `R2
      |  3 -> `R6
      |  5 -> `R4
      |  8 -> `R7
      |  1 |  6 |  7 | 10 | 13 | 16 | 17 | 18 | 22 | 23 | 24
      | 25 | 27 | 30 | 32 | 33 | 41 | 42 | 55 | 56 | 59
      | 51 -> `R1
      | 12 | 28 | 29 | 38 -> `R1b
      | 58 -> `R3
      |  0 -> `R0
      | _ -> raise Unknown_command
  in
  let content_length_and_crc_of_response = function
    | `R1 | `R6 -> 32, true
    | `R7 -> 32, true
    | `R4 -> 45, false
    | `R3 -> 39, false
    | `R2 -> 127, true
    | `R1b -> 32, true
    | `R0 -> 0, true
  in
  let string_of_response_type = function
    | `R0  -> "R0"
    | `R1  -> "R1"
    | `R1b -> "R1b"
    | `R2  -> "R2"
    | `R3  -> "R3"
    | `R4  -> "R4"
    | `R6  -> "R6"
    | `R7  -> "R7"
  in
  let last_xx = ref (-1) in
  try while true do
    let xx = input_byte ic in
    let clk = (xx land 32) <> 0 in (* Clock bit: 5 *)
    let cmd = (xx lsr 4) land 1 in (* Cmd bit : 4 *)
    let busy = (xx land 1) = 0 in (* Busy bit, aka Dat0 : 0 *)
    let d = xx land 15 in (* Dat0 to Dat3 : bits 0 to 3 *)
    if false && !last_xx != xx then
      begin
        Printf.printf "%02x %d%d%d %d\n" xx
          (if clk then 1 else 0) cmd (if busy then 1 else 0) d;
        last_xx := xx;
      end;
    let print_crc16 oc a =
      Printf.fprintf oc "%04x %04x %04x %04x" a.(0) a.(1) a.(2) a.(3)
    in
    let data_done end_bit =
      q_data := `idle;
      let u = Buffer.contents data_buffer in
      let m = String.length u in
      Buffer.clear data_buffer;
      Printf.printf "%08d DATA %a (end %08d)"
        !data_start_index print_crc16 received_crc16s !index;
      if computed_crc16s <> received_crc16s then
        Printf.printf " E(CRC<>%a)" print_crc16 computed_crc16s;
      if end_bit <> 15 && end_bit <> 14 then
        Printf.printf " E(END=0x%x<>0xf|0xe)" end_bit;
      Printf.printf "\n";

      (* Dump data *)
      if !show_data then begin
        let rec loop i j =
          if i = m then begin
            if j > 0 then Printf.printf "\n";
            ()
          end else begin
            if j = 0 then begin
              Printf.printf "        0x%04x" i;
            end;
            if j = 16 then begin
              Printf.printf "\n";
              loop i 0
            end else begin
              let x = u.[i] in
              Printf.printf " %02x" (Char.code x);
              loop (i + 1) (j + 1)
            end
          end
        in
        loop 0 0
      end;
      if not !data_block_mode then data_enabled := false;
    in
    let command_done () =
      if !mosi then begin
        Printf.printf "%08d     %-3s %2d  %a CRC=0x%02x %c (%s)"
          !start_index
          (if !application_specific then "ACMD" else " CMD")
          !command
          get_and_print_words ()
          !crc
          (if busy then 'B' else '-')
          (string_of_response_type !response_type);
        if !unknown_command then
          Printf.printf " E(UNKNOWN_CMD)";
        if !crc <> !computed_crc then
          Printf.printf " E(CRC<>0x%02x)" !computed_crc;
        if cmd <> 1 then Printf.printf " E(ZSB)";
        Printf.printf "\n";
        begin match !command with
          | 18 | 25 ->
            data_enabled := true; data_block_mode := true
          | 17 | 24 | 51 ->
            data_enabled := true; data_block_mode := false
          | _ -> data_enabled := false
        end;
      end else begin
        begin match !response_type with
        | `R0 ->
          Printf.printf "%08d R0 E(WTF)" !start_index
        | `R4 ->
          Printf.printf "%08d R4 %a"
            !start_index
            get_and_print_words ()
        | `R1 | `R1b | `R7 as r ->
          Printf.printf "%08d %-3s(%s %2d) %a CRC=0x%02x %c"
            !start_index
            (string_of_response_type r)
            (if !application_specific then "ACMD" else " CMD")
            !command
            get_and_print_words ()
            !crc
            (if busy then 'B' else '-');
          if !crc <> !computed_crc then
            Printf.printf " E(CRC<>0x%02x)" !computed_crc;
        | `R2 ->
          Printf.printf "%08d R2           %a"
            !start_index
            get_and_print_words ();
          if !command <> 63 then
            Printf.printf " E(CMD<>63)";
        | `R3 ->
          Printf.printf "%08d R3           %a"
            !start_index
            get_and_print_words ();
          if !command <> 63 then
            Printf.printf " E(CMD<>63)";
        | `R6 ->
          Printf.printf "%08d R6           %a CRC=0x%02x %c"
            !start_index
            get_and_print_words ()
            !crc
            (if busy then 'B' else '-');
          if !crc <> !computed_crc then
            Printf.printf " E(CRC<>0x%02x)" !computed_crc;
          if !command <> 3 then Printf.printf " E(CMD<>3)";
          
        end;
        if cmd <> 1 then Printf.printf " E(ZSB)";
        Printf.printf "\n";
        application_specific := !command = 55;
      end;
      q := `idle
    in
    let do_command () =
      match !q with
      | `idle ->
        if cmd = 0 then begin
          clear_crc ();
          tick_crc cmd;
          q := `started 0;
          start_index := !index;
          byte := ((!byte lsl 1) lor cmd) land 0xff;
          (*Printf.printf "%08d COMMAND START\n" !start_index;*)
        end
      | `started n ->
        byte := ((!byte lsl 1) lor cmd) land 0xff;
        tick_crc cmd;
        let n = n + 1 in
        if n = 7 then begin
          mosi := !byte land 0x40 <> 0;
          command := !byte land 0x3f;
          byte := 0xff;
          clear_words ();
          if !mosi then begin
            begin try
              response_type := type_of_response !command;
              unknown_command := false;
            with
            | Unknown_command ->
              response_type := `R0;
              unknown_command := true;
            end;
            content_length := 32;
            q := `contents 32;
            has_crc := true;
          end else begin
            let m, h_crc =
              content_length_and_crc_of_response
              !response_type
            in
            has_crc := h_crc;
            content_length := m;
            q := `contents m
          end
        end else
          q := `started n
      | `contents n ->
        let next () =
          if !has_crc then
            q := `crc 0
          else
            command_done ()
        in
        if n = 0 then
          next ()
        else begin
          tick_crc cmd;
          push_word_bit cmd;
          let n = n - 1 in
          if n = 0 then
            next ()
          else
            q := `contents n
        end
      | `crc 7 -> command_done ()
      | `crc n ->
        crc := ((!crc lsl 1) lor cmd) land 0x7f;
        q := `crc(n + 1)
    in
    let do_data () =
      let de = !data_enabled in
      match !q_data with
      | `idle ->
        begin match d with
        | 0 ->
          if de then begin
            q_data := `high 0;
            data_start_index := !index;
            Printf.printf "%08d DATA START\n" !index;
            clear_crc16s ();
          end else begin
            Printf.printf "%08d DATA START NOT IN DATA MODE\n" !index
          end
        | 14 -> q_data := `busy !index;
          Printf.printf "%08d DATA BUSY\n" !index;
        | 15 -> ()
        | _ ->
          Printf.printf "%08d spurious value 0x%x on data lines\n" !index d;
        end
      | `busy n ->
        begin match d with
        | 14 -> ()
        | 15 -> let t_busy = !index - n in
          Printf.printf "%08d DATA BUSY END duration %d\n" !index t_busy;
          q_data := `idle
        | _ ->
          Printf.printf "%08d spurious value 0x%x on busy data line\n" !index d;
          q_data := `idle
        end
      | `high n when de ->
        crc16_add_nibble d;
        q_data := `low(d,n);
      | `low(d',n) when de ->
        crc16_add_nibble d;
        let b = (d' lsl 4) lor d in
        let c = Char.chr b in
        Buffer.add_char data_buffer c;
        let n = n + 1 in
        if n = !block_length then
          q_data := `crc 0
        else
          q_data := `high n
      | `crc n when de ->
        for i = 0 to 3 do
          received_crc16s.(i) <-
                (received_crc16s.(i) lsl 1) lor ((d lsr i) land 1)
        done;
        let n = n + 1 in
        if n = 16 then
          q_data := `end_bit
        else
          q_data := `crc n
      | `end_bit when de -> data_done d
      | _ ->
        Printf.printf "%08d DATA ABORT\n" !index;
        q_data := `idle
    in
    if not !last_clk && clk then begin
      do_data ();
      do_command ();
    end;
    last_clk := clk;
    incr index;
  done with
  | End_of_file -> ()
