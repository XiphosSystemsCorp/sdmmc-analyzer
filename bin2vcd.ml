(* Bin2vcd

   Copyright (C) 2014 Xiphos Systems Corporation

   Author: Berke Durak <obd@xiphos.com>

 *)


let pf = Printf.printf
let fp = Printf.fprintf

let convert ic oc =
        fp oc "\
		$timescale 1 us $end\n\
		$scope module top $end\n\
		$var wire 1 ! Channel_0 $end\n\
		$var wire 1 \" Channel_1 $end\n\
		$var wire 1 # Channel_2 $end\n\
		$var wire 1 $ Channel_3 $end\n\
		$var wire 1 %% Channel_4 $end\n\
		$var wire 1 & Channel_5 $end\n\
		$var wire 1 ' Channel_6 $end\n\
		$var wire 1 ( Channel_7 $end\n\
		$upscope $end\n\
		$enddefinitions $end\n\"";
	let labels = "!\"#$%&'(" in
	let rec loop t q =
		let q' = input_byte ic in
		if q <> q' then begin
			fp oc "#%d\n" t;
			let rec loop2 j =
				if j < 8 then begin
					if (q lxor q') land (1 lsl j) <> 0 then begin
						fp oc "%d%c\n" (if (q' land (1 lsl j)) <> 0 then 1 else 0) labels.[j]
					end;
					loop2 (j + 1)
				end
			in
			loop2 0
		end;
		loop (t + 1) q'
	in
	begin try
		loop 0 0;
	with
		| End_of_file -> ()
	end;
	flush oc
        
let _ =
        convert stdin stdout
