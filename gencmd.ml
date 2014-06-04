(* Gencmd

   Copyright (C) 2014 Xiphos Systems Corporation

   Author: Berke Durak <obd@xiphos.com>

 *)

let word = ref 0l

module Opt = struct
	open Arg

	let set_bit i () = word := Int32.logor !word (Int32.shift_left 1l i)

	let set_bits i m q () =
		if q < 0 || q >= 1 lsl m then
			raise (Bad(Printf.sprintf "Value %d out of %d-bit range" q m))
		else
			word :=
				Int32.logor
					(Int32.logand
						!word
						(Int32.lognot
							(Int32.shift_left
								(Int32.of_int ((1 lsl m) - 1))
								i)))
					(Int32.shift_left (Int32.of_int q) i)

	let spec = align
	[
		"-data",		Unit(set_bit 31),			" Data mode";
		"-read",		Unit(set_bit 30),			" Read, not write";
		"-fast",		Unit(set_bit 29),			" Fast clock";
		"-short",		Unit(set_bits 24 2 0b01),		" Short response";
		"-long",		Unit(set_bits 24 2 0b10),		" Long response";
		"-sectors",		Int(fun q -> set_bits 16 7 q ()),	" Number of sectors";
		"-slot",		Int(fun q -> set_bits 15 1 q ()),	" SD card slot";
		"-no-stop",		Unit(set_bit 8),			" No stop command \
								at end of multi-block writes";
		"-cmd",			Int(fun q -> set_bits 0 6 q ()),	" Command number";
	]

	let anonymous u = raise (Bad "Extraneous argument")
end

let _ =
	Arg.parse Opt.spec Opt.anonymous
		"I have the displeaasure to announce you that your arguments are bad.";
	Printf.printf "%08lx\n" !word
