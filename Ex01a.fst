module Ex01a

open FStar.Exn
open FStar.All

type filename = string

val isWriteable : filename -> Tot bool
let isWriteable fname = match fname with
	| "output.txt" -> true
	| _ -> false

val write : f:filename{isWriteable f} -> string -> ML unit
let write f s = FStar.IO.print_string ("Dummy write of string " ^ s ^ " to file " ^ f ^ "\n")

val checkedWrite : filename -> string -> ML unit
let checkedWrite target content = if isWriteable target then write target content