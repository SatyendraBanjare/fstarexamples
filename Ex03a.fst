module Ex03a

open FStar.Mul

type nat = x:int{x>=0}
type pos = x:int{x>=1}

val factorial : nat -> Tot pos
let rec factorial n = match n with
	| 0 -> 1
	| _ -> n * factorial (n - 1)
