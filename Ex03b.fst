module Ex03b

type nat = x:int{x>=0}

val fibonacci : nat -> Tot nat
let rec fibonacci n =
	if n <= 1 then 1
	else fibonacci (n - 1) + fibonacci (n - 2)
