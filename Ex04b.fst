module Ex04b

type nat = n:int{n>=0}

val length : list 'a -> Tot nat
let rec length l = match l with
	| [] -> 0
	| _ :: tl -> 1 + length tl

val append : list 'a -> list 'a -> Tot (list 'a)
let rec append l1 l2 = match l1 with
	| [] -> l2
	| hd :: tl -> hd :: append tl l2

val append_length_is_the_sum_of_arg_lengths : l1:list 'a -> l2:list 'a -> Lemma (length (append l1 l2) = length l1 + length l2)
let rec append_length_is_the_sum_of_arg_lengths l1 l2 = match l1 with
	| [] -> ()
	| _ :: tl1 -> append_length_is_the_sum_of_arg_lengths tl1 l2
