module Ex04c

val append : list 'a -> list 'a -> Tot (list 'a)
let rec append l1 l2 = match l1 with
	| [] -> l2
	| hd1 :: tl1 -> hd1 :: append tl1 l2

val mem: #a:eqtype -> a -> list a -> Tot bool
let rec mem #a x l = match l with
	| [] -> false
	| hd :: tl -> hd = x || mem x tl

val append_mem : #a:eqtype -> l1:list a -> l2:list a -> x:a -> Lemma (mem x (append l1 l2) <==> mem x l1 || mem x l2)
let rec append_mem #a l1 l2 x = match l1 with
	| [] -> ()
	| hd1 :: tl1 -> append_mem tl1 l2 x
