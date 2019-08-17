module Ex04f

open FStar.List.Tot

val foldl : f:('a -> 'b -> 'b) -> list 'a -> 'b -> Tot 'b
let rec foldl f l acc = match l with
	| [] -> acc
	| hd::tl -> foldl f tl (f hd acc)

val append_associativity : l1:list 'a -> l2:list 'a -> l3:list 'a
	-> Lemma (l1 @ (l2 @ l3) == (l1 @ l2) @ l3)
let rec append_associativity l1 l2 l3 = match l1 with
	| [] -> ()
	| p1::q1 -> append_associativity q1 l2 l3

val reverse : list 'a -> list 'a
let rec reverse l = match l with
	| [] -> []
	| hd :: tl -> reverse tl @ [hd]

val foldl_cons_is_reverse : l1:list 'a -> l2:list 'a
	-> Lemma (foldl Cons l1 l2 == reverse l1 @ l2)
let rec foldl_cons_is_reverse (l1:list 'a) (l2:list 'a) = match l1 with
	| [] -> ()
	| p::q -> 
		append_associativity (reverse q) [p] l2;
		foldl_cons_is_reverse q (p :: l2)
