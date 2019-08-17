module Ex04e

type option 'a =
	| None : option 'a
	| Some : v:'a -> option 'a

val find : f:('a -> bool) -> l:list 'a -> out:option 'a{Some? out ==> f (Some?.v out)}
let rec find f l = match l with
	| [] -> None
	| p::q -> if f p then Some p else find f q

val find2 : f:('a -> bool) -> l:list 'a -> out:option 'a
let rec find2 f l = match l with
	| [] -> None
	| p::q -> if f p then Some p else find2 f q

val find2_lm : f:('a -> bool) -> l:list 'a
	-> Lemma (let out = find2 f l in Some? out ==> f (Some?.v out))
let rec find2_lm f l = match l with
	| [] -> ()
	| p::q -> find2_lm f q