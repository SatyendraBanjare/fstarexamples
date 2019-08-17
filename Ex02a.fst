module Ex02a

type filename = string

val canWrite : filename -> Tot bool
let canWrite f = match f with
    | "output.txt" -> true
    | _ -> false

val canRead : filename -> Tot bool
let canRead f = canWrite f || f = "input.txt"
