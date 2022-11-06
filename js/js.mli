type t
type value =
| Undefined
| Null
| Bool of bool
| String of string
| Number of float
val typeof : value -> string

val open' : string -> (t -> unit) -> unit
val call : t -> string -> value list -> value