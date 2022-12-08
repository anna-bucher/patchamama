type t = string * (unit -> unit)
val run : string -> ?skip:string list -> ?group:char -> t list -> unit

val color : [< `Group | `Test | `Fail | `Info | `Grey | `Ok] -> string -> string

module Assert : sig
  val true_or : string -> bool -> unit
  val equal_or : ('a -> 'a -> string, unit, string) format -> 'a -> 'a -> unit
  val throws : string -> (unit -> 'b) -> unit
  val pass : (unit -> 'a) -> unit
  val fail : string -> 'b
  val failf : ('a -> string, unit, string) format -> 'a -> 'b
  val snapshot : string -> string -> string -> string -> unit
end

module Utils : sig
  val read_file : string -> bytes option
  val write_file : string -> bytes -> unit
  val with_tmp_dir : (string -> 'a) -> unit
end