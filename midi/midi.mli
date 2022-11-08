type m
type interface = MMSystem | CoreMIDI | ALSA
val interface_to_string : interface -> string
val open' : (m -> unit) -> unit

module Input_device : sig
  type info
  type t
  val list : m -> info list
  val find_device : m -> string -> info option

  val info : t -> info
  val interface : info -> interface
  val name : info -> string

  val create : m -> ?buf_size:int -> interface -> string -> (t -> unit) -> unit
  val open' : m -> ?buf_size:int -> info -> (t -> unit) -> unit

  val read : t -> int
end

module Output_device : sig
  type info
  type t
  val list : m -> info list
  val find_device : m -> string -> info option

  val info : t -> info
  val interface : info -> interface
  val name : info -> string

  val create : m -> ?buf_size:int -> ?latency:int -> interface -> string -> (t -> unit) -> unit
  val open' : m -> ?buf_size:int -> ?latency:int -> info -> (t -> unit) -> unit

end