type interface = MMSystem | CoreMIDI | ALSA
val interface_to_string : interface -> string

module Input_device : sig
  type t
  val create : interface -> string -> (t -> unit) -> unit
  val interface : t -> interface
  val name : t -> string
end

module Output_device : sig
  type t
  val create : interface -> string -> (t -> unit) -> unit
  val interface : t -> interface
  val name : t -> string
end