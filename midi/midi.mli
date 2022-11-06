module Device : sig
  type t
  type interface = MMSystem | CoreMidi | ALSA
  val create_virtual_input : interface -> string -> (t -> unit) -> unit
end