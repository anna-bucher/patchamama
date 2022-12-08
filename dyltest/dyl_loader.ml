module type S = sig
  val call : int -> int
end

module type Options = sig
  val path : string
  val fn_name : string
end

module T = struct
  type fn
  external load : string -> string -> fn  = "caml_load"
  external call : fn -> int -> int = "caml_call"
end

module Make ( O : Options )  = struct
  let call = 
    let fn = T.load O.path O.fn_name in
    T.call fn
end