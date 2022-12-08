module type S = sig
  val call : int -> int
end

module type Options = sig
  val path : string
  val fn_name : string
end

module Make ( O : Options ) : S