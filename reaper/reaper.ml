module T = struct
  external test : unit -> unit = "caml_reaper_test"
end

let test = T.test
