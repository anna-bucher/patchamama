module C = Configurator.V1

let packages = [("portmidi", "2.0.3")]

let () =
  C.main ~name:"js-pkg-config" (fun c ->
      let default : C.Pkg_config.package_conf =
        {
          libs = List.map (fun (p, _) -> Printf.sprintf "-l%s" p) packages;
          cflags = [];
        }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc -> (
            let package =
              String.concat " "
                (List.map (fun (p, _) -> Printf.sprintf "%s" p) packages)
            in
            let expr =
              String.concat ", "
                (List.map
                   (fun (p, v) -> Printf.sprintf "%s >= %s" p v)
                   packages)
            in
            match C.Pkg_config.query_expr_err pc ~package ~expr with
            | Error msg -> failwith msg
            | Ok deps -> deps)
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
