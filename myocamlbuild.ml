(* OASIS_START *)
(* OASIS_STOP *)
open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             (* Internal syntax extension *)
             flag ["ocaml"; "compile"; "pa_table"] & S[A"-ppopt"; A "src/table.cma"; A"-ppopt"; A "src/table_syntax.cma"];
             flag ["ocaml"; "ocamldep"; "pa_table"] & S[A"-ppopt"; A "src/table.cma"; A"-ppopt"; A "src/table_syntax.cma"];
             flag ["ocaml"; "doc"; "pa_table"] & S[A"-ppopt"; A "src/table.cma"; A"-ppopt"; A "src/table_syntax.cma"];
             dep ["ocaml"; "ocamldep"; "pa_table"] ["src/table_syntax.cma"]
         | _ -> ())
