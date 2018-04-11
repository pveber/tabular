open Core_kernel
open Ppxlib
open Ast_builder.Default

let extension_name = "tabular"

module Struct = struct
  let row_module ~loc =
    module_binding ~loc
      ~name:(Located.mk ~loc "Row")
      ~expr:(pmod_structure ~loc [])

  let generator ~loc ~path:_ (rec_flag, _) = [
    pstr_module ~loc (row_module ~loc) ;
  ]
end

module Sig = struct
  let row_module ~loc =
    module_declaration ~loc
      ~name:(Located.mk ~loc "Row")
      ~type_:(pmty_signature ~loc [])

  let generator ~loc ~path:_ (rec_flag, _) = [
    psig_module ~loc (row_module ~loc) ;
  ]
end

let str_type_decl =
  Deriving.Generator.make_noarg Struct.generator

let sig_type_decl =
  Deriving.Generator.make_noarg Sig.generator

let tabular =
  Deriving.add
    ~str_type_decl
    ~sig_type_decl
    extension_name
