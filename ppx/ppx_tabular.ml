open Core_kernel
open Ppxlib
open Ast_builder.Default

let extension_name = "tabular"

type table_signature = {
  name : string ;
  fields : label_declaration list ;
}

module Struct = struct
  let row_module ~loc tsig =
    let td =
      type_declaration ~loc
        ~name:(Located.mk ~loc "t")
        ~params:[]
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some (ptyp_constr ~loc (Located.mk ~loc (Longident.(parse tsig.name))) []))
    in
    let contents = [
      pstr_type ~loc Nonrecursive [ td ] ;
    ] in
    module_binding ~loc
      ~name:(Located.mk ~loc "Row")
      ~expr:(pmod_structure ~loc contents)

  let generator ~loc tsig = [
    pstr_module ~loc (row_module ~loc tsig) ;
  ]
end

module Sig = struct
  let row_module ~loc tsig  =
    let td =
      type_declaration ~loc
        ~name:(Located.mk ~loc "t")
        ~params:[]
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some (ptyp_constr ~loc (Located.mk ~loc (Longident.(parse tsig.name))) []))
    in
    let contents = [
      psig_type ~loc Nonrecursive [ td ] ;
    ] in
    module_declaration ~loc
      ~name:(Located.mk ~loc "Row")
      ~type_:(pmty_signature ~loc contents)

  let generator ~loc tsig = [
    psig_module ~loc (row_module ~loc tsig) ;
  ]
end

let with_record_type gen ~loc ~path:_ (_, tds) =
  match tds with
  | [ td ] -> (
      match td.ptype_kind with
      | Ptype_record ld ->
        let tsig = {
          name = td.ptype_name.txt ;
          fields = ld ;
        }
        in
        gen ~loc tsig
      | _ -> failwith "tabular expect a record type"
    )
  | _ -> failwith "tabular expects a single type"

let str_type_decl =
  Deriving.Generator.make_noarg (with_record_type Struct.generator)

let sig_type_decl =
  Deriving.Generator.make_noarg (with_record_type Sig.generator)

let tabular =
  Deriving.add
    ~str_type_decl
    ~sig_type_decl
    extension_name
