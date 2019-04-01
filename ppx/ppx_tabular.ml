open Core_kernel
open Ppxlib
open Ast_builder.Default

let extension_name = "tabular"

type field_type =
  | Int
  | Float
  | String

type field = {
  field_name : string ;
  field_typ : field_type ;
}

type table_signature = {
  name : string ;
  fields : field list ;
}

module Struct = struct
  let row_module ~loc tsig =
    let contents = [%str
      type nonrec t = t
      let fields = [%e elist ~loc (List.map tsig.fields ~f:(fun fld -> estring ~loc fld.field_name))]
    ] in
    module_binding ~loc
      ~name:(Located.mk ~loc "Row")
      ~expr:(pmod_structure ~loc contents)

  let generator ~loc tsig = [
    pstr_module ~loc (row_module ~loc tsig) ;
  ]
end

module Sig = struct
  let row_module ~loc _tsig  =
    let contents =
      [%sig: type nonrec t = t val fields : string list]
    in
    module_declaration ~loc
      ~name:(Located.mk ~loc "Row")
      ~type_:(pmty_signature ~loc contents)

  let generator ~loc tsig = [
    psig_module ~loc (row_module ~loc tsig) ;
  ]
end

let field_typ_of_core_type = function
  | Ptyp_constr ({ txt = Lident constr ; _ }, _) -> (
      match constr with
      | "int" -> Int
      | "float" -> Float
      | "string" -> String
      | _ -> failwith "unsupported field type"
    )
  | _ -> failwith "unsupported field type"

let field_of_label_declaration (ld : label_declaration) =
  { field_name = ld.pld_name.txt ;
    field_typ = Int }

let with_record_type gen ~loc ~path:_ (_, tds) =
  match tds with
  | [ td ] -> (
      match td.ptype_kind with
      | Ptype_record ld ->
        let tsig = {
          name = td.ptype_name.txt ;
          fields = List.map ~f:field_of_label_declaration ld ;
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
