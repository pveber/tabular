open Printf
open Camlp4.PreCast
open Syntax
open Table_lib

(* checks injectivity of [f] on [l] *)
let check_unique f l =
  let tbl = Hashtbl.create 50 in
  List.iter
    (fun x ->
      let (_loc, id) = f x in
      if Hashtbl.mem tbl id then
	Loc.raise _loc(Failure "this tag or label is not unique")
      else Hashtbl.add tbl id ())
    l

let mod_ctyp (_loc, l) = 
  List.fold_left 
    (fun m id ->
      <:ctyp< $id: Ast.ident_of_ctyp m$ . $uid:id$ >>)
    <:ctyp< $uid:List.hd l$ >> 
    (List.tl l)

let mod_expr (_loc, l) = 
  List.fold_left 
    (fun m id -> <:expr< $m$ . $uid:id$ >>)
    <:expr< $uid:List.hd l$ >> 
    (List.tl l)

let mod_name (_loc, l) =
  match l with
  | [s] -> Some s
  | _ -> None

(*
 ********************************
 * CODE GENERATION FOR VARIANTS *
 ******************************** 
*)
let poly_type _loc poly l =
  if poly then
    let cases =
      List.fold_right
	(fun (id, label) accu ->
	  <:ctyp< `$id$ | $accu$ >>) 
	l <:ctyp< >> 
    in
    <:ctyp< [ = $cases$ ] >>
  else
    let cases =
      List.fold_right
	(fun (id, label) accu ->
	  <:ctyp< $uid:id$ | $accu$ >>) 
	l <:ctyp< >> 
    in
    <:ctyp< [ $cases$ ] >>

let poly_of_string _loc poly l e =
  let cases =
    List.fold_right
      (fun (id, label) accu -> 
	let e =
	  if poly then <:expr< ` $id$ >>
	  else <:expr< $uid:id$ >> in
	<:match_case< $str: String.escaped label$ -> $e$ | $accu$ >>)
      l <:match_case< _ -> raise $uid:e$ >> 
  in
  <:expr< fun [ $cases$ ] >>

let poly_to_string _loc poly l =
  let cases =
    List.fold_right
      (fun (id, label) accu -> 
	 let p =
	   if poly then <:patt< ` $id$ >> 
	   else <:patt< $uid:id$ >> in
	 <:match_case< $p$ -> $str: String.escaped label$ | $accu$ >>)
      l <:match_case< >> 
  in
  <:expr< fun [ $cases$ ] >>

let poly_to_int _loc poly l =
  let n, cases =
    List.fold_left 
      (fun (n, cases) (id, label) -> 
	 let p = 
	   if poly then <:patt< ` $id$ >> 
	   else <:patt< $uid:id$ >> in 
	 (n + 1,
	  <:match_case< $cases$ | $p$ -> $int: string_of_int n$ >>))
      (0, <:match_case< >>) l in
  <:expr< fun [ $cases$ ] >>

let poly_of_int _loc poly l e =
  let n, cases =
    List.fold_left 
      (fun (n, l) (id, cases) -> 
	 let e =
	   if poly then <:expr< ` $id$ >>
	   else <:expr< $uid:id$ >> in
	 (n + 1,
	  <:match_case< $int:string_of_int n$ -> $e$ | $l$ >>))
      (0, <:match_case< >>) l in
  <:expr< fun [ $cases$ | _ -> raise $uid:e$ ] >>

(* string normalization *)
let eval_string s = Camlp4.Struct.Token.Eval.string ~strict:() s

let rec string_of_ident = function
  | <:ident< $lid:s$ >> -> s
  | <:ident< $uid:s$ >> -> s
  | _ -> assert false
 
(* provides a code generation-friendly representation of the type of a column *)
let convert_col_type _loc (typ, opt) =
  let conv = match typ with
    | `String ->
      (<:ctyp< string >>,
       <:expr< Table_utils.id >>,
       <:expr< Table_utils.id >>,
       Some "string",
       <:expr< String.compare >>)

    | `Bool ->
      (<:ctyp< bool >>,
       <:expr< Table_utils.bool_of_string Bad_format >>,
       <:expr< string_of_bool >>,
       Some "bool",
       <:expr< Pervasives.compare >>)

    | `Poly_var l ->
      (poly_type _loc true l,
       poly_of_string _loc true l "Bad_format",
       poly_to_string _loc true l,
       None,
       <:expr< fun a b ->
	 Pervasives.compare
	   ($poly_to_int _loc true l$ a)
	   ($poly_to_int _loc true l$ b) >>)

    | `Int ->
      (<:ctyp< int >>,
       <:expr< Table_lib.int_of_string Bad_format >>,
       <:expr< string_of_int >>,
       Some "int",
       <:expr< Pervasives.compare >>)

    | `Float ->
      (<:ctyp< float >>,
       <:expr< Table_lib.float_of_string Bad_format >>,
       <:expr< string_of_float >>,
       Some "float",
       <:expr< Pervasives.compare >>)

    | `Module m ->
      let m_ctyp = mod_ctyp m
      and m_expr = mod_expr m
      and m_name = mod_name m in
      (<:ctyp< $id:Ast.ident_of_ctyp m_ctyp$ . t >>,
       <:expr< $m_expr$ . of_string >>,
       <:expr< $m_expr$ . to_string >>,
       m_name,
       <:expr< $m_expr$.compare >>) in

  let add_suffix = function
      None -> None
    | Some s -> Some (s ^ "_option") 
  in

  let compare_opt cmp =
    <:expr< Table_lib.compare_opt $cmp$ >> 
  in

  let t, of_s, to_s, s, cmp =
    if not opt then conv
    else
      let (t, of_s, to_s, s, cmp) = conv in
      (<:ctyp< option $t$ >>,
	 <:expr< Table_lib.Option.of_string $of_s$ >>,
	 <:expr< Table_lib.Option.map ~f:$to_s$ >>,
	 add_suffix s,
	 compare_opt cmp)

  in
  (object
    method t = t
    method of_string = of_s
    method to_string = to_s
    method name = s
    method compare = cmp
   end)



let record_row_type_fields _loc l =
  List.fold_right
    (fun (_loc, name, label, typ) accu -> 
      let field = <:ctyp< $lid:name$ : $typ#t$ >> in
      <:ctyp< $field$ ; $accu$ >>)
    l <:ctyp<>>  

let expand_table_sig _loc name l =

  <:sig_item<
module $uid:String.capitalize name$ : sig
type row = { $record_row_type_fields _loc l$ };
end
  >>


EXTEND Gram
  GLOBAL: sig_item;

  sig_item: LEVEL "top" [
    [ "type" ; LIDENT "table"; name = LIDENT; "="; 
      "{"; l = col_list; "}" -> expand_table_sig _loc name l]
  ];

  variants: [
    [ l = poly_var -> (true, l)
    | l = mono_var -> (false, l) ]
  ];

  poly_var: [
    [ "["; 
      l = 
	LIST1 [ 
	  "`"; 
	  id = [ id = ident -> (_loc, string_of_ident id) ];
	  label = OPT [ s = STRING -> (_loc, eval_string s) ] ->
	  (id, Option.value ~default:id label) 
	] 
	SEP "|";
      "]" ->
      check_unique fst l;
      check_unique snd l;
      List.map (fun ((_, a), (_, b)) -> (a, b)) l ]
  ];

  mono_var: [
    [ OPT "|";
      l = 
	LIST1 [ 
	  id = [ id = UIDENT -> (_loc, id) ];
	  label = OPT [ s = STRING -> (_loc, eval_string s) ] ->
	  (id, Option.value ~default:id label) 
	] 
	SEP "|" ->
        check_unique fst l;
        check_unique snd l;
	List.map (fun ((_, a), (_, b)) -> (a, b)) l ]
  ];



  col: [
    [ name = LIDENT; 
      label = OPT [ s = STRING -> eval_string s ];
      typopt = 
	OPT [ ":"; 
	      typ = [ LIDENT "string" -> `String
		    | LIDENT "bool" -> `Bool
		    | l = poly_var -> `Poly_var l
		    | LIDENT "int" -> `Int
		    | LIDENT "float" -> `Float
		    | m = LIST1 [ x = UIDENT -> x ] SEP "." -> 
			`Module (_loc, m) ];
	      o = OPT [ LIDENT "option" ]
		  -> (typ, o <> None) ] -> 
	let typ =
	  match typopt with 
	      None -> (`String, false )
	    | Some x -> x in
	(_loc,
	 name,
	 Option.value label ~default:name,
	 convert_col_type _loc typ) ]
  ];

  col_list: [
    [ x = col; ";"; l = SELF -> x :: l
    | x = col; ";" -> [x]
    | x = col -> [x] ]
  ];

END
