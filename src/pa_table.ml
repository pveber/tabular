open Printf
open Camlp4.PreCast
open Syntax
open Table_lib

exception Bad_format

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

let poly_of_string _loc poly l =
  let cases =
    List.fold_right
      (fun (id, label) accu -> 
	let e =
	  if poly then <:expr< ` $id$ >>
	  else <:expr< $uid:id$ >> in
	<:match_case< $str: String.escaped label$ -> $e$ | $accu$ >>)
      l <:match_case< _ -> failwith "Bad format while reading variant" >> 
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
  | _ -> failwith "string_of_ident"
 
(* provides a code generation-friendly representation of the type of a column *)
let convert_col_type _loc (typ, opt) =
  let conv = match typ with
    | `String ->
      (<:ctyp< string >>,
       <:expr< Table_lib.id >>,
       <:expr< Table_lib.id >>,
       Some "string",
       <:expr< String.compare >>)

    | `Bool ->
      (<:ctyp< bool >>,
       <:expr< bool_of_string >>,
       <:expr< string_of_bool >>,
       Some "bool",
       <:expr< Pervasives.compare >>)

    | `Poly_var l ->
      (poly_type _loc true l,
       poly_of_string _loc true l,
       poly_to_string _loc true l,
       None,
       <:expr< fun a b ->
	 Pervasives.compare
	   ($poly_to_int _loc true l$ a)
	   ($poly_to_int _loc true l$ b) >>)

    | `Int ->
      (<:ctyp< int >>,
       <:expr< int_of_string >>,
       <:expr< string_of_int >>,
       Some "int",
       <:expr< Pervasives.compare >>)

    | `Float ->
      (<:ctyp< float >>,
       <:expr< float_of_string >>,
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

(* adds the index of each field in the list of field defined by the
   type *)
let add_index l =
  List.mapi
    (fun i (_loc,name,label,typ) -> _loc, i, name, label, typ)
    l

(*
 ******************
 * ROW CONVERSION *
 ******************
 *)

let row_record_fields _loc l =
  List.fold_right
    (fun (_loc, _, name, _, typ) accu -> 
      let field = <:ctyp< $lid:name$ : $typ#t$ >> in
      <:ctyp< $field$ ; $accu$ >>)
    l <:ctyp<>>  

let row_of_array _loc l =
  let fields = 
    List.fold_right
      (fun (_loc, index, name, _, typ) accu -> 
	<:rec_binding< $lid:name$ = $typ#of_string$ a.($`int:index$) ; $accu$ >>)
      l <:rec_binding<>>
  in
  <:str_item<value row_of_array a = { $fields$ };>>

let array_of_row _loc l =
  let elts = 
    List.fold_right
      (fun (_loc, index, name, _, typ) accu -> 
	<:expr< $typ#to_string$ row.$lid:name$ ; $accu$ >>)
      l <:expr<>>
  in
  <:str_item<value array_of_row row = [| $elts$ |];>>
    
let list_of_row _loc l =
  let elts = 
    List.fold_right
      (fun (_loc, index, name, _, typ) accu -> 
	<:expr< [ ($typ#to_string$ row.$lid:name$) :: $accu$ ] >>)
      l <:expr< [] >>
  in
  <:str_item<value list_of_row row = $elts$;>>


let table_class_type_methods _loc l =
  let init = <:class_sig_item< 
    method row : int -> row;
    method sub : array bool -> table;
    method length : int;
    method labels : list string;
    method iter : (row -> unit) -> unit;
    method stream : Stream.t row;
  >>
  in
  List.fold_right
    (fun (_loc, _, name, _, typ) accu ->
      <:class_sig_item<method $lid:name$ : array $typ#t$; $accu$>>)
    l init

let labels_item _loc l =
  let labels = 
    List.fold_right
      (fun (_loc, _, _, label, _) accu -> 
	<:expr< [ $str:label$ :: $accu$ ] >>)
      l <:expr< [] >>
  in
  <:str_item<value labels = $labels$;>>
    
let table_object_row_method _loc l =
  let body =
    List.fold_right
      (fun (_loc, _, name, _, typ) accu ->
	<:rec_binding< $lid:name$ = $lid:name$.(i) ; $accu$>>)
      l <:rec_binding<>>
  in
  <:class_str_item<method row i = { $body$ };>>

let table_object_sub_method _loc = function
  | [] -> failwith "table_object_sub_method" (* FIXME: ensure elsewhere that this cannot happen*)
  | (_loc, _, name, _, _) :: _ as l ->
    let make_call =
      List.fold_left
	(fun accu (_loc, _, name, _, _) ->
	  <:expr< $accu$ ~ $lid:name$ : (Table_lib.Array.sub $lid:name$ b) >>)
	<:expr< make >> l
    in
    let body =
      <:expr<
	if Array.length $lid:name$ <> Array.length b
	then raise (Invalid_argument "table#sub")
	else $make_call$
      >>
    in
    <:class_str_item<method sub b = $body$;>>

let table_object_length_method _loc = function
  | [] -> failwith "table_object_length_method" (* FIXME: ensure elsewhere that this cannot happen*)
  | (_loc, _, name, _, _) :: _ ->
      <:class_str_item<method length = Array.length $lid:name$;>>

let table_object_labels_method _loc l =
  <:class_str_item<method labels = labels;>>

let table_object_methods _loc l =
  let init = <:class_str_item<
$table_object_row_method _loc l$;
$table_object_sub_method _loc l$;
$table_object_length_method _loc l$;
$table_object_labels_method _loc l$;
method iter f = for i = 0 to self#length - 1 do f (self#row i) done;
method stream = Table_lib.Stream.init self#length self#row
  >>
  in
  List.fold_right
    (fun (_loc, _, name, _, _) accu ->
      <:class_str_item<method $lid:name$ = $lid:name$; $accu$>>)
    l init

let table_make_str_item _loc l =
  let def = match l with
    | [] -> 
      <:expr<
        object (s)
	  method row i = raise (Invalid_argument "table#row: empty table");
	  method sub _ = s;
	end
      >>
    | (_loc, _, name, _, _) :: t as l ->
      let arg_check e =
	List.fold_right
	  (fun (_loc, _, name2, _, _) accu ->
	    <:expr<
	      if Array.length $lid:name$ <> Array.length $lid:name2$
	      then raise (Invalid_argument (Printf.sprintf "table#make: col %s and %s have different length" $str:name$ $str:name2$))
	      else $accu$
	    >>)
	  t e
      in
      let init = 
	arg_check <:expr<object (self) $table_object_methods _loc l$ end>> 
      in
      List.fold_right
	(fun (_loc, _, name, _, _) accu ->
	<:expr< fun ~ $name$ -> $accu$ >>)
	l init
  in
  <:str_item<value rec make = $def$;>>

let table_of_stream_body _loc l =
  let fun_call =
    List.fold_right
      (fun (_loc, _, name, _, _) accu ->
        <:expr< $accu$ ~ $name$ : (Array.map (fun row -> row.$lid:name$) rows) >>)
      l <:expr<make>>
  in
  <:expr<
    let rows = Table_lib.Stream.to_array xs in
    $fun_call$
  >>

let expand_table_sig _loc name l =
  <:sig_item<
module $uid:String.capitalize name$ : sig
  type row = { $row_record_fields _loc l$ };
  class type table = object
      $table_class_type_methods _loc l$
  end;
  include Table.S with type row := row
                  and type table := table;
end
  >>

let expand_table_str _loc name l =
  <:str_item<
module $uid:String.capitalize name$ = struct
  module T = struct
    type row = { $row_record_fields _loc l$ };
    class type table = object
        $table_class_type_methods _loc l$
    end;
    $labels_item _loc l$;
    $row_of_array _loc l$;
    $list_of_row _loc l$;
    $array_of_row _loc l$;
    $table_make_str_item _loc l$;
    value table_of_stream xs = $table_of_stream_body _loc l$;
  end;
  include T;
  include Table_lib.Impl(T);
end
>>


EXTEND Gram
  GLOBAL: sig_item str_item;

  sig_item: LEVEL "top" [
    [ "type" ; LIDENT "table"; name = LIDENT; "="; 
      "{"; l = col_list; "}" -> expand_table_sig _loc name (add_index l)]
  ];

  str_item: LEVEL "top" [
    [ "type" ; LIDENT "table"; name = LIDENT; "="; 
      "{"; l = col_list; "}" -> expand_table_str _loc name (add_index l)]
  ];

  (* variants: [ *)
  (*   [ l = poly_var -> (true, l) *)
  (*   | l = mono_var -> (false, l) ] *)
  (* ]; *)

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

  (* mono_var: [ *)
  (*   [ OPT "|"; *)
  (*     l =  *)
  (*       LIST1 [  *)
  (*         id = [ id = UIDENT -> (_loc, id) ]; *)
  (*         label = OPT [ s = STRING -> (_loc, eval_string s) ] -> *)
  (*         (id, Option.value ~default:id label)  *)
  (*       ]  *)
  (*       SEP "|" -> *)
  (*       check_unique fst l; *)
  (*       check_unique snd l; *)
  (*       List.map (fun ((_, a), (_, b)) -> (a, b)) l ] *)
  (* ]; *)



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
