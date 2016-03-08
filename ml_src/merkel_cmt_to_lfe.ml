(* .cmt file in -> lfe out *)
(* This module, like all the code is this project, is a hack.
   Its only purpose is to aid experimentation and exploration.
   This means that any hacka is allowed. Modularity, tests or
   beauty are not concerns.
*)
open Typedtree

exception Error of string

type record = { name: string;
                fields: string list
              }

type state = { mod_name: string;
               records: record list
             }

type sexp =
    Atom of string
  | Sexp of sexp list

let atom s =
   Atom ("'" ^ (String.uncapitalize_ascii s))

(* Pervasives is quite comprehensive. Perhaps it would be better
   to ship with our own Pervasives that maps to the corresponding
   erlang functions.
*)
let translate_fn_name = function
  | "Pervasives.>"   -> ">"
  | "Pervasives.>="  -> ">="
  | "Pervasives.=="  -> "=:="
  | "Pervasives.="   -> "=:="
  | "Pervasives.<="  -> "=<"
  | "Pervasives.<"   -> "<"
  | "Pervasives.+"   -> "+"
  | "Pervasives.+."  -> "+"
  | "Pervasives.-"   -> "-"
  | "Pervasives.-."  -> "-"
  | "Pervasives.*"   -> "*"
  | "Pervasives.*."  -> "*"
  | "Pervasives./"   -> "div"
  | "Pervasives./."  -> "/"
  | "Pervasives.mod" -> "rem"
  | "Pervasives.&&"  -> "andalso"
  | "Pervasives.||"  -> "orelse"
  | "Pervasives.not" -> "not"
  | "Pervasives.@"   -> "++"
  | other            -> other

let var state pat =
  match pat.pat_desc with
  | Tpat_var (id, loc) ->
    Atom (Ident.name id)
  | _                     ->
    raise (Error "Not a var")

let id expr =
  match expr.exp_desc with
    | Texp_ident (path, loc, typ) ->
      Atom (translate_fn_name (Path.name path))
    | _                           -> raise (Error "Not an id")

let constant = function
  | Asttypes.Const_int i            ->
    Atom (string_of_int i)
  | Asttypes.Const_float f          ->
    Atom (f ^ "0") (* handle trailing '.' *)
  (* TODO: Should strings be handled as lists or binaries? *)
  | Asttypes.Const_string (s, None) ->
    Atom ("\"" ^ s ^ "\"")
  | _           ->
    Atom "constant_not_implemented"

let rec expr state e =
  match e.exp_desc with
  (* Note: optional arguments make things tricky *)
  | Texp_apply (fn, args)                             ->
    Sexp (id(fn) :: (strict_args state args))
  | Texp_constant c                                   ->
    constant c
  | Texp_construct (loc, desc, exprs)                 ->
    let exprs = List.map (expr state) exprs in
    constructor state exprs desc
  | Texp_ident (path, loc, typ)                       ->
    Atom (translate_fn_name (Path.name path))
  | Texp_for _                                        ->
    raise (Error "imperative construct (for) not supported")
  | Texp_function (arg_label, cases, partial) ->
    let clauses = expand_or_patterns cases in
    let rest = match_clauses state ~wrap_patterns:true clauses in
    Sexp ((Atom "match-lambda") :: rest)
  | Texp_ifthenelse (test, if_true, else_option ) ->
    Sexp [Atom "if"; (expr state test);
          (expr state if_true); (else_expr state else_option)]
  | Texp_let (Asttypes.Nonrecursive, value_bindings, in_expr)  ->
    Sexp [Atom "let*"; Sexp (bindings state value_bindings);
          expr state in_expr]
  (* Ignore exceptions for now. *)
  | Texp_match (eval, cases, [], partial)             ->
    let clauses = expand_or_patterns cases in
    let rest = (match_clauses state ~wrap_patterns:false clauses) in
    Sexp ((Atom "case") :: (expr state eval) :: rest)
  | Texp_record (fields, None)                        ->
    make_record state fields
  | Texp_tuple exprs                                  ->
    let els = List.map (expr state) exprs in
    Sexp ((Atom "tuple") :: els)
  | Texp_variant (label, e)                           ->
      variant_expr state label e
  | Texp_while (cond, body)                           ->
    raise (Error "imperative construct (while) not supported")
  | _                                                 ->
    Atom "todo:expr"
and
  find_record records (_, label, _) =
    let open Types in
    let needle = (label : label_description).lbl_name in
    let predicate r = List.mem needle r.fields in
    List.find predicate records
and
  sort_record_fields field_exprs = function
    | []                        -> []
    | field_name :: field_names ->
      let open Types in
      let predicate (_, (dscr : label_description), _) = dscr.lbl_name = field_name in
      let next = List.find predicate field_exprs in
      next :: (sort_record_fields field_exprs field_names)
and
make_record state fields =
  let record = find_record state.records (List.hd fields) in
  let sorted_fields = sort_record_fields fields record.fields in
  let fields_sexp = List.map (fun (_, _, e) -> expr state e) sorted_fields in
  Sexp ((Atom "tuple") :: (atom record.name) :: fields_sexp)
and
  else_expr state = function
    | None   -> atom "false"
    | Some e -> expr state e
and
  strict_args state = function
    | []                     -> [];
    | (_, (Some e)) :: exprs ->
      (expr state e) :: (strict_args state exprs)
    | (_, None) :: _         -> raise (Error "Missing expression in arg position")
and
  constructor state rest desc =
    let open Types in
    match (desc.cstr_name, desc.cstr_res.desc, rest) with
      | "false", _, _            -> Atom "'false"
      | "true", _, _             -> Atom "'true"
      | "[]", _, _               -> Atom "()"
      | "::", _, _               ->
        Sexp ((Atom "cons") :: rest)
      (* Treat variants as atoms for now *)
      | c, (Types.Tconstr _), [] ->
        atom c
      | c, (Types.Tconstr _), __ ->
        Sexp ((Atom "tuple" :: (atom c) :: rest))
      | c, _, _                  ->
         raise (Error ("missing constructor " ^ c))
and
  pattern state pat =
    match pat.pat_desc with
    | Tpat_alias (p, id, loc)                ->
      Sexp [Atom "="; (pattern state p);
            Atom (Ident.name id)]
    | Tpat_any                               ->
      Atom "_"
    (* HACK! to handle multi-argument functions *)
    | Tpat_array patterns                    ->
      Sexp (List.map (pattern state) patterns)
    | Tpat_constant c                        ->
      constant c
    | Tpat_construct (loc, desc, patterns)   ->
      let patterns = List.map (pattern state) patterns in
      constructor state patterns desc
    | Tpat_or (p1, p2, rowdesc)              ->
      raise (Error "or pattern not expanded")
    | Tpat_tuple patterns                    ->
      let elements = List.map (pattern state) patterns in
      Sexp ((Atom "tuple") :: elements)
    | Tpat_var (id, loc)                     ->
      Atom (Ident.name id)
    | Tpat_variant (label, pattern, rowdesc) ->
      variant_pattern state label pattern rowdesc
    | _                                      ->
      Atom "pattern_not_implemented"
and
  variant_pattern state label p rowdesc =
    let atom = atom label in
    match p with
      | None   ->  atom
      | Some p ->
        Sexp [Atom "tuple"; atom; (pattern state p)]
and
  variant_expr state label e =
    let atom = atom label in
    match e with
      | None   ->  atom
      | Some e ->
        Sexp [Atom "tuple"; atom; (expr state e)]
and
  bindings state = function
    | []                      -> []
    | {vb_pat; vb_expr} :: bs ->
      (Sexp [pattern state vb_pat; expr state vb_expr]) :: bindings state bs
and
  match_clauses state ~wrap_patterns = function
    | []            -> [];
    | case :: cases ->
      let lhs = pattern state case.c_lhs in
      let lhs_sexp = if wrap_patterns then (Sexp [lhs]) else lhs in
      let rhs = expr state case.c_rhs in
      let rest = (match_clauses state ~wrap_patterns cases) in
      match case.c_guard with
      | None       ->
          (Sexp [lhs_sexp; rhs]) :: rest
      | Some guard ->
          let wh = Sexp [Atom "when"; (expr state guard)] in
          (Sexp [lhs_sexp; wh; rhs]) :: rest
and
(*
  OCaml has or-pattern, which lfe/erlang does not support.
  All clauses containing or patterns must be duplicated.
  The approach taken here is ad-hoc and inefficient.
  We should inspect all patterns that may contain other patterns.
  List.flatmap (expand_case current_case) cases
*)
  flatten_or p =
    match p.pat_desc with
    | Tpat_or (p1, p2, _) -> (flatten_or p1) @ (flatten_or p2)
    | _                   -> [p]
and
  simple_expand_or case pattern make_case = function
    | Tpat_or _ ->
        let ps = flatten_or pattern in
        List.map make_case ps
    | _         -> (* not suspectible to or patterns, we hope...*)
      [case]
and
  expand_or_patterns = function
    | []            -> [];
    | case :: cases ->
      let current_pattern = case.c_lhs in
      match current_pattern.pat_desc with
      | Tpat_alias (p, id, loc) ->
        let make_case p =
          let newp = {current_pattern with pat_desc = Tpat_alias (p, id, loc)} in
          {case with c_lhs = newp}
        in (simple_expand_or case p make_case p.pat_desc) @ (expand_or_patterns cases)
      | other                   ->
        let make_case p = {case with c_lhs = p} in
        (simple_expand_or case current_pattern make_case other) @ (expand_or_patterns cases)

let rec decurry patterns case =
    match case.c_rhs.exp_desc with
      | Texp_function (_, [subcase], _) -> (* Next arg *)
        decurry (case.c_lhs :: patterns) subcase
      | Texp_function (_, cases, _)     ->
        let new_case c =
          let old = c.c_lhs in
          let pattern = Tpat_array (List.rev (old :: patterns)) in
          let new_lhs = {c.c_lhs with pat_desc = pattern } in
          { c with c_lhs = new_lhs }
        in
        List.map new_case cases
      | _                               -> (* Implementation *)
        let pattern = Tpat_array (List.rev (case.c_lhs :: patterns)) in
        let new_lhs = {case.c_lhs with pat_desc = pattern } in
        [{ case with c_lhs = new_lhs }]
and
  define_function state is_rec vb =
    match vb.vb_expr.exp_desc with
    | Texp_function (arg_label, cases, partial) ->
      (* Note: All functions are single argument functions.
         This gets weird in Erlang land.
      *)
      let decurried = List.flatten (List.map (decurry []) cases) in
      let clauses = expand_or_patterns decurried in
      let rest = match_clauses state ~wrap_patterns:false clauses in
      Sexp ((Atom "defun") :: (var state vb.vb_pat) :: rest)
    | _                                    ->
      raise (Error "top level define did not define a function!")

let rec define_functions state is_rec value_bindings =
  List.map (define_function state is_rec) value_bindings

let add_records state0 types =
  let open Types in
  match types with
  | []                                 -> state0
  | {typ_id; typ_type} :: declarations ->
    match typ_type.type_kind with
      | Type_record (fields, _) ->
        let field_names = List.map (fun f -> Ident.name f.ld_id) fields in
        let record = { name=(Ident.name typ_id); fields = field_names } in
        let records = record :: state0.records in
        {state0 with records = records }
      | _                       -> state0

let parse_structure_item state0 = function
  | Tstr_type (is_rec, typedecls)       ->
    let state = add_records state0 typedecls in
    (state, [])
  | Tstr_value (is_rec, value_bindings) ->
    (state0, define_functions state0 is_rec value_bindings)
  | _                                   ->
    raise (Error "parse_structure_item")

let rec parse_structure_items state0 code = function
  | []            -> List.rev code
  | item :: items ->
    let (state, new_code) = parse_structure_item state0 item.str_desc in
    parse_structure_items state (new_code :: code) items

let to_lfe state = function
  | (Cmt_format.Implementation impl) ->
    let impl_defs = parse_structure_items state [] impl.str_items in
    (Sexp [ Atom "defmodule"; (Atom state.mod_name);
            Sexp [Atom "export"; Atom "all"]]) :: (List.flatten impl_defs)
  | _                                ->
    raise (Error "file did not contain implementation")

let rec output_sexp = function
  | Atom str  -> Format.printf " %s " str
  | Sexp sexp ->
    Format.printf "(";
    List.iter output_sexp sexp;
    Format.printf ")"

let () =
  let open Cmt_format in
  let cmt_file = (Array.get Sys.argv 1) in
  let cmt = read_cmt cmt_file in
  let state = { mod_name = cmt.cmt_modname;
                records = []
              } in
  let sexp = to_lfe state cmt.cmt_annots in
  List.iter output_sexp sexp;
  Format.printf "\n"

