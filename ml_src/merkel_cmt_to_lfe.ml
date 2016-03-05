(* .cmt file in -> lfe out *)
open Typedtree

exception ParseError of string

type state = { mod_name: string }

type sexp =
    Atom of string
  | Sexp of sexp list

let atom s =
   Atom ("'" ^ (String.lowercase_ascii s))

let translate_fn_name = function
  | "Pervasives.<=" -> "=<"
  | "Pervasives.==" -> "=:="
  | "Pervasives.="  -> "=:="
  | "Pervasives.+"  -> "+"
  | "Pervasives.-"  -> "-"
  | "Pervasives.*"  -> "*"
  | other           -> other

let var state pat =
  match pat.pat_desc with
  | Tpat_var (id, loc) ->
    Atom (Ident.name id)
  | _                     ->
    raise (ParseError "Not a var")

let id expr =
  match expr.exp_desc with
    | Texp_ident (path, loc, typ) ->
      Atom (translate_fn_name (Path.name path))
    | _                           -> raise (ParseError "Not an id")

let constant = function
  | Asttypes.Const_int i ->
    Atom (string_of_int i)
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
  | Texp_function (arg_label, cases, partial) ->
    let clauses = expand_or_patterns cases in
    let rest = match_clauses state ~wrap_patterns:true clauses in
    Sexp ((Atom "match-lambda") :: rest)
  | Texp_ifthenelse (test, if_true, else_option ) ->
    Sexp [Atom "if"; (expr state test);
          (expr state if_true); (else_expr state else_option)]
  | Texp_let (Nonrecursive, value_bindings, in_expr)  ->
    Sexp [Atom "let*"; Sexp (bindings state value_bindings);
          expr state in_expr]
  (* Ignore exceptions for now. *)
  (* We should probably check for or-patterns here *)
  | Texp_match (eval, cases, [], partial)             ->
    let clauses = expand_or_patterns cases in
    let rest = (match_clauses state ~wrap_patterns:false clauses) in
    Sexp ((Atom "case") :: (expr state eval) :: rest)
  | Texp_tuple exprs                                  ->
    let els = List.map (expr state) exprs in
    Sexp ((Atom "tuple") :: els)
  | Texp_variant (label, e)                           ->
      variant_expr state label e
  | _                                                 ->
    Atom "todo:expr"
and
  else_expr state = function
    | None   -> atom "false"
    | Some e -> expr state e
and
  strict_args state = function
    | []                     -> [];
    | (_, (Some e)) :: exprs ->
      (expr state e) :: (strict_args state exprs)
    | (_, None) :: _         -> raise (ParseError "Missing expression in arg position")
and
  constructor state rest desc =
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
         raise (ParseError ("missing constructor " ^ c))
and
  pattern state pat =
    match pat.pat_desc with
    | Tpat_alias (p, id, loc)                ->
      Sexp [Atom "="; (pattern state p);
            Atom (Ident.name id)]
    | Tpat_any                               ->
      Atom "_"
    | Tpat_constant c                        ->
      constant c
    | Tpat_construct (loc, desc, patterns)   ->
      let patterns = List.map (pattern state) patterns in
      constructor state patterns desc
    | Tpat_or (p1, p2, rowdesc)              ->
      raise (ParseError "or pattern not expanded")
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
  The approach taken here is ad-hoc and inefficient,
  and broken in at least one known case, namely:
    let only_small_lists = function
      | [_] | [_;_] as x  -> x
      | _ -> []
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
        in (simple_expand_or case p make_case p.pat_desc)
      | other                   ->
        let make_case p = {case with c_lhs = p} in
        (simple_expand_or case current_pattern make_case other) @ (expand_or_patterns cases)

let define_function state is_rec vb =
  match vb.vb_expr.exp_desc with
  | Texp_function (arg_label, cases, partial) ->
    (* Note: All functions are single argument functions.
       This gets weird in Erlang land.
    *)
    let clauses = expand_or_patterns cases in
    let rest = match_clauses state ~wrap_patterns:true clauses in
    Sexp ((Atom "defun") :: (var state vb.vb_pat) :: rest)
  | _                                    ->
    raise (ParseError "top level define did not define a function!")

let rec define_functions state is_rec value_bindings =
  List.map (define_function state is_rec) value_bindings

let parse_structure_item state = function
  | Tstr_type (is_rec, typedecls)       ->
    []
  | Tstr_value (is_rec, value_bindings) ->
    define_functions state is_rec value_bindings
  | _                                   ->
    raise (ParseError "parse_structure_item")

let to_lfe state = function
  | (Cmt_format.Implementation impl) ->
    let impl_defs = List.map (fun x -> parse_structure_item state x.str_desc) impl.str_items in
    (Sexp [ Atom "defmodule"; (Atom state.mod_name);
            Sexp [Atom "export"; Atom "all"]]) :: (List.flatten impl_defs)
  | _                                ->
    raise (ParseError "file did not contain implementation")

let rec output_sexp = function
  | Atom str  -> Format.printf " %s " str
  | Sexp sexp ->
    Format.printf "(";
    List.iter output_sexp sexp;
    Format.printf ")"

let () =
  let cmt_file = (Array.get Sys.argv 1) in
  let cmt = Cmt_format.read_cmt cmt_file in
  let state = { mod_name = cmt.cmt_modname } in
  let sexp = to_lfe state cmt.cmt_annots in
  List.iter output_sexp sexp;
  Format.printf "\n"

