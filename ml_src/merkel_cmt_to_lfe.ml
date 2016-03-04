(* .cmt file in -> lfe out *)
open Typedtree

exception ParseError of string

type state = { mod_name: string }

type sexp =
    Atom of string
  | Sexp of sexp list

let translate_fn_name = function
  | "Pervasives.<=" -> "=<"
  | "Pervasives.+" -> "+"
  | "Pervasives.-" -> "-"
  | "Pervasives.*" -> "*"
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
  | Texp_apply (fn, args)                            ->
    Sexp (id(fn) :: (strict_args state args))
  | Texp_constant c                                  ->
    constant c
  | Texp_construct (loc, desc, exprs)                ->
    constructor_exprs state exprs desc.cstr_name
  | Texp_ident (path, loc, typ)                      ->
    Atom (translate_fn_name (Path.name path))
  | Texp_ifthenelse (test, if_true, (Some if_false)) ->
    Sexp [Atom "if"; (expr state test);
          (expr state if_true); (expr state if_false)]
  | _                                                ->
    Atom "todo:expr"
and
  strict_args state = function
  | []                     -> [];
  | (_, (Some e)) :: exprs ->
    (expr state e) :: (strict_args state exprs)
  | (_, None) :: _         -> raise (ParseError "Missing expression in arg position")
and
  constructor_exprs state exprs = function
  | "false" -> Atom "'false"
  | "true"  -> Atom "'true"
  | _       -> Atom "constructor_expression_not_implemented"

let rec pattern state pat =
  match pat.pat_desc with
  | Tpat_any                             ->
    Atom "_"
  | Tpat_constant c                      ->
    constant c
  | Tpat_construct (loc, desc, patterns) ->
    constructor_pattern state patterns desc.cstr_name
  | Tpat_var (id, loc)                   ->
    Atom (Ident.name id)
  | _                                    ->
    Atom "pattern_not_implemented"
and
  constructor_pattern state patterns = function
  | "[]" -> Atom "()"
  | "::" ->
    let patterns = List.map (pattern state) patterns in
    Sexp ((Atom "cons") :: patterns)
  | _    -> Atom "constructor_pattern_not_implemented"


let rec function_clauses state = function
  | []            -> [];
  | case :: cases ->
    let lhs = pattern state case.c_lhs in
    let rhs = expr state case.c_rhs in
    match case.c_guard with
    | None       ->
        (Sexp [ Sexp [lhs]; rhs]) :: (function_clauses state cases)
    | Some guard ->
        let wh = Sexp [Atom "when"; (expr state guard)] in
        (Sexp [ Sexp [lhs]; wh; rhs]) :: (function_clauses state cases)

let define_function state is_rec vb =
  match vb.vb_expr.exp_desc with
  | Texp_function (arg_label, cases, partial) ->
    (* Note: All functions are single argument functions *)
    Sexp ((Atom "defun") :: (var state vb.vb_pat) :: (function_clauses state cases))
  | _                                    ->
    raise (ParseError "top level define did not define a function!")

let rec define_functions state is_rec value_bindings =
  List.map (define_function state is_rec) value_bindings

let parse_structure_item state = function
  | Tstr_value (is_rec, value_bindings) ->
    define_functions state is_rec value_bindings
  | _ ->
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

