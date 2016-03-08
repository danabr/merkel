# Interoperability

This page describes how various OCaml concepts translate into the Erlang world.

## Module names
The module `List` in ML, becomes `list` in Erlang. The module name `BumpyCase` becomes
`bumpyCase` in Erlang. The module name `Lower_case` becomes `lower_case` in Erlang.

## Atoms <-> Variants
Variants (regular and polymorphic)  are turned into atoms or tuples. E.g. the polymorphic
variant `` `Red `` is converted into the atom `'red'`, and the variant
`Rgb of int*int*int` is turned into the tuple `{rgb, {r,g,b}}`.

Here is an example:

```ocaml
(* wdays.ml *)
type wday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let day_of_week = function
  | Monday    -> 1
  | Tuesday   -> 2
  | Wednesday -> 3
  | Thursday  -> 4
  | Friday    -> 5
  | Saturday  -> 6
  | Sunday    -> 7

(*
$ erl
1> {ok, wednesday} = wdays:int_to_wday(wdays:day_of_week(wednesday)).
{ok,wednesday}
2> wdays:int_to_wday(8).
error
*)
let int_to_wday = function
  | 1 -> (`Ok Monday)
  | 2 -> (`Ok Tuesday)
  | 3 -> (`Ok Wednesday)
  | 4 -> (`Ok Thursday)
  | 5 -> (`Ok Friday)
  | 6 -> (`Ok Saturday)
  | 7 -> (`Ok Monday)
  | _ -> `Error
```
