# Experiments

This page describes various experiements with ocaml->erlang interoperability.

## Atoms <-> polymorphic variants
Polymorphic variants are turned into atoms and tuples. E.g. the polymorphic
variant `` `Red `` is converted into the atom `'red'`, and the polymorphic variant
`` `Rgb of int*int*int `` is turned into the tuple `{rgb, {r,g,b}}`.

Here is an example:

```ocaml
type wday = [ `Monday | `Tuesday | `Wednesday | `Thursday | `Friday | `Saturday | `Sunday ]

(* Note the explicit type required to catch pattern matching errors
  when using polymorphic variants instead of regular variants. *)
let day_of_week (d : wday) =
  match d with
  | `Monday    -> 1
  | `Tuesday   -> 2
  | `Wednesday -> 3
  | `Thursday  -> 4
  | `Friday    -> 5
  | `Saturday  -> 6
  | `Sunday    -> 7

(*
$ erl
1> {ok, wednesday} = 'Wdays':int_to_wday('Wdays':day_of_week(wednesday)).
{ok,wednesday}
2> 'Wdays':int_to_wday(8).
error
*)
let int_to_wday = function
  | 1 -> (`Ok `Monday)
  | 2 -> (`Ok `Tuesday)
  | 3 -> (`Ok `Wednesday)
  | 4 -> (`Ok `Thursday)
  | 5 -> (`Ok `Friday)
  | 6 -> (`Ok `Saturday)
  | 7 -> (`Ok `Monday)
  | _ -> `Error
```
