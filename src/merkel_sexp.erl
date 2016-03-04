-module(merkel_sexp).

-export([to_string/1]).

to_string([])   -> error(badarg);
to_string(Sexp) ->
  lists:flatten(str(Sexp)).

str(X) when is_list(X) ->
  Sub = [str(E) || E <- X],
  [$(, string:join(Sub, " "), $)];
str(X)        -> to_list(X).

to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_float(X)   -> float_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X).
