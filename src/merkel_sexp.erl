-module(merkel_sexp).

-export([to_string/1]).

to_string(Sexp) when is_list(Sexp) ->
  lists:flatten(str(Sexp)).


str([])                       -> [];
str([S]) when is_list(S)      ->
  [$(, str(S), $)];
str([X])      ->
  to_list(X);
str([S|Rest]) when is_list(S) ->
  [$(, str(S), $), " ", str(Rest)];
str([X|Rest])                 ->
  [to_list(X), " ", str(Rest)].

to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_float(X)   -> float_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X).
