-module(merkel_sexp).

-export([to_string/1]).

to_string(Sexp) when is_list(Sexp) ->
  lists:flatten(str(Sexp)).


str([])                          -> [];
str([A]) when is_atom(A)         ->
  atom_to_list(A);
str([A|Rest]) when is_atom(A)    ->
  [atom_to_list(A), " ", str(Rest)];
str([B]) when is_binary(B)       ->
  binary_to_list(B);
str([B|Rest]) when is_binary(B)  ->
  [binary_to_list(B), " ", str(Rest)];
str([I]) when is_integer(I)      ->
  integer_to_list(I);
str([I|Rest]) when is_integer(I) ->
  [integer_to_list(I), " ", str(Rest)];
str([S]) when is_list(S)         ->
  [$(, str(S), $)];
str([S|Rest]) when is_list(S)    ->
  [$(, str(S), $), " ", str(Rest)].
