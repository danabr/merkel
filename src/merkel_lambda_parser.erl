%% ocaml 4.02.1 lambda -> sexp
-module(merkel_lambda_parser).

-export([ parse/1
        , parse_file/1
        ]).

parse_file(FileName) when is_list(FileName) ->
  case file:read_file(FileName) of
    {ok, Str}      -> parse(binary_to_list(Str));
    {error, _}=Err -> Err
end.

parse("(" ++ String) when is_list(String) ->
  {Tree, _} = parse_args(String, []),
  Tree.

parse_args([], Args)            -> {lists:reverse(Args), []};
parse_args(" " ++ Rest0, Args)  -> parse_args(Rest0, Args);
parse_args("\n" ++ Rest0, Args) -> parse_args(Rest0, Args);
parse_args(")" ++ Rest0, Args)  -> {lists:reverse(Args), Rest0};
parse_args("(" ++ Rest0, Args)  ->
  {Tree, Rest1} = parse_args(Rest0, []),
  parse_args(Rest1, [Tree|Args]);
parse_args("[" ++ Rest0, Args)  ->
  {Tree, Rest1} = parse_bracket_args(Rest0, []),
  %% Should we tag these somehow?
  parse_args(Rest1, [Tree|Args]);
parse_args(Chars, Args)        ->
  {Arg, Rest} = parse_arg(Chars, []),
  parse_args(Rest, [list_to_binary(Arg)|Args]).

parse_arg(")" ++ Rest, Arg)  -> {lists:reverse(Arg), [$)|Rest]};
parse_arg("\n" ++ Rest, Arg) -> {lists:reverse(Arg), Rest};
parse_arg(" " ++ Rest, Arg)  -> {lists:reverse(Arg), Rest};
parse_arg("[" ++ Rest, Arg)  -> {lists:reverse(Arg), [$[|Rest]};
parse_arg("\"" ++ Rest, Arg) ->
  parse_str(Rest, [$"|Arg]);
parse_arg([C|Rest], Arg)        ->
  parse_arg(Rest, [C|Arg]).

parse_str("\"" ++ Rest, Arg)->
  parse_arg(Rest, [$"|Arg]);
parse_str([C|Rest], Arg)    ->
  parse_str(Rest, [C|Arg]).

parse_bracket_args("[" ++ _, _Arg)   -> error(nested_brackets);
parse_bracket_args("]" ++ Rest, Arg) ->
  {lists:reverse(Arg), Rest};
parse_bracket_args(Chars, Args)    ->
  {Arg, Rest} = parse_bracket_arg(Chars, []),
  parse_bracket_args(Rest, [list_to_binary(Arg)|Args]).

parse_bracket_arg("]" ++ Rest, Arg) -> {lists:reverse(Arg), [$]|Rest]};
parse_bracket_arg(" " ++ Rest, Arg) -> {lists:reverse(Arg), Rest};
parse_bracket_arg([C|Rest], Arg)    ->
  parse_bracket_arg(Rest, [C|Arg]).
