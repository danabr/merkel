-module(merkel_im).

-export([from_lambda/1]).

-define(OCAML_TRUE, "1a").

% -record(state, #{ mod = error(module_name_not_defined)
%                 , funs = []
%                 , exports = []
%                 }).

from_lambda(["setglobal", _ModName, In]) ->
  Funs = functions(In),
  CFuns = [ {cerl:c_fname(module_info, 0),
             cerl:c_fun([],
              cerl:c_call(cerl:abstract(erlang),
                          cerl:abstract(get_module_info),
                          [cerl:abstract(test)]))
            },
            { cerl:c_fname(module_info, 1),
              cerl:c_fun([cerl:ann_c_var([], x)],
                cerl:c_call(cerl:abstract(erlang),
                            cerl:abstract(get_module_info),
                            [cerl:abstract(test), cerl:ann_c_var([], x)]))
            }
          ],
  Exports = lists:map(fun({N, _}) -> N end, Funs ++ CFuns),
  cerl:c_module(cerl:abstract(test), Exports, Funs ++ CFuns).

%% It is probably smarter to get the exports from makeblock.
functions(["makeblock"|_])     -> [];
functions(["let", Funs, In])   ->
  func_lets(Funs) ++ functions(In);
functions(["letrec", Fun, In]) ->
  [function(Fun)|functions(In)].

func_lets([])                    -> [];
func_lets([Fun, "=", Impl|Lets]) ->
  [function([Fun, Impl])|func_lets(Lets)].

function([Name, Fun]) ->
  {cerl:c_fname(scrub_name(Name), arity(Fun)), func(Fun)}.

arity(["function"|ArgsAndBody]) ->
  length(lists:droplast(ArgsAndBody)).

func(["function"|ArgsAndBody]) ->
  {Args, Body} = args_and_body(ArgsAndBody),
  io:format("Function: ~p -> ~p~n", [Args, Body]),
  cerl:c_fun(args(Args), expr(Body)).

%% Ocaml string concatenation
expr(["apply", ["field", "15", ["global", "Pervasives!"]]|Args]) ->
  io:format("Apply: ^:~p~n", [Args]),
  cerl:c_call(cerl:c_atom(erlang), cerl:c_atom('++'),
              lists:map(fun(A) -> expr(A) end, Args));
%% Externals are not handled that neatly...
expr(["lists_reverse", A]) ->
  cerl:c_call(cerl:c_atom(lists), cerl:c_atom(reverse),
              [expr(A)]);
expr(["apply", Name|Args])            ->
  io:format("Apply: ~p:~p~n", [Name, Args]),
  cerl:c_call(cerl:c_atom(test), scrub_atom(Name),
              lists:map(fun(A) -> expr(A) end, Args));
%% Tuple access
expr(["field", Index, Expr])          ->
  bif(element, [cerl:c_int(Index), expr(Expr)]);
expr(["if", Test, True, False])       ->
  TestExp = expr(Test),
  TrueExp = expr(True),
  FalseExp = expr(False),
  AtomTrue = cerl:c_atom(true),
  AtomFalse = cerl:c_atom(false),
  TrueClause = cerl:c_clause([AtomTrue], TrueExp),
  FalseClause = cerl:c_clause([AtomFalse], FalseExp),
  FailClause = if_fail(),
  cerl:c_case(TestExp, [TrueClause, FalseClause, FailClause]);
expr(["let", Lets, In])               ->
  lets(Lets, In);
expr(["letrec", FuncDef, In])         ->
  letrec(FuncDef, In);
expr(["makeblock"|_])                 ->
  %% Ignore?
  cerl:c_atom(makeblock);
expr(["mod", Lhs, Rhs])               ->
  op('rem', Lhs, Rhs);
expr(["+", A, B])                     ->
  op('+', A, B);
expr(["+.", A, B])                    ->
  op('+', A, B);
expr(["-", A, B])                     ->
  op('-', A, B);
expr(["-.", A, B])                    ->
  op('-', A, B);
expr(["*", A, B])                     ->
  op('*', A, B);
expr(["*.", A, B])                    ->
  op('*', A, B);
expr(["/", A, B])                     ->
  op('div', A, B);
expr(["/.", A, B])                    ->
  op('/', A, B);
expr(["==", A, B])                    ->
  op('=:=', A, B);
expr(["caml_equal", A, B])            ->
  op('=:=', A, B);
expr(["<.", A, B])                    ->
  op('<', A, B);
expr(["<", A, B])                     ->
  op('<', A, B);
expr(["<=.", A, B])                   ->
  op('=<', A, B);
expr(["<=", A, B])                    ->
  op('=<', A, B);
expr(["&&", Lhs, Rhs])                ->
  expr(["if", Lhs, Rhs, ?OCAML_TRUE]);
%% Tuple construction
expr(["0:"|Vals] )                    ->
  cerl:c_tuple(lists:map(fun(V) -> expr(V) end, Vals));
expr(Other)                           ->
  case classify(Other) of
    {float, Val}  -> cerl:abstract(Val);
    {int, Val}    -> cerl:c_int(Val);
    {string, Val} -> cerl:c_float(Val);
    {var, Name}   -> cerl:ann_c_var([], Name)
  end.

lets(Exps, Finally) ->
  lets([Var, "="|Rest], Finally);
lets([Var, "=", Expr], Finally) ->
  cerl:c_let([Var], expr(Expr), expr(Finally));
lets([Var, "=", Expr|Rest], Finally) ->
  cerl:c_let([Var], expr(Expr), lets(Rest, Finally)).

letrec([Name, Func], In) ->
  cerl:c_letrec([{scrub_var(Name), func(Func)}], expr(In)).

op(Op, Lhs, Rhs) ->
  LhsExp = expr(Lhs),
  RhsExp = expr(Rhs),
  bif(Op, [LhsExp, RhsExp]).

bif(Fun, Args) ->
  cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(Fun), Args).

classify([$"|Rest])             -> % Assume string
  {string, lists:droplast(Rest)};
classify(Val) when is_list(Val) ->
  io:format("Classify ~p~n", [Val]),
  case classify_numeral(Val) of
    {ok,  {int, Int}}    -> {int, Int};
    {ok, {float, Float}} -> {float, Float};
    error       ->
      {var, scrub_name(Val)}
  end.

classify_numeral(Val) ->
  try list_to_integer(Val) of
    Int -> {ok, {int, Int}}
  catch
    error:badarg ->
      try list_to_float("0" ++ Val ++ "0") of
        Float -> {ok, {float, Float}}
      catch
        error:badarg -> error
      end
  end.

if_fail() ->
  Cv = cerl:ann_c_var([], omega),
  fail_clause([Cv], cerl:c_atom(if_clause)).

fail_clause(Pats, Reason) ->
  cerl:c_clause(Pats, cerl:c_primop(cerl:c_atom(match_fail), [Reason])).

args_and_body(ArgsAndBody)  ->
  args_and_body(ArgsAndBody, []).

args_and_body([Body], Args)            ->
  {lists:reverse(Args), Body};
args_and_body([Arg|ArgsAndBody], Args) ->
  args_and_body(ArgsAndBody, [Arg|Args]).

args(Args) -> [ scrub_var(Name) || Name <- Args ].


%% The ocaml ids are formatted like "fib/1001", to handle nested let
%% bindings with the same identifier. Erlang does not support this.
%% Skip for now.
scrub_var(Name) ->
  cerl:ann_c_var([], scrub_name(Name)).

scrub_atom(Str) ->
  cerl:c_atom(scrub_name(Str)).

scrub_name(Str) ->
  list_to_atom(lists:takewhile(fun(C) -> C =/= $/ end, Str)).

