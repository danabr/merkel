-module(merkel_im).

-export([from_lambda/1]).

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
  lets(Funs) ++ functions(In);
functions(["letrec", Fun, In]) ->
  [function(Fun)|functions(In)].

lets([])                    -> [];
lets([Fun, "=", Impl|Lets]) ->
  [function([Fun, Impl])|lets(Lets)].

function([Name, Fun]) ->
  {cerl:c_fname(scrub_name(Name), arity(Fun)), func(Fun)}.

arity(["function"|ArgsAndBody]) ->
  length(lists:droplast(ArgsAndBody)).

func(["function"|ArgsAndBody]) ->
  {Args, Body} = args_and_body(ArgsAndBody),
  cerl:c_fun(args(Args), expr(Body)).

expr(["if", Test, True, False]) ->
  TestExp = expr(Test),
  TrueExp = expr(True),
  FalseExp = expr(False),
  AtomTrue = cerl:c_atom(true),
  AtomFalse = cerl:c_atom(false),
  TrueClause = cerl:c_clause([AtomTrue], TrueExp),
  FalseClause = cerl:c_clause([AtomFalse], FalseExp),
  FailClause = if_fail(),
  cerl:c_case(TestExp, [TrueClause, FalseClause, FailClause]);
expr(_Other)                    ->
  cerl:c_atom(true).

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

args(Args) ->
  [ cerl:ann_c_var([], scrub_name(Name)) || Name <- Args ].

%% The ocaml ids are formatted like "fib/1001", to handle nested let
%% bindings with the same identifier. Erlang does not support this.
%% Skip for now.
scrub_name(Str) ->
  list_to_atom(lists:takewhile(fun(C) -> C =/= $/ end, Str)).

