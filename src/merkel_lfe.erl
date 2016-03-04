%% ocaml lambda -> lfe
-module(merkel_lfe).

-export([to_lfe/2]).

-record(state, { mod_name :: atom() }).

to_lfe(ModName, In) when is_atom(ModName), is_list(In) ->
  State = #state{mod_name = ModName},
  lfe(State, In).

lfe(#state{mod_name=ModName}=S, [<<"setglobal">>, _, Lets]) ->
  [[defmodule, ModName, [export, all]]|functions(Lets, S)].

%% top level lets and letrecs equal defun
%% TODO: Rename exported functions (drop /..)
functions([<<"makeblock">>|_], _State)    -> [];
functions([<<"let">>, Funs, In], State)   ->
  func_lets(Funs, State) ++ functions(In, State);
functions([<<"letrec">>, Fun, In], State) ->
  [function(Fun, State)|functions(In, State)].

func_lets([], _State)                       -> [];
%% (let (name/1001 = (function ..) ...) (...))
func_lets([Fun, <<"=">>, Impl|Lets], State) ->
  [function([Fun, Impl], State)|func_lets(Lets, State)].

function([Name, [<<"function">>|ArgsAndBody]], State) ->
  {Args, Body} = args_and_body(ArgsAndBody),
  [defun, Name, Args, expr(Body, State)].


%% Tuples
expr([<<"0:">>|Elements], State)                             ->
  Lits = [ literal(E, State) || E <- Elements ],
  ['tuple'|Lits];
expr([<<"makeblock">>|Elements], State)                      ->
  %% In the world of ocaml-lambda, lists are tuples
  %% (head,tail). That means there is no way to distinguish
  %% whether we want to construct a list or a tuple.
  %% Reason to give up?
  ['tuple'|exprs(Elements, State)];
%% field may extract from either a list or a tuple.
%% Either the head or tail of a list may be extracted,
%% so we only car about Index=0 (hd) or Index=1 (tl).
expr([<<"field">>, Index, Var], State) when Index=:=<<"0">>;
                                            Index=:=<<"1">>  ->
  ListPart = case Index of <<"0">> -> car; _ -> cdr end,
  VarExpr = var(Var, State),
  ListExpr = [ListPart, VarExpr],
  TupleExpr = [element, tuple_index(integer(Index)), Var],
  ['if', [is_list, VarExpr], ListExpr, TupleExpr];
expr([<<"field">>, Index, Var], State)                       ->
  [element, tuple_index(integer(Index)), var(Var, State)];
%% Function construction and application
expr([<<"apply">>, Fun|Args], State)                         ->
  ArgsExpr = [ expr(Arg, State) || Arg <- Args ],
  [var(Fun, State)|ArgsExpr];
expr([<<"function">>|ArgsAndBody], State)                    ->
  {Args, Body} = args_and_body(ArgsAndBody),
  [lambda, Args, expr(Body, State)];
%% Control structures
expr([<<"if">>, Test, True, False], State)                   ->
  %% lambda allows if expressions on lists, where the empty list
  %% evaluates to false. We handle this with a case expression.
  FalseExpr = expr(False, State),
  ['case', expr(Test, State), [<<"'false">>, FalseExpr],
                              [<<"()">>, FalseExpr],
                              [<<"_">>, expr(True, State)]];
expr([<<"switch*">>, Expr|Cases], State)                     ->
  %% Expr seems always to refer to a tuple.
  %% We pattern match on the first element (the tag) since we cannot
  %% have {tuple|_} patterns.
  ['case', [element, 1, expr(Expr, State)]|cases(Cases, State)];
%% Exception handling
%% > (try (throw 4) (catch ((tuple _ _ _) (usage)))))
expr([<<"catch">>, Expr, <<"with">>, Elements, Do], State)   ->
  Tuple = [tuple|exprs(Elements, State)],
  C = ['catch', [[tuple, <<"'throw">>, Tuple, <<"_">>], expr(Do, State)]],
  ['try', expr(Expr, State), C];
expr([<<"exit">>|Elements], State)                           ->
  ['throw', [tuple|exprs(Elements, State)]];
%% Assignment
expr([<<"let">>, Defs, In], State)                           ->
  lets(Defs, In, State);
expr([<<"letrec">>, [Name, Func], In], State)                ->
  ['letrec-function', [[var(Name, State), expr(Func, State)]], expr(In, State)];
%% Boolean operators
expr([<<"&&">>, Lhs, Rhs], State)                            ->
  op('andalso', Lhs, Rhs, State);
%% Comparison operators
expr([<<"<">>, Lhs, Rhs], State)                             ->
  op('<', Lhs, Rhs, State);
expr([<<"<=.">>, Lhs, Rhs], State)                           ->
  op('=<', Lhs, Rhs, State);
expr([<<"<=">>, Lhs, Rhs], State)                            ->
  op('=<', Lhs, Rhs, State);
expr([<<"caml_equal">>, Lhs, Rhs], State)                    ->
  op('=:=', Lhs, Rhs, State);
expr([<<"==">>, Lhs, Rhs], State)                            ->
  op('=:=', Lhs, Rhs, State);
expr([<<"!=">>, Lhs, Rhs], State)                            ->
  op('=/=', Lhs, Rhs, State);
%% Integer + Float operators
expr([<<"mod">>, Lhs, Rhs], State)                           ->
  op('rem', Lhs, Rhs, State);
expr([<<"+.">>, Lhs, Rhs], State)                            ->
  op('+', Lhs, Rhs, State);
expr([<<"+">>, Lhs, Rhs], State)                             ->
  op('+', Lhs, Rhs, State);
expr([<<"-.">>, Lhs, Rhs], State)                            ->
  op('-', Lhs, Rhs, State);
expr([<<"-">>, Lhs, Rhs], State)                             ->
  op('-', Lhs, Rhs, State);
expr([<<"*">>, Lhs, Rhs], State)                             ->
  op('*', Lhs, Rhs, State);
expr([<<"/">>, Lhs, Rhs], State)                             ->
  op('div', Lhs, Rhs, State);
%% LFE-inspired erlang calling convention.
%% Not part of standard lambda.
%% See ml_fib.ml for an example.
expr([<<":">>, Mod, Fun|Args], State) when is_binary(Mod),
                                           is_binary(Fun)    ->
  [<<":">>, Mod, Fun|exprs(Args, State)];
%% Literals
expr(X, State) when is_binary(X)                             ->
  literal(X, State);
expr(Expr, _State)                                           ->
  error(Expr).

exprs(Exprs, State) ->
  lists:map(fun(E) -> expr(E, State) end, Exprs).


lets([Name, <<"=">>, Expr], In, State)      ->
  ['let', [def(Name, Expr, State)], expr(In, State)];
%% a = alias
lets([Name, <<"=a">>|Lets], In, State)      ->
  lets([Name, <<"=">>|Lets], In, State);
lets([Name, <<"=">>, Expr|Lets], In, State) ->
  ['let', [def(Name, Expr, State)], lets(Lets, In, State)].

def(Name, Expr, State) ->
  [var(Name, State), expr(Expr, State)].

op(Op, Lhs, Rhs, State) ->
  [Op, expr(Lhs, State), expr(Rhs, State)].

%% "0a" may mean either 'false' or [] (the empty list).
%% For now we treat it as the empty list.
%% TODO: Can we find out from the context?
literal(<<"0a">>, _State)                     ->
  <<"()">>;
literal(<<"1a">>, _State)                     ->
  <<"'true">>;
literal(X, State) when is_binary(X)           ->
  literal(binary_to_list(X), State);
literal([$-|_]=X, State)                      ->
  numeral(X, State);
literal([C|_]=X, State) when $0 =< C, C =< $9 ->
  numeral(X, State);
literal(X, State)                             ->
  var(X, State).

var(Name, State) when is_binary(Name) ->
  var(binary_to_list(Name), State);
var(Name, State) when is_list(Name)   ->
  var(Name, State, []).

var([], _State, Acc)      ->
  error({not_a_var, lists:reverse(Acc)});
var([$/|Rest], _State, Acc)  ->
  list_to_binary(lists:reverse(Acc) ++ [$/|Rest]);
var([C|Rest], State, Acc) ->
  var(Rest, State, [C|Acc]).

numeral(X, _State) ->
  try integer(X)
  catch
    error:badarg -> list_to_float("0" ++ X ++ "0")
  end.

integer(Val) when is_binary(Val) ->
  binary_to_integer(Val);
integer(Val) when is_list(Val)   ->
  list_to_integer(Val).

tuple_index(N) when N >= 0 -> N+1.

cases([], _State)                                      -> [];
cases([<<"case">>, <<"tag">>, Tag, Expr|Cases], State) ->
  Case = [tag(Tag), expr(Expr, State)],
  [Case|cases(Cases, State)].

args_and_body(ArgsAndBody)  ->
  args_and_body(ArgsAndBody, []).

args_and_body([Body], Args)            ->
  {lists:reverse(Args), Body};
args_and_body([Arg|ArgsAndBody], Args) ->
  args_and_body(ArgsAndBody, [Arg|Args]).

tag(Tag) when is_binary(Tag) ->
  integer(lists:droplast(binary_to_list(Tag))).

%% scrub_name(Str) when is_binary(Str) ->
%%   scrub_name(binary_to_list(Str));
%% scrub_name(Str) when is_list(Str)   ->
%%   list_to_atom(lists:takewhile(fun(C) -> C =/= $/ end, Str)).
