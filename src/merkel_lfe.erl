%% ocaml lambda -> lfe
-module(merkel_lfe).

-export([to_lfe/2]).

-record(state, { mod_name :: atom() }).

to_lfe(ModName, In) when is_atom(ModName), is_list(In) ->
  State = #state{mod_name = ModName},
  lfe(State, In).

lfe(#state{mod_name=ModName}=S, [<<"setglobal">>, _, Lets]) ->
  [[defmodule, ModName, [export, all]]|functions(Lets, S)].

%% top level let's and letrec's equal defun 
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
  [defun, scrub_name(Name), Args, expr(Body, State)].


%% Tuples
expr([<<"0:">>|Elements], State)                 ->
  Lits = [ literal(E, State) || E <- Elements ],
  ['tuple'|Lits];
expr([<<"makeblock">>, <<"0">>|Elements], State) ->
  ElExprs = [ expr(E, State) || E <- Elements ],
  ['tuple'|ElExprs];
expr([<<"field">>, Index, Var], State)           ->
  [element, tuple_index(integer(Index)), var(Var, State)];
%% Function construction and application
expr([<<"apply">>, Fun|Args], State)             ->
  ArgsExpr = [ expr(Arg, State) || Arg <- Args ],
  [var(Fun, State)|ArgsExpr];
expr([<<"function">>|ArgsAndBody], State)        ->
  {Args, Body} = args_and_body(ArgsAndBody),
  [lambda, Args, expr(Body, State)];
%% Control structures
expr([<<"if">>, Test, True, False], State)       ->
  ['if', expr(Test, State), expr(True, State), expr(False, State)];
%% Assignment
expr([<<"let">>, Defs, In], State)               ->
  lets(Defs, In, State);
expr([<<"letrec">>, [Name, Func], In], State)    ->
  ['letrec-function', [[var(Name, State), expr(Func, State)]], expr(In, State)];
%% Boolean operators
expr([<<"&&">>, Lhs, Rhs], State)                ->
  op('andalso', Lhs, Rhs, State);
%% Comparison operators
expr([<<"<">>, Lhs, Rhs], State)                 ->
  op('<', Lhs, Rhs, State);
expr([<<"caml_equal">>, Lhs, Rhs], State)        ->
  op('=:=', Lhs, Rhs, State);
expr([<<"==">>, Lhs, Rhs], State)                ->
  op('=:=', Lhs, Rhs, State);
%% Integer + Float operators
expr([<<"mod">>, Lhs, Rhs], State)               ->
  op('rem', Lhs, Rhs, State);
expr([<<"+">>, Lhs, Rhs], State)                 ->
  op('+', Lhs, Rhs, State);
expr([<<"-">>, Lhs, Rhs], State)                 ->
  op('-', Lhs, Rhs, State);
expr([<<"*">>, Lhs, Rhs], State)                 ->
  op('*', Lhs, Rhs, State);
expr([<<"/">>, Lhs, Rhs], State)                 ->
  op('div', Lhs, Rhs, State);
%% Literals
expr(X, State) when is_binary(X)                 ->
  literal(X, State);
expr(Expr, _State)                               ->
  error(Expr).


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
var([$/|_], _State, Acc)  ->
  list_to_binary(lists:reverse(Acc));
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

args_and_body(ArgsAndBody)  ->
  args_and_body(ArgsAndBody, []).

args_and_body([Body], Args)            ->
  {lists:reverse(Args), Body};
args_and_body([Arg|ArgsAndBody], Args) ->
  args_and_body(ArgsAndBody, [scrub_name(Arg)|Args]).

scrub_name(Str) when is_binary(Str) ->
  scrub_name(binary_to_list(Str));
scrub_name(Str) when is_list(Str)   ->
  list_to_atom(lists:takewhile(fun(C) -> C =/= $/ end, Str)).
