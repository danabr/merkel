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
  Exports = exports(Funs),
  cerl:c_module(cerl:abstract(test), Exports, CFuns).

exports(Funs) ->
  FromMod = lists:map(fun({N, A, _}) ->
              cerl:c_fname(list_to_atom(N), A)
            end, Funs),
  [ cerl:c_fname(module_info, 0)
  , cerl:c_fname(module_info, 1)
  | FromMod
  ].

%% It is probably smarter to get the exports from makeblock.
functions(["makeblock"|_])     -> []; 
functions(["let", Funs, In])   -> 
  lets(Funs) ++ functions(In);
functions(["letrec", Fun, In]) -> 
  [function(Fun)|functions(In)].

lets([])                    -> [];
lets([Fun, "=", Impl|Lets]) ->
  [function([Fun, Impl])|lets(Lets)].

function([Name, Fun]) -> {scrub_name(Name), arity(Fun), Fun}.

arity(["function"|ArgsAndBody]) ->
  length(lists:droplast(ArgsAndBody)).

%% The ocaml ids are formatted like "fib/1001", to handle nested let
%% bindings with the same identifier. Erlang does not support this.
%% Skip for now.
scrub_name(Str) ->
  lists:takewhile(fun(C) -> C =/= $/ end, Str).

