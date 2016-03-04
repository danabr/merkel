-module(merkel_lfe_comp).

-export([string/1]).

string(LFE) when is_list(LFE) ->
  {ok, Tokens, _} = lfe_scan:string(LFE),
  case lfe_comp:forms(forms(Tokens)) of
    {ok, [Res]} -> Res;
    Err         -> Err
  end.

forms([])      -> [];
forms(Tokens0) ->
  {ok, _, Forms, Tokens} = lfe_parse:form(Tokens0),
  [Forms|forms(Tokens)].
