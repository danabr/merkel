# merkel - playing with compiling ocaml to beam files

This is hack to compile ocaml files into binaries runnable in the BEAM
virtual machine. Built purely for the sake of exploration. Not meant (or
even barely suitable) for actual production use.

## Prerequisites
* Erlang/OTP 17 or later
* LFE (Lisp flavoured Erlang)
* OCaml 4.02.1


## Running
```
erlc -o ebin/ src/*.erl
./bin/merkelc examples/ml_fib.ml
```
```erlang
erl -pa ebin
1> Sexp = merkel_lambda_parser:parse_file("ml_fib.lma").
2> LFESexp = merkel_lfe:to_lfe(ml_fib, Sexp).
3> LFE = merkel_sexp:to_string(LFESexp).
4> {ok, ml_fib, Bin} = merkel_lfe_comp:string(LFE).
5> ok = file:write_file("ebin/ml_fib.beam", Bin).
6> l(ml_fib).
7> ml_fib:'fib/1009'(10).
55
8> ml_fib:'fibf/1011'(10).
55.0
9> ml_fib:'rev/1013'([1,2,3]).
[3,2,1]
```
