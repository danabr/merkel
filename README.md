# merkel - playing with compiling ocaml to beam files

This is hack to compile ocaml files into binaries runnable in the BEAM
virtual machine. Built purely for the sake of exploration. Not meant (or
even barely suitable) for actual production use.

## Prerequisites
* Erlang/OTP 17 or later [1]
* LFE (Lisp flavoured Erlang) [2]
* OCaml 4.02.1 [3]


## Running

### lambda -> lfe
```
$ erlc -o ebin/ src/*.erl
$ ./bin/merkelc-lambda examples/ml_fib.ml
```
```erlang
erl -pa ebin
1> {ok, Sexp} = merkel_lambda_parser:parse_file("ml_fib.lma").
2> LFESexp = merkel_lfe:to_lfe(ml_fib, Sexp).
3> LFE = string:join([merkel_sexp:to_string(C) || C <- LFESexp], "\n").
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

### typedtree (cmt) -> lfe
```
$ ./bin/merkelc-typedtree examples/fac.ml
$ ocamlfind ocamlc -package compiler-libs.common -o ./bin/merkel_cmt_to_lfe ml_src/merkel_cmt_to_lfe.ml
$ ./bin/merkel_cmt_to_lfe examples/fac.cmt
$ ./bin/merkel_cmt_to_lfe examples/fac.cmt > /tmp/fac.lfe
$ lfec -o ebin/ /tmp/fac.lfe
$ erl -pa ebin
1> 'Fac':fac(10).
3628800
```

## Approach
This section describes the various approaches taken to generate BEAM files from
ocaml souuce code.

### Approach 0: lambda to Core Erlang
The first approach taken was to take the output from `ocamlc -dlambda`
("the untyped lambda form" [4]) and generate Core Erlang [5].

The lambda form was chosen because it seemed fairly easy to parse. It also seemed
to be at roughly the same abstraction level as Core Erlang, without too many
ML level abstractions that would be hard to transfer to the Erlang world.

I chose Core Erlang because it seemed easier than generating BEAM bytecode directly.
It also meant that I could generate some fairly dumb code and hopefully the erlang
compiler would take care of optimizing it.

This approach failed because generating proper Core Erlang was too messy.
Although by all means achievable, it wasn't very well suitable for the rapid
prototyping I was trying to achieve. Nevertheless, I managed to generate some
working code (see the now defunct merkel_im module), which was encouraging.

### Approach 1: lambda to lfe
While working with the lambda form, it occurred to me it would probably be quite easy
to transform it into (some) lisp. So I switched out Core Erlang in favour of LFE. This
was a good decision. LFE was much easier to work with which let me focus on understanding
the lambda format.

Now being able to take on more complex structures, I noted a few issues with lambda
which ultimately led me to abandon it. Ironically, they were all related to types.

The lambda format does not distinguish between tuples and lists. Lists are essentially
{head,tail} tuples (like in lisp). However, at the BEAM level, we DO care about the
distinction. Knowing what to generate in each case became impossible, and only going
for one of them would make the generated modules awkward to use from other BEAM
languages.

Another example of "type confusion" is that the empty list (nil) and the atom
both have the same value ("0a"). Thus, the condition of "if" expression can be a list.

### Approach 2: typedtree to lfe
The ocaml compiler internally manages a "typed tree" (basically an AST annotated with
type informaton). This is naturally much richer in information than the untyped lambda
form.

Note:
  * Work in progress
  * The tree is too high level to be convenient to work with. We need to consider things
    like currying, optional parameters, etc. My current feeling is that something in-between
    the typed tree and the lambda form is what I am looking for.
  * ocaml uses currying and only has single argument functions in the typed tree.
    We need to "uncurry" these somehow, to make it feasible to call multi argument
    functions from erlang land.
  * ocaml comes with "or patterns", which lfe and erlang don't support.
    These patterns need to be unnested.


## Other possible approaches
* ocaml bytecode -> BEAM bytecode [6]
  js_of_ocaml took such an approach. Need to keep in mind that BEAM is register-based whereas
  ocaml's VM is stack based. Type confusion may make this approach impossible.

## References
[1] https://github.com/erlang/otp

[2] https://github.com/rvirding/lfe

[3] https://github.com/ocaml/ocaml

[4] https://realworldocaml.org/v1/en/html/the-compiler-backend-byte-code-and-native-code.html

[5] https://www.it.uu.se/research/group/hipe/cerl/

[6] http://erlangonxen.org/more/beam
