## Mirage type-classes

A set of functors and combinators to pretty-print (using sexplib), to
convert to and from and JSON and Cstruct buffers.

```ocaml
# Tc.show Tc.string "Hello world!";;
- : string = "\"Hello world!\""
# Tc.to_json (Tc.pair Tc.int Tc.string) (3, "foo");;
- : Ezjsonm.t = `A [`String "3"; `String "foo"]
```

A slightly more complex example, using autogen code instead of functor
composition:

```ocaml
# camlp4o;;
# require "sexplib.syntax";;
# require "comparelib.syntax";;
# require "bin_prot.syntax";;
# module M = struct
    type t = { foo: int; bar: string list } with sexp, bin_prot, compare
  end;;
# module X = Tc.S0(M);;
# let t = { foo = 3; bar = [ "hello"; "world" ] };;

# Tc.to_json (module X) t;;
- : Ezjsonm.t =
`A
  [`A [`String "foo"; `String "3"];
   `A [`String "bar"; `A [`String "hello"; `String "world"]]]

# Tc.write_string (module X) t;;
- : string = "\003\002\005hello\005world"
```