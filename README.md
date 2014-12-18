## Mirage type-classes

A set of functors and combinators to convert to and from and JSON values
and Cstruct buffers.

```ocaml
# require "tc";;

# Tc.show Tc.string "Hello world!";;
- : string = "[\"Hello world!\"]"

# Tc.to_json (Tc.pair Tc.int Tc.string) (3, "foo");;
- : Ezjsonm.value = `A [`Float 3.; `String "foo"]
```

A slightly more complex example, using autogen code instead of functor
composition:

```ocaml
# require "tc";;

# type t = { foo: int; bar: string list };;

# let t =
    let module M = Tc.Pair (Tc.Int) (Tc.List(Tc.String)) in
    let to_t (foo, bar) = { foo; bar} and of_t {foo; bar} = (foo, bar) in
    Tc.biject (module M) to_t of_t;;

# Tc.show t { foo = 3; bar = [ "hello"; "world" ] }
- : string = "[3,[\"hello\",\"world\"]]"

# let j = Tc.to_json t { foo = 3; bar = [ "hello"; "world" ] }
- : Ezjsonm.value = `A [`Float 3.; `A [`String "hello"; `String "world"]]

# Tc.write_string Tc.string "hello world";;
- : string = "\011hello world"
```
