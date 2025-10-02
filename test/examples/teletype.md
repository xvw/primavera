# Teletype example

> As is often the case, to demonstrate abstraction of effects or
> dependency injection, we use the usual `teletype` example. A
> programme capable of interacting with the console to **print** and
> **read**.

## Description of possible interactions

We begin by describing all possible interactions in our programme, the
primitives `read_line` and `print` in an `Fx` module:

```ocaml
module Fx = struct
  open Primavera
  
  let print message = 
    perform (fun h -> h # print message)
  
  let print_line message = 
    print (message ^ "\n")

  let read_line () =
    perform (fun h -> h # read_line)
end
```

The handler (`h`) passed via the `Primavera.perform` function is used
to call one of its methods (and allow inference to track the
operations that can be executed).

The `print_line` function reuses the `print` operation and simply adds
a line break at the end of the message to be printed.

## Description of a programme that uses these primitives

Now that we have our operations, we can use them to describe the
ancient `teletype` programme! To do this, we use the `let*` (`bind`)
operator to sequence operations with _effects_.

```ocaml
let teletype () = 
  let open Primavera in
  let* () = Fx.print_line "What is your name?" in
  let* name = Fx.read_line () in 
  Fx.print_line ("Hello " ^ name) ;;
```

If we inspect the type of our `teletype` programme, we can observe
several things:

```ocaml
# teletype ;;
- : unit ->
    (unit, < print : string -> unit; read_line : string; .. >) Primavera.t
= <fun>
```

A _primavera_ programme generally takes the following form:

```
'input -> ('outpout, <set_of_operation; ..> t)
```

Here, we can see that our programme takes `unit` and returns `unit`,
and that it propagates the following operations:

- `print` which takes a `string` and returns `unit`
- `read_line` which returns a `string`

## Programme interpretation

Now that we have described a programme, we need to execute it,
providing concrete implementations for the operations that can be
propagated, using `Primavera.run` and giving an object to the flag
`handler`:

```ocaml
# Primavera.run
   ~handler:object 
     method print message = 
       print_string message

     (* Here we hook the call of read_line to 
        works in test-context  *)
     method read_line = 
       "Xavier"
   end
   teletype
   ()
  ;;
What is your name?
Hello Xavier
- : unit = ()
```

Hey, it works!
