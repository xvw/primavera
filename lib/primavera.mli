(** As mentioned in the
    {{:https://gr-im.github.io/a/dependency-injection.html} following
    article}, sometimes the trivial ways of abstracting effects or
    dependencies in OCaml offer ‘too much power’ (type construction
    through modules or continuation control for effects).

    The goal of Primavera is to provide an approach compatible with
    inference to control its dependencies by taking advantage of
    OCaml's type inference and using
    {{:https://ocaml.org/manual/5.1/objectexamples.html} objects}.

    This approach, although more limited than effects and modules,
    allows operations to be captured as handlers using row
    polymorphism of objects. *)

(** {1 Identity}

    Primavera is built on top of an identity monad to abstract
    dependencies whose normal form is a regular value. *)

include Sig.S1 with type 'a output = 'a

(** {1 Requirements} *)

module Req = Sig.Req (** @canonical Primavera.Req *)

(** {1 Building a Reader} *)

module Make : sig
  module S1 (S : Req.S1) : Sig.S1 with type 'a output = 'a S.t
  module S2 (S : Req.S2) : Sig.S2 with type ('a, 'b) output = ('a, 'b) S.t
end

(** {1 Example}

    As is often the case, to demonstrate abstraction of effects or
    dependency injection, we use the usual [teletype] example. A
    programme capable of interacting with the console to {b print} and
    {b read}.

    We begin by describing all possible interactions in our programme,
    the primitives [read_line] and [print] in an [Fx] module using the
    function {!val:Primavera.perform}:

    {eof@ocaml[
      module Fx = struct
        open Primavera

        let print message = perform (fun handler -> handler#print message)
        let print_line message = print (message ^ "\n")
        let read_line () = perform (fun handler -> handler#read_line)
      end
    ]eof}

    The [handler] passed via the [Primavera.perform] function is used
    to call one of its methods (and {b allow inference to track the
    operations that can be executed}).

    We can observe the signature of our module to understand how
    operations propagate through the type system.

    {eof@ocaml[
      # #show_module Fx;;
      module Fx :
        sig
          val print : 'a -> ('b, < print : 'a -> 'b; .. >) Primavera.t
          val print_line : string -> ('a, < print : string -> 'a; .. >) Primavera.t
          val read_line : unit -> ('a, < read_line : 'a; .. >) Primavera.t
        end
    ]eof}

    Now that we have our operations, we can use them to describe the
    ancient [teletype] programme! To do this, we use the [let*]
    ({!val:Primavera.bind}) operator to sequence operations with {i effects}.

    {eof@ocaml[
      let teletype () =
        let open Primavera in
        let* () = Fx.print_line "What is your name?" in
        let* name = Fx.read_line () in
        Fx.print_line ("Hello " ^ name)
      ;;
    ]eof}

    If we inspect the type of our [teletype] programme, we can observe
    several things:

    {eof@ocaml[
      # let t = teletype ;;
      val t :
        unit ->
        (unit, < print : string -> unit; read_line : string; .. >) Primavera.t =
        <fun>
    ]eof}

    A {i primavera} programme generally takes the following form:
    ['input -> ('outpout, <set_of_operation; ..> t)].

    Here, we can see that our programme takes [unit] and returns [unit],
    and that it propagates the following operations:

    - [print] which takes a [string] and returns [unit]
    - [read_line] which returns a [string].

    Now that we have described a programme, we need to execute it,
    providing concrete implementations for the operations that can be
    propagated, using {!val:Primavera.run} and giving an object to the
    flag [handler]:

    {eof@ocaml[
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
        What is your name?
        Hello Xavier
        - : unit = ()
    ]eof} *)
