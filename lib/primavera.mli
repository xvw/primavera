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
