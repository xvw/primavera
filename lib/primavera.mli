include Sig.S1 with type 'a output = 'a

(** {1 Requirements} *)

module Req = Sig.Req (** @canonical Primavera.Req *)

(** {1 Building a Reader} *)

module Make : sig
  module S1 (_ : Req.S1) : Sig.S1
end
