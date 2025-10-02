(** Set of reusable interfaces. *)

(** {1 Requirements} *)

module Req : sig
  (** The requirements describe the minimum interfaces for building
      Primavera instances on parameterised types (of arity 1 or
      2). They broadly follow the minimal interface of a Functor, an
      Applicative and a Monad.

      It would be possible to pool definitions to reduce the minimum
      definitions, however, these simplifications are left to the user
      (to keep the API simple). *)

  (** Requirements for types of kind 1. *)
  module type S1 = sig
    (** The main type. *)
    type 'a t

    (** [return x] lift [x] in the context of the type {!type:t}. *)
    val return : 'a -> 'a t

    (** [map f x] mapping from [f] over [x].
        Lift a function from ['a -> 'b] to a function from ['a t -> 'b t]. *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** [apply f x] given [f] and [x] apply [f] on [x]. *)
    val apply : ('a -> 'b) t -> 'a t -> 'b t

    (** [bind x f] sequentially apply [f] on [x]. *)
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Requirements for types of kind 2. *)
  module type S2 = sig
    (** The main type. *)
    type ('a, 'e) t

    (** [return x] lift [x] in the context of the type {!type:t}. *)
    val return : 'a -> ('a, 'e) t

    (** [map f x] mapping from [f] over [x].
        Lift a function from ['a -> 'b] to a function from ['a t -> 'b t]. *)
    val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

    (** [apply f x] given [f] and [x] apply [f] on [x]. *)
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t

    (** [bind x f] sequentially apply [f] on [x]. *)
    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  (** Requirements for types of kind 3. *)
  module type S3 = sig
    (** The main type. *)
    type ('a, 'e, 'f) t

    (** [return x] lift [x] in the context of the type {!type:t}. *)
    val return : 'a -> ('a, 'e, 'f) t

    (** [map f x] mapping from [f] over [x].
        Lift a function from ['a -> 'b] to a function from ['a t -> 'b t]. *)
    val map : ('a -> 'b) -> ('a, 'e, 'f) t -> ('b, 'e, 'f) t

    (** [apply f x] given [f] and [x] apply [f] on [x]. *)
    val apply : ('a -> 'b, 'e, 'f) t -> ('a, 'e, 'f) t -> ('b, 'e, 'f) t

    (** [bind x f] sequentially apply [f] on [x]. *)
    val bind : ('a, 'e, 'f) t -> ('a -> ('b, 'e, 'f) t) -> ('b, 'e, 'f) t
  end
end

(** {1 Complete interfaces}

    Complete interfaces generated based on requirements. {!module:Req} *)

module type S1 = sig
  type 'a output
  type ('a, 'handler) t

  val run : handler:'handler -> ('a -> ('b, 'handler) t) -> 'a -> 'b output
  val perform : ('handler -> 'a output) -> ('a, 'handler) t

  include Req.S2 with type ('a, 'b) t := ('a, 'b) t

  val replace : 'a -> ('b, 'handler) t -> ('a, 'handler) t
  val ignore : ('a, 'handler) t -> (unit, 'handler) t
  val zip : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t
  val join : (('a, 'handler) t, 'handler) t -> ('a, 'handler) t

  val compose
    :  ('a -> ('b, 'handler) t)
    -> ('c -> ('a, 'handler) t)
    -> 'c
    -> ('b, 'handler) t

  val map2
    :  ('a -> 'b -> 'c)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t

  val map3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t
    -> ('d, 'handler) t

  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t
    -> ('d, 'handler) t
    -> ('e, 'handler) t

  val map5
    :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t
    -> ('d, 'handler) t
    -> ('e, 'handler) t
    -> ('f, 'handler) t

  module Infix : sig
    val ( !! ) : ('handler -> 'a output) -> ('a, 'handler) t
    val ( <$> ) : ('a -> 'b) -> ('a, 'handler) t -> ('b, 'handler) t
    val ( <$ ) : 'a -> ('b, 'handler) t -> ('a, 'handler) t
    val ( $> ) : ('a, 'handler) t -> 'b -> ('b, 'handler) t
    val ( <*> ) : ('a -> 'b, 'handler) t -> ('a, 'handler) t -> ('b, 'handler) t
    val ( <&> ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t
    val ( <* ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a, 'handler) t
    val ( *> ) : ('a, 'handler) t -> ('b, 'handler) t -> ('b, 'handler) t
    val ( << ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a, 'handler) t
    val ( >> ) : ('a, 'handler) t -> ('b, 'handler) t -> ('b, 'handler) t

    val ( >>= )
      :  ('a, 'handler) t
      -> ('a -> ('b, 'handler) t)
      -> ('b, 'handler) t

    val ( =<< )
      :  ('a -> ('b, 'handler) t)
      -> ('a, 'handler) t
      -> ('b, 'handler) t

    val ( <=< )
      :  ('a -> ('b, 'handler) t)
      -> ('c -> ('a, 'handler) t)
      -> 'c
      -> ('b, 'handler) t

    val ( >=> )
      :  ('a -> ('b, 'handler) t)
      -> ('b -> ('c, 'handler) t)
      -> 'a
      -> ('c, 'handler) t
  end

  module Syntax : sig
    val ( let+ ) : ('a, 'handler) t -> ('a -> 'b) -> ('b, 'handler) t
    val ( and+ ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t

    val ( let* )
      :  ('a, 'handler) t
      -> ('a -> ('b, 'handler) t)
      -> ('b, 'handler) t

    val ( and* ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t
  end

  include module type of Infix
  include module type of Syntax
end
