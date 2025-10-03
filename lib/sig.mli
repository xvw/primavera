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
  (** Complete interface built on top of {!module-type:Req.S1}. *)

  (** {1 Types} *)

  (** The type of expression that dependency injection must produce. *)
  type 'a output

  (** The type that describes a calculation that uses dependency
      injection. ['a] is the normal form of the programme and
      ['handler] is the object that describes the set of
      dependencies. *)
  type ('a, 'handler) t

  (** {1 Running and performing} *)

  (** Performs a computation with a given handler. *)
  val run : handler:'handler -> ('a -> ('b, 'handler) t) -> 'a -> 'b output

  (** Convert a regular function (which takes an handler) into an
      performable effect. *)
  val perform : ('handler -> 'a output) -> ('a, 'handler) t

  (** {1 Regular functions} *)

  include Req.S2 with type ('a, 'b) t := ('a, 'b) t (** @inline *)

  (** [replace x comp] discard the result of [comp] by [x]. *)
  val replace : 'a -> ('b, 'handler) t -> ('a, 'handler) t

  (** [ignore comp] same as regular {!val:ignore} function but for
      effectful computation. *)
  val ignore : ('a, 'handler) t -> (unit, 'handler) t

  (** [zip a b] is the monoidal product between [a] and [b]. *)
  val zip : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t

  (** [join comp] flatten the result of [comp]. *)
  val join : (('a, 'handler) t, 'handler) t -> ('a, 'handler) t

  (** [compose f g x] is the kleisli composition of [f] and [g]. *)
  val compose
    :  ('a -> ('b, 'handler) t)
    -> ('c -> ('a, 'handler) t)
    -> 'c
    -> ('b, 'handler) t

  (** [map2] lift a function 2-arity function. *)
  val map2
    :  ('a -> 'b -> 'c)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t

  (** [map3] lift a function 3-arity function. *)
  val map3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t
    -> ('d, 'handler) t

  (** [map4] lift a function 4-arity function. *)
  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t
    -> ('d, 'handler) t
    -> ('e, 'handler) t

  (** [map5] lift a function 5-arity function. *)
  val map5
    :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
    -> ('a, 'handler) t
    -> ('b, 'handler) t
    -> ('c, 'handler) t
    -> ('d, 'handler) t
    -> ('e, 'handler) t
    -> ('f, 'handler) t

  (** {1 Infix Operators} *)

  module Infix : sig
    (** Infix operators. *)

    (** [f <$> x] is [map f x] (infix version of {!val:map}) *)
    val ( <$> ) : ('a -> 'b) -> ('a, 'handler) t -> ('b, 'handler) t

    (** [a <$ x] is [replace a x] (infix version of {!val:replace}) *)
    val ( <$ ) : 'a -> ('b, 'handler) t -> ('a, 'handler) t

    (** [x $> a] is [replace a x] (infix (flipped) version of {!val:replace}) *)
    val ( $> ) : ('a, 'handler) t -> 'b -> ('b, 'handler) t

    (** [f <*> x] is [apply f x] (infix version of {!val:apply}) *)
    val ( <*> ) : ('a -> 'b, 'handler) t -> ('a, 'handler) t -> ('b, 'handler) t

    (** [a <&> b] is [zip a b] (infix version of {!val:zip}) *)
    val ( <&> ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t

    (** [a <* b] Perform [a] and [b] but discard [b]. *)
    val ( <* ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a, 'handler) t

    (** [a *> b] Perform [a] and [b] but discard [a]. *)
    val ( *> ) : ('a, 'handler) t -> ('b, 'handler) t -> ('b, 'handler) t

    (** [a << b] Sequentially perform [a] following by [b] but discard [b]. *)
    val ( << ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a, 'handler) t

    (** [a >> b] Sequentially perform [a] following by [b] but discard [a]. *)
    val ( >> ) : ('a, 'handler) t -> ('b, 'handler) t -> ('b, 'handler) t

    (** [m >>= f] is [bind m f] (infix version of {!val:bind}) *)
    val ( >>= )
      :  ('a, 'handler) t
      -> ('a -> ('b, 'handler) t)
      -> ('b, 'handler) t

    (** [f =<< m] is [bind m f] (infix (flipped) version of {!val:bind}) *)
    val ( =<< )
      :  ('a -> ('b, 'handler) t)
      -> ('a, 'handler) t
      -> ('b, 'handler) t

    (** [f <=< g] is [compose f g] (infix version of {!val:compose}) *)
    val ( <=< )
      :  ('a -> ('b, 'handler) t)
      -> ('c -> ('a, 'handler) t)
      -> 'c
      -> ('b, 'handler) t

    (** [f >=> g] is [compose g f] (infix (flipped) version of
        {!val:compose}) *)
    val ( >=> )
      :  ('a -> ('b, 'handler) t)
      -> ('b -> ('c, 'handler) t)
      -> 'a
      -> ('c, 'handler) t
  end

  (** {1 Binding Operators} *)

  module Syntax : sig
    (** Bindings operators. *)

    (** [let+ x = m in f x] is [map (fun x -> f x) m].

        (binding version of {!val:map}) *)
    val ( let+ ) : ('a, 'handler) t -> ('a -> 'b) -> ('b, 'handler) t

    (** [let+ x = m and+ y = n in f x y] is
        [map (fun (x, y) -> f x y) (zip m n)].

        (binding version of {!val:zip}) *)
    val ( and+ ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t

    (** [let* x = m in f x] is [bind m (fun x -> f x)].

        (binding version of {!val:bind}) *)
    val ( let* )
      :  ('a, 'handler) t
      -> ('a -> ('b, 'handler) t)
      -> ('b, 'handler) t

    (** [let* x = m and* y = n in f x y] is
        [bind (zip m n) (fun (x, y) -> f x y)].

        (binding version of {!val:zip}) *)
    val ( and* ) : ('a, 'handler) t -> ('b, 'handler) t -> ('a * 'b, 'handler) t
  end

  (** {1 Included operators} *)

  include module type of Infix (** @inline *)

  include module type of Syntax (** @inline *)
end

module type S2 = sig
  (** Complete interface built on top of {!module-type:Req.S2}. *)

  (** {1 Types} *)

  (** The type of expression that dependency injection must produce. *)
  type ('a, 'e) output

  (** The type that describes a calculation that uses dependency
      injection. ['a] is the normal form of the programme and
      ['handler] is the object that describes the set of
      dependencies. *)
  type ('a, 'e, 'handler) t

  (** {1 Running and performing} *)

  (** Performs a computation with a given handler. *)
  val run
    :  handler:'handler
    -> ('a -> ('b, 'e, 'handler) t)
    -> 'a
    -> ('b, 'e) output

  (** Convert a regular function (which takes an handler) into an
      performable effect. *)
  val perform : ('handler -> ('a, 'e) output) -> ('a, 'e, 'handler) t

  (** {1 Regular functions} *)

  include Req.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t (** @inline *)

  (** [replace x comp] discard the result of [comp] by [x]. *)
  val replace : 'a -> ('b, 'e, 'handler) t -> ('a, 'e, 'handler) t

  (** [ignore comp] same as regular {!val:ignore} function but for
      effectful computation. *)
  val ignore : ('a, 'e, 'handler) t -> (unit, 'e, 'handler) t

  (** [zip a b] is the monoidal product between [a] and [b]. *)
  val zip
    :  ('a, 'e, 'handler) t
    -> ('b, 'e, 'handler) t
    -> ('a * 'b, 'e, 'handler) t

  (** [join comp] flatten the result of [comp]. *)
  val join : (('a, 'e, 'handler) t, 'e, 'handler) t -> ('a, 'e, 'handler) t

  (** [compose f g x] is the kleisli composition of [f] and [g]. *)
  val compose
    :  ('a -> ('b, 'e, 'handler) t)
    -> ('c -> ('a, 'e, 'handler) t)
    -> 'c
    -> ('b, 'e, 'handler) t

  (** [map2] lift a function 2-arity function. *)
  val map2
    :  ('a -> 'b -> 'c)
    -> ('a, 'e, 'handler) t
    -> ('b, 'e, 'handler) t
    -> ('c, 'e, 'handler) t

  (** [map3] lift a function 3-arity function. *)
  val map3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('a, 'e, 'handler) t
    -> ('b, 'e, 'handler) t
    -> ('c, 'e, 'handler) t
    -> ('d, 'e, 'handler) t

  (** [map4] lift a function 4-arity function. *)
  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> ('a, 'e1, 'handler) t
    -> ('b, 'e1, 'handler) t
    -> ('c, 'e1, 'handler) t
    -> ('d, 'e1, 'handler) t
    -> ('e, 'e1, 'handler) t

  (** [map5] lift a function 5-arity function. *)
  val map5
    :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
    -> ('a, 'e1, 'handler) t
    -> ('b, 'e1, 'handler) t
    -> ('c, 'e1, 'handler) t
    -> ('d, 'e1, 'handler) t
    -> ('e, 'e1, 'handler) t
    -> ('f, 'e1, 'handler) t

  (** {1 Infix Operators} *)

  module Infix : sig
    (** Infix operators. *)

    (** [f <$> x] is [map f x] (infix version of {!val:map}) *)
    val ( <$> ) : ('a -> 'b) -> ('a, 'e, 'handler) t -> ('b, 'e, 'handler) t

    (** [a <$ x] is [replace a x] (infix version of {!val:replace}) *)
    val ( <$ ) : 'a -> ('b, 'e, 'handler) t -> ('a, 'e, 'handler) t

    (** [x $> a] is [replace a x] (infix (flipped) version of {!val:replace}) *)
    val ( $> ) : ('a, 'e, 'handler) t -> 'b -> ('b, 'e, 'handler) t

    (** [f <*> x] is [apply f x] (infix version of {!val:apply}) *)
    val ( <*> )
      :  ('a -> 'b, 'e, 'handler) t
      -> ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t

    (** [a <&> b] is [zip a b] (infix version of {!val:zip}) *)
    val ( <&> )
      :  ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t
      -> ('a * 'b, 'e, 'handler) t

    (** [a <* b] Perform [a] and [b] but discard [b]. *)
    val ( <* )
      :  ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t
      -> ('a, 'e, 'handler) t

    (** [a *> b] Perform [a] and [b] but discard [a]. *)
    val ( *> )
      :  ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t
      -> ('b, 'e, 'handler) t

    (** [a << b] Sequentially perform [a] following by [b] but discard [b]. *)
    val ( << )
      :  ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t
      -> ('a, 'e, 'handler) t

    (** [a >> b] Sequentially perform [a] following by [b] but discard [a]. *)
    val ( >> )
      :  ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t
      -> ('b, 'e, 'handler) t

    (** [m >>= f] is [bind m f] (infix version of {!val:bind}) *)
    val ( >>= )
      :  ('a, 'e, 'handler) t
      -> ('a -> ('b, 'e, 'handler) t)
      -> ('b, 'e, 'handler) t

    (** [f =<< m] is [bind m f] (infix (flipped) version of {!val:bind}) *)
    val ( =<< )
      :  ('a -> ('b, 'e, 'handler) t)
      -> ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t

    (** [f <=< g] is [compose f g] (infix version of {!val:compose}) *)
    val ( <=< )
      :  ('a -> ('b, 'e, 'handler) t)
      -> ('c -> ('a, 'e, 'handler) t)
      -> 'c
      -> ('b, 'e, 'handler) t

    (** [f >=> g] is [compose g f] (infix (flipped) version of
        {!val:compose}) *)
    val ( >=> )
      :  ('a -> ('b, 'e, 'handler) t)
      -> ('b -> ('c, 'e, 'handler) t)
      -> 'a
      -> ('c, 'e, 'handler) t
  end

  (** {1 Binding Operators} *)

  module Syntax : sig
    (** Bindings operators. *)

    (** [let+ x = m in f x] is [map (fun x -> f x) m].

        (binding version of {!val:map}) *)
    val ( let+ ) : ('a, 'e, 'handler) t -> ('a -> 'b) -> ('b, 'e, 'handler) t

    (** [let+ x = m and+ y = n in f x y] is
        [map (fun (x, y) -> f x y) (zip m n)].

        (binding version of {!val:zip}) *)
    val ( and+ )
      :  ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t
      -> ('a * 'b, 'e, 'handler) t

    (** [let* x = m in f x] is [bind m (fun x -> f x)].

        (binding version of {!val:bind}) *)
    val ( let* )
      :  ('a, 'e, 'handler) t
      -> ('a -> ('b, 'e, 'handler) t)
      -> ('b, 'e, 'handler) t

    (** [let* x = m and* y = n in f x y] is
        [bind (zip m n) (fun (x, y) -> f x y)].

        (binding version of {!val:zip}) *)
    val ( and* )
      :  ('a, 'e, 'handler) t
      -> ('b, 'e, 'handler) t
      -> ('a * 'b, 'e, 'handler) t
  end

  (** {1 Included operators} *)

  include module type of Infix (** @inline *)

  include module type of Syntax (** @inline *)
end
