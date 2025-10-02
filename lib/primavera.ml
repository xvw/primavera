module Req = Sig.Req

module Make = struct
  module S1 (T : Req.S1) = struct
    type 'a output = 'a T.t
    type ('a, 'handler) t = 'handler -> 'a output

    let run ~handler program input = (program input) handler
    let perform program = program
    let return x _ = T.return x
    let map f handler x = T.map f (handler x)
    let apply fs handler x = T.apply (fs x) (handler x)
    let bind handler f x = T.bind (handler x) (fun a -> (f a) x)
    let replace x xs = map (fun _ -> x) xs
    let ignore x = replace () x
    let zip a b = apply (map (fun a b -> a, b) a) b
    let map2 f a b = apply (map f a) b
    let map3 f a b c = apply (map2 f a b) c
    let map4 f a b c d = apply (map3 f a b c) d
    let map5 f a b c d e = apply (map4 f a b c d) e
    let join m = bind m (fun x -> x)
    let compose g f x = bind (f x) (fun r -> g r)

    module Infix = struct
      let ( !! ) = perform
      let ( <$> ) = map
      let ( <$ ) = replace
      let ( $> ) m x = replace x m
      let ( <*> ) = apply
      let ( <&> ) = zip
      let ( <* ) a b = map2 Fun.const a b
      let ( *> ) a b = map2 (fun _ x -> x) a b
      let ( >>= ) = bind
      let ( =<< ) f m = bind m f
      let ( <=< ) = compose
      let ( >=> ) f g = compose g f
      let ( >> ) ma mb = ma >>= Fun.const mb
      let ( << ) ma _ = ma
    end

    module Syntax = struct
      let ( let+ ) x f = map f x
      let ( and+ ) = zip
      let ( and* ) = zip
      let ( let* ) = bind
    end

    include Infix
    include Syntax
  end
end

module Id = struct
  type 'a t = 'a

  let return x = x
  let map f = f
  let apply f = f
  let bind x f = f x
end

include Make.S1 (Id)
