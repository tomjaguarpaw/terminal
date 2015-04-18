{-# LANGUAGE LambdaCase #-}

module Proxy where

data Active uf df r = RespondUpstream   r (AwaitUpstream uf df r)
                    | RequestDownstream (df (Active uf df r))

data AwaitUpstream uf df r = AwaitUpstream (uf r -> Active uf df r)

identity :: Functor f => AwaitUpstream f f r
identity = AwaitUpstream (\f -> RequestDownstream
                                (fmap (\r -> RespondUpstream r identity) f))

data Socket a b r = Same a (b -> r)

echo :: AwaitUpstream (Socket a a) f r
echo = AwaitUpstream (\(Same a f) -> RespondUpstream (f a) echo)

(>->) :: Functor h => AwaitUpstream f g r -> AwaitUpstream g h (Active f g r) -> AwaitUpstream f h r
(>->) (AwaitUpstream f) (AwaitUpstream g) = AwaitUpstream $ \h ->
  case f h of
    RespondUpstream   r  next -> RespondUpstream r (next >-> AwaitUpstream g)
    RequestDownstream dfact   -> sendDownstream dfact (AwaitUpstream g)
  
activateUpstream :: Functor h => Active f g r -> AwaitUpstream g h (Active f g r) -> Active f h r
activateUpstream f g = case f of
  RespondUpstream r await -> RespondUpstream r (await >-> g)
  RequestDownstream fact  -> sendDownstream fact g

sendDownstream :: Functor h => g (Active f g r) -> AwaitUpstream g h (Active f g r) -> Active f h r
sendDownstream f (AwaitUpstream g) = case g f of
  RespondUpstream r await -> activateUpstream r await
  RequestDownstream hact -> activateDownstream hact

activateDownstream :: Functor h => h (Active g h (Active f g r)) -> Active f h r
activateDownstream h = RequestDownstream $ flip fmap h $ \case
  RespondUpstream r await  -> activateUpstream r await
  RequestDownstream dfact -> activateDownstream dfact
