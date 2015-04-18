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
    RequestDownstream dfact   -> flut dfact (AwaitUpstream g)
  
flit :: Functor h => Active f g r -> AwaitUpstream g h (Active f g r) -> Active f h r
flit f g = case f of
  RespondUpstream r await -> RespondUpstream r (await >-> g)
  RequestDownstream fact  -> flut fact g

flut :: Functor h => g (Active f g r) -> AwaitUpstream g h (Active f g r) -> Active f h r
flut f (AwaitUpstream g) = case g f of
  RespondUpstream r await -> flit r await
  RequestDownstream hact -> flat hact

flat :: Functor h => h (Active g h (Active f g r)) -> Active f h r
flat h = RequestDownstream $ flip fmap h
         (\x -> case x of
             RespondUpstream r await  -> flit r await
             RequestDownstream dfact -> flat dfact
         )
