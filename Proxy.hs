{-# LANGUAGE LambdaCase #-}

module Proxy where

data Active uf df m r = RespondUpstream   r (AwaitUpstream uf df m r)
                      | RequestDownstream (df (m (Active uf df m r)))

data AwaitUpstream uf df m r = AwaitUpstream (uf r -> m (Active uf df m r))

identity :: (Monad m, Functor f) => AwaitUpstream f f m r
identity = AwaitUpstream (\f -> return (RequestDownstream
                                (fmap (\r -> return (RespondUpstream r identity)) f)))

data Socket a b r = Same a (b -> r)

echo :: Monad m => AwaitUpstream (Socket a a) f m r
echo = AwaitUpstream (\(Same a f) -> return (RespondUpstream (f a) echo))

(>->) :: (Monad m, Functor h) =>
         AwaitUpstream f g m r
      -> AwaitUpstream g h m (m (Active f g m r))
      -> AwaitUpstream f h m r
(>->) (AwaitUpstream f) g = AwaitUpstream $ \h -> do
  fh <- f h
  activateUpper fh g
  
activateUpper :: (Monad m, Functor h) =>
                 Active f g m r
              -> AwaitUpstream g h m (m (Active f g m r))
              -> m (Active f h m r)
activateUpper f g@(AwaitUpstream gf) = case f of
  RespondUpstream r await -> return (RespondUpstream r (await >-> g))
  RequestDownstream fact  -> sendDownstream (gf fact)

sendDownstream :: (Monad m, Functor h) =>
                  m (Active g h m (m (Active f g m r)))
               -> m (Active f h m r)
sendDownstream h' = do
  h'' <- h'
  case h'' of
    RespondUpstream r await -> do
      r' <- r
      activateUpper r' await
    RequestDownstream dfact -> return (RequestDownstream (fmap sendDownstream dfact))
