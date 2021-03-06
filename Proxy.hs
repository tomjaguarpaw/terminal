{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Proxy where

data Active uf df m r = RespondUpstream   r (Proxy uf df m r)
                      | RequestDownstream (df (Active uf df m r))
                      | M (m (Active uf df m r))

data Proxy uf df m r = Proxy (uf r -> Active uf df m r)

identity :: (Monad m, Functor f) => Proxy f f m r
identity = Proxy (\f -> (RequestDownstream
                         (fmap (\r -> (RespondUpstream r identity)) f)))

data Simple a b r = Simple a (b -> r)
                  deriving Functor

data NeverRespond r = NeverRespond
                    deriving Functor

data NeverRequest a = NeverRequest !(NeverRequest a)
                    deriving Functor

absurd :: NeverRequest a -> b
absurd (NeverRequest n) = absurd n

echo :: Monad m => Proxy (Simple a a) f m r
echo = Proxy (\(Simple a f) -> RespondUpstream (f a) echo)

(>->) :: (Monad m, Functor c) =>
         Proxy a b m r
      -> Proxy b c m (Active a b m r)
      -> Proxy a c m r
(>->) (Proxy f) g = Proxy $ \h -> M (activateUpper (f h) g)
  
logger :: (Show a, Show b) => Proxy (Simple a b) (Simple a b) IO r
logger = Proxy $ \(Simple a f) -> M $ do
  print ("Sending " ++ show a)
  return (RequestDownstream (Simple a (\b -> M $ do
                                        print ("Receiving " ++ show b)
                                        return (RespondUpstream (f b) logger))))
                                        
count :: Monad m => Int -> Proxy (Simple a Int) NeverRequest m r
count n = Proxy (\(Simple _ f) -> (RespondUpstream (f (n + 1)) (count (n + 1))))

activateUpper :: (Monad m, Functor c) =>
                 Active a b m r
              -> Proxy b c m (Active a b m r)
              -> m (Active a c m r)
activateUpper f g@(Proxy gf) = case f of
  RespondUpstream r await -> return (RespondUpstream r (await >-> g))
  RequestDownstream fact  -> sendDownstream (gf fact)
  M m                     -> do
    f' <- m
    activateUpper f' g

sendDownstream :: (Monad m, Functor c) =>
                  (Active b c m (Active a b m r))
               -> m (Active a c m r)
sendDownstream h' = do
  case h' of
    RespondUpstream r await -> activateUpper r await
    RequestDownstream dfact -> return (RequestDownstream (fmap (M . sendDownstream) dfact))
    M m                     -> m >>= sendDownstream

foo :: Show a => Proxy (Simple a Int) NeverRequest IO r
foo = logger >-> logger >-> count 0

iter :: Monad m =>
        f r
     -> Proxy f NeverRequest m r
     -> m (r, Proxy f NeverRequest m r)
iter a (Proxy f) = iterActive (f a)

iterActive :: Monad m =>
              Active t NeverRequest m r
           -> m (r, Proxy t NeverRequest m r)
iterActive fa = case fa of
    RequestDownstream n    -> absurd n
    RespondUpstream r next -> return (r, next)
    M m                    -> m >>= iterActive

main = do
  (r, foo) <- iter (Simple 10 id) foo
  print r
  (r, foo) <- iter (Simple 20 id) foo
  print r
  (r, foo) <- iter (Simple 30 id) foo
  print r

  
