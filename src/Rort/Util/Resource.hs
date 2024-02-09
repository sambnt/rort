module Rort.Util.Resource ( Resource
                          , get
                          , free
                          , allocate
                          , fromResourceT
                          ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import qualified Control.Monad.Trans.Resource as ResourceT

-- TODO: Revisit this type, use Acquire
data Resource a = Resource a (IO ())

instance Functor Resource where
  fmap f (Resource a fre)  = Resource (f a) fre

instance Applicative Resource where
  pure a = Resource a (pure ())
  (Resource rf rfFree) <*> (Resource ra raFree) =
    Resource (rf ra) (rfFree <> raFree)

instance Monad Resource where
  (Resource ma maFree) >>= f =
    let
      (Resource mb mbFree) = f ma
    in
      Resource mb (maFree <> mbFree)

get :: Resource a -> a
get (Resource a _ ) = a

free :: Resource a -> IO ()
free (Resource _ rFree) = rFree

fromResourceT :: (ReleaseKey, a) -> Resource a
fromResourceT (releaseKey, a) = Resource a (ResourceT.release releaseKey)

allocate
  :: MonadResource m
  => IO a
  -- ^ Acquire the resource
  -> (a -> IO ())
  -- ^ Free the resource
  -> m (Resource a)
allocate acquire destroy =
  fromResourceT <$> ResourceT.allocate acquire destroy
