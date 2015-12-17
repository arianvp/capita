{-#LANGUAGE ExistentialQuantification, DeriveFunctor #-}
module Trace where
import Control.Concurrent
import Control.Monad.Free
import Control.DeepSeq
import Data.IORef

{-
  data Free f a = Pure a	 
                | Free (f (Free f a)

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) `fmap` m)

 -}
newtype IVar a = IVar (IORef (IVarContents a))
data IVarContents a = Full a | Blocked [a -> Par ()]

data ParF next =  Fork next next
                | Done
                |  forall a. Get (IVar a) (a -> next)
                |  forall a. Put (IVar a) a next
                |  forall a. New (IVar a -> next)

instance Functor ParF where
  fmap f (Fork n n2) = Fork (f n) (f n2)
  fmap f (Done) = Done
  fmap f (Get v g) = Get v (f . g)
  fmap f (Put k a n) = Put k a (f n)
  fmap f (New g) = New (f . g)
  


type Par = Free ParF

fork :: Par () -> Par ()
fork p = Free (Fork (Free Done) p)


new :: Par (IVar a)
new = Free (New Pure)

get :: IVar a -> Par a
get x = Free (Get x Pure)

put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (Free (Put v a (Pure ())))





