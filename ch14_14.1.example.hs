{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RebindableSyntax #-}

import Control.Monad.Indexed qualified as I
import Data.Coerce (coerce)
import Fcf (Eval, Filter, Find, IsJust, Not, TyEq, type (<=<), type (=<<))
import GHC.TypeLits (Nat, type (+))
import Language.Haskell.DoNotation (Monad, pure, (>>=))
import System.IO qualified as IO
import Prelude hiding (Monad, pure, (>>=))

newtype Ix m i j a = Ix
  { unsafeRunIx :: m a
  }
  deriving (Functor, Applicative, Monad)

instance Functor m => I.IxFunctor (Ix m) where
  imap = fmap

instance (Applicative m, Monad m) => I.IxPointed (Ix m) where
  ireturn = pure

instance (Applicative m, Monad m) => I.IxApplicative (Ix m) where
  iap :: forall i j k a b. Ix m i j (a -> b) -> Ix m j k a -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance Monad m => I.IxMonad (Ix m) where
  ibind :: forall i j k a b. (a -> Ix m j k b) -> Ix m i j a -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b

data LinearState = LinearState
  { linearNextKey :: Nat
  , linearOpenKeys :: [Nat]
  }

newtype Linear s (i :: LinearState) (j :: LinearState) a = Linear
  { unsafeRunLinear :: Ix IO i j a
  }
  deriving (I.IxFunctor, I.IxPointed, I.IxApplicative, I.IxMonad)

newtype Handle s key = Handle
  { unsafeGetHandle :: IO.Handle
  }

openFile
  :: FilePath
  -> IO.IOMode
  -> Linear
      s
      ('LinearState next open)
      ('LinearState (next + 1) (next ': open))
      (Handle s next)
openFile = coerce IO.openFile

type IsOpen (key :: k) (ts :: [k]) = IsJust =<< Find (TyEq key) ts

type Close (key :: k) (ts :: [k]) = Filter (Not <=< TyEq key) ts

closeFile
  :: Eval (IsOpen key open) ~ 'True
  => Handle s key
  -> Linear
      s
      ('LinearState next open)
      ('LinearState next (Eval (Close key open)))
      ()
closeFile = coerce IO.hClose

-- If I try to write this point-free GHC gets angry. Not sure why that is:
--
-- • Couldn't match representation of type ‘a0’ with that of ‘IO a’
--     arising from a use of ‘coerce’
--
-- 79 | runLinear = coerce
--    |             ^^^^^^
runLinear :: (forall s. Linear s ('LinearState 0 '[]) ('LinearState n '[]) a) -> IO a
runLinear a = coerce a
