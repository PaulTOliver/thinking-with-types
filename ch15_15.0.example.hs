{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Constraint (Dict (Dict))
import Data.Foldable (for_)
import Data.Kind (Type)

data SBool (b :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

fromSBool :: SBool b -> Bool
fromSBool STrue = True
fromSBool SFalse = False

toSBool :: Bool -> SomeSBool
toSBool True = SomeSBool STrue
toSBool False = SomeSBool SFalse

withSomeSBool :: SomeSBool -> (forall (b :: Bool). SBool b -> r) -> r
withSomeSBool (SomeSBool s) f = f s

class Monad (LoggingMonad b) => MonadLogging (b :: Bool) where
  type LoggingMonad b = (r :: Type -> Type) | r -> b
  logMsg :: String -> LoggingMonad b ()
  runLogging :: LoggingMonad b a -> IO a

instance MonadLogging 'False where
  type LoggingMonad 'False = IO
  logMsg _ = pure ()
  runLogging = id

instance MonadLogging 'True where
  type LoggingMonad 'True = WriterT [String] IO
  logMsg s = tell [s]
  runLogging m = do
    (a, w) <- runWriterT m
    for_ w putStrLn
    pure a

program :: MonadLogging b => LoggingMonad b ()
program = do
  logMsg "hello world"
  pure ()

dict :: (c 'True, c 'False) => SBool b -> Dict (c b)
dict STrue = Dict
dict SFalse = Dict

test :: Bool -> IO ()
test bool =
  withSomeSBool (toSBool bool) $ \(sb :: SBool b) ->
    case dict @MonadLogging sb of
      Dict -> runLogging @b program
