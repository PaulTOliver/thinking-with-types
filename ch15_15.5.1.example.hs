{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson (Value, object, (.=))
import Data.Constraint (Dict (Dict))
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Singletons (Sing, SingI, sing)
import Data.Singletons.Base.TH (genSingletons, singDecideInstance)
import Data.Singletons.Decide qualified as D

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f

withSigma :: (forall (a :: k). Sing a -> f a -> r) -> Sigma f -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma = Sigma sing

fromSigma
  :: forall k (a :: k) (f :: k -> Type)
   . (SingI a, D.SDecide k)
  => Sigma f
  -> Maybe (f a)
fromSigma (Sigma s f) =
  case s D.%~ sing @a of
    D.Proved D.Refl -> Just f
    D.Disproved _ -> Nothing

class Dict1 c (f :: k -> Type) where
  dict1 :: Sing (a :: k) -> Dict (c (f a))

data LogType = JsonMsg | TextMsg deriving (Eq, Ord, Show)

$(genSingletons [''LogType])
$(singDecideInstance ''LogType)

data family LogMsg (msg :: LogType)

newtype instance LogMsg 'JsonMsg = Json Value deriving (Eq, Show)

newtype instance LogMsg 'TextMsg = Text String deriving (Eq, Show)

instance
  ( c (LogMsg 'JsonMsg)
  , c (LogMsg 'TextMsg)
  )
  => Dict1 c LogMsg
  where
  dict1 SJsonMsg = Dict
  dict1 STextMsg = Dict

catSigmas
  :: forall k (a :: k) f
   . ( SingI a
     , D.SDecide k
     )
  => [Sigma f]
  -> [f a]
catSigmas = mapMaybe fromSigma

logs :: [Sigma LogMsg]
logs =
  [ toSigma $ Text "hello"
  , toSigma $ Json $ object ["world" .= (5 :: Int)]
  , toSigma $ Text "structured logging"
  ]

jsonLogs :: [LogMsg 'JsonMsg]
jsonLogs = catSigmas logs

textLogs :: [LogMsg 'TextMsg]
textLogs = catSigmas logs
