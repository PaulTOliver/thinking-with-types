{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Aeson (Value (Array, Object, String), object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key (fromString)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Vector (fromList)
import GHC.Generics (C, D, D1, Generic, K1, M1, Meta (MetaData, MetaSel), Rep, S, (:*:), (:+:))
import GHC.TypeLits (ErrorMessage (Text), KnownSymbol, Symbol, TypeError, symbolVal)

class GSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type family RepName (x :: Type -> Type) :: Symbol where
  RepName (D1 ('MetaData nm _ _ _) _) = nm

type family TypeName (t :: Type) :: Symbol where
  TypeName t = RepName (Rep t)

type family ToJSONType (a :: Type) :: Symbol where
  ToJSONType Int = "integer"
  ToJSONType Integer = "integer"
  ToJSONType Float = "number"
  ToJSONType Double = "number"
  ToJSONType String = "string"
  ToJSONType Bool = "boolean"
  ToJSONType [a] = "array"
  ToJSONType a = TypeName a

makeTypeObj :: forall a. KnownSymbol (ToJSONType a) => Value
makeTypeObj = object ["type" .= String (pack $ symbolVal $ Proxy @(ToJSONType a))]

makePropertyObj :: forall name. KnownSymbol name => Value -> Value
makePropertyObj v = object [fromString (symbolVal $ Proxy @name) .= v]

instance
  ( KnownSymbol nm
  , KnownSymbol (ToJSONType a)
  )
  => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 a))
  where
  gschema = do
    emitRequired @nm
    return $ makePropertyObj @nm $ makeTypeObj @a
  {-# INLINE gschema #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol nm
  , KnownSymbol (ToJSONType a)
  )
  => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 (Maybe a)))
  where
  gschema = return $ makePropertyObj @nm $ makeTypeObj @a
  {-# INLINE gschema #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol nm
  , KnownSymbol (ToJSONType [a])
  , KnownSymbol (ToJSONType a)
  )
  => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 [a]))
  where
  gschema = do
    emitRequired @nm
    return $ makePropertyObj @nm $ mergeObjects innerType $ makeTypeObj @[a]
   where
    innerType = object ["items" .= makeTypeObj @a]
  {-# INLINE gschema #-}

instance
  {-# OVERLAPPING #-}
  KnownSymbol nm
  => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 String))
  where
  gschema = do
    emitRequired @nm
    return $ makePropertyObj @nm $ makeTypeObj @String
  {-# INLINE gschema #-}

instance (GSchema f, GSchema g) => GSchema (f :*: g) where
  gschema = mergeObjects <$> gschema @f <*> gschema @g
  {-# INLINE gschema #-}

instance
  TypeError
    ('Text "JSON Schema does not support sum types")
  => GSchema (f :+: g)
  where
  gschema = error "JSON Schema does not support sum types"
  {-# INLINE gschema #-}

instance GSchema a => GSchema (M1 C _1 a) where
  gschema = gschema @a
  {-# INLINE gschema #-}

instance
  ( GSchema a
  , KnownSymbol nm
  )
  => GSchema (M1 D ('MetaData nm _1 _2 _3) a)
  where
  gschema = do
    sch <- gschema @a
    return $
      object
        [ "title" .= String (pack $ symbolVal $ Proxy @nm)
        , "type" .= String "object"
        , "properties" .= sch
        ]
  {-# INLINE gschema #-}

schema :: forall a. (GSchema (Rep a), Generic a) => Value
schema = mergeObjects v $ object ["required" .= Array (fromList $ String <$> reqs)]
 where
  (v, reqs) = runWriter $ gschema @(Rep a)
{-# INLINE schema #-}

pp :: Value -> IO ()
pp = LC8.putStrLn . encodePretty
