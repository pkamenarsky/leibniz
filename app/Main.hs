{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Aeson as A

import GHC.Generics

import Unsafe.Coerce
import Control.Category

import Prelude hiding (id)

-- | Two types are equal if they are _equal in all contexts_.
newtype Leibniz a b = Leibniz (forall f. f a -> f b)

instance A.ToJSON (Leibniz a b) where
  toJSON _ = A.toJSON ()

instance A.FromJSON (Leibniz a b) where
  parseJSON _ = return $ Leibniz unsafeCoerce

instance Show (Leibniz a b) where
  show _ = ""

instance Category Leibniz where
  id    = Leibniz id
  _ . _ = Leibniz unsafeCoerce

type (∽) = Leibniz

infix 4 ∽

-- | Unpack a Leibniz equality.
runLeibniz :: forall f a b. a ∽ b -> f a -> f b
runLeibniz (Leibniz f) = f

-- | Equality is symmetric.
symm :: forall a b. a ∽ b -> b ∽ a
symm _ = Leibniz unsafeCoerce

-- | Coerce a value of type `b` to a value of the Leibniz-equal type `a`.
coerceSymm :: forall a b. a ∽ b -> b -> a
coerceSymm _ = unsafeCoerce

-- | Coerce a value of type `a` to a value of the Leibniz-equal type `b`.
coerce :: forall a b. a ∽ b -> a -> b
coerce _ = unsafeCoerce

--------------------------------------------------------------------------------

data SubReq a =
    SubReq1 String (a ∽ String)
  | SubReq2 Double (a ∽ Double)
  deriving (Show, Generic, A.ToJSON)

data Req a =
    Req1 Int (a ∽ Int)
  | Req2 Int (a ∽ Bool)
  | Req3 (SubReq a)
  deriving (Show, Generic, A.ToJSON)

data Exists f = forall a. A.ToJSON a => Exists (f a)

class FromJsonE a where
  parseJsonE :: A.Value -> Maybe (Exists a)

instance FromJsonE Req where
  parseJsonE _ = Just (Exists (Req1 4 id))

match :: (FromJsonE req, Monad m) => A.Value -> (forall a. A.ToJSON a => req a -> m a) -> m A.Value
match v f = case parseJsonE v of
  Just (Exists v) -> do
    a <- f v
    return $ A.toJSON a

evalSubReq :: SubReq a -> a
evalSubReq (SubReq1 a proof) = coerceSymm proof a
evalSubReq (SubReq2 a proof) = coerceSymm proof a

evalReq :: Req a -> a
evalReq (Req1 a proof) = coerceSymm proof a
evalReq (Req2 a proof) = coerceSymm proof (a > 50)
evalReq (Req3 a) = evalSubReq a

main :: IO ()
main = print json
  where
    a    = evalReq (Req3 (SubReq1 "sub_req_1" id))
    json = A.encode (Req3 (SubReq1 "sub_req_1" id))
