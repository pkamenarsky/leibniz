{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Unsafe.Coerce
import Control.Category

import Prelude hiding (id)

-- | Two types are equal if they are _equal in all contexts_.
newtype Leibniz a b = Leibniz (forall f. f a -> f b)

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

data Req a =
    Req1 Int (a ∽ Int)
  | Req2 Int (a ∽ Bool)
  | Req3 (SubReq a)

evalSubReq :: SubReq a -> a
evalSubReq (SubReq1 a proof) = coerceSymm proof a
evalSubReq (SubReq2 a proof) = coerceSymm proof a

evalReq :: Req a -> a
evalReq (Req1 a proof) = coerceSymm proof a
evalReq (Req2 a proof) = coerceSymm proof (a > 50)
evalReq (Req3 a) = evalSubReq a

main :: IO ()
main = print a
  where
    a = evalReq (Req3 (SubReq1 "sub_req_1" id))
