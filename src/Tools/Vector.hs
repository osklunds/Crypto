
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tools.Vector
( Nat(..)
, Add
, Vector(..)
)
where

data Nat = Zero | Succ Nat

type family Add n m where
  Add Zero n = n
  Add (Succ n) m = Add n (Succ m)


data Vector (n :: Nat) (a :: *) where
    Nil  :: Vector Zero a
    (:-) :: a -> Vector n a -> Vector (Succ n) a

infixr :-

