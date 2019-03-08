--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Type-level programming                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Lab where

--------------------------------------------------------------------------------

import GHC.TypeLits

--------------------------------------------------------------------------------

type family Not (b :: Bool) :: Bool where
    Not True  = False
    Not False = True

--------------------------------------------------------------------------------

-- | A singleton type for type-level booleans.
data SBool b where
    STrue  :: SBool True
    SFalse :: SBool False

instance Eq (SBool b) where
    STrue  == STrue  = True
    SFalse == SFalse = True

instance Show (SBool b) where
    show STrue  = "STrue"
    show SFalse = "SFalse"

-- | `inot` @b@ computes the boolean negation of @b@.
inot :: SBool b -> SBool (Not b)
inot STrue  = SFalse
inot SFalse = STrue

-- | A kind-polymorphic proxy type.
data Proxy (a :: k) = Proxy

class KnownBool b where
    boolVal :: Proxy b -> Bool

instance KnownBool True where
    boolVal _ = True

instance KnownBool False where
    boolVal _ = False

--------------------------------------------------------------------------------

data HList (xs :: [*]) :: * where
    HNil  :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)

hhead :: HList (a ': as) -> a
hhead (HCons x _) = x

instance Show (HList '[]) where
    show HNil = "[]"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
    show (HCons x xs) = show x ++ " : " ++ show xs

--------------------------------------------------------------------------------
