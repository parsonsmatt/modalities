{-# LANGUAGE FunctionalDependencies #-}
module Logic.Modal where

import Control.Comonad

class (Functor g, Functor f) => Adjunction f g | f -> g, g -> f where
    unit   :: a -> g (f a)
    counit :: f (g a) -> a
    left   :: (f a -> b) -> a -> g b
    right  :: (a -> g b) -> f a -> b

newtype Compose f g a = Compose { decompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f = Compose . fmap (fmap f) . decompose

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure
    Compose fgf <*> Compose fga = Compose ((<*>) <$> fgf <*> fga)

instance (Adjunction f g, Applicative f, Applicative g) => Monad (Compose g f) where
    return = Compose . unit
    m >>= f = Compose . fmap (right (decompose . f)) . decompose $ m

instance (Adjunction f g) => Comonad (Compose f g) where
    extract = counit . decompose
    extend f = Compose . fmap (left (f . Compose)) . decompose
