module Logic.Modal where

import Data.Functor.Adjunction
import Control.Comonad
import Control.Comonad

class (Monad dia, Comonad box) => Modal4 dia box where


class (Modal4 dia box) => Modal5 dia box where
  axiomK :: box (a -> b) -> box a -> box b
  axiomT :: box a -> a
  axiom5 :: dia a -> box (dia a)
