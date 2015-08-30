{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance MuRef RandVarExpr
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language TypeFamilies #-}

module PGM.Graph
       ( Graph(..)
       , reifyExpr
       ) where

import Control.Applicative hiding ( Const )
import Data.Foldable ( Foldable )
import Data.Reify
import Data.Traversable ( Traversable )

import PGM.Vars ( RandVarExpr(..), Val )

data GRandVarExpr a = GTopLevel String [(Val, Double)]
                    | GConst Val
                    | GAdd a a
                    | GMul a a
                    | GAbs a
                    | GSignum a
              deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance MuRef RandVarExpr where
  type DeRef RandVarExpr = GRandVarExpr
  mapDeRef _ (TopLevel name probs) = pure $ GTopLevel name probs
  mapDeRef _ (Const val) = pure $ GConst val
  mapDeRef f (Signum x) = GSignum <$> f x
  mapDeRef f (Abs x) = GAbs <$> f x
  mapDeRef f (Add x y) = GAdd <$> f x <*> f y
  mapDeRef f (Mul x y) = GMul <$> f x <*> f y

reifyExpr :: RandVarExpr -> IO (Graph GRandVarExpr)
reifyExpr = reifyGraph
