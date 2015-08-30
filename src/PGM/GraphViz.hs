{-# OPTIONS_GHC -Wall #-}

module PGM.GraphViz
       ( previewGraph
       , previewGraph'
       ) where

import Control.Concurrent ( threadDelay )
import Data.Foldable ( toList )
import Data.GraphViz ( Labellable, toLabelValue, preview )
import Data.GraphViz.Attributes.Complete ( Label )
import qualified Data.Graph.Inductive as FGL

import PGM.Graph ( Graph(..), GRandVarExpr(..) )
import PGM.Vars ( RandVarExpr(..) )

-- | show a nice Dot graph
previewGraph :: Graph GRandVarExpr -> IO ()
previewGraph gr = do
  preview $ toFGLGraph gr
  threadDelay 100000

-- | show a nice Dot graph with labeled edges
previewGraph' :: Graph GRandVarExpr -> IO ()
previewGraph' gr = do
  preview $ FGL.emap (\(FGLEdge x) -> FGLEdge' x) $ toFGLGraph gr
  threadDelay 100000

toFGLGraph :: Graph GRandVarExpr -> FGL.Gr FGLNode FGLEdge
toFGLGraph (Graph nodes _) = FGL.mkGraph fglNodes fglEdges
  where
    fglNodes = map (\(k,gexpr) -> (k, FGLNode (k, gexpr))) nodes
    fglEdges = concatMap nodeToEdges nodes
      where
        nodeToEdges (k,gexpr) = map (\p -> (p,k,FGLEdge (p,k,gexpr))) (getParents gexpr)

getParents :: GRandVarExpr Int -> [Int]
getParents = toList

data FGLNode = FGLNode (Int, GRandVarExpr Int)
data FGLEdge = FGLEdge (Int, Int, GRandVarExpr Int)
data FGLEdge' = FGLEdge' (Int, Int, GRandVarExpr Int)
instance Eq FGLEdge where
  (==) (FGLEdge (p0,k0,g0)) (FGLEdge (p1,k1,g1)) = (==) (p0,k0,g0) (p1,k1,g1)
instance Eq FGLEdge' where
  (==) (FGLEdge' (p0,k0,g0)) (FGLEdge' (p1,k1,g1)) = (==) (p0,k0,g0) (p1,k1,g1)
instance Ord FGLEdge where
  compare (FGLEdge (p0,k0,g0)) (FGLEdge (p1,k1,g1)) = compare (p0,k0,g0) (p1,k1,g1)
instance Ord FGLEdge' where
  compare (FGLEdge' (p0,k0,g0)) (FGLEdge' (p1,k1,g1)) = compare (p0,k0,g0) (p1,k1,g1)

instance Labellable FGLEdge where
  toLabelValue (FGLEdge (p,k,_)) = toLabelValue $ show p ++ " --> " ++ show k
instance Labellable FGLEdge' where
  toLabelValue (FGLEdge' (_,_,gexpr)) = toLabelValue $ show gexpr

tlv :: Int -> String -> Label
tlv k s = toLabelValue $ show k ++ ": " ++ s

instance Labellable FGLNode where
  toLabelValue (FGLNode (k, (GTopLevel name probs))) = tlv k (show (TopLevel name probs))
  toLabelValue (FGLNode (k, (GConst val))) = tlv k (show (Const val))
  toLabelValue (FGLNode (k, (GAdd _ _))) = tlv k "+"
  toLabelValue (FGLNode (k, (GMul _ _))) = tlv k "*"
  toLabelValue (FGLNode (k, (GAbs _))) = tlv k "abs"
  toLabelValue (FGLNode (k, (GSignum _))) = tlv k "signum"
