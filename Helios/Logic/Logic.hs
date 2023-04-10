{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- TODO: as 1st-order term-rewriting system

module Helios.Logic.Logic where

import Data.List

import Helios.Logic.Term

--------------------------------------------------------------------------------
-- Propositional logic
--------------------------------------------------------------------------------

data PROP

sigProp :: Sig PROP
sigProp =
  signature
  [ "~_"
  , "_=>_"
  , "_&_"
  , "_|_"
  ]

instance SIG PROP where
  _SIG_ = sigProp

--------------------------------------------------------------------------------
-- Sequent calculus and semantic tableaux
--------------------------------------------------------------------------------

data Sequent term
  = Sequent
    { antecedents :: [term]
    , consequents :: [term]
    }

data Refut term
  = Refut (Either term term) [([term], [term])]
  deriving ( Eq )

instance Show term => Show (Refut term) where
  show (Refut top bot) =
    concat
    [ "Refut ("
    , showTop top
    , ") ["
    , intercalate ";" (map showBot bot)
    , "]"
    ]
      where
        showTop (Left x) =
          show x ++ "○"
        showTop (Right x) =
          "○" ++ show x
        showBot (l,r) =
          intercalate "," (map show l) ++ "○" ++ intercalate "," (map show r)

left, right :: [a] -> ([a],[a])
left = (,[])
right = ([],)
both :: [a] -> [a] -> ([a],[a])
both = (,)

analyticRefutationRules :: [Refut (Term PROP)]
analyticRefutationRules =
  [ Refut (Left "p&q") [left ["p","q"]]
  , Refut (Right "p&q") [right ["p"], right ["q"]]
  , Refut (Left "p|q") [left ["p"], left ["q"]]
  , Refut (Right "p|q") [right ["p","q"]]
  , Refut (Left "p=>q") [left ["q"], right ["p"]]
  , Refut (Right "p=>q") [both ["p"] ["q"]]
  , Refut (Left "~p") [right ["p"]]
  , Refut (Right "~p") [left ["p"]]
  ]
