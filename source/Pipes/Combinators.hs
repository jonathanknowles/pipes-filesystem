module Pipes.Combinators where

import Pipes ((>->), Pipe)

import qualified Pipes.Prelude as P

filterMap :: Monad m => (a -> Bool) -> (a -> b) -> Pipe a b m r
filterMap p m = P.map (\x -> if p x then Just $ m x else Nothing) >-> P.concat
