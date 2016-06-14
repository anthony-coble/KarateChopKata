module KarateChop where

import Data.Vector.Primitive as V
import Control.Applicative ((<$>), (<*>))

class Search a where
  search :: a -> Int -> Maybe Int

-- Approach one: Classic array chop array

data VectorArgs = VectorArgs (Vector Int) Int

instance Search VectorArgs where
  search (VectorArgs _ 0)  _ = Nothing
  search (VectorArgs vector length) value = case value `compare` focus of
    GT -> (+) <$> Just middle <*> search (VectorArgs (V.drop middle vector) (length - middle)) value
    LT -> search (VectorArgs (V.take index vector) index) value
    EQ -> Just index
    where focus = vector ! index
          index = middle - 1
          middle = midpoint length 
        
-- Approach two: Classic array index math

data VectorArgs2 = VectorArgs2 (Vector Int) Int

instance Search VectorArgs2 where
  search (VectorArgs2 _ 0)  _ = Nothing
  search (VectorArgs2 vector length) value = search' (index length) (midpoint length)
    where search' position 1 = if value == (vector ! position) 
                               then Just position
                               else Nothing
          search' position change = case value `compare` (vector ! position) of
            GT -> search' (position + midpoint change) (midpoint change)
            LT -> search' (position - midpoint change) (midpoint change)
            EQ -> Just position
          index i = midpoint i - 1
         

midpoint :: Int -> Int
midpoint length = (length `quot` 2) + (length `rem` 2)

