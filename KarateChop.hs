module KarateChop where

import Data.Vector.Primitive as V
import Control.Applicative ((<$>), (<*>))

class Search a where
  search :: a -> Int -> Maybe Int

-- Approach one: Classic array

data VectorArgs = VectorArgs (Vector Int) Int

instance Search VectorArgs where
  search (VectorArgs _ 0)  _ = Nothing
  search (VectorArgs vector length) value = case value `compare` focus of
    GT -> (+) <$> (Just middle) <*> (search (VectorArgs (V.drop middle vector) (length - middle)) value)
    LT -> search (VectorArgs (V.take index vector) index) value
    EQ -> Just index
    where focus = vector ! index
          index = middle - 1
          middle = midpoint length 
        

midpoint :: Int -> Int
midpoint length = (length `quot` 2) + (length `rem` 2)

