{-# LANGUAGE TemplateHaskell #-}

module Task1
       (
        chooseByIndices2
       ) where

import           Control.Monad
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.List
import qualified Data.Text as T

--import Task1_ (chooseByIndices, len, listFields, SmthToText(toText))

chooseByIndices2 :: Lift t => [Int] -> t -> Q Exp
chooseByIndices2 ids tup = do
  ttup <- lift tup
  return $ kek ttup where
    kek (TupE arr) = TupE (map (arr !!) ids)
    kek _ = error "Not a tuple"

--chooseByIndices3 ids tup = $(chooseByIndices (len tup) ids) tup
{-
data MyData = MyData
     { foo :: String
     , bar :: Int
     }

listFields ''MyData
-}
