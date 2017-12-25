{-# LANGUAGE TemplateHaskell #-}
module Task1_
       (
         chooseByIndices
       , len
       , listFields
       , SmthToText(..)
       ) where

import           Control.Monad
import           Language.Haskell.TH
import Data.Data
import Data.Functor.Const
import qualified Data.Text as T

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n ids = do
   as <- replicateM n (newName "x")
   lamE [tupP (map varP as)] $ tupE (map (map varE as !!) ids)

len :: Data a => a -> Int
len = getConst . gfoldl (\(Const c) _ -> Const (c+1)) (const 0)


{-
class SmthToText a where
  toText :: a -> T.Text

listFields :: Name -> Q [Dec]
listFields name = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name

    let names = map (\(name', _, _) -> name') fields

    let showField :: Name -> Q Exp
        showField name' = [|\x -> T.pack (s ++ " = " ++ show ($(varE name') x))|]
            where s = nameBase name'

    let showFields :: Q Exp
        showFields = listE $ map showField names

    [d|instance SmthToText $(conT name) where
        toText x = T.pack $ show ($showFields)|]
-}
