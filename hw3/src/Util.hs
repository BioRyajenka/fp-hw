module Util
       ( fromRight
       ) where

fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x
