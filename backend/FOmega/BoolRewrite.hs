{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies, TypeOperators #-}
module BoolRewrite where

    import Data.Type.Bool

    type And j k = (j && k) ~ True => j ~ True
        
  
