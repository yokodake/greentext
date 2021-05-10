{-# LANGUAGE TemplateHaskell #-}
module Utils where

import           Language.Haskell.TH.Syntax (mkName, nameBase)
import           Lens.Micro                 (ASetter', set, (&), (.~))
import           Lens.Micro.TH              (DefName (TopName), lensField,
                                             lensRules, makeLensesWith)

makeLenses' = makeLensesWith 
    $ lensRules 
    & lensField .~ (\_ _ n -> case nameBase n of
                              '_':_ -> []
                              xs    -> [TopName (mkName ('_':xs))])
