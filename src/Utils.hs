{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Utils (module Utils, F.sizeOf, Storable) where

import           Foreign                    (Ptr, Storable)
import qualified Foreign                    as F
import           Language.Haskell.TH.Syntax (mkName, nameBase)
import           Lens.Micro                 (ASetter', set, (&), (.~))
import           Lens.Micro.TH              (DefName (TopName), lensField,
                                             lensRules, makeLensesWith)

-- | like 'Lens.Micro.makeLenses' but generates lenses starting with @_@
--   for fields that don't start with it.
makeLenses_ = makeLensesWith
    $ lensRules
    & lensField .~ (\_ _ n -> case nameBase n of
                              '_':_ -> []
                              xs    -> [TopName (mkName ('_':xs))])

-- | reverse the type variables so the first type applied arg is @b@,
-- @a@ will almost always be inferrred from the function arg anyways.
castptr :: forall b a. Ptr a -> Ptr b
castptr = F.castPtr

-- | sizeof as a polymorphic variable instead, way better with @-XTypeApplications@
-- than passing a dummy term, also much more succint for some cases
sizeof :: forall a. Storable a => Int
sizeof = F.sizeOf (undefined :: a)
