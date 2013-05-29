module ExternalDataCEventOracle where

import Foreign
import Curry
import CurryPrelude

data CRef

type Ref = Ptr CRef

newtype C_Ref = C_Ref Ref

instance Show C_Ref
instance Read C_Ref
instance Eq   C_Ref
instance BaseCurry C_Ref
instance Curry C_Ref


