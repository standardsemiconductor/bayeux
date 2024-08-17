module Bayeux.Vector
  ( VecSig(..)
  ) where

import Yosys.Rtl

newtype VecSig n a = VecSig{ unVecSig :: SigSpec }
