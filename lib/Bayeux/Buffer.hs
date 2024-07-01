{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bayeux.Buffer
  ( MonadBuffer(..)
  ) where

import Bayeux.Cell ((===), inc, shr, patm, (~>), wildm)
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl (Rtl)
import Bayeux.Signal hiding (input)
import Bayeux.Width
import Data.Array
import Data.Finite
import Data.Semigroup
import Data.String
import Data.Word
import GHC.TypeNats

class MonadBuffer m where
  buffer
    :: Encode e
    => Width e
    => KnownNat n
    => OptSig e
    -> m (OptSig (Array (Finite n) e))

instance MonadBuffer Rtl where
  buffer
    :: forall e n
     . Encode e
    => Width e
    => KnownNat n
    => OptSig e
    -> Rtl (OptSig (Array (Finite n) e))
  buffer input = do
    ix <- process $ \ix  -> do
      ixPlusOne <- inc ix
      ix' <- patm ix
        [ maxBound ~> val (0 :: Finite n)
        , wildm ixPlusOne
        ]
      patm (valid input)
        [ True ~> ix'
        , wildm ix
        ]
    isFull <- ix === val maxBound
    buf <- process $ \buf -> do
      let shamt :: Word8
          shamt = fromIntegral w
      shiftedBuf <- shr buf $ val shamt
      let la = fromIntegral $ width (undefined :: Array (Finite n) e)
          le = fromIntegral w
          input' :: Sig (Array (Finite n) e)
          input' = Sig $ mconcat
            [ spec (value input)
            , fromString $ show ((la - 1) * le) <> "'" <> concat (replicate (la - 1) (replicate le '0'))
            ]
          mask :: Sig (Array (Finite n) e)
          mask  = let zs = replicate le '0'
                      ones = replicate (la - le) '1'
                  in fromString $ show la <> "'" <> zs <> ones
      maskedBuf <- shiftedBuf `C.and` mask
      buf' <- input' `C.or` maskedBuf
      patm (valid input)
        [ True ~> buf'
        , wildm buf
        ]
    flip OptSig buf <$> process (const $ isFull `C.logicAnd` valid input)
    where
      w :: Integer
      w = width (undefined :: e)
