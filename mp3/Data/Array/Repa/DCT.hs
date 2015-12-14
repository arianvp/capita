module Data.Array.Repa.DCT where
import Control.Monad.Identity
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

import Data.Array.CArray (CArray)
import Data.Array.Repa ((:.)(..), Array, DIM1, DIM2, DIM3, Z(..), (-^), (+^), (/^))
import Data.Array.Repa.Repr.ForeignPtr (F)

import qualified Data.Array.CArray as C
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Math.FFT as FFT

{-# INLINE c2r #-}
c2r :: CArray Int Double -> Array F DIM1 Double
c2r carr = case C.toForeignPtr carr of
  (n, fptr) -> let sh = Z:.n in
    R.computeS $ R.fromFunction sh $ \ix ->
      unsafePerformIO $ withForeignPtr fptr $ \ptr ->
        peekElemOff ptr $ R.toIndex sh ix

{-# INLINE r2c #-}
r2c :: Array F DIM1 Double -> CArray Int Double
r2c rarr = unsafePerformIO $ do
  let _:.nelem = R.extent rarr
      fptr = RF.toForeignPtr rarr
  C.unsafeForeignPtrToCArray fptr (0,nelem-1)

dct4 :: Array F DIM1 Double -> Array F DIM1 Double
dct4 = c2r . FFT.dct4 . r2c

-- We assume an Even n as input
--
--
{-# INLINE reverse' #-}
reverse' :: R.Source r e => Array r DIM1 e -> Array R.D DIM1 e
reverse' a = R.backpermute (R.extent a) (\(Z:.i) -> Z:.((R.size (R.extent a))-1-i)) a

{-# INLINE mdct #-}
--      2N                   -> N
mdct :: Array F  DIM1 Double -> Array F DIM1 Double
mdct x =
  let sz = R.size (R.extent x)
      sl = sz `div` 4
      sl2 = sl*2
      sl3 = sl*3
      sl4 = sz
      a = R.extract (Z:.0) (Z:.sl) x
      b = R.extract (Z:.sl+1) (Z:. sl2) x 
      c = R.extract (Z:.sl2+1) (Z:. sl3) x 
      d = R.extract (Z:.sl3+1) (Z:. sl4) x
      k = reverse' c -^ d
      l = a -^ reverse' b
      t = k `R.append` l
  in dct4 $ runIdentity $ R.computeP t

{-# INLINE imdct #-}
--       N                   -> 2N
imdct :: Array F DIM1 Double -> Array F DIM1 Double
imdct x =
  let k = mdct x
      sz = R.size (R.extent k)
      sl = sz `div` 2
      _A = R.extract (Z:.0) (Z:.sl) k
      _B = R.extract (Z:.(sl+1)) (Z:.sz) k
      g = _A -^ reverse' _A
      h = _B +^ reverse' _B
  in  runIdentity $ R.computeP $ R.map (/2) (g `R.append` h)
      


