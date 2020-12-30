{-
   Advent of Code 2020 day 14 solution.

   Author : mEFErqlg

   Run command : ghcc build day14.hs containers && ./day14 < input.txt
-}


{-# language ScopedTypeVariables #-}
{-# language CPP #-}

module Main where

import           System.IO (isEOF)
import           Data.Char (isDigit)
import           Text.Printf (printf)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Bits ((.&.), (.|.), xor, complement, shiftL, shiftR, popCount, setBit, clearBit)
import           Data.Int (Int64)
import           Data.Word (Word64)

import           Data.List (groupBy, sort)
import           System.CPUTime


-- part1
part1 :: [String] -> [(Int, Char)] -> Map Int Integer -> Map Int Integer
part1 []     mask m = m
part1 (l:ls) mask m =
  case l of
    'm':'e':'m':line ->
        let
            (address, value) :: (Int, Integer) = parseMem line
            value' = memOp mask value
        in
            part1 ls mask (M.insert address value' m)

    'm':'a':'s':'k':line' ->
        part1 ls (parseMask1 line') m
  where
    memOp :: [(Int, Char)] -> Integer -> Integer
    memOp []                val = val
    memOp ((idx, cmd):cmds) val =
      case cmd of
        '0' -> memOp cmds (clearBit val idx)
        '1' -> memOp cmds (setBit val idx)
        _   -> memOp cmds val


parseMask1 :: String -> [(Int, Char)]
parseMask1 str =
  let
      bitStr = dropWhile (not . isValid) str
  in
      filter ((/= 'X') . snd) $ zip [0..] (reverse bitStr)
  where
    isValid c = c `elem` ['X', '1', '0']


-- part2
type AddrTy = Word64

data Region = Region
  { regSBit :: AddrTy
  , regXBit :: AddrTy
  , regVal  :: Integer
  }

data Mask = Mask
  { maskSBit :: AddrTy
  , maskXBit :: AddrTy
  }

instance Show Region where
  show (Region s x v) = printf "REG | s:%064b x:%064b v:%4d" s x v

instance Show Mask where
  show (Mask s x) = printf "REG | s:%064b x:%064b" s x


part2 :: [String] -> Mask -> [Region] -> [Region]
part2 []       _                     regions = regions
part2 (l:ls)   mask@(Mask sBit xBit) regions =
  case l of
    'm':'e':'m':line ->
      let (addrI, val)   = parseMem line
          setBit         = (addrI .|. sBit) .&. (complement xBit)
          regions'       = overwrite (Region setBit xBit val) regions
      in  part2 ls mask regions'

    'm':'a':'s':'k':line ->
      part2 ls (parseMask2 line) regions


parseMask2 :: String -> Mask
parseMask2 str =
  let
      xmask = (foldl f 0 str)
      smask = (foldl g 0 str)
  in
     Mask { maskSBit = smask, maskXBit = xmask }
  where
    f :: AddrTy -> Char -> AddrTy
    f xmask c = case c of
                  'X' -> xmask `shiftL` 1 .|. 1
                  '0' -> xmask `shiftL` 1 .|. 0
                  '1' -> xmask `shiftL` 1 .|. 0
                  _   -> xmask
    g :: AddrTy -> Char -> AddrTy
    g smask c = case c of
                  '1' -> smask `shiftL` 1 .|. 1
                  'X' -> smask `shiftL` 1 .|. 0
                  '0' -> smask `shiftL` 1 .|. 0
                  _   -> smask


overwrite :: Region -> [Region] -> [Region]
overwrite inputRegion regions =
  inputRegion : concatMap (\region -> regionDiff region inputRegion) regions


focus36 :: AddrTy
focus36 = 0x800000000        -- 2^36 - 2^35

focus64 :: AddrTy
focus64 = 0x8000000000000000 -- 2^64 - 2^63


regionDiff:: Region -> Region -> [Region]
regionDiff ra@(Region sa1 xa1 val1) (Region sb1 xb1 _) =
  if
        let cxab = complement (xa1 .|. xb1)
        in  (sa1 .&. cxab) /= (sb1 .&. cxab)
  then
        [ra]
  else
        go sa1 xa1 0 focus64 (focus64 - 1)
  where
    csb1  = complement sb1
    go _   _   _    0     _    = []
    go sa  xa  pref focus post =
      if
            ((xa1 .&. focus) `xor` focus) == 0 && (xb1 .&. focus) == 0
      then
            let
                sa_ = (sa .&. pref) .|. (sa1 .&. post)
                sa' = sa_ .|. ( sb1 .&. focus)
                sr  = sa_ .|. (csb1 .&. focus)
                xa' = ((xa  .&. pref) .|. (xa1 .&. post)) .&. (complement focus)
                xr  = xa'
                r   = Region { regSBit = sr, regXBit = xr, regVal = val1 }
            in
                r : go sa' xa' pref' focus' post'
      else
            go sa xa pref' focus' post'
      where
        focus' = focus `shiftR` 1
        post'  = post  `shiftR` 1
        pref'  = pref + focus


calcXVal :: Region -> Integer
calcXVal (Region sa xa val) = val * (2 ^ (popCount xa))


parseMem :: (Read a, Read b) => String -> (a, b)
parseMem str =
   let
       (memStr, valueStr)  = break (== '=') str
       address             = read $ takeWhile isDigit $ dropWhile (not . isDigit) $ memStr
       value               = read $ dropWhile (not . isDigit) valueStr
   in
       (address, value)


formatTime :: Integral a => a -> String
formatTime f
  | f > 10^12  = show (fromIntegral f / 10^12) ++ "s"
  | f > 10^9   = show (fromIntegral f / 10^9) ++ "ms"
  | otherwise  = show (fromIntegral f / 10^6) ++ "µs"


main :: IO ()
main = do
  lines <- readInput
  -- print "-- Commands --"
  -- mapM_ print lines
  -- print "--------------"

  start <- getCPUTime
  let m1 = part1 lines [] M.empty
  let ans1 = M.foldl (+) 0 m1
  putStrLn $ "Sum of all memory values: " ++ show ans1
  end <- getCPUTime
  putStrLn $ "Time elapsed on part1: " ++ formatTime (end - start)

  putStrLn ""

  start <- getCPUTime
  let rs   = part2 lines (Mask 0 0) []
  let ans2 = sum $ fmap calcXVal rs
  putStrLn $ "Sum of all memory values: " ++ show ans2
  end <- getCPUTime
  putStrLn $ "Time elapsed on part2: " ++ formatTime (end - start)

-- NOTE: Uncomment below line to Show statstics.
-- #define DEBUG
#ifdef DEBUG
  mapM_ (\(xCount, size) -> printf "X's count: %3d, how many: %6d\n" xCount size)
               $ fmap (\ss -> (head ss, length ss))
               $ groupBy (==)
               $ sort
               $ fmap (\r -> (popCount (regXBit r))) m2
  printf "Total region counts: %d\n" (length m2)
#endif
  pure ()


readInput :: IO [String]
readInput = do
  b <- isEOF
  if b then pure []
  else do
    l  <- getLine
    ls <- readInput
    pure (l:ls)


test :: IO ()
test = do
   let all1s = 2^8 - 1
   let ma = "000000000000000000000000000000X1001X"
   let mb = "00000000000000000000000000000000X0XX"
   let mc = "000000000000000000000000000000000000"
   let Mask { maskSBit = sax, maskXBit = xa} = parseMask2 ma
   let Mask { maskSBit = sbx, maskXBit = xb} = parseMask2 mb
   let Mask { maskSBit = sbc, maskXBit = xc} = parseMask2 mc
   let a = 42
   let b = 26
   let c = 58 :: AddrTy
   let sa = (a .|. sax) .&. (complement xa)
   let sb = (b .|. sbx) .&. (complement xb)
   let sc = (c .|. sbc) .&. (complement xc)
   let cxab = complement (xa .|. xb)
   let len = 6
   let fmtSingle = "%06b\n"
   let fmtSingleS = "%6s\n"
   print ""
   printf (" a: " ++ fmtSingle) a
   printf ("sa: " ++ fmtSingle) sa
   printf ("xx: " ++ fmtSingle) (sa .&. cxab)
   printf ("xa: " ++ fmtSingle) xa
   printf ("ma: " ++ fmtSingleS) (drop (36 - len) ma)
   print ""
   printf (" b: " ++ fmtSingle) b
   printf ("sb: " ++ fmtSingle) sb
   printf ("xx: " ++ fmtSingle) (sb .&. cxab)
   printf ("xb: " ++ fmtSingle) xb
   printf ("mb: " ++ fmtSingleS) (drop (36 - len) mb)
   print ""
   printf (" c: " ++ fmtSingle) c
   printf ("sc: " ++ fmtSingle) sc
   printf ("xc: " ++ fmtSingle) xc
   printf ("mc: " ++ fmtSingleS) (drop (36 - len) mc)
   print ""

   let ra = Region sa xa 100
       rb = Region sb xb 1
       rc = Region sc xc 50
   let a_b   = rb: concatMap (\r -> regionDiff r rb) [ra]
   let a_b_c = rc: concatMap (\r -> regionDiff r rc) a_b
   let b_a  =  ra: concatMap (\r -> regionDiff r ra) [rb]
   -- let fmt = "sa: %036b\nxa: %036b\nval: %3d\n\n"
   let fmt = "sa: %06b\nxa: %06b\nval: %3d\n\n"
   print "a - b"
   mapM_ (\r -> printf fmt (regSBit r) (regXBit r) (regVal r)) a_b
   print "--------------------"
   -- print "a - b - c"
   -- mapM_ (\r -> printf fmt (regSBit r) (regXBit r) (regVal r)) a_b_c
   -- print "--------------------"
   print "b - a"
   mapM_ (\r -> printf fmt (regSBit r) (regXBit r) (regVal r)) b_a
   print "--------------------"
