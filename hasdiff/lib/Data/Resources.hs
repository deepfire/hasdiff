{-# LANGUAGE Strict, StrictData #-}
module Data.Resources where

import Data.Maybe (fromMaybe)
import Data.Text                          qualified as T
import Data.Text.Read                     qualified as T
import Data.Text.IO                       qualified as IO
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import GHC.Stats                          qualified as GhcStats
import System.Posix.Files                 qualified as IO
import Quiet (Quiet (..))


newtype Live    = Live    { unLive    :: Int } deriving (Enum, Eq, Generic, Integral, Num, Ord, Real) deriving Show via (Quiet Live)
newtype Heap    = Heap    { unHeap    :: Int } deriving (Enum, Eq, Generic, Integral, Num, Ord, Real) deriving Show via (Quiet Heap)
newtype RSS     = RSS     { unRSS     :: Int } deriving (Enum, Eq, Generic, Integral, Num, Ord, Real) deriving Show via (Quiet RSS)

data Stats =
  Stats
  { sTime :: !UTCTime
  , sLive :: !Live
  , sHeap :: !Heap
  , sRSS  :: !RSS
  }

nullStats :: UTCTime -> Stats
nullStats t =
  Stats
  { sTime = t
  , sLive = 0
  , sHeap = 0
  , sRSS  = 0
  }

readResourceStats :: IO Stats
readResourceStats = do
  mkProcStats <$> getCurrentTime
              <*> GhcStats.getRTSStats
              <*> readProcList "/proc/self/stat"
 where
   mkProcStats :: UTCTime -> GhcStats.RTSStats -> [Int] -> Stats
   mkProcStats !sTime !(GhcStats.gc -> gc) !(head . drop 23 -> rss) =
     Stats
       { sLive       = Live . fromIntegral . GhcStats.gcdetails_live_bytes $ gc
       , sHeap       = Heap . fromIntegral . GhcStats.gcdetails_mem_in_use_bytes $ gc
       , sRSS        = RSS $ rss * 4096
       , ..
       }
   mkProcStats t _ [] = nullStats t

readProcList :: FilePath -> IO [Int]
readProcList fp = do
    fs <- IO.getFileStatus fp
    if readable fs
    then do
        !cs <- IO.readFile fp
        return $ map (fromMaybe 0 . readMaybeText) (T.words cs)
    else
        return []
  where
    readable fs = IO.intersectFileModes (IO.fileMode fs) IO.ownerReadMode == IO.ownerReadMode

readMaybeText :: T.Text -> Maybe Int
readMaybeText t =
  case T.decimal t of
    Right (v, _)  -> Just v
    _             -> Nothing
