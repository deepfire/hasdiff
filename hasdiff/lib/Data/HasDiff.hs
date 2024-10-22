{-# LANGUAGE Strict, StrictData #-}
module Data.HasDiff where

import Control.Concurrent                 qualified as Conc
import Control.Concurrent.Async           qualified as Async
import Control.Concurrent.Chan.Unagi (InChan, OutChan, newChan, readChan, writeChan)
import Control.Exception (IOException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra (handleIOExceptT)
import Data.Digest.XXHash.FFI.C           qualified as XXHash -- https://xxhash.com/doc/v0.8.2/
import Data.IntMap                        qualified as IntMap
import Data.Word (Word8, Word64)
import Data.Text (Text)
import Data.Text                          qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Foreign.Ptr (Ptr)
import System.Mem                         qualified as Sys
import System.IO                          qualified as IO
import System.IO.MMap                     qualified as MMap
import Text.Printf (hPrintf)

import Reflex hiding (Request)
import Reflex.Vty hiding (Request)

import Reflex.Extra
import Reflex.Vty.Widget.Extra
import Reflex.Vty.Widget.Viewport.Linear

import Data.Basis
import Data.Resources
import Util

data Mapping =
  Mapping
  { mFilepath :: FilePath
  , mPtr      :: Ptr Word8
  , mSize     :: Int
  , mOfft     :: Int
  , mSizeRaw  :: Int
  } deriving (Show)

mappingDiffCtx :: Mapping -> DiffCtx
mappingDiffCtx Mapping{..} =
  DiffCtx { dcPtr = mPtr, dcSize = fromInteger . toInteger $ mSize }

data DiffCtx =
  DiffCtx
  { dcPtr  :: {-# UNPACK #-} !(Ptr Word8)
  , dcSize :: {-# UNPACK #-} !(Word64)
  } deriving (Show)

mapFile :: FilePath -> ExceptT Text IO DiffCtx
mapFile f = do
  r <- fmap (mappingDiffCtx . uncurry4 (Mapping f)) $ handleIOExceptT (\(ioe :: IOException) -> showT ioe) $
       MMap.mmapFilePtr f   MMap.ReadOnly Nothing
  liftIO $ hPrintf IO.stderr "Mapped file: %s\n" (show f)
  pure r

data DiffState =
  DiffState
  { dsSizes   :: !(Int, Int)
  , dsSamples :: !Int
  , dsMatches :: !(Int, Int)
  }

runDiff :: (FilePath, DiffCtx) -> (FilePath, DiffCtx) -> ExceptT Text IO ()
runDiff (fp0, d0) (fp1, d1) = do
  startTime <- liftIO $ getCurrentTime <&> roundUTCTimeSec

  (statsW, statsR) :: (InChan Stats, OutChan Stats) <-
    liftIO newChan
  liftIO . (Async.link =<<) . Async.async $ forever $ do
    Conc.threadDelay 1000
    Sys.performBlockingMajorGC
    writeChan statsW =<< readResourceStats

  liftIO $ mainWidget $ withCtrlC $ initManager_ $ do
    !setupE <- getPostBuild
    !sE <- performEventAsync $
           ffor setupE $
             \() fire -> liftIO . (Async.link =<<) . Async.async $ forever $
               readChan statsR >>= fire
    !sB :: Behavior t (Maybe Stats) <- current <$> holdDyn Nothing (Just <$> sE)
    row $ mdo
      -- fid1 <- fmap fst $ tile' (fixed 30) $
      --   col $ grout (fixed 5) $ do
      --     !tis <- isFocused fid1
      --     boxTitle (fmap (bool roundedBoxStyle doubleBoxStyle) (current tis)) " Command line: " $ do
      --       grout (fixed 1) $ text (constant $ "file0: " <> T.pack fp0)
      --       grout (fixed 1) $ text (constant $ "file1: " <> T.pack fp1)
      !fid2 <- fmap fst $ tile' (fixed 50) $
        col $ grout (fixed 7) $ do
          !tis <- isFocused fid2
          boxTitle (fmap (bool roundedBoxStyle doubleBoxStyle) (current tis)) " Time: " $ do
           --grout (fixed 1) $ text (constant $ "start time: " <> T.pack (formatTime defaultTimeLocale "%c" startTime))
           --grout (fixed 1) $ text           (("now:        " <>) . maybe "" (T.pack . formatTime defaultTimeLocale "%c" . sTime) <$> sB)
           grout (fixed 1) $ text           (("live, kB:   " <>) . maybe "" (showT . (`div` 1024) . unLive . sLive) <$> sB)
           grout (fixed 1) $ text           (("heap, kB:   " <>) . maybe "" (showT . (`div` 1024) . unHeap . sHeap) <$> sB)
           grout (fixed 1) $ text           (("RSS, kB:    " <>) . maybe "" (showT . (`div` 1024) . unRSS  . sRSS)  <$> sB)
      pure ()

-- c_xxh64 :: Ptr a -> CSize -> CULLong -> IO CULLong
-- mmapFilePtr :: FilePath -> Mode -> Maybe (Int64, Int) -> IO (Ptr a, Int, Int, Int)  (ptr,rawsize,offset,size)
