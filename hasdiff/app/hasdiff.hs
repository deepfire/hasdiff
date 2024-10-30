{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-unused-matches #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- depends on (as per the cabal file):
-- , async
-- , reflex
-- , reflex-vty
-- , text
-- , transformers
-- , unagi-chan
-- , vty

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async           qualified as Async
import Control.Concurrent.Chan.Unagi (newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Text (pack)

import Graphics.Vty                       qualified as V
import Reflex hiding (Request)
import Reflex.Vty hiding (Request)


main :: IO ()
main = do
  (w, r) <- liftIO newChan
  liftIO . (Async.link =<<) . Async.async $ forever $ do
    -- comment the threadDelay out to get a memory instahog
    threadDelay 1
    writeChan w =<< pure (0 :: Int)

  liftIO $ mainWidget $ initManager_ $ do
    setupE <- getPostBuild
    inp <- input
    sE <- performEventAsync $
           ffor setupE $
             \() fire -> liftIO . (Async.link =<<) . Async.async $ forever $
               readChan r >>= fire
    sB <- current <$> holdDyn 0 sE
    col $ grout (fixed 17) $ do
      boxTitle (constant doubleBoxStyle) "" $
        grout (fixed 1) $ text (pack . show <$> sB)
    pure $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing
