{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Monad.Trans.Except
import Options.Generic

import Data.Basis

import Data.HasDiff
import TopHandler


data DiffFiles = DiffFiles FilePath FilePath
  deriving (Generic, Show)

data Command w =
  Diff
  { cdOrigin   :: w ::: FilePath <?> "Origin file"
  , cdModified :: w ::: FilePath <?> "Modified file"
  }
  deriving (Generic)

instance ParseRecord (Command Wrapped)
deriving instance Show (Command Unwrapped)

main :: IO ()
main = toplevelExceptionHandler $ do
  cmd <- unwrapRecord "hasdiff"
  orDie renderError (runCommand cmd)

data Error = Error (Command Unwrapped) Text

renderError :: Error -> Text
renderError (Error cmd err) =
  "ERROR: While executing operation:\n\n    '" <> showT cmd <> "'\n\n  " <> err

runCommand :: Command Unwrapped -> ExceptT Error IO ()
runCommand cmd@(Diff{cdOrigin, cdModified})  =
  (,) <$> withExceptT (Error cmd) ((cdOrigin,)   <$> mapFile cdOrigin)
      <*> withExceptT (Error cmd) ((cdModified,) <$> mapFile cdModified)
    >>= le . uncurry runDiff
 where le = withExceptT (Error cmd)
