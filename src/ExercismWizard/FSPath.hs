module ExercismWizard.FSPath
  ( module FP
  , toText
  )
where

import qualified Data.Text as T
import Filesystem.Path.CurrentOS as FP hiding (toText)
import qualified Filesystem.Path.CurrentOS

toText :: FP.FilePath -> T.Text
toText = either id id . Filesystem.Path.CurrentOS.toText
