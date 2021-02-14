module ExercismWizard.Main
  ( main
  )
where

import Control.Monad
import ExercismWizard.CommandParse
import ExercismWizard.Execute
import Prelude hiding (FilePath)

main :: IO ()
main = join $ execute <$> findCli <*> getArgs
