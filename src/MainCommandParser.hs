module MainCommandParser
  ( mainCmdParser
  )
where



#include "qprelude/default.inc"

import           Brick.Types

import           UI.Butcher.Monadic        as Butcher
import           UI.Butcher.Monadic.Types  as Butcher
import           UI.Butcher.Monadic.Pretty as Butcher
import           UI.Butcher.Monadic.Param  as Butcher

import qualified Text.PrettyPrint as PP

import           Data.Dynamic

import           System.Environment



mainCmdParser
  :: Text
  -> (() -> IO ())-- trigger shutdown
  -> Butcher.CmdParser
       Identity
       (IO [Text])
       ()
mainCmdParser _hist shutdownT = do
  addCmd "exit" $ do
    addCmdHelp $ PP.text "Stop the program"
    addCmdImpl $ do
      shutdownT ()
      return [Text.pack "exiting.."]
  addCmd "echo" $ do
    rest <- addRestOfInputStringParam "STRING" mempty
    addCmdImpl $ do
      return [Text.pack rest]
  addCmd "env" $ do
    addCmdImpl $ do
      envPairs <- getEnvironment
      return [Text.pack $ key ++ " = " ++ val | (key, val) <- envPairs]


