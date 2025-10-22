module Main where

import Data.Text qualified
import Hilcode.Clock qualified as Clock
import Hilcode.FileSystem (AbsDir)
import Hilcode.FileSystem qualified as FileSystem
import Hilcode.Glob qualified
import Hilcode.Logger (LogLevel (..))
import Hilcode.Logger qualified as Logger

main :: IO ()
main = do
    clock :: Clock.Handle IO <- Clock.new
    let logger :: Logger.Handle IO = Logger.new clock DEBUG
    logger.debug "Hephaestus started"
    let fileSystem :: FileSystem.Handle IO = FileSystem.new
    currentDir :: AbsDir <- fileSystem.getCurrentDir
    logger.debug $ Data.Text.show currentDir
    logger.info $ Data.Text.show $ Hilcode.Glob.mkGlob "**/*/**/**/**/*/*/?*?*?*.java"
    logger.debug "Done"
