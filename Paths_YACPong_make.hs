-- mimic cabal's Paths_* module when building with make
module Paths_YACPong_make where
import System.FilePath.Posix ((</>))
getDataFileName path = return $ "data" </> path
