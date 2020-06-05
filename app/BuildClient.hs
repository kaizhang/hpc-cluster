{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Workflow
import Control.Workflow.Main
import           Data.Version           (showVersion)
import           Data.Serialize.Text      ()
import Control.Workflow.Coordinator.Local (LocalConfig (..))
import           Paths_hpc_cluster            (version)
import Text.Printf (printf)

import           Builders
import Types

build "wf" [t| SciFlow Config |] $
    build_base >> build_compute >> build_gpu >> build_nas

main :: IO ()
main = defaultMain header descr commands wf
  where
    header = printf "build_client-v%s" $ showVersion version
    descr = "build computing node image."
    commands =
        [ runParser (\_ _ _ -> return $ LocalConfig 1)
        , deleteParser
        , showParser
        , viewParser ]