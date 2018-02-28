{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Builders
import           Data.Serialize.Text      ()
import           Scientific.Workflow.Main (MainOpts (..), defaultMainOpts,
                                           mainWith)

mainWith defaultMainOpts $ build_base >> build_compute >> build_gpu >> build_nas
