{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Builders
import           Scientific.Workflow.Main (MainOpts (..), defaultMainOpts,
                                           mainWith)

mainWith defaultMainOpts build_master
