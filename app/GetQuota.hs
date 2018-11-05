{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text          as T
import Control.Monad
import qualified Data.Text.IO       as T
import           Shelly             hiding (FilePath)
import Data.Ord
import Data.List
import Data.Function (on)
import           Options.Applicative

data GetQuotaOpt = GetQuotaOpt
    { _host :: [T.Text]
    }

parser :: Parser GetQuotaOpt
parser = GetQuotaOpt
    <$> (fmap (T.splitOn "," . T.pack) . strOption) (long "hosts")

data Record = Record
    { _uid :: T.Text
    , _blocks_used :: Int
    , _inodes_used :: Int
    , _device :: T.Text
    } deriving (Show)

showQuota :: GetQuotaOpt -> IO ()
showQuota GetQuotaOpt{..} = do
    results <- forM _host $ \h ->
        shelly $ silently $ sshPairs h [( "xfs_quota",
            ["-x", "-c", "report -ubita"] )]
    let rs = concatMap toRecord results
    forM_ (userUsage rs) $ \(a,b,c) -> 
        print (a, show (fromIntegral b / 1e9) <> "T"
            , show (fromIntegral c / 1e6) <> "m")

userUsage :: [Record] -> [(T.Text, Int, Int)]
userUsage rs = map f $ groupBy ((==) `on` _uid) $ sortBy (comparing _uid) rs
  where
    f xs = (_uid $ head xs, sum $ map _blocks_used xs, sum $ map _inodes_used xs)

toRecord :: T.Text -> [Record]
toRecord = concatMap (f . filter (not . T.null) . T.lines) . tail . T.splitOn "User quota on "
  where
    f (d:_:_:_:xs) = flip map xs $ \x ->
        let fs = filter (not . T.null) $ T.split (==' ') x
        in Record { _uid = fs !! 0
                  , _blocks_used = read $ T.unpack $ fs !! 1
                  , _inodes_used = read $ T.unpack $ fs !! 6
                  , _device = d }

main :: IO ()
main = execParser opts >>= showQuota
  where
    opts = info (helper <*> parser) ( fullDesc <>
        header ("Show disk usage") )