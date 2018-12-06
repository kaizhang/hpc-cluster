{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text          as T
import Control.Monad
import qualified Data.Text.IO       as T
import           Shelly             hiding (FilePath)
import Data.Ord
import Data.Maybe
import Data.List
import Data.Function (on)
import           Options.Applicative
import Text.Printf (printf)

data GetQuotaOpt = GetQuotaOpt
    { _host :: [T.Text]
    , _merge :: Bool
    , _exclude :: Maybe [T.Text]
    }

parser :: Parser GetQuotaOpt
parser = GetQuotaOpt
    <$> (fmap (T.splitOn "," . T.pack) . strOption) (long "hosts")
    <*> switch (long "merge")
    <*> (optional . fmap (T.splitOn "," . T.pack) . strOption)
            ( long "exclude"
           <> short 'x'
           <> metavar "DEVICE" )

data Record = Record
    { _uid :: T.Text
    , _blocks_used :: Int
    , _inodes_used :: Int
    , _device :: T.Text
    }

instance Show Record where
    show Record{..} = printf "%s\t%.3fT\t%.3fm"
        (T.unpack $ _uid <> "@" <> _device)
        (fromIntegral _blocks_used / 1024**3 :: Double) 
        (fromIntegral _inodes_used / 1e6 :: Double)

showQuota :: GetQuotaOpt -> IO ()
showQuota GetQuotaOpt{..} = do
    results <- forM _host $ \h ->
        fmap (toRecord h) $ shelly $ silently $ sshPairs h [( "xfs_quota",
            ["-x", "-c", "report -ubita"] )]
    let rs = (if _merge then mergeRecord else id) $
            filter (\x -> not $ _device x `elem` fromMaybe [] _exclude) $
            filter (not . T.isPrefixOf "#" . _uid) $
            filter ((/= "root") . _uid) $
            concat results
    mapM_ print rs

mergeRecord :: [Record] -> [Record]
mergeRecord rs = map f $ groupBy ((==) `on` _uid) $ sortBy (comparing _uid) rs
  where
    f xs = Record
        { _uid =_uid $ head xs
        , _blocks_used = sum $ map _blocks_used xs
        , _inodes_used = sum $ map _inodes_used xs
        , _device = "cluster" }

toRecord :: T.Text   -- ^ Hostname
         -> T.Text -> [Record]
toRecord host txt = concatMap (f . filter (not . T.null) . T.lines) $
    tail $ T.splitOn "User quota on " txt
  where
    f (d:_:_:_:xs) = flip map xs $ \x ->
        let fs = filter (not . T.null) $ T.split (==' ') x
        in Record { _uid = fs !! 0
                  , _blocks_used = read $ T.unpack $ fs !! 1
                  , _inodes_used = read $ T.unpack $ fs !! 6
                  , _device = host <> ":" <> T.init (snd $ T.breakOnEnd "(" d) }

main :: IO ()
main = execParser opts >>= showQuota
  where
    opts = info (helper <*> parser) ( fullDesc <>
        header ("Show disk usage") )