{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Shelly             hiding (FilePath)
import           System.IO
import           Options.Applicative

data AddMachineOpt = AddMachineOpt
    { _machines :: FilePath
    , _overwrite :: Bool
    , _include :: Maybe [T.Text]
    }

parser :: Parser AddMachineOpt
parser = AddMachineOpt
    <$> strOption ( long "machines"
                 <> metavar "MACHINES" )
    <*> switch (long "overwrite")
    <*> (optional . fmap (T.splitOn "," . T.pack) . strOption) (long "include")

data Role = Compute
          | GPU
          | NAS
          deriving (Eq)

instance Show Role where
    show Compute = "compute"
    show GPU     = "gpu"
    show NAS     = "nas"

data Machine = Machine
    { name             :: T.Text
    , role             :: Role
    , network_interface :: T.Text
    , hardaddr         :: T.Text
    , ipaddr           :: T.Text
    , files            :: [T.Text]
    , cpus             :: Maybe Int
    , cores_per_cpu    :: Maybe Int
    , threads_per_core :: Maybe Int
    , memory           :: Maybe T.Text
    , hard_drive       :: Maybe T.Text
    , slurm_gres       :: Maybe T.Text
    } deriving (Show)

addMachines :: AddMachineOpt -> IO ()
addMachines AddMachineOpt{..} = do
    machines <- filter fn <$> readMachine _machines
    mapM_ (addMachine _overwrite) machines
  where
    fn x = case _include of
        Just m -> name x `elem` m
        Nothing -> True

addMachine :: Bool       -- ^ Whether to overwrite existing machines
           -> Machine
           -> IO ()
addMachine overwrite machine = do
    exitCode <- shelly $ errExit False $ silently $
        run_ "wwsh" ["node", "list", name machine] >> lastExitCode
    when (exitCode == 0 && overwrite) $ do
        putStrLn $ "Removing " ++ T.unpack (name machine)
        shelly $ silently $ run_ "wwsh" ["-y", "node", "delete", name machine]
    when (exitCode /= 0 || overwrite) $ do
        kernel <- T.strip <$> shelly (silently $ run "uname" ["-r"])
        putStrLn $ "Adding " ++ T.unpack (name machine)
        shelly $ silently $ do
            run_ "wwsh" [ "-y", "node", "new", name machine
                , "--ipaddr=" `T.append` ipaddr machine
                , "--hwaddr=" `T.append` hardaddr machine
                , "-D", network_interface machine ]
            run_ "wwsh" [ "-y", "provision", "set", name machine
                , "--vnfs=" `T.append` T.pack (show $ role machine)
                , "--bootstrap=" `T.append` kernel
                , "--files=" `T.append` T.intercalate "," file ]

        when (role machine == Compute || role machine == GPU) $ do
            shelly $ silently $ do
                run_ "wwsh" [ "-y", "object", "modify", "-s"
                    , "FILESYSTEMS=\"mountpoint=/tmp:dev=sda1:type=ext4:size=fill\""
                    , name machine ]
                run_ "wwsh" [ "-y", "object", "modify", "-s"
                    , "DISKFORMAT=\"sda1\"", name machine ]
                run_ "wwsh" [ "-y", "object", "modify", "-s"
                    , "DISKPARTITION=\"sda\"", name machine ]

                case slurm_gres machine of
                    Nothing -> return ()
                    Just gres -> do
                        run_ "wwsh" [ "-y", "object", "modify", "-s"
                            , T.pack $ "SLURM_GRES=" ++ "\"" ++ parseGres gres ++ "\""
                            , name machine ]
                        run_ "wwsh" [ "-y", "provision", "set", name machine
                            , "--fileadd=gres.conf" ]

            withFile "/etc/slurm/slurm.conf" AppendMode $ \h -> hPutStrLn h $ unwords $
                [ "NodeName=" ++ T.unpack (name machine)
                , "Sockets=" ++ show (fromMaybe 1 $ cpus machine)
                , "CoresPerSocket=" ++ show (fromMaybe 1 $ cores_per_cpu machine)
                , "ThreadsPerCore=" ++ show (fromMaybe 1 $ threads_per_core machine)
                ] ++ case slurm_gres machine of
                    Nothing -> []
                    Just gres -> ["Gres=" ++ T.unpack gres]
                ++ case memory machine of
                    Nothing -> []
                    Just m -> ["RealMemory=" ++ T.unpack m]
  where
    file = files machine ++ [ "dynamic_hosts", "passwd", "group", "shadow"
        , "network", "auto.master", "auto.home", "auto.share", "idmapd.conf"
        , "ohpc.sh" ]
    parseGres gres = unlines $ map f $ zip [0..] $ concatMap (
        (\[f1,f2,f3] -> replicate (read $ T.unpack f3) (f1,f2)) .
        T.splitOn ":" . T.strip ) $ T.splitOn "," gres
      where
        f (i, (name, ty)) = case name of
            "gpu" -> unwords
                ["Name=gpu", "Type=" ++ T.unpack ty, "File=/dev/nvidia" ++ show i]
            _ -> error "Unknown gres name"

readMachine :: FilePath -> IO [Machine]
readMachine fl = do
    c <- T.readFile fl
    return $ map f $ tail $ T.lines c
  where
    f input = Machine f1 (readRole f2) f3 f4 f5
        (if T.null f6 then [] else map T.strip $ T.splitOn "," f6)
        (if T.null f7 then Nothing else Just $ read $ T.unpack f7)
        (if T.null f8 then Nothing else Just $ read $ T.unpack f8)
        (if T.null f9 then Nothing else Just $ read $ T.unpack f9)
        (if T.null f10 then Nothing else Just f10)
        (if T.null f11 then Nothing else Just f11)
        (if T.null f12 then Nothing else Just f12)
      where
        [f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12] =
            map (T.dropAround (=='\"')) $ T.splitOn "\t" input
        readRole x = case x of
            "nas"     -> NAS
            "compute" -> Compute
            "gpu"     -> GPU
            _         -> error "Unknown type!"

main :: IO ()
main = execParser opts >>= addMachines
  where
    opts = info (helper <*> parser) ( fullDesc <>
        header ("Add new machines to the cluster") )