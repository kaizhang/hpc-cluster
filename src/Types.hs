{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Default
import qualified Data.Text    as T
import           GHC.Generics
import Data.Binary

newtype Addr = Addr T.Text deriving (Generic)

instance FromJSON Addr where
    parseJSON = fmap readAddr . parseJSON

instance ToJSON Addr where
    toJSON = toJSON . showAddr

instance Binary Addr

showAddr :: Addr -> T.Text
showAddr (Addr x) = x

readAddr :: T.Text -> Addr
readAddr = Addr

data Config = Config
    { _image_root_directory  :: FilePath         -- ^ The directory used to store
                                                 -- the built images.
    , _host_name             :: T.Text           -- ^ the name of the server.
    , _host_ip               :: Addr             -- ^ The INTERNAL IP address of
                                                 -- the server
    , _host_eth_internal     :: T.Text           -- ^ The name of the ethernet used
                                                 -- for INTERNAL networking.
    , _internal_netmask      :: Addr
    , _host_eth_provision    :: T.Text
    , _ntp_server            :: Addr
    , _bos_mirror            :: Addr
    , _nvidia_libs_installer :: Maybe FilePath
    } deriving (Generic)

instance FromJSON Config
instance ToJSON Config
instance Binary Config

instance Default Config where
    def = defaultConfig

defaultConfig :: Config
defaultConfig = Config
    { _image_root_directory = "/opt/ohpc/admin/images/"
    , _host_name = "frontend.test.local"
    , _host_ip = Addr "10.1.0.1"
    , _host_eth_internal = "em1"
    , _internal_netmask = Addr "255.255.0.0"
    , _host_eth_provision = "em2"
    , _ntp_server = Addr "centos.pool.ntp.org"
    , _bos_mirror = Addr "http://vault.centos.org/7.2.1511/os/x86_64/"
    , _nvidia_libs_installer = Nothing
    }
