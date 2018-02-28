{-# LANGUAGE TemplateHaskell #-}

module Files where

import qualified Data.ByteString
import Data.FileEmbed

nas_post :: Data.ByteString.ByteString
nas_post = $(embedFile "scripts/nas_post.sh")

nvidia_sh :: Data.ByteString.ByteString
nvidia_sh = $(embedFile "scripts/nvidia.sh")

ohpc_sh :: Data.ByteString.ByteString
ohpc_sh = $(embedFile "scripts/ohpc.sh")

nvidia_device_files :: Data.ByteString.ByteString
nvidia_device_files = $(embedFile "scripts/nvidia-device-files")

nvidia_device_files_service :: Data.ByteString.ByteString
nvidia_device_files_service = $(embedFile "scripts/nvidia-device-files.service")
