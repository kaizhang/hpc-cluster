{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Builders where

import           Actions.Client
import           Actions.Master
import           Control.Lens           ((.=))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask, asks)
import qualified Data.ByteString.Char8  as B
import           Data.Maybe
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Scientific.Workflow
import           Shelly                 (fromText, rm_rf, run, run_, shelly)
import           System.IO

import           Files
import           Types

build_master :: Builder ()
build_master = do
    nodeS "Setup_master" [| \() -> setup_master <$> ask >>= liftIO
        |] $ submitToRemote .= Just False
    nodeS "Install_resource_manager_master" [| \() ->
        install_resource_manager_master <$> asks _host_name >>= liftIO
        |] $ submitToRemote .= Just False
    node "Install_file_system_master" [| \() -> install_file_system_master
        |] $ submitToRemote .= Just False
    node "Install_development_tools_master" [| \() -> install_development_tools_master
        |] $ submitToRemote .= Just False
    node "Install_extra_tools_master" [| \() -> install_extra_tools_master
        |] $ submitToRemote .= Just False
    nodeS "Install_nvidia" [| \() -> do
        installer <- asks _nvidia_libs_installer
        case installer of
            Nothing -> return ()
            Just x  -> liftIO $ install_nvidia_libs $ T.pack x
        |] $ submitToRemote .= Just False
    ["Setup_master"] ~> "Install_resource_manager_master"
    ["Setup_master"] ~> "Install_file_system_master"
    ["Setup_master"] ~> "Install_development_tools_master"
    ["Setup_master"] ~> "Install_extra_tools_master"
    ["Install_development_tools_master"] ~> "Install_nvidia"

build_base :: Builder ()
build_base = do
    nodeS "Setup_base" [| \() -> do
        base_root <- (++ "/base") <$> asks _image_root_directory
        setup_client <$> asks _host_ip <*> return base_root >>= liftIO
        return base_root
        |] $ submitToRemote .= Just False
    nodeS "Install_file_system_base" [| \base_root -> do
        install_file_system_client <$> asks _host_ip <*>
            return base_root >>= liftIO
        return base_root
        |] $ submitToRemote .= Just False
    ["Setup_base"] ~> "Install_file_system_base"

build_compute :: Builder ()
build_compute = do
    nodeS "Setup_compute" [| \base_root -> do
        compute_root <- (<> "/compute") <$> asks _image_root_directory
        liftIO $ shelly $ do
            rm_rf $ fromText $ T.pack compute_root
            run_ "cp" ["-a", T.pack base_root, T.pack compute_root]
        return compute_root
        |] $ submitToRemote .= Just False
    node "Install_resource_manager_client" [| \compute_root -> do
        install_resource_manager_client compute_root
        return compute_root
        |] $ submitToRemote .= Just False
    node "Install_extra_tools_client" 'install_extra_tools_client $
        submitToRemote .= Just False
    node "Build_compute" [| \compute_root -> build_client compute_root
        |] $ submitToRemote .= Just False
    path ["Install_file_system_base", "Setup_compute"
        , "Install_resource_manager_client", "Install_extra_tools_client", "Build_compute"]

build_gpu :: Builder ()
build_gpu = do
    nodeS "Setup_gpu" [| \compute_root -> do
        gpu_root <- (<> "/gpu") <$> asks _image_root_directory
        liftIO $ shelly $ do
            rm_rf $ fromText $ T.pack gpu_root
            run_ "cp" ["-a", T.pack compute_root, T.pack gpu_root]
        return gpu_root
        |] $ submitToRemote .= Just False
    nodeS "Build_driver_gpu" [| \gpu_root -> do
        installer <- fromMaybe (error "nvidia installer was not specified!") <$>
            asks _nvidia_libs_installer
        kernel <- T.strip <$> shelly (run "uname" ["-r"])
        liftIO $ do
            B.writeFile (gpu_root ++ "/etc/modprobe.d/nouveau.conf")
                "blacklist nouveau\noptions nouveau modeset=0"
            build_nvidia_driver installer kernel $ T.pack gpu_root <>
                "/lib/modules/" <> kernel <>
                "/kernel/drivers/video/"
            B.writeFile (gpu_root ++ "/usr/sbin/nvidia-device-files")
                nvidia_device_files
            shelly $ run_ "chmod" ["755", T.pack gpu_root <> "/usr/sbin/nvidia-device-files"]
            B.writeFile (gpu_root ++ "/etc/systemd/system/nvidia-device-files.service")
                nvidia_device_files_service
            shelly $ run_ "chroot" [T.pack gpu_root, "systemctl", "enable", "nvidia-device-files.service"]
        return gpu_root
        |] $ submitToRemote .= Just False
    node "Build_gpu" 'build_client $ submitToRemote .= Just False
    path ["Install_extra_tools_client", "Setup_gpu", "Build_driver_gpu", "Build_gpu"]

build_nas :: Builder ()
build_nas = do
    nodeS "Setup_nas" [| \base_root -> do
        nas_root <- (<> "/nas") <$> asks _image_root_directory
        liftIO $ shelly $ do
            rm_rf $ fromText $ T.pack nas_root
            run_ "cp" ["-a", T.pack base_root, T.pack nas_root]
        return nas_root
        |] $ submitToRemote .= Just False
    node "Install_extra_tools_nas" 'install_extra_tools_nas $
        submitToRemote .= Just False
    node "NAS_post_script" [| \nas_root -> do
        let rc_local = nas_root ++ "/etc/rc.d/rc.local"
        withFile rc_local AppendMode $ \h -> B.hPutStrLn h nas_post
        shelly $ run_ "chmod" ["+x", T.pack rc_local]
        return nas_root
        |] $ submitToRemote .= Just False
    node "Build_nas" 'build_client $ submitToRemote .= Just False
    path [ "Install_file_system_base", "Setup_nas", "Install_extra_tools_nas"
         , "NAS_post_script", "Build_nas"]
