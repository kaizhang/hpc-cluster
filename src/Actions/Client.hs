{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Actions.Client
    ( setup_client
    , install_file_system_client
    , install_resource_manager_client
    , install_extra_tools_client
    , install_extra_tools_nas
    , build_nvidia_driver
    , build_client
    ) where

import qualified Data.ByteString as B
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import           Shelly          hiding (FilePath)

import           Types
import           Utilities

setup_client :: Addr -> FilePath -> IO ()
setup_client sms_ip chroot' = shelly $ print_commands True $ escaping False $ do
    rm_rf $ fromText chroot

    -- Build initial chroot image
    cmd "wwmkchroot" "centos-7" chroot

    cmd "cp" "-p" "/etc/resolv.conf" $ chroot `T.append` "/etc/resolv.conf"

    -- add cluster key to base image
    errExit False $ cmd "wwinit" "ssh_keys"
    cmd "cat" "~/.ssh/cluster.pub" ">>" $
        chroot <> "/root/.ssh/authorized_keys"

    -- Add Network Time Protocol (NTP) support
    chrootInstall chroot "ntp"

    -- Enable NTP time service on computes and identify master host as local NTP server
    cmd "chroot" chroot "systemctl" "enable" "ntpd"
    cmd "echo" (T.unwords ["server", showAddr sms_ip]) ">>" $
        chroot <> "/etc/ntp.conf"

    -- Add kernel drivers
    chrootInstall chroot "kernel"
    -- Include modules user environment
    chrootInstall chroot "lmod-ohpc"
    -- add zsh
    chrootInstall chroot "zsh"
    return ()
  where
    chroot = T.pack chroot'

install_file_system_client :: Addr -> FilePath -> IO ()
install_file_system_client sms_ip chroot' = shelly $ escaping False $ do
    -- Install autofs
    chrootInstall chroot "autofs"
    cmd "chroot" chroot "systemctl" "enable" "autofs"

    -- add NFS client mounts of share directories to base image
    cmd "echo" ("\"" <> showAddr sms_ip <> ":/opt/ohpc/pub /opt/ohpc/pub nfs nfsvers=3 0 0\"")
        ">>" $ chroot <> "/etc/fstab"
    return ()
  where
    chroot = T.pack chroot'

install_resource_manager_client :: FilePath -> IO ()
install_resource_manager_client chroot' = shelly $ escaping False $ do
    -- Add Slurm client support
    chrootGroupInstall chroot "ohpc-slurm-client"

    -- Restrict ssh access on compute nodes to only allow access by users who have an
    -- active job associated with the node.
    cmd "echo" "\"account required pam_slurm.so\"" ">>" $
        chroot <> "/etc/pam.d/sshd"

    -- Install mrsh
    -- mrsh is a secure remote shell utility, like ssh, which uses munge for authentication
    -- and encryption.
    chrootInstall chroot "mrsh-ohpc" "mrsh-rsh-compat-ohpc" "mrsh-server-ohpc"

    -- Enable xinetd in compute node image
    cmd "chroot" chroot "systemctl" "enable" "xinetd"
    -- Enable slurmd in compute node image
    cmd "chroot" chroot "systemctl" "enable" "slurmd"
    return ()
  where
    chroot = T.pack chroot'

install_extra_tools_client :: FilePath -> IO FilePath
install_extra_tools_client chroot = shelly $ do
    run_ "yum" [ "-y", "--installroot=" <> T.pack chroot, "install"
        , "vim", "cairo", "pango", "libtiff", "perl-Env", "tkinker"
        ]
    return chroot

install_extra_tools_nas :: FilePath -> IO FilePath
install_extra_tools_nas chroot = shelly $ do
    run_ "yum" [ "-y", "--installroot=" <> T.pack chroot, "install"
        , "parted", "xfsdump", "xfsprogs"]
    return chroot

build_nvidia_driver :: FilePath -> T.Text -> T.Text -> IO ()
build_nvidia_driver nvidia_installer kernel output = shelly $ errExit False $
    cmd (fromText $ T.pack nvidia_installer)
        "--silent"
        ("--kernel-install-path=" <> output)
        ("--kernel-name=" <> kernel)

build_client :: FilePath -> IO ()
build_client chroot' = shelly $ escaping False $ do
    -- Assemble Virtual Node File System (VNFS) image
    cmd "wwvnfs" "-y" "--chroot" chroot
    return ()
  where
    chroot = T.pack chroot'
