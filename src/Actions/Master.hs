{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Actions.Master
    ( setup_master
    , install_file_system_master
    , install_resource_manager_master
    , install_development_tools_master
    , install_extra_tools_master
    , install_nvidia_libs
    ) where

import qualified Data.ByteString as B
import Data.Monoid ((<>))
import qualified Data.Text       as T
import           Shelly          hiding (FilePath)

import           Files
import           Types
import           Utilities

setup_master :: Config -> IO ()
setup_master Config{..} =
    shelly $ escaping False $ configuration >> enable_service >>
        config_warewulf >> import_files >> cmd "wwbootstrap" "`uname -r`" >>
        return ()
  where
    configuration = do
        cmd "echo" (showAddr _host_ip) _host_name ">>" "/etc/hosts"
        cmd "echo" _host_name ">" "/etc/hostname"

        -- Disable firewall
        errExit False $ disable "firewalld"
        errExit False $ stop "firewalld"

        -- Enable OpenHPC repository for local use
        install "http://build.openhpc.community/OpenHPC:/1.2/CentOS_7.2/x86_64/ohpc-release-1.2-1.x86_64.rpm"

        -- Add provisioning services on master node
        groupInstall "ohpc-base"
        groupInstall "ohpc-warewulf"

        -- Enable internal interface for provisioning
        cmd "ifconfig" _host_eth_internal (showAddr _host_ip) "netmask"
            (showAddr _internal_netmask) "up"

    enable_service = do
        -- Enable NTP server
        enable "ntpd.service"
        cmd "echo" (T.unwords ["server", showAddr _ntp_server]) ">>" "/etc/ntp.conf"
        restart "ntpd"
        -- Enable tftp service for compute node image distribution
        liftIO $ gsub "^\\s+disable\\s+= yes" " disable = no" "/etc/xinetd.d/tftp"
    config_warewulf = do
        -- Configure Warewulf provisioning to use desired internal interface
        liftIO $ gsub "device = eth1" ("device = " <> _host_eth_internal)
            "/etc/warewulf/provision.conf"
        -- Enable http access for Warewulf cgi-bin directory to support newer apache syntax
        let mod_file = "/etc/httpd/conf.d/warewulf-httpd.conf"
        liftIO $ gsub "cgi-bin>$" "cgi-bin>\\n Require all granted" mod_file
        liftIO $ gsub "Allow from all" "Require all granted" mod_file
        cmd "perl" "-ni" "-e" "\"print unless /^\\s+Order allow,deny/\"" mod_file
        -- Restart/enable relevant services to support provisioning
        restart "xinetd"
        enable "mariadb.service"
        restart "mariadb"
        enable "httpd.service"
        restart "httpd"

        -- Set provisioning interface as the default networking device
        cmd "echo" ("GATEWAYDEV=" <> _host_eth_provision) ">" "/tmp/setup.network.xxx"
        cmd "wwsh" "-y" "file" "import" "/tmp/setup.network.xxx" "--name" "network"
        cmd "wwsh" "-y" "file" "set" "network" "--path"
            "/etc/sysconfig/network" "--mode=0644" "--uid=0"

        -- Set slurm generic resource config
        cmd "echo" "\"%{SLURM_GRES}\"" ">" "/tmp/slurm_gres.xxx"
        cmd "wwsh" "-y" "file" "import" "/tmp/slurm_gres.xxx" "--name" "gres.conf"
        cmd "wwsh" "-y" "file" "set" "gres.conf" "--path"
            "/etc/slurm/gres.conf" "--mode=0644" "--uid=0"

        -- Override default OS repository (optional) - set BOS_MIRROR variable to desired repo location
        liftIO $ gsubWith '#' "^YUM_MIRROR=(\\S+)" ("YUM_MIRROR=" <> showAddr _bos_mirror)
            "/usr/libexec/warewulf/wwmkchroot/centos-7.tmpl"
    import_files = do
        importFile "/etc/passwd"
        importFile "/etc/group"
        importFile "/etc/shadow"

install_file_system_master :: IO ()
install_file_system_master = shelly $ escaping False $ do
    -- Install autofs
    install "autofs"

    -- Export OpenHPC public packages from master server to cluster compute nodes
    cmd "echo" "\"/opt/ohpc/pub *(ro,no_subtree_check,fsid=11)\"" ">>" "/etc/exports"
    cmd "exportfs" "-a"
    restart "nfs"
    enable "nfs-server"
    start "autofs"

    importFile "/etc/auto.master"
    cmd "touch" "/etc/auto.home"
    cmd "touch" "/etc/auto.share"
    importFile "/etc/auto.home"
    importFile "/etc/auto.share"
    importFile "/etc/autofs.conf"
    importFile "/etc/idmapd.conf"

install_resource_manager_master :: T.Text -> IO ()
install_resource_manager_master sms_name = shelly $ escaping False $ do
    -- Add resource management services on master node
    -- yum -y install pbspro-server-ohpc
    groupInstall "ohpc-slurm-server"
    liftIO $ gsub "ControlMachine=\\S+" ("ControlMachine=" `T.append` sms_name)
        "/etc/slurm/slurm.conf"

    install "mrsh-ohpc"
    install "mrsh-rsh-compat-ohpc"
    -- Identify mshell and mlogin in services file
    cmd "echo" "\"mshell 21212/tcp # mrshd\"" ">>" "/etc/services"
    cmd "echo" "\"mlogin 541/tcp # mrlogind\"" ">>" "/etc/services"

    -- start munge and slurm controller on master host
    enable "munge"
    enable "slurmctld"
    start "munge"
    start "slurmctld"
    importFile "/etc/slurm/slurm.conf"
    importFile "/etc/munge/munge.key"
    return ()

-- TODO: GCC version is hardcoded
install_development_tools_master :: IO ()
install_development_tools_master = do
    shelly $ escaping False $ do
        install "kernel-devel"
        groupInstall "ohpc-autotools"
        install "valgrind-ohpc"
        install "EasyBuild-ohpc"
        install "spack-ohpc"
        install "R_base-ohpc"
        install "gnu-compilers-ohpc"
        run_ "ln" ["-s", "/opt/ohpc/pub/compiler/gcc/5.4.0/bin/gcc"
            , "/opt/ohpc/pub/compiler/gcc/5.4.0/bin/cc"]
    B.writeFile "/etc/profile.d/ohpc.sh" ohpc_sh
    shelly $ escaping False $ importFile "/etc/profile.d/ohpc.sh"
    return ()


install_extra_tools_master :: IO ()
install_extra_tools_master = shelly $ do
    run_ "yum"
        [ "-y", "install"
        , "vim", "tmux", "zsh", "unzip", "mlocate", "readline-devel"
        , "zlib-devel", "libcurl-devel", "cairo-devel", "pango-devel"
        , "bzip2-devel", "xz-devel", "pcre-devel", "libjpeg-turbo-devel"
        , "libtiff-devel", "openssl-devel", "python-devel", "sqlite-devel"
        ]

-- | Install NVIDIA utilities and libraries (on master node, shared scross whole cluster)
install_nvidia_libs :: T.Text -> IO ()
install_nvidia_libs nvidia_installer = do
    shelly $ do
        appendToPath "/opt/ohpc/pub/compiler/gcc/5.4.0/bin/"
        setenv "LD_LIBRARY_PATH" "/opt/ohpc/pub/compiler/gcc/5.4.0/lib/:$LD_LIBRARY_PATH"
        cmd (fromText nvidia_installer) "-s"
            "--x-prefix=/opt/ohpc/pub/compiler/nvidia/"
            "--opengl-prefix=/opt/ohpc/pub/compiler/nvidia/"
            "--utility-prefix=/opt/ohpc/pub/compiler/nvidia/"
            "--documentation-prefix=/opt/ohpc/pub/compiler/nvidia/"
            "--no-kernel-module"
    B.writeFile "/etc/profile.d/nvidia.sh" nvidia_sh
    shelly $ escaping False $ importFile "/etc/profile.d/nvidia.sh"
    return ()
