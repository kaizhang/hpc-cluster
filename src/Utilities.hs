{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Utilities
    ( install
    , chrootInstall
    , groupInstall
    , chrootGroupInstall
    , start
    , enable
    , stop
    , disable
    , restart
    , gsub
    , gsubWith
    , importFile
    ) where

import           Control.Exception (SomeException (..))
import qualified Data.ByteString   as B
import qualified Data.Text         as T
import           Shelly            hiding (FilePath)

default (T.Text)

install :: T.Text -> Sh ()
install x = catch_sh (cmd "rpm" "-q" x) $ \(SomeException _) ->
    cmd "yum" "install" "-y" x

chrootInstall :: ShellCmd result => T.Text -> result
chrootInstall chroot = cmd "yum" "-y" ("--installroot=" `T.append` chroot) "install"

groupInstall :: ShellCmd result => result
groupInstall = cmd "yum" "groupinstall" "-y"

chrootGroupInstall :: ShellCmd result => T.Text -> result
chrootGroupInstall chroot = cmd "yum" "-y" ("--installroot=" `T.append` chroot) "groupinstall"

start :: ShellCmd result => result
start = cmd "systemctl" "start"

enable :: ShellCmd result => result
enable = cmd "systemctl" "enable"

disable :: ShellCmd result => result
disable = cmd "systemctl" "disable"

stop :: ShellCmd result => result
stop = cmd "systemctl" "stop"

restart :: ShellCmd result => result
restart = cmd "systemctl" "restart"

gsub :: T.Text -> T.Text -> T.Text -> IO ()
gsub = gsubWith '/'

gsubWith :: Char -> T.Text -> T.Text -> T.Text -> IO ()
gsubWith sep pattern repl fl = shelly $ print_commands True $ run_ "perl"
    ["-pi", "-e", regex, fl]
  where
    regex = T.intercalate (T.singleton sep) ["s", pattern, repl, "g"]

importFile :: T.Text -> Sh ()
importFile = cmd "wwsh" "file" "import"
