{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import System.Random
import Shelly
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative

data CreateUserOpt = CreateUserOpt
    { username :: T.Text
    , host     :: T.Text
    , path     :: T.Text
    }

parser :: Parser CreateUserOpt
parser = CreateUserOpt
        <$> strArgument (metavar "USERNAME")
        <*> strArgument (metavar "HOST")
        <*> strArgument (metavar "PATH")

main :: IO ()
main = execParser opts >>= createUser 
  where
    opts = info (helper <*> parser) ( fullDesc <>
        header ("Create new users on cluster") )

generatePassword :: IO T.Text
generatePassword = T.pack <$> replicateM 10 rand
  where
    rand = T.index alphabet <$> randomRIO (0, T.length alphabet - 1)
    alphabet = T.pack $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

createUser :: CreateUserOpt -> IO ()
createUser CreateUserOpt{..} = do
    p <- generatePassword

    host <- shelly $ do
        -- Add the user without creating home directory
        run_ "useradd" ["--no-create-home", username]
        setStdin $ username <> ":" <> p
        run_ "chpasswd" []

        -- Creating home directory on the remote server
        run_ "pdsh" ["-w", host, "mkdir " <> remote_dir]

        -- Write entry in auto.home
        appendfile "/etc/auto.home" $ T.intercalate "\t"
            [ username
            , "-nfsvers=3,hard,intr,async,noatime"
            , host <> ":" <> path <> "/&" ]

        -- Reload autofs
        run_ "systemctl" ["reload", "autofs"]

        -- Change permission
        run_ "chown" [username <> ":" <> username, "/home/" <> username]
        run_ "chmod" ["700", "/home/" <> username]

        -- Sync
        run_ "wwsh" ["file", "sync"]

        run "hostname" []

    T.putStrLn "Here is your account:"
    T.putStrLn $ "Username: " <> username
    T.putStrLn $ "Password: " <> p
    T.putStrLn $ "Please use \"ssh " <> username <> "@" <> T.strip host <> "\" to login."
  where
    remote_dir = path <> "/" <> username