{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import System.Random
import Shelly hiding (FilePath)
import qualified Shelly as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative
import Data.Text.Prettyprint.Doc

data CreateUserOpt = CreateUserOpt
    { username :: T.Text
    , nas      :: T.Text
    , path     :: T.Text
    , dry_run  :: Bool
    }

parser :: Parser CreateUserOpt
parser = CreateUserOpt
        <$> strOption ( long "username"
                     <> short 'u'
                     <> metavar "USERNAME" )
        <*> strOption ( long "nas"
                     <> short 'n'
                     <> metavar "NAS" )
        <*> strOption ( long "path"
                     <> short 'p'
                     <> metavar "PATH" )
        <*> switch ( long "dry-run")

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
createUser opt
    | dry_run opt = createUser' (\a b -> return $ show_command a b)
        (liftIO . T.putStrLn) opt
    | otherwise = createUser' run_ return opt

createUser' :: (S.FilePath -> [T.Text] -> Sh a)  -- ^ Function to execute commands
            -> (a -> Sh ())
            -> CreateUserOpt
            -> IO ()
createUser' exec sh CreateUserOpt{..} = do
    p <- generatePassword

    host <- shelly $ do
        -- Add the user without creating home directory
        exec "useradd" ["--no-create-home", username] >>= sh
        unless dry_run $ setStdin $ username <> ":" <> p
        exec "chpasswd" [] >>= sh

        -- Creating home directory on the remote server
        exec "pdsh" ["-w", nas, "mkdir " <> remote_dir] >>= sh

        -- Write entry in auto.home
        unless dry_run $ appendfile "/etc/auto.home" $ T.intercalate "\t"
            [ username
            , "-nfsvers=3,hard,intr,async,noatime"
            , nas <> ":" <> path <> "/&" ]

        -- Reload autofs
        exec "systemctl" ["reload", "autofs"] >>= sh

        -- Change permission
        exec "chown" [username <> ":" <> username, "/home/" <> username] >>= sh
        exec "chmod" ["700", "/home/" <> username] >>= sh

        -- Sync
        exec "wwsh" ["file", "sync"] >>= sh

        silently $ run "hostname" []

    putStrLn $ showMessage username p (T.strip host)
  where
    remote_dir = path <> "/" <> username

showMessage :: T.Text -> T.Text -> T.Text -> String
showMessage u p h = show $ vsep
    [ "Here is your account:"
    , ""
    , indent 4 $ "Username: " <+> pretty u
    , indent 4 $ "Password: " <+> pretty p
    , ""
    , "Please use" <+> dquotes (pretty $ "ssh " <> u <> "@" <> h) <+> "to login."
    ]