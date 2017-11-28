import System.Posix.User
import Text.Tabl

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Create a table row for one user entry.
row
  :: UserEntry -- ^ user
  -> [T.Text]  -- ^ table row
row ue = map T.pack [show $ userID ue, userName ue, userGecos ue]

-- | Table containing all system users and their respective basic
-- information.
main :: IO ()
main = do
  users <- getAllUserEntries
  T.putStrLn $ tabl EnvAscii DecorNone DecorNone [AlignRight] (map row users)
