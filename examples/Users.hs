import System.Posix.User
import Text.Tabl

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- | Create a table row for one user entry.
createRow :: UserEntry   -- ^ user
          -> [Text.Text] -- ^ table row
createRow ue = map Text.pack [show $ userID ue, userName ue, userGecos ue]

-- | Table containing all system users and their respective basic
-- information.
main :: IO ()
main = do
  users <- getAllUserEntries
  let cells = map createRow users
  Text.putStrLn $ tabl EnvAscii DecorNone DecorNone [AlignRight] cells

