import System.Posix.User
import Text.Tabl

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Create a table row for one user entry.
createRow :: UserEntry -- ^ user
          -> [T.Text]  -- ^ table row
createRow ue = map T.pack [show $ userID ue, userName ue, userGecos ue]

-- | Table containing all system users and their respective basic
-- information.
main :: IO ()
main = do
  users <- getAllUserEntries
  let cells = map createRow users
  T.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells
  where
    hdecor = DecorNone
    vdecor = DecorNone
    aligns = [AlignRight]
