import Control.Arrow
import Control.Monad.Loops
import Data.Char
import System.Posix.Directory
import System.Posix.Files
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Compute a human-readable size of a file.
computeSize
  :: FileStatus -- ^ file handle
  -> String     -- ^ human-readable size representation
computeSize status
  | size > (mega * 2) = show (round (size / mega) :: Integer) ++ "MB"
  | size > (kilo * 2) = show (round (size / kilo) :: Integer) ++ "kB"
  | otherwise         = show (round size          :: Integer) ++ "B"
  where
    size = fromIntegral $ fileSize status :: Double
    mega = 1024 * 1024                    :: Double
    kilo = 1024                           :: Double

-- | List all files in a directory.
listDirectory
  :: FilePath                  -- ^ directory path
  -> IO [(String, FileStatus)] -- ^ directory contents (path, status)
listDirectory dir = do
  stream <- openDirStream dir
  names <- unfoldWhileM (not . null) (readDirStream stream)
  closeDirStream stream
  handles <- mapM (getFileStatus . (concat [dir, "/"] ++)) names
  return $ zip names handles

-- | List header files in /var/log and their respective sizes.
main :: IO ()
main = do
  files <- listDirectory "/var/log"
  let files'  = filter (isRegularFile . snd) files
  let files'' = map (second computeSize) files'

  let aligns = [AlignLeft, AlignIndex (T.findIndex isAlpha)]
  let cells  = map (\(name, size) -> [T.pack name, T.pack size]) files''
  T.putStrLn $ tabl EnvAscii (DecorNegate DecorAll) DecorAll aligns cells
