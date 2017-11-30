# Text.Tabl

[![Build Status](https://travis-ci.org/lovasko/tabl.svg?branch=master)](https://travis-ci.org/lovasko/tabl)

`Text.Tabl` is a Haskell module that arranges multiple `Data.Text.Text`
instances into a single table layout, while providing means of alignment
and visual decoration. The only exported function of the module is `tabl`:

```haskell
tabl
  :: Environment -- ^ output environment
  -> Decoration  -- ^ horizontal decorations
  -> Decoration  -- ^ vertical decorations
  -> [Alignment] -- ^ column alignments
  -> [[T.Text]]  -- ^ table cell data
  -> T.Text      -- ^ resulting table
```

An example output of the `tabl` function within the ASCII-art environment:
```
$ ./Constants
+----------------+---------+-----------+
| Name           | SI Unit |     Value |
+----------------+---------+-----------+
| Speed of light | m/s     | 299792458 |
| Atmosphere     | Pa      |    101325 |
| Absolute zero  | C       |   -273.15 |
+----------------+---------+-----------+
```


## Distribution
There are two ways of obtaining the `Text.Tabl` module: Hackage and source
code from the git repository. While the Hackage version is a stable
release of the module, the GitHub version is the development branch and
might not always compile or function properly.

### Hackage
```sh
$ cabal install tabl
```

### Building from source
```sh
$ git clone https://github.com/lovasko/tabl.git
$ cd tabl/
$ stack build --pedantic --haddock
```

### Dependencies
`Text.Tabl` strives to be as lightweight as possible and depends only on
the following three packages:
* `base`
* `safe`
* `text`

It is written in the Haskell 2010 language and uses the
`OverloadedStrings` extension.


## API
The following sections provide detailed description of the layout
settings, which can be separated into three categories: environment
adaptation, column alignments, and both vertical and horizontal
decoration.

### Environment
The meaning and realisation of a table layout is dependant on the context
it appears in: a Markdown or HTML table is different from a ASCII-art one
showcased in the introductory section. The `Text.Tabl` module currently
supports two contexts represented by the `Environment` type:
 * `EnvAscii` denoting the ASCII-art approach
 * `EnvLatex` denoting the LaTeX source code

The way that the codebase is organised makes adding new environments easy:
apart from the actual layout code, the process boils down to creating a
new `Environment` constructor and adding the appropriate pattern matching
rule in the main `tabl` function.

While the introductory example is using the `EnvAscii` environment, the
output of equivalent table within the `EnvLatex` would look like this:
```tex
\begin{tabular}{ | l | l | r | }
\hline
Name & SI Unit & Value \\
\hline
Speed of light & m/s & 299792458 \\
Atmosphere & Pa & 101325 \\
Absolute zero & C & -273.15 \\
\hline
\end{tabular}
```

### Alignment
The library provides five alignment options on per-column basis
represented by the `Alignment` data type and its constructors:
 * `AlignLeft`
 * `AlignCentre`
 * `AlignRight`
 * `AlignText T.Text`
 * `AlignIndex (T.Text -> Maybe Int)`

The first three alignments provide basic self-describing alignments, while
`AlignText` allows for centering around a first matching substring within each
cell. `AlignIndex` provides a bit more flexibility where a function matches a
position within the cell content, e.g. first non-alphanumeric character or a
first upper-case letter. In case any of the two latter alignments fail to
match, the whole content defaults to behaviour identical to `AlignLeft`.

It is possible, much like with Haskell functions and their arguments, to
only partially specify the table alignments, starting from the left-most
column. The default alignment is `AlignLeft` - which means that passing
the empty list `[]` as the list of alignments to the `tabl` function will
result in a table with all columns and their respective cells aligned to
the left.

### Decoration
Another added value by the `Text.Tabl` module is the process of decorating
the table by visually separating columns and rows. Both the decoration
process and decoration interface are decomposed into two dimensions:
horizontal and vertical, while both at being treated equally.

Each space between two rows or two columns is indexed, where the top and
left spaces share the index zero. Therefore, if a table is comprised of
`n` rows, there are (inclusive) `0..n` positions that could possibly hold
decoration.

The description of the decoration is embodied in the `Decoration` type,
specifically within one (or more) of its constructors:
 * `DecorNone`
 * `DecorAll`
 * `DecorInner`
 * `DecorOuter`
 * `DecorOnly [Int]`
 * `DecorExcept [Int]`
 * `DecorUnion [Decoration]`
 * `DecorIsect [Decoration]`

Essentially, the most powerful decoration constructors are `DecorOnly` and
`DecorExcept`, which allow for a precise selection of indices that should
contain the decoration. Other decoration definitions, such as `DecorInner`
or `DecorAll` are convenience constructors that help the user achieve a
set goal without the need to specify the width nor the height of the
table.

Moreover, the `DecorUnion` and `DecorIsect` constructors are used to
perform set operations on top of a list of decorations - union and
intersection respectively.

It is important to note that none of the constructors that take a list as an
argument work with infinite lists, as they would just block indefinitely. The
examples listed below demonstrate various decoration options and can be used as
a further study material on this topic.

## Examples
The following section contain various examples that use the `Text.Tabl`
library to render textual data.

### Constants
The following code recreates the table from the introductory section:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl
import qualified Data.Text.IO as T

-- | Table containing few physics constants.
main :: IO ()
main = T.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells 
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    aligns = [AlignLeft, AlignLeft, AlignRight]
    cells  = [ ["Name", "SI Unit", "Value"]
             , ["Speed of light", "m/s", "299792458"]
             , ["Atmosphere", "Pa", "101325"]
             , ["Absolute zero", "C", "-273.15"] ]
```

### List of UNIX system users
The following code lists all users (their IDs, names and full
descriptions) on the system:

```haskell
import System.Posix.User
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Create a table row for one user entry.
createRow
  :: UserEntry -- ^ user
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
```

After compiling and running the code, we get:
```
$ ./Users | tail -7
   66 uucp       UUCP pseudo-user
   68 pop        Post Office Owner
   78 auditdistd Auditdistd unprivileged user
   80 www        World Wide Web Owner
  845 hast       HAST unprivileged user
65534 nobody     Unprivileged user
  964 git_daemon git daemon
```

### Tic-tac-toe
The following code creates a random (possibly invalid) state of the famous
child game Tic-tac-toe and renders the playing area:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List.Split
import Data.Word
import Safe
import System.Random
import Text.Tabl
import qualified Data.Text.IO as T

-- | Table containing the play grid of tic-tac-toe.
main :: IO ()
main = do
  xs <- replicateM 9 randomIO :: IO [Word8]
  let cells = chunksOf 3 $ map (mark . (`mod` 3)) xs 
  T.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells
  where
    mark x = lookupJust x [(0, " "), (1, "X"), (2, "O")]
    hdecor = DecorAll
    vdecor = DecorAll
    aligns = []
```

An example run of the compiled program:
```
$ ./TicTacToe
+---+---+---+
| O | X | X |
+---+---+---+
|   | O |   |
+---+---+---+
| O |   |   |
+---+---+---+
```

### Multiplication table
The following code will create a simple elementary-school-level
multiplication table based on the provided integer `n`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Create the multiplication table.
numbers
  :: Int        -- ^ table side size
  -> [[T.Text]] -- ^ multiplication table
numbers n = header : zipWith (:) digits content
  where
    header  = " " : digits
    digits  = map (T.pack . show) [1..n]
    content = map (map (T.pack . show)) mults
    mults   = map (flip map [1..n] . (*)) [1..n]

-- | Table containing basic integer products.
main :: IO ()
main = do
  [n] <- getArgs
  let cells = numbers (read n)
  T.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells
    where
      hdecor = DecorOnly [1]
      vdecor = DecorOnly [1]
      aligns = repeat AlignRight
```

When running the code with e.g. `n = 7`:
```
$ ./Multiply 7
  | 1  2  3  4  5  6  7
--+--------------------
1 | 1  2  3  4  5  6  7
2 | 2  4  6  8 10 12 14
3 | 3  6  9 12 15 18 21
4 | 4  8 12 16 20 24 28
5 | 5 10 15 20 25 30 35
6 | 6 12 18 24 30 36 42
7 | 7 14 21 28 35 42 49
```

### Decimals
The following creates a table of values `100/n`, where `0 < n < 21`. This
example showcases alignment around the decimal dot character, which increases
the readability of floating-point numbers.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn $ tabl EnvAscii DecorOuter DecorAll aligns cells
  where
    aligns = [AlignRight, AlignText "."]
    cells  = zipWith (\x y -> [x, y]) xs ys
    xs     = map (T.pack . show) nums
    ys     = map (T.pack . take 5 . show . over) (map fromIntegral nums)
    nums   = [1..20] :: [Integer]
    over   = ((100.0 :: Double) /)
```

The code above produces the following table:
```
$ ./Decimals
+----+---------+
|  1 | 100.0   |
|  2 |  50.0   |
|  3 |  33.33  |
|  4 |  25.0   |
|  5 |  20.0   |
|  6 |  16.66  |
|  7 |  14.28  |
|  8 |  12.5   |
|  9 |  11.11  |
| 10 |  10.0   |
| 11 |   9.090 |
| 12 |   8.333 |
| 13 |   7.692 |
| 14 |   7.142 |
| 15 |   6.666 |
| 16 |   6.25  |
| 17 |   5.882 |
| 18 |   5.555 |
| 19 |   5.263 |
| 20 |   5.0   |
+----+---------+
```

### File sizes
The following example lists all regular files stored in the `/var/log`
directory and prints out their sizes in a human-readable form. This example
showcases the `AlignIndex` which aligns the column based on a predicate. In
this case, we want to align on the first character that is not a letter from
the alphabet.

```haskell
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
  T.putStrLn $ tabl EnvAscii DecorNone DecorAll aligns cells
```

A sample run could look like this:
```
$ ./FileSizes | tail -n +24 | head -n +10
| CDIS.custom               |   12B  |
| daily.out                 |    7MB |
| displaypolicyd.log        |  111kB |
| displaypolicyd.stdout.log |   50kB |
| fsck_hfs.log              |  856kB |
| fuse-ext2_util.log        |    4kB |
| hdiejectd.log             |    5kB |
| install.log               |   32MB |
| monthly.out               |    7kB |
| notifyd.log               |    0B  |
```

## License
The `tabl` module is licensed under the terms of the 2-clause BSD
license.  For more information please consult the [LICENSE](LICENSE)
file. In case you need a different license, feel free to contact the
author.

## Author
Daniel Lovasko <daniel.lovasko@gmail.com>
