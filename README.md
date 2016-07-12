# Text.Tabl
`Text.Tabl` is a Haskell module that provides an easy way of arranging
multiple `Data.Text.Text` instances into a single table layout, while
providing means of alignment and visual decoration. The only exported function
of the module is `tabl`:

```haskell
tabl :: Environment -- ^ output environment
     -> Decoration  -- ^ horizontal decorations
     -> Decoration  -- ^ vertical decorations
     -> [Alignment] -- ^ column alignments
     -> [[Text]]    -- ^ table cell data
     -> Text        -- ^ resulting table
```

The output of the `tabl` function within the ASCII/command-line
environment (when printed to the `stdout`) can look e.g. like this:
```
$ ./LanguageHistory
+----------+------+------------------+
| Language | Year | Author           |
+----------+------+------------------+
| C        | 1972 | Dennis Ritchie   |
| Lisp     | 1958 | John McCarthy    |
| Python   | 1991 | Guido van Rossum |
+----------+------+------------------+
```

## Settings
The following sections provide detailed description of the layout settings,
that can be separated into three categories: environment adaptation, column
alignments, and both vertical and horizontal decoration.

### Environment

### Alignment

### Decoration

## Examples
The following section contain various examples that use the `Text.Tabl` library
to render textual data.

### Language history
The following code recreates the table from the introductory section:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl
import Data.Text.IO as Text

main :: IO ()
main = Text.putStrLn $ tabl EnvAscii hdecor vdecor [] info
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    info = [["Language", "Year", "Author"],
            ["C", "1972", "Dennis Ritchie"],
            ["Lisp", "1958", "John McCarthy"],
            ["Python", "1991", "Guido van Rossum"]]
```

### List of UNIX system users
The following code lists all users (their IDs, names and full descriptions) on
the system:

```haskell
import System.Posix.User
import Text.Tabl
import qualified Data.Text as Text

-- | Create a table row for one user entry.
createRow :: UserEntry   -- ^ user
          -> [Text.Text] -- ^ table row
createRow ue = map Text.pack [show $ userID ue, userName ue, userGecos ue]

-- | Print all system users and their respective basic information.
main :: IO ()
main = do
  users <- getAllUserEntries
  let cells = map createRow users
  putStrLn $ tabl EnvAscii DecorNone DecorNone [AlignRight] cells
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
import System.Random
import Text.Tabl
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  fields <- replicateM 9 randomIO :: IO [Word8]
  let table = chunksOf 3 $ map (mark . (`mod` 3)) fields
  Text.putStrLn $ tabl EnvAscii DecorAll DecorAll (repeat AlignCentre) table
  where
    mark 0 = " "
    mark 1 = "X"
    mark 2 = "O"
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

