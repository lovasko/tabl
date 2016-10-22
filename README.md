# Text.Tabl
`Text.Tabl` is a Haskell module that arranges multiple `Data.Text.Text`
instances into a single table layout, while providing means of alignment
and visual decoration. The only exported function of the module is `tabl`:

```haskell
tabl :: Environment -- ^ output environment
     -> Decoration  -- ^ horizontal decorations
     -> Decoration  -- ^ vertical decorations
     -> [Alignment] -- ^ column alignments
     -> [[T.Text]]  -- ^ table cell data
     -> T.Text      -- ^ resulting table
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

## Dependencies
`Text.Tabl` strives to be as lightweight as possible and depends only on
the following two packages:
* `base`
* `text`

It is written in the Haskell 2010 language and uses the
`OverloadedStrings` extension.

## Settings
The following sections provide detailed description of the layout settings,
which can be separated into three categories: environment adaptation, column
alignments, and both vertical and horizontal decoration.

### Environment
The meaning and realisation of a table layout is dependant on the context it
appears in: a Markdown or HTML table is different from a ASCII-art one
showcased in the introductory section. The `Text.Tabl` library currently
supports two contexts - ASCII-art and LaTeX, represented by type ` Environment`
with two constructors, both of zero arity: `EnvAscii` and `EnvLatex`.

The way that the codebase is organised makes adding new environments easy:
apart from the actual layout code, the process boils down to creating a new 
`Environment` constructor and adding the appropriate pattern matching rule in
the main `tabl` function.

While the introductory example is using the `EnvAscii` environment, the output
of equivalent table within the `EnvLatex` would look like this:
```tex
\begin{tabular}{ | l | l | l | }
\hline \\
Language & Year & Author
\hline \\
C & 1972 & Dennis Ritchie \\
Lisp & 1958 & John McCarthy \\
Python & 1991 & Guido van Rossum \\
\hline
\end{tabular}
```

### Alignment
The first major feature of the `Text.Tabl` library is the ability to specify
the alignment of each column of the table. The library provides three alignment
options represented by the `Alignment` data type. It's three constructors:
`AlignLeft`, `AlignCentre` and `AlignRight` denote the left, centre and right
alignments respectively.

It is possible, much like with Haskell functions and their arguments, to only
partially specify the table alignments, starting from the left. The default
alignment is `AlignLeft` - which means that passing the empty list `[]` as the
list of alignments to the `tabl` function will result in a table with all
columns and their respective cells aligned to the left.

The examples sections below provides a variety of use-cases for many
combinations.

### Decoration
The second of the two major added values provided by the `Text.Tabl` library is
the process of decorating the table by visually separating both and/or rows. 
Both the decoration process and decoration interface are decomposed into two
dimensions: horizontal and vertical, while both at being treated equally.

Each space between two rows or two columns is indexed, where the top and left
spaces share the index zero. Therefore, if a table is comprised of `n` rows,
there are `0..n` positions that could possibly hold decoration.

The description of the decoration is embodied in the `Decoration` type,
specifically within one (or more) of it's constructors:
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
contain the decoration. Other decoration definitions, such as `DecorInner` or
`DecorAll` are convenience constructors that help the user achieve a set goal
without the need to specify the width nor the height of the table.

Moreover, the `DecorUnion` and `DecorIsect` constructors are used to perform
set operations on top of a list of decorations, union and intersection
respectively.

The examples listed below demonstrate various decoration options and can be
used as a further study material on this topic.

## Examples
The following section contain various examples that use the `Text.Tabl` library
to render textual data.

### Language history
The following code recreates the table from the introductory section:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl
import Data.Text.IO as T

main :: IO ()
main = T.putStrLn $ tabl EnvAscii hdecor vdecor [] info
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
import qualified Data.Text as T

-- | Create a table row for one user entry.
createRow :: UserEntry -- ^ user
          -> [T.Text]  -- ^ table row
createRow ue = map T.pack [show $ userID ue, userName ue, userGecos ue]

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
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  fields <- replicateM 9 randomIO :: IO [Word8]
  let table = chunksOf 3 $ map (mark . (`mod` 3)) fields
  T.putStrLn $ tabl EnvAscii DecorAll DecorAll (repeat AlignCentre) table
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

### Multiplication table
The following code will create a simple elementary-school-level multiplication
table based on the provided integer `n`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

numbers :: Int
        -> [[T.Text]]
numbers n = header : zipWith (:) digits content
  where
    header = " " : digits
    digits = map (T.pack . show) [1..n]
    content = map (map (T.pack . show)) mults
    mults = map (flip map [1..n] . (*)) [1..n]

main :: IO ()
main = do
  [n] <- getArgs
  let hdecor = DecorOnly [1]
  let vdecor = DecorOnly [1]
  let aligns = repeat AlignRight
  T.putStrLn $ tabl EnvAscii hdecor vdecor aligns (numbers (read n))
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

