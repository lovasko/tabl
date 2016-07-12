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

