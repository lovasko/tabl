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

