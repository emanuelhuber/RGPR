# Filepath(s) with correct extension(s)

```r
getFName(fPath, ext = c(".hd", ".dt1"), throwError = TRUE)
```

## Arguments

- `fPath`: [`character[1]`] Filepath.
- `ext`: `character` Extensions to check (e.g., `".hd"`).
- `throwError`: (`logical[1]`) If TRUE, an error is thrown if the filepath with one of the extension does not exist. If FALSE, it returns NULL for the missing extension

## Returns

```
[`list`] The list keys correspond to `ext` and 
         the values to the filepaths 
         (e.g., `$hd  -> xline01.hd`).
```

 

Returns the filepaths with the correct extension and check for upper and lower case extension (e.g., ".txt" or ".TXT")