# Decode an RF creation/modification date from a DZT file

```r
.readRFDate(con, where = 31L)
```

## Arguments

- `con`: Open binary file connection.
- `where`: (`integer(1)`) Byte offset from the start of the file.

## Returns

A list with `$date` (character, `"YYYY-MM-DD"`) and `$time` (character, `"HH:MM:SS"`).

## Description

Reads 4 bytes at the specified offset and decodes them as a packed DOS-style date+time stamp.


