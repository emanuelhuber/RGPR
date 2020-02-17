
#-----------------------------------------
# bit to integer conversion
.bit2int <- function(x){
  sum(as.integer(x) * 2^( rev(seq_along(x)) - 1))
}

# open file if not already openend
.openFileIfNot <- function(dsn){
  if(!inherits(dsn, "connection")){
    return( file(dsn, 'rb') )
  }else{
    return(dsn)
  }
}

.closeFileIfNot <- function(dsn){
  if(inherits(dsn, "connection")){
    invisible(tryCatch(close(dsn), error = function(e){NULL} ))
  }
}

# number of bytes in connection
# file.info(filename)$size
.flen <- function(con){
  pos0 <- seek(con)
  seek(con,0,"end")
  pos <- seek(con)
  seek(con,where=pos0,"start")
  return(pos)
}

# unsigned integer, 2 bytes
.readBin_int1 <- function(x, n = 1){
  readBin(x, what = "int", n = n, size = 1, signed = FALSE, endian = "little")
}

# unsigned integer, 2 bytes
.readBin_ushort <- function(x, n = 1){
  readBin(x, what = "int", n = n, size = 2, signed = FALSE, endian = "little")
}

# signed integer, 2 bytes
.readBin_short <- function(x){
  readBin(x, what = "int", size = 2, signed = TRUE, endian = "little")
}

# float, 4 bytes
.readBin_float <- function(x){
  readBin(x, what = "numeric", size = 4, endian = "little")
}

# float, 8 bytes
.readBin_float8 <- function(x){
  readBin(x, what = "numeric", size = 8, endian = "little")
}

# char, 4 bytes
.readBin_char <- function(x, n = 1){
  readBin(x, what = "character", n = n, size = 1, endian = "little")
}

#  returns the current position in the specified file/connection con
.ftell <- function(con){
  return(seek(con))
}

# .skipBin <- function(con, n, size = 1L){
#   if(n > 0) invisible(readBin(con, "integer", n = n, size = size))
# }
