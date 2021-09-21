


writeBinary <- function(object, con, what = "numeric", size = NA_integer_,
                        endian = .Platform$endian, useBytes = FALSE, 
                        eos = ""){
  if(what == "character"){
    storage.mode(object) <- "character"
    writeChar(object, con, nchars = size, eos = eos, useBytes = useBytes)
  }else if(what == "numeric"){
    storage.mode(object) <- "double"
    writeBin(object, con, size = size, endian = endian, useBytes = useBytes)
  }else if(what == "integer"){
    storage.mode(object) <- "integer"
    writeBin(object, con, size = size, endian = endian, useBytes = useBytes)
  }else if(what == "raw"){
    storage.mode(object) <- "raw"
    writeBin(object, con, size = 1, endian = endian, useBytes = useBytes)
  }
  
}
