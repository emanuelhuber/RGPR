

DIR <- "R"

trimStr <- function (x) gsub("^\\s+|\\s+$", "", x)

lf <- list.files(DIR)

fun <- function(x){x[2]}

concept <- c()
list_concept <- list()
for(j in seq_along(lf)){
  x <- scan(file.path(DIR, lf[j]), what = character(), sep = "\n")
  k <- which(grepl("@concept", x))
  matches <- regmatches(x[k], regexec("@concept\\s*(.*?)$", x[k]))
  if(length(matches) > 0 && length(matches[[1]]) > 0){
    # stop("df")
    mcon <- trimStr(sapply(matches, fun))
    # if(mcon[1] == "CRS") stop("lkj")
    maxit <- diff(c(k, length(x)))
    for(i in seq_along(k)){
      tst <- TRUE
      it <- 1
      while(tst && it < maxit[i]){
        # while(grepl("@concept|#", x[k[i] + 1])){
        #   k[i] <- k[i] + 1
        # }
        k[i] <- k[i] + 1
        if(grepl("setGeneric", x[k[i]])){
          matches2 <- regmatches(x[k[i]], 
                                 regexec("setGeneric\\((?:name\\s*=\\s*)?\\\"(.*?)\\\"", x[k[i] ]))
          tst <- FALSE
        }else if(grepl("setMethod", x[k[i] ])){
          matches2 <- regmatches(x[k[i]], 
                                 regexec("setMethod\\((?:f\\s*=\\s*)?\\\"(.*?)\\\"", x[k[i]]))
          tst <- FALSE
        }else if(grepl("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*<-\\s*function\\s*\\(", x[k[i] ])){
          matches2 <- regmatches(x[k[i]], 
                                 regexec("^(.*?)\\s?<-\\s?function", x[k[i]]))
          tst <- FALSE
        }
        it <- it + 1
      }
      if(length(matches2) == 0) stop("dsf")
      # stop("lkj")
      # matches2[[1]][2]
      if(mcon[i] == "CRS" && is.na(trimStr(matches2[[1]][2]))) stop("lkjl")
      if(trimStr(matches2[[1]][2]) == ".na.character")  stop("lkjlkjlkj")
      list_concept[[mcon[i]]] <- c(list_concept[[mcon[i]]], trimStr(matches2[[1]][2]))
    }
    
    tst <- mcon %in% concept
    if(!all(tst)) concept <- c(concept, mcon[!tst])
  }
}

write.table( concept , file = "inst/list_of_concepts____.txt", row.names = FALSE, col.names = FALSE) 

library(yaml)

write_yaml(list_concept, file = "inst/list_of_concept_functions.yml")

RShowDoc("KEYWORDS")

