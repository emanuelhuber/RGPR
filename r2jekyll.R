#!/usr/bin/env Rscript

#Be sure to make r2jekyll.R executable with chmod +x r2jekyll.R. Then, to convert my .Rmd file to a .md file and take care of any .png file housekeeping, I navigate to my _drafts directory and execute in the terminal:

# Rscript --vanilla 01_RGPR_tutorial_basic-processing.Rmd



#library(knitr)
library(rmarkdown)

# Get the filename given as an argument in the shell.
args <- commandArgs(TRUE)
filename <- args[1]

# Check that it's a .Rmd file.
if(!grepl(".Rmd", filename)) {
  stop("You must specify a .Rmd file.")
}

output <- sub('.Rmd', '.md', filename)

#knit(filename, output)


rmarkdown::render(filename, output_format = 'all')



x0 <- readLines(output)

x <- paste(x0, collapse = "!@#:")

x1 <- gsub("!@#:[[:space:]]*<pre>", " ", x)
x2 <- gsub("</pre>!@#:", " ", x1)
# remove space before punctuation!
x3 <- gsub("\\s+([,;:)!\\.\\?])", "\\1", x2)
#x3 <- gsub("\\s+([[:punct:]])", "\\1", x2)
x4 <- strsplit(x3, split="!@#:", fixed = TRUE)


writeLines(x4[[1]], "test.txt")



writeLines(x4[[1]], output)



x0 <- c("To extract the samples 100 to 300 of the 15",
       "    <pre>$^{th}$</pre>",
       ", to 150",
	   "<pre>$^{th}$</pre>",
		":")

