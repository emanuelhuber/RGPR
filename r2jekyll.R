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



all_data <- readLines(output)

all_data <-  gsub('<pre>', '', all_data)
all_data <-  gsub('</pre>', '', all_data)


writeLines(all_data, output)
