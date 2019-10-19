#!/usr/bin/env Rscript

# Be sure to make r2jekyll.R executable with chmod +x r2jekyll.R. 
# Then, to convert my .Rmd file to a .md file 
# and take care of any .png file housekeeping, I navigate 
# to my _drafts directory and execute in the terminal:

# Rscript --vanilla r2jekyll.R 00_RGPR_tutorial_import-GPR-data.Rmd
# Rscript --vanilla r2jekyll.R 01_RGPR_tutorial_plot-GPR-data.Rmd
# Rscript --vanilla r2jekyll.R 02_RGPR_tutorial_basic-GPR-data-processing.Rmd
# Rscript --vanilla r2jekyll.R 03_RGPR_tutorial_processing-GPR-data-with-pipe-operator.Rmd
# Rscript --vanilla r2jekyll.R 04_RGPR_tutorial_GPR-data-survey.Rmd
# Rscript --vanilla r2jekyll.R 05_RGPR_tutorial_GPR-data-migration.Rmd
# Rscript --vanilla r2jekyll.R 70_RGPR_tutorial_class-GPR.Rmd
# Rscript --vanilla r2jekyll.R 07_RGPR_tutorial_hyperbola_fitting.Rmd
# Rscript --vanilla r2jekyll.R 80_RGPR_GPR-data-free-to-download.Rmd


# CHECK
# https://gist.github.com/emanuelhuber/11835e6840868029d7c4721b7f7bf465


#------------------------------------------------------------------------------#
# TODO: rewrite the directory "01_RGPR_tutorial_basic-processing_deleteme_files"
# into "01_RGPR_tutorial_basic-processing_files"
#------------------------------------------------------------------------------#


#library(knitr)
library(rmarkdown)

# Get the filename given as an argument in the shell.
args <- commandArgs(TRUE)
filename <- args[1]

print(filename)

# Check that it's a .Rmd file.
if(!grepl(".Rmd", filename)) {
  stop("You must specify a .Rmd file.")
}

tempfile <- sub('.Rmd', '_deleteme.Rmd', filename)
mdtempfile <- sub('.Rmd', '_tp.md', filename)
mdfile <- sub('.Rmd', '.md', filename)

#knit(filename, mdfile)


x0 <- readLines(filename)


sel <- grepl("([[:blank:]]|[[:punct:]]){1}\\${1}(.+?)\\$", x0)

x1 <- x0
x1[sel] <- gsub("(\\${1}(.+?)\\$)", "<pre>\\1<\\/pre>", x0[sel])

x2 <- paste(x1, collapse = "!@#:")

x3 <- gsub("(\\${2}(.+?)\\${2})", "<pre>\\1<\\/pre>", x2)

x4 <- strsplit(x3, split="!@#:", fixed = TRUE)

writeLines(x4[[1]], tempfile)


message("run 'rmarkdown::render', tempfile = ",tempfile, 
        ", output_file = ", mdtempfile)

rmarkdown::render(tempfile, output_format = 'all', output_file = mdtempfile)



x0 <- readLines(mdtempfile)

x <- paste(x0, collapse = "!@#:")

x1 <- gsub("<pre>\\${2}", "$$", x)
x2 <- gsub("\\${2}</pre>", "$$!@#:", x1)

x3 <- gsub("(!@#:[[:space:]]*)<pre>\\${1}", " $", x2)
x4 <- gsub("\\${1}</pre>(!@#:)", "$ ", x3)
# remove space before punctuation!
x5 <- gsub("\\s+([,;:)!\\.\\?])", "\\1", x4)
#x3 <- gsub("\\s+([[:punct:]])", "\\1", x2)
x6 <- strsplit(x5, split="!@#:", fixed = TRUE)


writeLines(x6[[1]], mdfile)



unlink(mdtempfile)
unlink(tempfile)


x0 <- c("To extract the samples 100 to 300 of the 15",
       "    <pre>$^{th}$</pre>",
       ", to 150",
	   "<pre>$^{th}$</pre>",
		":")

