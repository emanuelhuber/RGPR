

pth <- "."

source(file.path(pth, "inst", "readthedoc_rd_to_md_functions.R"))

# first time
makeReadTheDocx(path = pth)

# UPDATES
# 1. Convert Rd to md
# convert_man(path = ".")

updateReadTheDocx(path = pth)


# index <- file.path (pth, "docs", "index.rst")
# brio::read_lines (index)






