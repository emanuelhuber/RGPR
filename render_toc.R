#' Render Table of Contents
#' 
#' A simple function to extract headers from an RMarkdown or Markdown document
#' and build a table of contents. Returns a markdown list with links to the 
#' headers using 
#' [pandoc header identifiers](http://pandoc.org/MANUAL.html#header-identifiers).
#' 
#' WARNING: This function only works with hash-tag headers.
#' 
#' Because this function returns only the markdown list, the header for the
#' Table of Contents itself must be manually included in the text. Use
#' `toc_header_name` to exclude the table of contents header from the TOC, or
#' set to `NULL` for it to be included.
#' 
#' @section Usage:
#' Just drop in a chunk where you want the toc to appear (set `echo=FALSE`):
#' 
#'     # Table of Contents
#' 
#'     ```{r echo=FALSE}
#'     render_toc("/path/to/the/file.Rmd")
#'     ```
#' 
#' @param filename Name of RMarkdown or Markdown document
#' @param toc_header_name The table of contents header name. If specified, any
#'   header with this format will not be included in the TOC. Set to `NULL` to
#'   include the TOC itself in the TOC (but why?).
#' @param base_level Starting level of the lowest header level. Any headers 
#'   prior to the first header at the base_level are dropped silently.
#' @param toc_depth Maximum depth for TOC, relative to base_level. Default is
#'   `toc_depth = 3`, which results in a TOC of at most 3 levels.
render_toc <- function(
  filename, 
  toc_header_name = "Table of Contents",
  base_level = NULL,
  toc_depth = 3
) {
  x <- readLines(filename, warn = FALSE)
  x <- paste(x, collapse = "\n")
  x <- paste0("\n", x, "\n")
  for (i in 5:3) {
    regex_code_fence <- paste0("\n[`]{", i, "}.+?[`]{", i, "}\n")
    x <- gsub(regex_code_fence, "", x)
  }
  x <- strsplit(x, "\n")[[1]]
  x <- x[grepl("^#+", x)]
  if (!is.null(toc_header_name)) 
    x <- x[!grepl(paste0("^#+ ", toc_header_name), x)]
  if (is.null(base_level))
    base_level <- min(sapply(gsub("(#+).+", "\\1", x), nchar))
  start_at_base_level <- FALSE
  x <- sapply(x, function(h) {
    level <- nchar(gsub("(#+).+", "\\1", h)) - base_level
    if (level < 0) {
      stop("Cannot have negative header levels. Problematic header \"", h, '" ',
           "was considered level ", level, ". Please adjust `base_level`.")
    }
    if (level > toc_depth - 1) return("")
    if (!start_at_base_level && level == 0) start_at_base_level <<- TRUE
    if (!start_at_base_level) return("")
    if (grepl("\\{#.+\\}(\\s+)?$", h)) {
      # has special header slug
      header_text <- gsub("#+ (.+)\\s+?\\{.+$", "\\1", h)
      header_slug <- gsub(".+\\{\\s?#([-_.a-zA-Z]+).+", "\\1", h)
    } else {
      header_text <- gsub("#+\\s+?", "", h)
      header_text <- gsub("\\s+?\\{.+\\}\\s*$", "", header_text) # strip { .tabset ... }
      header_text <- gsub("^[^[:alpha:]]*\\s*", "", header_text) # remove up to first alpha char
      header_slug <- paste(strsplit(header_text, " ")[[1]], collapse="-")
      header_slug <- tolower(header_slug)
    }
    paste0(strrep(" ", level * 4), "- [", header_text, "](#", header_slug, ")")
  })
  x <- x[x != ""]
  knitr::asis_output(paste(x, collapse = "\n"))
}