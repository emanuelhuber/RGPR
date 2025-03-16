

if(!require("r2readthedocs")) remotes::install_github("ropenscilabs/r2readthedocs")
library(r2readthedocs)

# NOT WORKING
# r2readthedocs("/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/RGPR")
# rtd_build()


if(!require("rd2markdown")) remotes::install_github("Genentech/rd2markdown")

library(rd2markdown)

convert_man <- function (path = ".") {
  docs <- file.path (path, "docs")
  fns_dir <- file.path (docs, "functions")
  if (!dir.exists (fns_dir)) {
    chk <- dir.create (fns_dir, recursive = TRUE)
  }
  
  flist <- list.files (file.path (path, "man"),
                       full.names = TRUE,
                       pattern = "\\.Rd$"
  )
  
  for (f in flist) {
    
    fshort <- utils::tail (strsplit (f, .Platform$file.sep) [[1]], 1L)
    fout <- file.path (fns_dir, gsub ("\\.Rd$", ".md", fshort))
    rd <- rd2markdown::get_rd(file = f)
    out <- rd2markdown::rd2markdown(rd)#, fragments = c("title", "usage", "description", "details"))
    cat(out, file = fout)
  }
}

makeReadTheDocx <- function(path, dev = FALSE, open = TRUE){
  path <- r2readthedocs:::convert_path (path)
  
  if (dir.exists (file.path (path, "docs"))) {
    stop (
      "'docs' directory already exists; please remove ",
      "before calling this function, or use ",
      "'rtd_build()' to rebuild current site",
      call. = FALSE
    )
  }
  
  r2readthedocs:::readthedocs_yaml (path)
  
  exdir <- system.file ("extdata", package = "r2readthedocs")
  flist <- list.files (exdir, full.names = TRUE)
  flist <- flist [-grep ("readthedocs\\.yaml$", flist)]
  flist <- gsub (exdir, "", flist)
  flist <- gsub (paste0 ("^", .Platform$file.sep), "", flist)
  
  for (f in flist) {
    r2readthedocs:::readthedocs_file(path, f)
  }
  
  convert_man(path)
  # we do not want to have the readme file as Introduction!
  #readme <- r2readthedocs:::convert_readme(path)
  #readme <- utils::tail (strsplit (readme, .Platform$file.sep) [[1]], 1L)
  
  addIntro(path)
  # no vignette... maybe latter.
  # r2readthedocs:::convert_vignettes(path)
  
  # update ".Rbuildignore"
  r2readthedocs:::rignore_amend(path)
  
  extend_index_rst_mod(path,  addintro = TRUE)#, readme)
  r2readthedocs:::edit_conf_py(path)
  
  static_dir <- file.path (path, "docs", "_static")
  if (!dir.exists (static_dir)) {
    dir.create (static_dir)
  }
  
  if (dev) {
    rm_pkg_deps (path) # to enable clean updates
    add_pkg_deps (path)
  } else if (dir.exists (file.path (path, "docs", "dependencies"))) {
    rm_pkg_deps (path)
  }
  
  r2readthedocs::rtd_clean(path)
  r2readthedocs::rtd_build (path, dev = dev)
  
  if (open) {
    rtd_open (path)
  }
}
#' Extend the index_rst file by adding readme, vignettes, and function index
#' @param path Path to root of package directory
#' @param readme name of package; used to rename 'README'.
#' @noRd
extend_index_rst_mod <- function (path, addintro = TRUE){# , readme) {
  
  index <- file.path (path, "docs", "index.rst")
  if (!file.exists (index)) {
    stop ("File [", index, "] not found")
  }
  
  u <- ""
  if(addintro) u <- c(u, paste0 ("   ", "intro.md"))
  
  x <- c (
    brio::read_lines (index),
    "",
    u,
    "",
    #add_index_section (path, "vignettes"),
    add_index_section_fx (path, "functions")
  )
  
  brio::write_lines (x, index)
}

add_index_section <- function (path, type = "functions") {
  the_dir <- file.path (path, "docs", type)
  if (!dir.exists (the_dir)) {
    return (NULL)
  }
  type_cap <- type
  substring (type_cap, 1, 1) <- toupper (substring (type_cap, 1, 1))
  x <- c (
    "",
    "",
    ".. toctree::",
    "   :maxdepth: 1",
    paste0 ("   :caption: ", type_cap),
    ""
  )
  for (f in list.files (the_dir)) {
    x <- c (x, paste0 ("   ", type, .Platform$file.sep, f)
    )
  }
  return (x)
}

.fNameWExt <- function(x){
  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
  # unlist(lapply(strsplit(basename(x),"[.]"), head , 1 ), use.names = FALSE)
}

add_index_section_fx <- function (path, type = "functions") {
  the_dir <- file.path (path, "docs", type)
  if (!dir.exists (the_dir)) {
    return (NULL)
  }
  type_cap <- type
  substring (type_cap, 1, 1) <- toupper (substring (type_cap, 1, 1))
  x <- c (
    "",
    ".. toctree::",
    "   :maxdepth: 1",
    paste0 ("   :caption: ", type_cap),
    ""
  )
  for (f in list.files (the_dir)) {
    fn <- .fNameWExt(f)
    x <- c (x, paste0 ("   ", fn ," <", type, .Platform$file.sep, f, ">")
    )
  }
  return (x)
}


updateReadTheDocx <- function(path, dev = FALSE, open = TRUE){
  path <- r2readthedocs:::convert_path (path)
  
  if (!dir.exists (file.path (path, "docs"))) {
    stop (
      "'docs' does not exist!",
      call. = FALSE
    )
  }

  convert_man(path)
  # readme <- r2readthedocs:::convert_readme(path)
  # readme <- utils::tail (strsplit (readme, .Platform$file.sep) [[1]], 1L)
  # r2readthedocs:::convert_vignettes(path)
  r2readthedocs:::rignore_amend(path)
  
  rmTocTrees(path)
  extend_index_rst_mod(path,  addintro = FALSE) #, readme)
  r2readthedocs:::edit_conf_py(path)
  
  static_dir <- file.path (path, "docs", "_static")
  if (!dir.exists (static_dir)) {
    dir.create (static_dir)
  }
  
  if (dev) {
    rm_pkg_deps (path) # to enable clean updates
    add_pkg_deps (path)
  } else if (dir.exists (file.path (path, "docs", "dependencies"))) {
    rm_pkg_deps (path)
  }
  
  r2readthedocs::rtd_clean(path)
  r2readthedocs::rtd_build (path, dev = dev)
  
  if (open) {
    r2readthedocs::rtd_open (path)
  }
}

addIntro <- function(path){
  intro <- file.path (path, "docs", "intro.md")
  brio::write_lines("# Introduction\n This is an intro.", intro)
}

rmTocTrees <- function(path){
  index <- file.path (path, "docs", "index.rst")
  if (!file.exists (index)) {
    stop ("File [", index, "] not found")
  }
  lines <- brio::read_lines (index)
  
  tst <- grep(":caption", lines)
  
  rml <- c()
  for(i in tst){
    if(grepl("Functions|Vignettes", lines[i])){
      rml <- c(rml, i - 0:2)
    }
  }
  #rml <- seq(from = min(rml), to = max(rml))
  lines <- lines[-rml]
  
  # # Define the pattern of lines to remove (adjust as needed)
  # pattern <- "^\\s*\\.\\.\\s*toctree::\\s*$|^\\s*:\\w+:\\s+.*$"
  # 
  # # Remove lines matching the pattern
  # lines <- lines[!grepl(pattern, lines)]
  
  # Generalized pattern for 'string <functions/string.md>'
  pattern <- "^\\s*[a-zA-Z.][a-zA-Z0-9._-]*\\s+<functions/[a-zA-Z.][a-zA-Z0-9._-]*\\.md>"
  
  # Remove lines matching the pattern
  lines <- lines[!grepl(pattern, lines)]
  
  # Write the filtered lines back to the file (or a new file)
  brio::write_lines(lines, index)
}
# 
# # first time
# makeReadTheDocx(path = "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/RGPR")
# 
# # UPDATES
# # 1. Convert Rd to md
# # convert_man(path = "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/RGPR")
# 
# updateReadTheDocx(path = "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/RGPR")
# 
# 
# index <- file.path (path, "docs", "index.rst")
# brio::read_lines (index)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # devtools::install_github("quantsch/Rd2md")
# rd_text <- function(x, fragment = FALSE) {
#   con <- textConnection(x)
#   on.exit(close(con), add = TRUE)
#   
#   set_classes(tools::parse_Rd(x, fragment = fragment, encoding = "UTF-8"))
# }
# convert_man <- function (path = ".") {
#   docs <- file.path (path, "docs")
#   fns_dir <- file.path (docs, "functions")
#   if (!dir.exists (fns_dir)) {
#     chk <- dir.create (fns_dir, recursive = TRUE)
#   }
#   
#   flist <- list.files (file.path (path, "man"),
#                        full.names = TRUE,
#                        pattern = "\\.Rd$"
#   )
#   
#   for (f in flist) {
#     
#     fshort <- utils::tail (strsplit (f, .Platform$file.sep) [[1]], 1L)
#     fout <- file.path (fns_dir, gsub ("\\.Rd$", ".md", fshort))
#     tst <- Rd2md::read_rdfile(f)
#     
#     # parsed_rd <- tools::parse_Rd(f, encoding = "UTF-8")
#     # tst <- Rd2md:::as_rdfile(parsed_rd, subclass = subclass)
#     
#     # tst <- rd_text(f, FALSE)
#     out <- Rd2md::as_markdown (tst, outfile = fout)
#     
#     x <- tst
#     find_section <- function(x, name) {
#       x[[which(sapply(x, inherits, name))]]
#     }
#     order_rdfile <- function(
#     x,
#     keep_first = NULL,
#     keep_last = NULL,
#     remove = NULL
#     ) {
#       section_names_in <- sapply(x, function(x) class(x)[1])
#       # match() returns NA for unmatched elements -> hence suppressing NA
#       x_sorted <- x[
#         na.omit(c(
#           match(keep_first, section_names_in, nomatch = NULL),
#           which(!(section_names_in %in% c(keep_first, keep_last))),
#           match(keep_last, section_names_in)
#         ))
#       ]
#       section_names_sorted <- sapply(x_sorted, function(x) class(x)[1])
#       x_sorted[which(!(section_names_sorted %in% remove))]
#     }
#     as_markdown.rdfile <- function(x, section_level = 1, ...) {
#       title <- Rd2md::as_markdown(
#         find_section(x, "tag_title"), section_level = section_level     )
# 
#       x <- order_rdfile(
#         x,
#         keep_first = paste0("tag_", c("description", "usage", "arguments")),
#         # title already concatenated above
#         # name and alias(es) will be shown in usage section
#         remove = paste0("tag_", c("name", "title", "alias", "aliases")),
#         keep_last = paste0("tag_", c("value", "see_also", "examples"))
#       )
# 
#       subsection_level <- section_level + 1
#       
#       for(i in seq_along(x)){
#         Rd2md::as_markdown(x[[i]],  section_level = subsection_level)
#       }
#       
#       rest <- paste0(
#         sapply(x, Rd2md::as_markdown, section_level = subsection_level), collapse = "")
# 
#       paste0(
#         title,
#         rest
#       )
#     }
#     
#     
#     
#     # out <- Rd2md::as_markdown (f, outfile = fout)
#     cat(out, file = fout)
#   }
#   
# }
#' 
#' convert_man(path = "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/RGPR")
#' # path <- "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/RGPR"
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # Convert RD attributes to S3 classes -------------------------------------
#' 
#' set_classes <- function(rd) {
#'   if (is.list(rd)) {
#'     rd[] <- lapply(rd, set_classes)
#'   }
#'   set_class(rd)
#' }
#' 
#' set_class <- function(x) {
#'   structure(x,
#'             class = c(attr(x, "class"), tag(x), "tag"),
#'             Rd_tag = NULL,
#'             srcref = NULL,
#'             macros = NULL
#'   )
#' }
#' 
#' tag <- function(x) {
#'   tag <- attr(x, "Rd_tag")
#'   if (is.null(tag)) return()
#'   
#'   gsub("\\", "tag_", tag, fixed = TRUE)
#' }
#' 
#' #' @export
#' `[.tag` <- function(x, ...) {
#'   structure(NextMethod(), class = class(x))
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' x <- f
#' rd_text <- function(x, fragment = FALSE) {
#'   con <- textConnection(x)
#'   on.exit(close(con), add = TRUE)
#'   
#'   set_classes(tools::parse_Rd(x, fragment = fragment, encoding = "UTF-8"))
#' }
#' 
#' 
#' rdtext <- rd_text(f, FALSE)
#' str(rdtext)
#' attributes(rdtext)$class
#' 
#' 
#' rd <- tools::parse_Rd(f, fragment = FALSE, encoding = "UTF-8")
#' u <- set_classes(rd)
#' 
#' tools::Rd2HTML(rd)
#' 
#' extend_index_rst <- function (path, readme) {
#'   
#'   index <- file.path (path, "docs", "index.rst")
#'   if (!file.exists (index)) {
#'     stop ("File [", index, "] not found")
#'   }
#'   
#'   x <- c (
#'     brio::read_lines (index),
#'     "",
#'     paste0 ("   ", readme),
#'     "",
#'     "",
#'     add_index_section (path, "vignettes"),
#'     add_index_section (path, "functions")
#'   )
#'   
#'   brio::write_lines (x, index)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' get_package_info <- function(pkg) {
#'   pkg_path <- path.expand(pkg)
#'   pkg_name <- basename(pkg_path)
#'   type <- "src"
#'   mandir <- "man"
#'   
#'   if (!dir.exists(pkg_path)) {
#'     # find.package will throw an error if package is not found
#'     pkg_path <- find.package(pkg_name)
#'     type <- "bin"
#'     mandir <- "help"
#'   }
#'   
#'   manpath <- file.path(pkg_path, mandir)
#'   if (!dir.exists(manpath))
#'     stop("Path does not exist:", manpath)
#'   
#'   list(
#'     path = pkg_path,
#'     name = pkg_name,
#'     type = type,
#'     mandir = mandir,
#'     manpath = manpath
#'   )
#' }
#' 
#' as_rdfile <- function(x, subclass = NULL, ...) {
#'   x_cls <- set_classes(x)
#'   # remove empty nodes
#'   x_cls[sapply(x_cls, is_empty)] <- NULL
#'   structure(
#'     x_cls,
#'     class = c(subclass, "rdfile")
#'   )
#' }
#' 
#' set_classes <- function(x) {
#'   if (is.list(x)) {
#'     # `[]<-` keeps the attributes of `rd`
#'     x[] <- lapply(x, set_classes)
#'   }
#'   set_class(x)
#' }
#' 
#' set_class <- function(x) {
#'   addtl_classes <- NULL
#'   # isTRUE will always return TRUE/FALSE, even if tag(x) returns with length 0
#'   if (isTRUE(tag(x) %in% section_tags)) {
#'     addtl_classes <- "rdsection"
#'   }
#'   structure(
#'     x,
#'     # attr(x, "class") instead of "class" to avoid base classes like "list".
#'     # "tag" is the superclass, required to define generics like "print.tag".
#'     class = c(attr(x, "class"), tag(x), addtl_classes, "tag"),
#'     # remove attributes that come with `tools::parse_Rd`, but are obsolete now
#'     Rd_tag = NULL,
#'     srcref = NULL,
#'     macros = NULL
#'   )
#' }
#' 
#' read_package_rdfiles <- function(pkg = ".", subclass = NULL) {
#'   pkg_rd_files <- tools::Rd_db(dir = pkg)
#'   lapply(pkg_rd_files, as_rdfile, subclass = subclass)
#' }
#' 
#' tag <- function(x) {
#'   tag <- attr(x, "Rd_tag")
#'   if (is.null(tag)) return()
#'   
#'   # Rd_tags start with "\\", e.g. "\\description",
#'   # hence we replace that with "tag_", e.g. "tag_description"
#'   gsub("\\", "tag_", tag, fixed = TRUE)
#' }
#' 
#' 
#' pkg_info <- get_package_info("/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/RGPR")
#' rd_files <- read_package_rdfiles(pkg_info$path, subclass = "refman_rdfile")
#' desc <- list(read_description(pkg_info$path))
#' 
#' refman_sections <- append(desc, rd_files)
#' 
#' refman <- paste0(
#'   sapply(refman_sections, parse_refman_content, output_format),
#'   collapse = ""
#' )
#' 
#' write_to_file(
#'   refman,
#'   file = output_file,
#'   sep = ""
#' )
#' 
#' Rd2md::render_refman(output_file = "test.md")
