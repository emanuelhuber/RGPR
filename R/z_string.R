
# used in function readGPS()
#' Extract string given a pattern
#' 
#' Get the match in a string given a regular expression and extract 
#' substrings.
#' @param x (`character`) A character vector where matches are sought, 
#'          or an object which can be coerced by [as.character()] 
#'          to a character vector.
#' @param pattern (`character`) String containing a regular expression 
#'                 to be matched (see [grep()]). 
#' @param start (`integer`) The first element of the matched string 
#'                              to be extracted (see [substr()]).
#' @param stop (`integer`) The last element of the matched string 
#'                              to be extracted (see [substr()]).
#' @return (`character`) The matched string 
#' @seealso [substr()] and [regexpr()]
#' @name GPRvirtual-class
#' @rdname GPRvirtual-class
#' @export
extractPattern <- function(x, pattern, start = 0, stop = -1){
  # pat_tr <- "(\\#[0-9]+)"
  matches <- regexpr(pattern, x, perl = TRUE)
  first <- attr(matches, "capture.start") + start
  last <- first + attr(matches, "capture.length") + stop
  return(mapply(substring, x, first, last, USE.NAMES = FALSE))
}

#' Trim string
#'
#' returns string w/o leading or trailing whitespace
#' @param x (`character`) a string or a vector of strings (characters)
#' @export
trimStr <- function (x) gsub("^\\s+|\\s+$", "", x)


  
