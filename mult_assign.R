library(purrr)
library(stringr)
library(dplyr)

#' Assignment of multiple variables at once
#' 
#' @param l LHS: variables
#' @param r RHS: values
#' @return invisible assignment
#' @note The RHS has to be parsed as a list, since otherwise the values' classes
#' are not preserved
#' @examples 
#' c(a, b) %<-% list(4, TRUE)
#' list(a, b) %<-% list(4, TRUE)
#' c(a, b) %<-% list("x", c(3,4))
#' c(a, b) %<-% list(1, 2, 3)

`%<-%` <- function(l, r) {

  envir <- as.environment(-1)
  l_call <- as.character(substitute(l))[-1]

  ###Warnings
  contains_list <- as.character(substitute(r)) %>%
    stringr::str_which("list")
  if (!is.list(r)){
    warning("The RHS must be a list and not a vector, 
            in order to preserve the argument's class")
  }
  if (!is_empty(contains_list) & last(contains_list) > 1) {
    warning("At the moment list objects are not supported")
  }

  if (length(l_call) != length(r)) {
    warning("LHS and RHS have not the same amount of args")
    if (length(r) - length(l_call) > 0) {
      r <- r[1:length(l_call)]
    }
  }

  Map(
    function(k, v) {
      assign(k, v, envir = envir)
    },
    l_call, r)

  invisible()
}
