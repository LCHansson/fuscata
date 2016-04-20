#' Vectorised addition with NA replacement
#' 
#' @param x LHS vector to add
#' @param y RHS vector to add
#' @param na_replacement Replace NA with this value
#' 
#' @export
`%+%` <- function(x, y, na_replacement = 0, .FUN = `+`) {
  # Type safety
  if (!is.numeric(na_replacement))
    stop("na_replacement must be numeric")

  # NA replacement
  x[is.na(x)] <- na_replacement
  y[is.na(y)] <- na_replacement
  
  .FUN(x, y)
}

#' NA removal in vectorised addition with multiple inputs
#' 
#' @param vecs a list of vectors
#' @param na_replacement Replace NA with this value
#' 
#' @export
plus_ <- function(vecs, na_replacement = 0) {
  if (length(vecs) == 1) {
    sum <- vecs[[1]]
  } else if (length(vecs) == 2) {
    sum <- `%+%`(vecs[[1]], vecs[[2]], na_replacement = na_replacement)
  } else {
    part_sum <- `%+%`(vecs[[1]], vecs[[2]], na_replacement = na_replacement)
    vecs <- vecs[-c(1, 2)] %>% append(list(part_sum))
    sum <- plus_(vecs, na_replacement = na_replacement)
  }
  
  return(sum)
}

#' NA removal in vectorised addition with multiple inputs (NSE version)
#' 
#' @param ... vectors to add
#' @param na_replacement Replace NA with this value
#' 
#' @export
plus <- function(..., na_replacement = 0) {
  # dots <- eval(substitute(list(...)))
  dots <- list(...)
  
  plus_(dots, na_replacement = na_replacement)
}
