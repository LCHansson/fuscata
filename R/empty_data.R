#' Does my data have holes?
#' 
#' @export
find_holes_in <- function(data) {
  has_na <- data %>% 
    purrr::map(is.na) %>% 
    purrr::map(any)
  
  has_na
}

#' Obfuscate data ny inserting random NA and/or NULL noise
#' 
#' @param data The data frame to obfuscate
#' @param ... Names of columns to obfuscate (NONFUNCT)
#' @param props Range of randomly selected proportion of data to obfuscate
#' @param values Empty values to insert (NONFUNCT)
#' 
#' @export
insert_holes_in <- function(data, ..., props = c(0.1, 0.2), values = list(NA, NULL)) {
  # Ensure there's an upper part to the range of proportions
  if (length(props) == 1)
    props[[2]] <- props[[1]]
  
  # Obfuscate
  data %>% 
    purrr::dmap(function(x, props, values) {
      cat(props)
      sample_size <- runif(1, props[[1]], props[[2]])
      
      places <- sample(
        x = c(TRUE, FALSE), 
        size = length(x),
        replace = TRUE,
        prob = c(sample_size, (1 - sample_size))
      )
      
      x[places] <- NA
      
      x
    }, props, values)
}
