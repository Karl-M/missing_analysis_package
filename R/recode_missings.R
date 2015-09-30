#'  Recode Values in Missings
#'
#'
#' Function for easy Recoding of values in vector or data.frame to NA
#' @param
#' data Object, can be a vector, a matrix, a dataframe or more general an array of arbitrarily high dimension
#' @param
#' a  Value(s) which should be recoded to NA
#' @keywords missings, recode
#' @export
#' @examples
#' #' to recode all columns in a data.frame
#'
#' x <- c(2, 4, 1, -2, -1, 10)
#' y <- c(-2, -1, 3, 2, 9, -1)
#' z <- c(-1, -3, 4, 12, 3, 2)
#' df <- data.frame(x,y, z)
#' df.miss <- recode_missings(data = df, a = c(1,2))

recode_missings <- function(data, a) {
  if (!assertive::is_vector(data) &
      !assertive::is_data.frame(data) &
      !assertive::is_matrix(data) &
      !assertive::is_array(data)) {
    warning("data object has to be an array")
    return(NA)
  }
  if (assertive::is_vector(data)) {
    i <- data %in% a
    data[i] <- NA
  }
  if (!assertive::is_vector(data)) {
    i <- apply(data, 2, function(x) x %in% a)
    data[i] <- NA
  }
  return(data)
}

