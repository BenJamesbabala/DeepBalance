#' @title Preprocessing function
#' @description This function preprocesses a vector
#' @author Peter Xenopoulos 
#' 
#' @param data Data frame
#' @param method Preprocessing method
#' @param ... Other paramemters
#' 
#' @return A transformed vector
#' 
#' @keywords preprocessing, transform
#' 
#' @export
#' 
#' @examples
#' PreProcess(data, method = "minmax")

PreProcess <- function(data, method = "zscore", ...) {

  # Transforms a data vector
  #
  # Args:
  #   data   : Data vector to transform
  #   method : Transformation to do
  #
  # Returns:
  #   Returns transformation vector
  #
  # Notes:
  #   Code cleaned using lintr

  ### Error Handling ###

  if (class(data) != "numeric") {
    stop("Can only transform numeric data!")
  }

  ### End Error Handling ###

  # 'center' transformation substracts the data's mean
  if (type == "center")
    return( data - mean(data, na.rm = TRUE) )

  # 'resize' transformation subtracts an arbitrary number (default 255)
  if (type == "resize")
    return( data - resize )

  # 'scale' transformation divides by the data's sd
  if (type == "scale")
    return( data / sd(data, na.rm = TRUE) )

  # 'centerMed' transformation subtracts the data's median
  if (type == "centerMed")
    return( data - median(data, na.rm = TRUE) )

  # 'zscore' subtracts the mean and divides by the sd
  if (type == "zscore")
    return( ( data - mean(data, na.rm = TRUE) ) / sd(data, na.rm = TRUE) )

  # 'minimax' transforms data between 0 and 1
  if (type == "minmax")
    return( (data - min(data)) / (max(data) - min(data)) )
}