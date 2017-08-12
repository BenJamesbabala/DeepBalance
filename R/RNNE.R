#' @title Ensemble of Random Neural Networks
#' @description This function trains an ensemble of random neural networks 
#'     using balanced bootstrapped data
#' @author Peter Xenopoulos 
#' 
#' @param formula Formula for containing outcome and all of the predictors of interest
#' @param train Training dataset
#' @param mtry Number of predictors to randomly try
#' @param total.nets Number of total networks to train
#' @param verbose Whether or not to be verbose
#' @param n.cores Number of cores to use
#' 
#' @keywords neural network, random ensemble, random neural network, balanced bootstrap
#' 
#' @export
#' 
#' @examples
#' RNNE(formula, train, mtry = 3, total.nets = 10, verbose = TRUE, n.cores = 1)

RNNE <- function(formula,
                 train,
                 mtry = 1,
                 total.nets = 1,
                 verbose = FALSE,
                 n.cores = 1) {

  # Trains an ensemble of random neural networks
  #    using balanced bootstrapped data
  #
  # Args:
  #   formula    : Formula used for model, e.g. y~x1+x2+x3
  #   train      : Training data set
  #   mtry       : Number of randomly selected variables to try for
  #                   each individual neural network
  #   total.nets : Total number of neural networks to use
  #   verbose    : Whether or not to be verbose with output
  #   n.cores    : Number of cores to use (if parallel)
  #
  # Returns:
  #   Returns list of trained neural networks

  # Error Handling

  return(TRUE)
}