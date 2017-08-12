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
#' @import parallel
#' @import nnet
#' @import dplyr
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
  #
  # Notes:
  #   Code cleaned using lintr

  # Error Handling
  # See that train is a data frame
  if (class(train) != "data.frame") {
    stop("Training data must be a of class `data.frame`!")
  }
  # See that mtry is >= 1
  if (mtry < 1) {
    stop("Parameters mtry is invalid!")
  }
  # See that total.nets is >= 1
  if (total.nets < 1) {
    stop("Parameters total.nets is invalid!")
  }
  # See that cores is acceptable
  avail.cores <- parallel::detectCores() - 1  # Find available cores
  if (n.cores > avail.cores) {
    stop("Too many cores selected! Use `detectCores()-1`!")
  }
  if (n.cores < 1) {
    stop("Cores can't be negative!")
  }

  # Get the data into a workable frame
  model.data <- model.frame(formula, train)

  # Start to find the minority class
  tab <- table(model.data[, 1] / nrow(model.data))

  # Determine what the majority is
  majority <- names(tab[2])
  if (tab[1] > 0.5) {
    majority <- names(tab[1])
  }

  # Get majority and minority separated df's
  maj <- dplyr::filter(model.data, model.data[, 1] == majority)
  min <- dplyr::filter(model.data, model.data[, 1] != majority)

  return(TRUE)
}