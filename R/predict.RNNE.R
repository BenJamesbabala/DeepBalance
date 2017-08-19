#' @title Predict from an ensemble of random neural networks
#' @description This function predicts the results of a random neural network
#'
#' @author Peter Xenopoulos 
#' 
#' @param models List of models
#' @param newdata New data to predict outcomes 
#' @param type Can be 'class' to predict class or 'prob' to predict probability
#' 
#' @return A vector with the predictions
#' 
#' @keywords neural network, prediction
#' 
#' @export
#' 
#' @import dplyr
#' 
#' @examples
#' predict.rnne(models, newdata, type = "Class")

predict.RNNE(models, 
             newdata, 
             type = "Class") {

  # Predicts the outcomes from an ensemble of neural networks
  #
  # Args:
  #   models  : A list of models from the RNNE function
  #   newdata : New data to predict outcomes for
  #   type    : Can either be 'class' to predict factor outcomes
  #               or 'prob' for probabilities
  #
  # Returns:
  #   Returns vector of predictions
  #
  # Notes:
  #   Code cleaned using lintr
  #
  # Imports:
  #   dplyr      : Used for data manipulation

  ### Error Handling ###

  # Check to see that there is a list of nnet objects
  if (class(newmodels[[1]]) != "nnet.formula") {
    stop("Must be nnet outcomes from RNNE()!")
  }

  # Check to see that we indeed have a list
  if (length(newmodels) < 1 | type(newmodels) != "list") {
    stop("Need a full LIST of models!")
  }

  # Check to see that we have either `prob` or `class`
  if (type !%in% c("class","prob")) {
    stop("type needs to be either `class` or `prob`")
  }
}