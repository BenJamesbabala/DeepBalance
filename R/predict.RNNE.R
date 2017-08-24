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
#' @import RSNNS
#' 
#' @examples
#' predict.rnne(models, newdata, type = "Class")

predict.RNNE <- function(models,
                         newdata,
                         type = "class") {

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
  #   RSNNS      : Used for prediction

  ### Error Handling ###

  # Check to see that there is a list of nnet objects
  if (class(models[[1]])[1] != "mlp") {
    stop("Must be mlp outcomes from RNNE()!")
  }

  # Check to see that we indeed have a list
  if (length(models) < 1 | class(models) != "list") {
    stop("Need a full LIST of models!")
  }

  # Check to see that we have either `prob` or `class`
  if (!is.element(type, c("class", "raw"))) {
    stop("type needs to be either `class` or `raw`")
  }

  ### End Error Handling ###

  # Create matrix to hold predictions
  predictions <- matrix(nrow = nrow(newdata), ncol = length(models))

  # Loop through the models (columns of predictions)
  for (c in 1:ncol(predictions)) {
    # The c-th column are the predictions from the c-th model
    predictions[, c] <- as.numeric(predict(models[[c]], newdata, type = type))
  }

  # Return the predictions
  rs <- rowSums(predictions)

  # Return prediction results
  if (type == "class") {
    # Average out probabilities
    # If > 0.5, label as 1
    rs <- rs / length(models)
    rs <- ifelse(rs > 0.5, 1, 0)
    return(rs)
  } else {
    # Average out probabilities if returning prob
    rs <- rs / length(models)
    return(rs)
  }
}