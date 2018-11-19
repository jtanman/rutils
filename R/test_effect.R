# By: James Tan

# Date: 3/16/2017

# Function to measure the pass through effect of increasing of certain columns in a data frame by a delta amount.
# Useful for when trying to measure the beta coefficient/effect of a variable but it is transformed through various
# scaling and pca processes.


#' @title Test Delta Effect of Generic Function
#'
#' @description
#' \code{test_effect} returns a data frame recording the results of changing an
#' input variable on a target variable after applying a function.
#' 
#' @param data data frame to test on
#' @param col name of the input column which is to be changed
#' @param measure name of the measure column in the resulting data frame
#' @param delta delta applied to observations in col
#' @param obs indexes for observations in data to test on
#' @param FUN function to apply to data frame in the form of FUN(data, ...)
#' @param ... additional params for FUN
#' 
#' @return Data frame with observation, original value, delta, original measure, new measure, effect
#' 
#' @export
test_effect <- function(data, col, measure, delta, obs, FUN, ...) {
    results <- data.frame()
    origdata <- data
    for (i in obs) {
        data <- origdata
        print(i)
        original_x <- data[i, col]
        if (!is.na(original_x)) {
            original <- FUN(data, ...)[i, measure]
            data[i, col] <- data[i, col] + delta
            new <- FUN(data, ...)[i, measure]
            effect <- new - original
            row <- data.frame(obs = i, orig_x = original_x, delta = delta, orig_measure = original, new_measure = new, 
                effect = effect)
            results <- rbind(results, row)
        }
    }
    return(results)
}
