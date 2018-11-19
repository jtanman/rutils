# By: James Tan

# Date: 3/7/2017

# Functions to parse and process auc curves and pr curves

requireNamespace('AUC', quietly = TRUE)
requireNamespace('ROCR', quietly = TRUE)
requireNamespace('PRROC', quietly = TRUE)
requireNamespace('ggplot2', quietly = TRUE)
requireNamespace('graphics', quietly = TRUE)

#' @title Get AUC
#'
#' @description
#' \code{get_auc} returns the area under the ROC curve for a list of predictions and actuals
#' 
#' @param results data frame with predicted probabilities under column named 'prob' and results
#' under column named 'actual'
#' 
#' @return AUC of the ROC curve
#' 
#' @export
get_auc <- function(results) {
    roc1 <- AUC::roc(results[, "prob"], results[, "actual"])
    return(AUC::auc(roc1))
}

#' @title Plot AUC Curve
#'
#' @description
#' \code{plot_auc} plots the ROC curve and displays the AUC for a list of predictions and actuals
#' 
#' @param results data frame with predicted probabilities under column named 'prob' and results
#' under column named 'actual'
#' 
#' @return plots the ROC curve and returns the AUC
#' 
#' @import graphics
#' 
#' @export
plot_auc <- function(results) {
    roc1 <- AUC::roc(results[, "prob"], results[, "actual"])
    plot(roc1, main = "OOB ROC Curve for Churn")
    mtext(paste("AUC: ", format(AUC::auc(roc1), digits = 4), sep = ""))
    grid()
    return(AUC::auc(roc1))
}

#' @title Get PR
#'
#' @description
#' \code{get_auc} returns the area under the PR curve for a list of predictions and actuals
#' 
#' @param results data frame with predicted probabilities under column named 'prob' and numeric
#' results under column named 'actual_num'
#' 
#' @return AUC of the PR curve
#' 
#' @export
get_pr <- function(results) {    
    pr.auc <- PRROC::pr.curve(scores.class0 = results[, "prob"], weights.class0 = results[, "actual_num"])
    return(pr.auc$auc.integral)
}

#' @title Plot PR Curve
#'
#' @description
#' \code{plot_pr} plots the PR curve and displays the AUC for a list of predictions and actuals
#' 
#' @param results data frame with predicted probabilities under column named 'prob' and numeric
#' results under column named 'actual_num'
#' 
#' @return plots the PR curve and returns the AUC
#' 
#' @import graphics
#' @importMethodsFrom ROCR plot
#' 
#' @export
plot_pr <- function(results) {    
    pred <- ROCR::prediction(results[, "prob"], results[, "actual_num"])
    RP.perf <- ROCR::performance(pred, "prec", "rec")
    pr.auc <- PRROC::pr.curve(scores.class0 = results[, "prob"], weights.class0 = results[, "actual_num"])
    plot(RP.perf, main = "OOB Precision-Recall Curve")
    mtext(paste("PR AUC: ", format(pr.auc$auc.integral, digits = 4), sep = ""))
    grid()
    return(pr.auc$auc.integral)
}

#' @title Plot AUC Curve with Color Cutoff
#'
#' @description
#' \code{plot_auc_color} plots the ROC curve with a color coded cutoff and displays the AUC for a
#' list of predictions and actuals
#' 
#' @param results data frame with predicted probabilities under column named 'prob' and results
#' under column named 'actual'
#' @param ggplot TRUE or FALSE depending on if you want a ggplot or normal r plot
#' 
#' @return plots the ROC curve
#' 
#' @importFrom grDevices rainbow
#' @import ggplot2
#' @import graphics
#' 
#' @export
plot_auc_color <- function(results, ggplot = TRUE) {
    roc1 <- AUC::roc(results[, "prob"], results[, "actual"])
    
    if (ggplot) {
        roc.curve <- data.frame(FPR = roc1$fpr, recall = roc1$tpr, cutoff = roc1$cutoffs)
        ggplot2::ggplot(roc.curve, aes(x = FPR, y = recall, colour = cutoff)) + geom_line(size = 2, lineend = "round") + scale_color_gradientn(colours = rainbow(7)) + 
            labs(title = paste("ROC Curve\nPR AUC:", format(AUC::auc(roc1), digits = 4), sep = " ")) + theme(plot.title = element_text(hjust = 0.5))
    } else {
        roc.color <- PRROC::roc.curve(scores.class0 = results[, "prob"], weights.class0 = results[, "actual_num"])
        roc.color$curve <- matrix(data = c(roc1$fpr, roc1$tpr, roc1$cutoffs), ncol = 3)
        plot(roc.color)
    }
    
}

#' @title Plot PR Curve with Color Cutoff
#'
#' @description
#' \code{plot_pr_color} plots the PR curve with a color coded cutoff and displays the AUC for a
#' list of predictions and actuals
#' 
#' @param results data frame with predicted probabilities under column named 'prob' and numeric
#' results under column named 'actual_num'
#' @param ggplot TRUE or FALSE depending on if you want a ggplot or normal r plot
#' 
#' @return plots the PR curve
#' 
#' @importFrom stats complete.cases
#' @import ggplot2
#' @import graphics
#' 
#' @export
plot_pr_color <- function(results, ggplot = TRUE) {
    pred <- ROCR::prediction(results[, "prob"], results[, "actual_num"])
    RP.perf <- ROCR::performance(pred, "prec", "rec")
    pr.matrix <- matrix(data = c(slot(RP.perf, "x.values")[[1]], slot(RP.perf, "y.values")[[1]], slot(RP.perf, "alpha.values")[[1]]), 
        ncol = 3)
    pr.matrix <- pr.matrix[complete.cases(pr.matrix), ]
    pr.auc <- PRROC::pr.curve(scores.class0 = results[, "prob"], weights.class0 = results[, "actual_num"])
    
    if (ggplot) {
        colnames(pr.matrix) <- c("recall", "precision", "cutoff")
        ggplot2::ggplot(data.frame(pr.matrix), aes(x = recall, y = precision, colour = cutoff)) + geom_line(size = 2, lineend = "round") + 
            scale_color_gradientn(colours = rainbow(7)) + labs(title = paste("PR Curve\nPR AUC:", format(pr.auc$auc.integral, 
            digits = 4), sep = " ")) + theme(plot.title = element_text(hjust = 0.5))
    } else {
        pr.auc$curve <- pr.matrix
        plot(pr.auc, main = "OOB PR Curve")
    }
    
}
