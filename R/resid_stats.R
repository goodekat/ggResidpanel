# Response versus predicted plot. -- UNDER CONSTRUCTION
#
# resid_stats <- function(model){
#
#   # Create a data frame with model statistics
#   model_values <- data.frame(statistic = c("Observations", "Error DF", "MSE",
#                                      "R-Square", "Adj-Rsqure"),
#                              value = round(c(dim(model$model)[1],
#                                              model$df.residual,
#                                              summary(model)[[6]],
#                                              summary(model)[[8]],
#                                              summary(model)[[9]]), 3))
#
#   # Save the plot of the table
#   stats <- tableGrob(model_values,
#             rows = rownames(rep("", 5)),
#             cols = colnames(rep("", 2)),
#             theme = ttheme_minimal())
#
# }
