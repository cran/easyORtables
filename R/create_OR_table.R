#' Create Odds Ratio Table
#'
#' This function creates a table displaying odds ratios with confidence intervals
#' for logistic regression models.
#'
#' @param ... One or more logistic regression models.
#' @param output_format The output format for the table. Can be "markdown", "latex", "html", or "plaintext". Default is "plaintext".
#' @param bootstrap_options A character vector of Bootstrap table styles for HTML output. Default is NULL.
#' @return Displays a formatted table of odds ratios with confidence intervals.
#' @importFrom stats glm confint
#' @import MASS
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @export
#' @examples
#' # Simulate data
#' set.seed(123)
#' dataset <- data.frame(
#'   response = sample(c(0, 1), 100, replace = TRUE),
#'   var1 = rnorm(100),
#'   var2 = rnorm(100),
#'   var3 = rnorm(100)
#' )
#'
#' # Fit logistic regression models
#' model1 <- glm(response ~ var1 + var2, family = binomial, data = dataset)
#' model2 <- glm(response ~ var1 + var2 + var3, family = binomial, data = dataset)
#'
#' # Create OR table as plaintext
#' create_OR_table(model1, model2)
#'
#' # Create OR table with custom bootstrap options and HTML
#' create_OR_table(model1, model2, output_format="html", bootstrap_options = c("striped", "hover"))
#'
create_OR_table <- function(..., output_format = c("markdown", "latex", "html", "plaintext"), bootstrap_options = NULL) {
  if (missing(output_format)) {
    output_format <- "plaintext"
  }


  # Ensure the output format is one of the allowed values
  output_format <- match.arg(output_format)

  # Capture the list of models
  models <- list(...)

  # Initialize an empty list to store results
  result_list <- list()

  # Initialize a vector to keep track of all unique variables across models
  all_vars <- unique(unlist(lapply(models, function(model) rownames(summary(model)$coefficients))))

  # Loop over each model and its index
  for (i in seq_along(models)) {
    model <- models[[i]]

    # Extract coefficients and their confidence intervals
    coef_estimates <- summary(model)$coefficients
    ORs <- exp(coef_estimates[, "Estimate"])
    CIs <- suppressMessages(exp(confint(model)))

    # Combine ORs and CIs into a single data frame
    OR_CI <- data.frame(
      OR = ORs,
      CI_lower = CIs[, 1],
      CI_upper = CIs[, 2]
    )

    # Create a formatted string for OR (CI_lower, CI_upper)
    OR_CI$formatted <- sprintf("%.2f (%.2f, %.2f)", OR_CI$OR, OR_CI$CI_lower, OR_CI$CI_upper)

    # Reindex OR_CI to include all variables and fill missing values with em dash
    formatted <- sapply(all_vars, function(var) {
      if (var %in% rownames(OR_CI)) {
        OR_CI$formatted[rownames(OR_CI) == var]
      } else {
        "--"
      }
    })

    # Add the formatted results to the result list
    result_list[[paste("Model", i)]] <- formatted
  }

  # Combine all results into a single data frame
  result_table <- do.call(cbind, result_list)
  rownames(result_table) <- all_vars

  # Generate the output in the specified format
  if (output_format == "latex") {
    # Print LaTeX code directly in the Viewer pane
    cat(kable(result_table, format = "latex", booktabs = TRUE, caption = "Odds Ratios with 95 Percent Confidence Intervals"))
  } else if (output_format == "html") {
    # Create HTML output with or without bootstrap options
    html_output <- kable(result_table, format = "html", caption = "Odds Ratios with 95% Confidence Intervals")
    if (!is.null(bootstrap_options) && is.character(bootstrap_options)) {
      html_output <- kable_styling(html_output, full_width = FALSE, bootstrap_options = bootstrap_options)
    } else {
      html_output <- kable_styling(html_output, full_width = FALSE)
    }
    print(html_output)
  } else if (output_format == "markdown") {
    # Create markdown output
    markdown_output <- kable(result_table, format = "markdown", caption = "Odds Ratios with 95% Confidence Intervals")
    cat(markdown_output)
  } else {
    # Create plaintext output
    noquote(result_table)
  }
}
