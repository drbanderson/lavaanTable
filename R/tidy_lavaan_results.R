#' Create a tidy results dataframe from a fitted lavaan object
#'
#' This function returns a formatted, tidy dataframe from a fitted lavaan object.
#' The object assumes at least one latent variable measured with reflective
#' indicators, although can be a simple CFA with no structural components.
#' There is also the abilty to include an indirect effect if testing mediation.
#' Note the model type codes used internally to specify the type of model object.
#'   Structural parameters: 1
#'   Measurement parameters: 2
#'   Indirect effect parameters: 3
#'   Covariance parameters: 4
#'   Model fit parameters: 5
#'
#' @param x fitted lavaan object
#' @param std.est Do you want standardized estimates? Defaults to FALSE
#'
#' @return dataframe
#'
#' @examples
#' tidy_lavaan()
#' tidy_lavaan(x, std.est = TRUE)
#'
#' @export

#' @importFrom magrittr %>%
#' @import dplyr
tidy_lavaan <- function(x, std.est = FALSE) {
  if(std.est == FALSE) {
    model.df <- as_data_frame(parameterEstimates(x))
  }
  else {
    model.df <- as_data_frame(standardizedSolution(x)) %>%
      rename(est = est.std)
  }
  model.df %>%
    select(rhs, op, lhs, est, se, pvalue, contains("label")) %>%
    filter(!rhs == lhs) %>%  # Drops intra-item variances
    # Drop unlabeled covariances only if the 'label' column is present
    {if("label" %in% names(.)) filter(., !(op == "~~" & label == "")) else .} %>%
    # Drop the intercepts
    filter(!op == "~1") %>%
    mutate(est = round(est, 2),
           se = round(se, 2),
           pvalue = round(pvalue, 3)) %>%
    mutate(estimate = paste(est, " (", se,")", sep = "")) %>%
    mutate(pvalue = paste("p =", pvalue),  # Tidy the p-value for display
           pvalue = case_when(
             pvalue == "p = 0" ~ "p < 0.001",
             str_detect(pvalue, "p = NA") ~ "",
             str_detect(pvalue, "p = 0.") ~ pvalue)) %>%
    mutate(estimate = if_else(pvalue == "", estimate,
                              paste(estimate, "; ", pvalue, sep = ""))) %>%
    mutate(model = case_when(
      op == "=~" ~ 2,  # Measurement parameter model code
      op == "~" ~ 1,  # Structural parameter model code
      op == "~~" ~ 4,  # Covariance parameter code
      op == ":=" ~ 3)) %>%  # Indirect effect parameter code
    mutate(parameter = case_when(
      model == 2 ~ paste(rhs, "<--", lhs),
      model == 1 ~ paste(lhs, "<--", rhs),
      model == 4 ~ paste(lhs, "~~", rhs),
      model == 3 ~ paste("a * b"))) %>%
    select(model, parameter, estimate) %>%
    arrange(model)
}
