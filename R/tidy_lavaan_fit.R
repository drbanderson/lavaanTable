#' Create a tidy model fit dataframe from a fitted lavaan object
#'
#' This function returns a tidy dataframe containing the Chi-squared statistic,
#' degrees of freedom, and p-value from a fitted lavaan object. The dataframe
#' is preformatted to align with the tidy_lavaan output format to easily
#' combine the tables together for presentation in a paper.
#'
#' @param x fitted lavaan object
#'
#' @return dataframe
#'
#' @examples
#' tidy_lavaanFit()
#'
#' @export

#' @importFrom magrittr %>%
#' @import dplyr

# Create a tidy dataframe with model fit indices...
#  Chi-square statistic and model observations
tidy_lavaanFit <- function(x) {
  as_data_frame(fitMeasures(x, c("chisq", "df", "pvalue"))) %>%
    rownames_to_column() %>%
    spread(rowname, value) %>%
    mutate(parameter = "Model Chi-sq") %>%
    mutate(chisq = round(chisq, 2),
           pvalue = round(pvalue, 3)) %>%
    mutate(estimate = paste(chisq, " df = ", df, sep = "")) %>%
    mutate(pvalue = paste("p =", pvalue),  # Tidy the p-value for display
           pvalue = case_when(
             pvalue == "p = 0" ~ "p < 0.001",
             str_detect(pvalue, "p = 0.") ~ pvalue)) %>%
    mutate(estimate = paste(estimate, "; ", pvalue, sep = "")) %>%
    select(parameter, estimate) %>%
    rbind(c("Observations", nobs(x), "")) %>%
    mutate(model = 5) %>% # Create the 'model fit parameters' model code
    select(model, parameter, estimate) %>% # Rearrange the dataframe
    arrange(desc(parameter)) # Rearrange the rows
}
