#' Calculate Convergence Thresholds for the rSSR curve
#'
#' Takes a dataframe created by renorm_SSR and calculates the range in values
#' and then calculates thresholds given. Suggested values are
#' 0.5, 0.1, and 0.005. The rSSR calculated for each MBP should decrease
#' with each increasing blanking period until they reach close to zero, which
#' We consider convergence. Since the rSSR curve generally bounces around an
#' assymptote and often does not reach or stay at 0, we set a threshold a priori
#'  for identifying convergence.
#'
#' @param rSSR_df a dataframe created by created by renorm_SSRduration compare
#' showing the renormalized sum of squares of the residuals between one
#' potential blanking period and the next.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group organisms. Common groupings are species and cohorts.
#' @param thresh_levels a single value or vector of values used to set
#' thresholds for identifying convergence.
#' @return A dataframe of rSSR values corresponding to the given convergence
#' threshold
#' @import dplyr
#' @export
conv_thresholds <- function(rSSR_df, var_groups, thresh_levels = c(0.05,0.01,0.005)){
  thresh_list <- list()

  for(s in 1:length(thresh_levels)){
    thresh_list[[s]] <- rSSR_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(var_groups)))|>
      dplyr::summarise(min_val = min(rSSR), max_val = max(rSSR)) |>
      dplyr::mutate(threshold = ((max_val - min_val) * thresh_levels[s]) + min_val,
                    thresh_level = thresh_levels[s])
  }

  thresh_list <- bind_rows(thresh_list)

  return(thresh_list)
}
#' @examples
#'
#' # Calculate the 95% "convergence" threshold for the rSSR data
#' conv_thresholds(rSSR_df = ex_rSSR,
#'                 var_groups = "fish_type",
#'                 thresh_levels = 0.05)
#'
