#' Calculate the Renormalized Sum of Squared Residuals
#'
#' Takes a dataframe of the proportion of events created by each potential
#' blanking period which "survived" a certain time (t) and calculates the sum
#' of squares of the residuals between one potential blanking period and the
#' next. This result is then renormalized by dividing the result by the number
#' of events created.
#'
#' @param time_df a dataframe created by duration compare showing the proportion
#'  of events created by each potential blanking period which "survived" a
#'  certain time (t)
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group organisms. Common groupings are species and cohorts.
#' @return A dataframe of the renormalized sum of squared residuals between each
#' potential blanking period and the subsequent one.
#' @import dplyr
#' @export
renorm_SSR <- function(time_df, var_groups=NULL){
  SR <- time_df |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(var_groups))) |>
    dplyr::mutate(SR = ifelse(!is.na(lead(t)),
                              ifelse(lead(t) == t,
                                     (lead(prop_res) - prop_res)^2,
                                     0
                              ),
                              0
    ))

  rSSR <- SR |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var_groups,"mbp_n")))) |>
    dplyr::summarise(SSR = sum(as.numeric(SR)), n = n()) |>
    dplyr::mutate(rSSR = SSR / n)

  return(rSSR)
}
