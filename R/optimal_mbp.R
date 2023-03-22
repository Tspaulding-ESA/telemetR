#' Idenitfy the Optimum MBP based on Convergence Threshold
#'
#' Takes dataframes created by renorm_SSR and conv_thresholds and determines the
#' corresponding "optimum" maximum blanking period (MBP) for each convergence
#' threshold.
#'
#' @param rSSR_df a dataframe created by created by renorm_SSRduration compare
#' showing the renormalized sum of squares of the residuals between one
#' potential blanking period and the next.
#' @param thresh_values a dataframe created by conv_thresholds corresponding to
#' the chosen convergence thresholds.
#' @return A dataframe showing the convergence value and corresponding maximum
#' blanking period.
#' @import dplyr
#' @export
opt_mbp <- function(rSSR_df, thresh_values){
  thresh_values$opt_mbp <- NA
  for(s in thresh_values$threshold){
    thresh_values$opt_mbp[thresh_values$threshold == s] <- rSSR$mbp_n[max(which(rSSR$rSSR > s))+1]
  }

  return(thresh_values)
}
