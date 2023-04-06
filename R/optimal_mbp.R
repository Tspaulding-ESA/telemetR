#' Idenitfy the Optimum MBP based on Convergence Threshold
#'
#' Takes dataframes created by `renorm_SSR()` and `conv_thresholds()` and
#' determines the corresponding "optimum" maximum blanking period (MBP) for each
#' convergence threshold.
#'
#' @param rSSR_df a dataframe created by created by renorm_SSRduration compare
#' showing the renormalized sum of squares of the residuals between one
#' potential blanking period and the next.
#' @param thresh_values a dataframe created by conv_thresholds corresponding to
#' the chosen convergence thresholds.
#' @returns A dataframe showing the convergence value and corresponding optimal
#' maximum blanking period for each grouping.
#' @import dplyr
#' @export
#' @examples
#' # Determine the optimum mbp
#' opt_mbp(rSSR_df = ex_rSSR,
#'         thresh_values = conv_thresh)
opt_mbp <- function(rSSR_df, thresh_values){
  tmp <- thresh_values
  tmp <- tmp |>
    dplyr::mutate(opt_mbp = rSSR_df$mbp_n[max(which(rSSR_df$rSSR > threshold))+1])
  return(tmp)
}
