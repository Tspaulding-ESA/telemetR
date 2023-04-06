#' Setup a Detection Dataframe for Identifying the Optimal Blanking Period
#'
#' Takes a dataframe with telemetry detection data and a list of potential
#' Blanking Period multipliers (n_val) and crosses them, duplicating the entire
#' dataframe by the length of n_val. This function is contained in blanking
#' event.This function can be slow depending on the size of the dataframe.
#'
#' @param data the detection dataframe with columns for sites, tag IDs,
#' datetime, any grouping variables, and the expected ping rate.
#' @param var_site the column name, in quotes, which identifies unique residency
#' sites, these sites should be as distinct as possible, such that it is
#' infrequent that organisms can be detected at two sites at a given time.
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_datetime the column name, in quotes, which identifies the date and
#' time of the detection event. This column should already have been converted
#' to POSIXct format.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group animals. Common groupings are species and cohorts.
#' @param var_ping_rate the column name, in quotes, which identifies the
#' temporal frequency at which the transmitter emits a detectable signal.
#' @param n_val a vector sequence of integers which can be multiplied by the
#' ping rate to construct multiple potential blanking periods. The range and
#' step values for n should be selected based on prior knowledge about general
#' behavior habits of the study organism and the functionality of the equipment.
#' For more information, please refer to Capello et. al. 2015.
#' @returns A dataframe which has been crossed with all integers in n_val
#' @import dplyr
#' @export
#' @examples
#' # reduce dataframe for optimal blanking period analysis
#' setup_blanking(data = filtered_detections,
#'                var_Id = "Tag_Code",
#'                var_site = "receiver_general_location",
#'                var_datetime = "DateTime_Local",
#'                var_groups = "fish_type",
#'                var_ping_rate = "tag_pulse_rate_interval_nominal",
#'                n_val = c(1:10))
setup_blanking <- function(data, var_site,var_Id,var_datetime,var_groups = NULL,
                           var_ping_rate, n_val){
  df <- data[,c(var_groups,var_site,var_Id,var_datetime,var_ping_rate)]
  cross <- tidyr::crossing(df, n_val)
  cross <- dplyr::arrange(cross, dplyr::across(dplyr::all_of(c(var_groups, var_Id,
                                                               "n_val",
                                                               var_datetime))))
  return(cross)
}
