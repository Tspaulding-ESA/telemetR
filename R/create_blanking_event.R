#' Create Potential Blanking Periods for Identifying Optimal Blanking Period
#'
#' Takes a dataframe with telemetry detection data and a list of potential
#' Blanking Period multipliers (n_val) and crosses them, duplicating the entire
#' dataframe by the length of n_val. Detections are grouped by individual,
#' site, and any supplied grouping variables. Then events are created by
#' collecting detections which occur within n_val*ping_rate from the next
#' detection. This function can be very slow depending on the size of the
#' dataframe.
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
#' @param time_unit the preferred unit of time to calculate durations, this
#' should correspond to the ping_rate, (i.e. if the ping rate is 3 seconds, the
#' preferred time_unit is seconds). If the preferred time_unit is on the same
#' scale as the ping_rate, the ping rate should be converted to the same scale.
#' @return A dataframe which has been crossed with all integers in n_val, and
#' which has been condensed into events. Please refer to Capello et. al. 2015
#' for further detail about the creation of these events.
#' @import dplyr
#' @export
blanking_event <- function(data,var_site,var_Id,var_datetime,var_groups = NULL,
                           var_ping_rate, n_val, time_unit){
  setup <- setup_blanking(data,
                          var_site = var_site,
                          var_Id = var_Id,
                          var_datetime = var_datetime,
                          var_groups= var_groups,
                          var_ping_rate = var_ping_rate,
                          n_val = n_val)

  event <- setup |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var_Id, "n_val")))) |>
    dplyr::mutate(dplyr::across(var_site, .fns = ~.x == lag(.x), .names = "same_loc"),
                  dplyr::across(c(var_ping_rate), .fns = ~ .x*n_val, .names = "mbp_n"),
                  dplyr::across(c(var_datetime),
                                .fns = ~ difftime(.x,
                                                  lag(.x),
                                                  units = time_unit),
                                .names = "td"),
                  same_time = td<=mbp_n,
                  event_change_binary = !(same_loc & same_time),
                  event_change_binary = ifelse(is.na(event_change_binary),
                                               FALSE,
                                               event_change_binary),
                  event_change = cumsum(event_change_binary))

  event_dur <- event |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var_groups, var_Id, "mbp_n",
                                                  "event_change", var_site)))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(var_datetime),
                                   .fns = list(start_time = ~ min(.x, na.rm = TRUE),
                                               end_time = ~ max(.x, na.rm = TRUE),
                                               n_det = ~ n()),
                                   .names = "{.fn}"
    )) |>
    dplyr::mutate(duration = as.numeric(difftime(end_time,
                                                 start_time,
                                                 units = "sec"))
    ) |>
    dplyr::ungroup()

  return(event_dur)
}
#' @examples
#'
#' # Create a dataframe of events blanked by a set of n_values from 1:500
#'
#' blanking_event(data = filtered_detections,
#'                var_Id = "Tag_Code",
#'                var_site = "receiver_general_location",
#'                var_datetime = "DateTime_Local",
#'                var_groups = "fish_type",
#'                var_ping_rate = "tag_pulse_rate_interval_nominal",
#'                n_val = seq(1,500,5),
#'                time_unit = "secs")
#'
