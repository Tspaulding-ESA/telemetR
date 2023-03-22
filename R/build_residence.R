#' Build Continuous Residence Events
#'
#' Takes a dataframe with telemetry detection data and a single optimum blanking
#' period chosen from the output of opt_mbp(), and groups detections by
#' individual, site, and any supplied grouping variables into residence events.
#' The residence events are created by collecting detections which occur within
#' the selected optimum maximum blanking period from the next detection. This
#' function can be very slow depending on the size of the dataframe.
#'
#' @param data the detection dataframe with columns for sites, tag IDs,
#' datetime, any grouping variables, and the expected ping rate.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group animals. Common groupings are species and cohorts.
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_datetime the column name, in quotes, which identifies the date and
#' time of the detection event. This column should already have been converted
#' to POSIXct format.
#' @param var_site the column name, in quotes, which identifies unique residency
#' sites, these sites should be as distinct as possible, such that it is
#' infrequent that organisms can be detected at two sites at a given time.
#' @param time_unit the unit of time used by the optimum maximum blanking
#' period, often on the same scale as the ping rate for the transmitter.
#' @param opt_mbp a single optimum blanking period chosen from the output of
#' opt_mbp()
#' @return A dataframe of detections which has been condensed into continuous
#' residence events based on the optimum maximum blanking period selected.
#' @import dplyr
#' @export
build_residence <- function(data, var_groups, var_Id, var_datetime, var_site,
                            opt_mbp, time_unit){
  residence <- data |>
    filter_at(dplyr::all_of(c(var_groups, var_site, var_Id, var_datetime)),
              all_vars(!is.na(.))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var_groups,var_Id)))) |>
    arrange(dplyr::across(dplyr::all_of(c(var_groups, var_Id, var_datetime)))) |>
    dplyr::mutate(dplyr::across(var_site,
                                .fns = ~.x == lag(.x),
                                .names = "same_loc"),
                  dplyr::across(var_datetime,
                                .fns = ~ difftime(.x,lag(.x), units = time_unit) <= opt_mbp,
                                .names = "same_time"),
                  same_group = if_all(any_of(c(var_groups)),
                                      .fns = ~.x == lag(.x)),
                  dplyr::across(.cols = same_loc:same_group,
                                .fns = ~ifelse(is.na(.x),TRUE,.x)),
                  event_change_binary = if_any(.cols = same_loc:same_group,
                                               .fns = ~ifelse(.x == FALSE, TRUE, FALSE)),
                  event_change_binary = ifelse(is.na(event_change_binary),
                                               FALSE,
                                               event_change_binary),
                  event_number = cumsum(event_change_binary)) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var_groups, var_site,
                                                  "event_number", var_Id)))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(var_datetime),
                                   .fns = list(start_time = ~ min(.x, na.rm = TRUE),
                                               end_time = ~ max(.x, na.rm = TRUE),
                                               n_det = ~ n()),
                                   .names = "{.fn}")) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(var_groups, var_Id, "event_number", var_site,
                                  "start_time", "end_time", "n_det"))) |>
    dplyr::mutate(duration = difftime(end_time,start_time,units = time_unit)) |>
    arrange(dplyr::across(dplyr::all_of(c(var_groups, var_Id, "start_time")))) |>
    distinct()

  return(residence)
}
