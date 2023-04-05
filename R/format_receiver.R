#' Format for Receiver data for filteRjsats
#'
#' This function takes a dataframe of receiver metadata and reformats specific
#' columns so that they can be read by the filtering functions in filteRjsats
#' package
#'
#' @param data the detection dataframe with columns for individual receivers,
#' tag IDs,datetime, and the expected ping rate.
#' @param var_receiver_serial the column name, in quotes, which identifies the
#' serial number of the detection receiver
#' @param var_receiver_make the column name, in quotes, which identifies the
#' make or brand of the detection receiver. Must be one of "ATS", "Lotek", or
#' "Tekno"
#' @param var_receiver_deploy the column name, in quotes, which identifies the
#' date and time the receiver was deployed
#' @param var_receiver_retrieve the column name, in quotes, which identifies the
#' date and time the receiver was retrieved
#' @param local_time_zone the local timezone used for analyses. Uses tz database
#' names (e.g. "America/Los_Angeles" for Pacific Time)
#' @param time_format a string value indicating the datetime format of all time
#' fields
#' @return A dataframe which contains fields renamed to match those required by
#' add_receivers() function
#' @import dplyr
#' @export
format_receivers <- function(data, var_receiver_serial, var_receiver_make,
                         var_receiver_deploy, var_receiver_retrieve,
                         local_time_zone, time_format){
  df <- data
  df <- data |>
    dplyr::rename("ReceiverSN" = var_receiver_serial,
           "Make" = var_receiver_make,
           "receiver_start" = var_receiver_deploy,
           "receiver_end" = var_receiver_retrieve)
  df <- df |>
    dplyr::mutate(ReceiverSN = as.character(ReceiverSN),
           Make = as.character(Make),
           receiver_start =
             lubridate::parse_date_time(as.character(receiver_start),
                                        tz = local_time_zone,
                                        orders = c(time_format)),
           receiver_end =
             lubridate::parse_date_time(as.character(receiver_end),
                                        tz = local_time_zone,
                                        orders = c(time_format)))
  return(df)
}
#' @examples
#'
#' # Rename columns to work with functions
#' format_receivers(data = receivers,
#'                  var_receiver_serial = "receiver_serial_number",
#'                  var_receiver_make = "receiver_make",
#'                  var_receiver_deploy = "receiver_start",
#'                  var_receiver_retrieve = "receiver_end",
#'                  local_time_zone = "America/Los_Angeles",
#'                  time_format = "%m-%d-%Y %H:%M:%S")
#'
