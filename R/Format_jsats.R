#' Format for filteRjsats
#'
#' This function takes a detection dataframe from a single receiver and
#' reformats specific columns so that they can be read by the filtering
#' functions in filteRjsats package
#'
#' @param data the detection dataframe with columns for individual receivers,
#' tag IDs,datetime, and the expected ping rate.
#' @param var_receiver the column name, in quotes, which identifies unique
#' detection receivers, usually a single hydrophone or antenna.
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_datetime_local the column name, in quotes, which identifies the
#' date and time of the detection event. This column should already have been converted
#' to POSIXct format and should be converted to the local timezone.
#' @param frequency the column name, in quotes, which identifies the maximum
#' temporal frequency at which transmitters in organisms emit a detectable
#' signal.
#' @param var_receiver_serial the column name, in quotes, which identifies the
#' serial number of the detection receiver
#' @param var_receiver_make the column name, in quotes, which identifies the
#' make or brand of the detection receiver. Must be one of "ATS", "Lotek", or
#' "Tekno"
#' @param var_org_ping the column name, in quotes, which identifies the maximum
#' temporal frequency at which transmitters in organisms emit a detectable
#' signal.
#' @return A standardized detection dataframe which can be read by filteRjsats
#' @export
format_jsats <- function(data, var_Id, var_datetime_local, var_frequency,
                         var_receiver_serial, var_receiver_make,
                         var_org_ping){
  df <- data
  df <- data %>%
    rename("Tag_Hex" = var_Id,
           "DateTime_Local" = var_datetime_local,
           "ReceiverSN" = var_receiver_serial,
           "Make" = var_receiver_make,
           "tag_pulse_rate_interval_nominal" = var_org_ping)
  return(df)
}
