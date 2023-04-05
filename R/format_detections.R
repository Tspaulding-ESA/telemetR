#' Format Detections for filteRjsats
#'
#' This function takes a detection dataframe from a single receiver and
#' reformats specific columns so that they can be read by the filtering
#' functions in filteRjsats package
#'
#' @param data the detection dataframe with columns for individual receivers,
#' tag IDs,datetime, and the expected ping rate.
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_datetime_local the column name, in quotes, which identifies the
#' date and time of the detection event. This column should already have been converted
#' to POSIXct format and should be converted to the local timezone.
#' @param var_frequency the column name, in quotes, which identifies the maximum
#' temporal frequency at which transmitters in organisms emit a detectable
#' signal, only for use before JSATS filtering.
#' @param var_receiver_serial the column name, in quotes, which identifies the
#' serial number of the detection receiver
#' @param var_receiver_make the column name, in quotes, which identifies the
#' make or brand of the detection receiver. Must be one of "ATS", "Lotek", or
#' "Tekno", only for use before JSATS filtering.
#' @param local_time_zone the local timezone used for analyses. Uses tz database
#' names (e.g. "America/Los_Angeles" for Pacific Time)
#' @param time_format a string value indicating the datetime format of all time
#' fields
#' @return A standardized detection dataframe which can be read by filteRjsats
#' @import dplyr
#' @export
format_detects <- function(data, var_Id, var_datetime_local, var_frequency = NULL,
                           var_receiver_serial, var_receiver_make = NULL,
                           local_time_zone, time_format){
  df <- data
  if(is.null(var_receiver_make) & is.null(var_frequency)){
    df <- data %>%
      dplyr::rename("Tag_Code" = var_Id,
                    "DateTime_Local" = var_datetime_local,
                    "ReceiverSN" = var_receiver_serial)
    df <- df %>%
      dplyr::mutate(Tag_Code = as.character(Tag_Code),
                    DateTime_Local =
                      lubridate::parse_date_time(as.character(DateTime_Local),
                                                 tz = local_time_zone,
                                                 orders = c(time_format)),
                    ReceiverSN = as.character(ReceiverSN))
    return(df)}
  if(!is.null(var_receiver_make)&!is.null(var_frequency)){
    df <- data %>%
      dplyr::rename("Tag_Code" = var_Id,
                    "DateTime_Local" = var_datetime_local,
                    "Freq" = var_frequency,
                    "ReceiverSN" = var_receiver_serial,
                    "Make" = var_receiver_make)
    df <- df %>%
      dplyr::mutate(Tag_Code = as.character(Tag_Code),
                    DateTime_Local =
                      lubridate::parse_date_time(as.character(DateTime_Local),
                                                 tz = local_time_zone,
                                                 orders = c(time_format)),
                    ReceiverSN = as.character(ReceiverSN),
                    Make = as.character(Make))
    return(df)}
  if(any(is.null(var_receiver_make),is.null(var_frequency))){
    errorCondition(writeLines(c("Are you formatting for filtering?",
                                "Receiver Make & frequency must be supplied, else leave NULL",
                                "Please refer to documentation")))}
}
#' @examples
#'
#' # Rename columns to work with functions
#' format_detects(data = filtered_detections,
#'                var_Id = "tag_id",
#'                var_datetime_local = "local_time",
#'                var_receiver_serial = "serial",
#'                local_time_zone = "America/Los_Angeles",
#'                time_format = "%Y-%m-%d %H:%M:%S")
#'
