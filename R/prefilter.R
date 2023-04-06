#' Apply the "prefilter" to a Detection Dataframe
#'
#' This function takes a detection dataframe output from format_detects and
#' filters out multipath signals (signals which are bounced off of surfaces,
#' usually seen in underwater systems with hard surfaces which reflect sound)
#' and spurious signals which do not occur within a user defined time frame
#' of the last detection (12x the ping rate for organisms or 3x the ping rate
#' for beacons). Following this, the dataframe is standardized so
#' that all detection dataframes from any technology type are identical and
#' superfluous fields are removed.
#'
#' @param data A dataframe which is the output from read_jstats() or
#' format_detects()
#' @param reference_tags A vector of potential reference (beacon) tag IDs
#' @param time_unit The unit of time used for analyses (seconds, minutes, hours,
#' days, weeks, months)
#' @param multipath_time A numeric maximum amount of time which must pass
#' between detections for a detection to be considered a "true", not a bounced,
#' signal.
#' @param org_ping_rate The expected time between transmissions emitted from
#' tags/transmitters implanted or attached to an organism
#' @param beacon_ping The expected time between transmissions emitted from
#' tags/transmitters used as beacon or reference tags to check receiver
#' functionality.
#' @returns A standardized detection dataframe with multipath detects removed
#' @import dplyr
#' @export
#' @examples
#' # Run the prefilter on a set of raw detection data
#'
#' #format the detection data
#' detects_formatted <- format_detects(data = raw_detections,
#'                                     var_Id = "tag_id",
#'                                     var_datetime_local = "local_time",
#'                                     var_receiver_serial = "serial",
#'                                     local_time_zone = "America/Los_Angeles",
#'                                     time_format = "%Y-%m-%d %H:%M:%S")
#'
#' #apply the prefilter
#' prefilter(data = detects_formatted,
#'           reference_tags = reftags,
#'           time_unit = "secs",
#'           multipath_time = 0.3,
#'           org_ping_rate = 3,
#'           beacon_ping = 30)
prefilter <- function(data, reference_tags, time_unit, multipath_time,
                      org_ping_rate, beacon_ping){
  temp <- data
  temp <- dplyr::arrange(.data = temp,Tag_Code)
  temp <- dplyr::group_by(.data = temp, Tag_Code)
  temp <- dplyr::arrange(.data = temp, Tag_Code, DateTime_Local)
  det_count <- dplyr::summarise(temp, det_count = dplyr::n())
  temp <- dplyr::left_join(temp, det_count, by = "Tag_Code")
  temp <- temp[temp$det_count > 1,]
  temp$time_diff_lag = difftime(temp$DateTime_Local, dplyr::lag(temp$DateTime_Local),
                                units = time_unit)
  temp$multipath = ifelse(temp$time_diff_lag >
                            lubridate::period(num = multipath_time,
                                               units = dplyr::case_when(
                                                 time_unit == "secs" ~ "seconds",
                                                 time_unit == "mins" ~ "minutes",
                                                 time_unit == "hours" ~ "hours",
                                                 time_unit == "days" ~ "days",
                                                 time_unit == "weeks" ~ "weeks"))|
                            temp$Tag_Code != dplyr::lag(temp$Tag_Code), FALSE, TRUE)
  temp$multipath = ifelse(is.na(temp$time_diff_lag) &
                            dplyr::lead(temp$multipath) == FALSE,
                          FALSE,
                          ifelse(is.na(temp$time_diff_lag),
                                 TRUE,
                                 temp$multipath))
  temp <- temp[temp$multipath == FALSE,] # filter out Multipath
  temp$time_diff_lag = difftime(temp$DateTime_Local, dplyr::lag(temp$DateTime_Local),
                                units = time_unit)
  temp$time_diff_lag = ifelse(temp$Tag_Code != dplyr::lag(temp$Tag_Code),NA,temp$time_diff_lag)
  temp$RefTag = ifelse(temp$Tag_Code %in% reference_tags, TRUE, FALSE) #Is it a ref tag?
  temp$CheckMBP = ifelse(temp$RefTag == TRUE, # If a ref tag,
                         (temp$time_diff_lag <
                            lubridate::period(num = (3*beacon_ping), # 2 hits in 3*Max PRI
                                              units = dplyr::case_when(
                                                time_unit == "secs" ~ "seconds",
                                                time_unit == "mins" ~ "minutes",
                                                time_unit == "hours" ~ "hours",
                                                time_unit == "days" ~ "days",
                                                time_unit == "weeks" ~ "weeks"))),
                         (temp$time_diff_lag <
                            lubridate::period(num = (12*org_ping_rate),
                                              units = dplyr::case_when(
                                                time_unit == "secs" ~ "seconds",
                                                time_unit == "mins" ~ "minutes",
                                                time_unit == "hours" ~ "hours",
                                                time_unit == "days" ~ "days",
                                                time_unit == "weeks" ~ "weeks")))) # 2 hits in 12*Max PRI
  temp$CheckMBP = ifelse(is.na(temp$time_diff_lag) &
                           dplyr::lead(temp$CheckMBP) == FALSE, #First Detection is FALSE if 2nd is FALSE
                         FALSE,
                         ifelse(is.na(temp$time_diff_lag),
                                TRUE,
                                temp$CheckMBP)) #Otherwise it's valid
  temp$CheckMBP = ifelse(temp$CheckMBP == FALSE &
                           !is.na(dplyr::lead(temp$time_diff_lag)) &
                           dplyr::lead(temp$time_diff_lag) <
                           # If invalid based on last detection,
                           # but following detection is <12x ping_rate
                           lubridate::period(num = (12*org_ping_rate),
                                             units = dplyr::case_when(
                                               time_unit == "secs" ~ "seconds",
                                               time_unit == "mins" ~ "minutes",
                                               time_unit == "hours" ~ "hours",
                                               time_unit == "days" ~ "days",
                                               time_unit == "weeks" ~ "weeks")),
                         TRUE, #Valid
                         temp$CheckMBP) #Return Previous Assignment
  temp <- temp[temp$CheckMBP == TRUE,]
  temp <- dplyr::select(.data =  temp,
                        -c(`det_count`))
  det_count <- dplyr::summarise(temp, det_count = dplyr::n())
  temp <- dplyr::left_join(temp, `det_count`, by = "Tag_Code")
  temp <- temp[temp$det_count > 1,]
  temp <- dplyr::ungroup(temp)
  temp <- dplyr::select(.data =  temp,
                        -c(time_diff_lag,multipath,RefTag,`det_count`))
  temp <- dplyr::arrange(temp, ReceiverSN, Tag_Code, DateTime_Local)
  temp <- temp[!is.na(temp$DateTime_Local),]
  prefilter_file <- temp
  prefilter_file
}
