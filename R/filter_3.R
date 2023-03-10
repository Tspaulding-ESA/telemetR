#' Basic Four Hit Filter for Detections
#'
#' This function takes a detection dataframe generated from the add_org()
#' function and filters it a second time to remove any remaining multipath
#' detections, and then check the remaining detections by comparing the time
#' between detections, for a rolling window of 4 detections to ensure it is less
#' 16.6x the stated pulse rate interval. Called by second_filter().
#'
#' @param org_file a dataframe of detections retrieved from add_org()
#' @param time_unit The unit of time used for analyses (seconds, minutes, hours,
#' days, weeks, months)
#' @param multipath_time A numeric maximum amount of time which must pass
#' between detections for a detection to be considered a "true", not a bounced,
#' signal.
#' @param org_ping_rate The expected time between transmissions emitted from
#' tags/transmitters implanted or attached to an organism
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_lotek <- function(org_file, time_unit, multipath, org_ping_rate){
  filtered <- org_file
  filtered <- dplyr::group_by(.data = filtered, Tag_Code)
  filtered <- dplyr::arrange(.data = filtered, Tag_Code, DateTime_Local)
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), #compare the time difference between each hit
                         units = time_unit)
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Code")
  filtered$multipath = ifelse(filtered$time_diff_lag >
                            lubridate::period( x = multipath_time,
                                               units = time_unit)|
                            filtered$Tag_Code != dplyr::lag(filtered$Tag_Code), FALSE, TRUE)
  filtered$multipath <- ifelse(filtered$multipath == TRUE &
                                 dplyr::lead(filtered$multipath == FALSE),
                               FALSE,
                               filtered$multipath)
  filtered <- filtered[filtered$det_count > 3 & filtered$multipath == FALSE,] #first filter out any final multipath and any tags with <3 detects
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = time_unit)
  # Calculating for every 4 hits
  filtered$tdiff <- as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$tag_pulse_rate_interval_nominal))
  filtered$tdiff <- ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff <- round(filtered$tdiff*100)/100
  filtered$tdiff <- ifelse(is.na(filtered$tdiff),dplyr::lead(filtered$tdiff),filtered$tdiff)
  filtered$cs <- difftime(dplyr::lead(filtered$DateTime_Local,3),
                          filtered$DateTime_Local,
                          units = time_unit)
  filtered$pr <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- filtered[filtered$cs < filtered$pr*16.6 | is.na(filtered$cs),]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Code")
  filtered
}

#' Basic Two Hit Filter for Detections
#'
#' This function takes a detection dataframe generated from the
#' add_org() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing
#' the time between each detection to ensure it is less 4x the stated pulse rate
#' interval. Called by second_filter_2h4h().
#'
#'
#' @param org_file a dataframe of detections retrieved from add_org()
#' @param time_unit The unit of time used for analyses (seconds, minutes, hours,
#' days, weeks, months)
#' @param multipath_time A numeric maximum amount of time which must pass
#' between detections for a detection to be considered a "true", not a bounced,
#' signal.
#' @param org_ping_rate The expected time between transmissions emitted from
#' tags/transmitters implanted or attached to an organism
#' @return A dataframe which has been filtered to remove false positives
#' @export
second_filter_tekno <- function(org_file, time_unit, multipath_time,
                                org_ping_rate){
  filtered <- org_file
  filtered$pr_nom <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- dplyr::group_by(.data = filtered, Tag_Code)
  filtered <- dplyr::group_by(.data = filtered, Tag_Code)
  filtered <- dplyr::arrange(.data = filtered, Tag_Code, DateTime_Local)
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), #compare the time difference between each hit
                         units = time_unit)
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Code")
  filtered$multipath <- ifelse(filtered$td > multipath_time | is.na(filtered$td), FALSE, TRUE) #identify any remaining multipath
  filtered$multipath <- ifelse(filtered$multipath == TRUE &
                                 dplyr::lead(filtered$multipath == FALSE),
                               FALSE,
                               filtered$multipath)
  filtered <- filtered[filtered$det_count > 3 & filtered$multipath == FALSE,] #first filter out any final multipath and any tags with <3 detects
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = time_unit)
  # Calculating for every two hits
  filtered$tdiff = as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$pr_nom))
  filtered$tdiff = ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff = round(filtered$tdiff*100)/100
  filtered$tdiff = ifelse(is.na(filtered$tdiff),
                          dplyr::lead(filtered$tdiff),
                          filtered$tdiff)
  filtered$pr_check = ifelse(filtered$td < filtered$pr_nom*4 |
                               is.na(filtered$td),
                             TRUE,
                             FALSE)
  filtered$pr_check = ifelse(filtered$pr_check == FALSE &
                               dplyr::lead(filtered$td) < filtered$pr_nom*4,
                             TRUE,
                             filtered$pr_check)
  filtered <- filtered[filtered$pr_check == TRUE,]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Code")

  filtered
}
