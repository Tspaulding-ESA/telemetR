#' Format Organism Data for add_org()
#'
#' This function takes a dataframe of org and tag data and renames the
#' columns to those expected by the add_org() function
#'
#' @param data a dataframe of org and tag data
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_release the column name, in quotes, which identifies the release
#' date and time in POSIX format and appropriate timezone
#' @param var_tag_life the column name, in quotes, which identified the expected
#' tag life in days
#' @param var_ping_rate the column name, in quotes which identifies the expected
#' ping rate of the tag/transmitter
#' @param local_time_zone the local timezone used for analyses. Uses tz database
#' names (e.g. "America/Los_Angeles" for Pacific Time)
#' @param time_format a string value indicating the datetime format of all time
#' fields
#' @return A dataframe which contains fields renamed to match those required by
#' add_org() function
#' @import dplyr
#' @export
format_org <- function(data, var_Id, var_release, var_tag_life, var_ping_rate,
                        local_time_zone, time_format){
  df <- data
  df <- data |>
    dplyr::rename("Tag_Code" = var_Id,
           "org_release_date" = var_release,
           "tag_life" = var_tag_life,
           "tag_pulse_rate_interval_nominal" = var_ping_rate)

  df <- df |>
    dplyr::mutate(Tag_Code = as.character(Tag_Code),
           org_release_date =
             lubridate::parse_date_time(as.character(org_release_date),
                                        tz = local_time_zone,
                                        orders = c(time_format)),
           tag_life = as.numeric(tag_life),
           tag_pulse_rate_interval_nominal =
             as.numeric(tag_pulse_rate_interval_nominal))
  return(df)
}

