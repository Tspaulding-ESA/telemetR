#' Add Organism Data to a Detection Dataframe
#'
#' This function takes a prefiltered detection dataframe from `prefilter()` and
#' joins it to organism data formatted using the `format_org()` function.
#' Detections are then filtered further based on the date and time of tag
#' release and expected battery life. Detections occurring before release of the
#' tag or after 2x the expected battery life are removed.
#'
#' @param prefilter_file a prefiltered detection dataframe from `prefilter()`
#' @param org a dataframe of organism data retrieved from `get_org_data()` or
#' `format_org()`
#' @param time_before_detection How long before detection could an organism be
#' released and still detected? Generally 2x the expected tag life.
#' @param time_unit The unit of time used for time_before_detection
#' (seconds, minutes, hours, days, weeks, months)
#' @return A filtered dataframe converting the raw detection data into rows
#' of detections
#' @import dplyr
#' @export
add_org <- function(prefilter_file, org, time_before_detection, time_unit){
  org_tmp <- org[org$org_release_date >
                   (min(prefilter_file$DateTime_Local)-
                      lubridate::period(time_before_detection,
                                        units = time_unit)),]
  prefilter_file$TagInFile = prefilter_file$Tag_Code %in% org_tmp$Tag_Code #Check for Study Tags
  message(paste0("Detections in File: ",length(prefilter_file$DateTime_Local)))
  file <- prefilter_file[prefilter_file$TagInFile == TRUE,]
  file <- dplyr::left_join(file,org_tmp, by= c("Tag_Code"))
  file$CheckDT = ifelse(file$DateTime_Local > file$org_release_date,
                        TRUE, #Detections after release
                        FALSE) #Detections before release
  file$CheckBattLife = ifelse(file$DateTime_Local <= file$org_release_date+lubridate::days(2*file$tag_life),
                              TRUE, #Detections before tag failure
                              FALSE) #Detections after tag failure
  file <- file[file$CheckDT == TRUE & file$CheckBattLife == TRUE,]
  org_file <- file

  if(length(file$DateTime_Local) > 1) {
    message(paste0("Number of Valid Tag IDs: ", length(unique(file$Tag_Code))))
    message(paste0("Number of Valid Detections: ", length(file$DateTime_Local)))
  } else {
    message("No Valid Detections")}

  org_file
}
#' @examples
#'
#' # Format the organism data
#' formatted_fish <- format_org(data = fish,
#'                              var_Id = "TagCode",
#'                              var_release = "Release_Date",
#'                              var_tag_life = "TagLife",
#'                              var_ping_rate = "PRI",
#'                              local_time_zone = "America/Los_Angeles",
#'                              time_format = "%Y-%m-%d %H:%M:%S")
#'
#' # Add organism data to the prefiltered detection data
#' add_org(prefilter_file = dat_filt1,
#'         org = formatted_fish,
#'         time_before_detection = 120,
#'         time_unit = "days")
