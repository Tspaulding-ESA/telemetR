#' Previously filtered detection data
#'
#' An example dataset of real acoustic telemetry detections of fish at several
#' receivers within the California Central Valley from 2021. These detections
#' have already been been processed to remove false positives using the various
#' filtering functions in this package and in companion package `filteRjsats`.
#'
#' @format ## `filtered_detections`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{serial}{The serial number of the detecting receiver}
#'   \item{local_time}{the local time of the detection (tz = America/Los_Angeles)}
#'   \item{tag_id}{The hexadecimal acoustic tag ID code}
#' }
#' @source Data collected by the California Department of Water Resources 2021
"filtered_detections"

#' Receiver Data
#'
#' An example dataset of real acoustic telemetry receivers within the California
#' Central Valley in 2021. These receivers are only those which match the
#' serial numbers in companion dataset `filtered_detections`. This data is
#' formatted to match the California Fish Tracking receiver metadata found here:
#' https://oceanview.pfeg.noaa.gov/CalFishTrack/.
#'
#' @format ## `receivers`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{dep_id}{A unique id is created for each receiver deployment}
#'   \item{receiver_make}{The brand of the acoustic receiver}
#'   \item{receiver_serial_number}{The serial number of the acoustic receiver}
#'   \item{latitude}{The decimal degree latitude (WGS1984) of the acoustic
#'   receiver at deployment}
#'   \item{longitude}{The decimal degree longitude (WGS1984) of the acoustic
#'   receiver at deployment}
#'   \item{receiver_location}{The site name of an individual receiver, often
#'   more than one `receiver_location` is found at a `receiver_general_location`}
#'   \item{receiver_general_location}{The more general geographic name of the
#'   location of the receiver}
#'   \item{receiver_river_km}{The number of river kilometers the receiver is
#'   from the Golden Gate Bridge}
#'   \item{receiver_start}{The start time of the reciever (generally when it
#'   was deployed)}
#'   \item{receiver_end}{The end time of the receiver (generally when it was
#'   retrieved)}
#' }
#' @source <https://oceanview.pfeg.noaa.gov/CalFishTrack/pageRealtime_download.html>
"receivers"

#' Fish Data
#'
#' An example dataset of real fish tagged with acoustic telemetry tags and
#' released within the California Central Valley in 2021 and 2022.
#'
#' @format ## `fish`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'   \item{TagCode}{The hexadecimal code of the implanted acoustic tag}
#'   \item{Release_Date}{The release date and time of the fish}
#'   \item{release_location}{The coded name of the release site}
#'   \item{length}{The length of the fish in millimeters}
#'   \item{weight}{The weight of the fish in grams}
#'   \item{tag_weight}{The weight of the implanted acoustic tag}
#'   \item{tag_model}{The model number of the implanted acoustic tag}
#'   \item{PRI}{The pulse rate interval (time between transmissions) of the
#'   implanted tag, as reported by the manufacturer}
#'   \item{TagLife}{The expected number of days the tag should continue to
#'   transmit, as reported by the manufacturer}
#' }
#' @source <https://oceanview.pfeg.noaa.gov/CalFishTrack/pageRealtime_download.html>
"fish"
