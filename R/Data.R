#' Unfiltered detection data
#'
#' An example dataset of real acoustic telemetry detections of fish at several
#' receivers within the California Central Valley from 2021. These detections
#' have not been processed to remove false positives.
#'
#' @format ## `raw_detections`
#' A data frame with 55,736 rows and 3 columns:
#' \describe{
#'   \item{serial}{The serial number of the detecting receiver}
#'   \item{local_time}{the local time of the detection (tz = America/Los_Angeles)}
#'   \item{tag_id}{The hexadecimal acoustic tag ID code}
#' }
#' @source Data collected by the California Department of Water Resources 2021
"raw_detections"

#' Prefiltered detection data
#'
#' An example dataset of real acoustic telemetry detections of fish at several
#' receivers within the California Central Valley from 2021. These detections
#' have already been been processed using `prefilter()` from this package or
#' companion package `filteRjsats`.
#'
#' @format ## `dat_filt1`
#' A data frame with 47,931 rows and 4 columns:
#' \describe{
#'   \item{ReceiverSN}{The serial number of the detecting receiver}
#'   \item{DateTime_Local}{the local time of the detection (tz = America/Los_Angeles)}
#'   \item{Tag_Code}{The hexadecimal acoustic tag ID code}
#'   \item{CheckMBP}{A calculated field from the prefilter checking the
#'    time between acoustic transmissions from the same tag was >0.3secs}
#' }
#' @source Data collected by the California Department of Water Resources 2021
"dat_filt1"

#' Filtered detection data with Organism Data
#'
#' An example dataset of real acoustic telemetry detections of fish at several
#' receivers within the California Central Valley from 2021. These detections
#' have already been been processed using `prefilter()` and `add_org()`.
#'
#' @format ## `dat_orgfilt`
#' A data frame with 47,343 rows and 16 columns:
#' \describe{
#'   \item{ReceiverSN}{The serial number of the detecting receiver}
#'   \item{DateTime_Local}{the local time of the detection (tz = America/Los_Angeles)}
#'   \item{Tag_Code}{The hexadecimal acoustic tag ID code}
#'   \item{CheckMBP}{A calculated field from the prefilter checking the
#'    time between acoustic transmissions from the same tag was >0.3secs}
#'   \item{TagInFile}{A calculated field from the add_fish filter which
#'    queries whether the tag code of the detection is associated with an
#'    organism.}
#'   \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'   \item{org_release_Date}{The release date and time of the fish}
#'   \item{release_location}{The coded name of the release site}
#'   \item{length}{The length of the fish in millimeters}
#'   \item{weight}{The weight of the fish in grams}
#'   \item{tag_weight}{The weight of the implanted acoustic tag}
#'   \item{tag_model}{The model number of the implanted acoustic tag}
#'   \item{tag_pulse_rate_interval_nominal}{The pulse rate interval (time
#'    between transmissions) of the implanted tag, as reported by the
#'    manufacturer}
#'   \item{tag_life}{The expected number of days the tag should continue to
#'    transmit, as reported by the manufacturer}
#'   \item{CheckDT}{A calculated field which checks whether the detection
#'    occurred after the release of the fish}
#'   \item{CheckBattLife}{A calculated field which checks whether the detection
#'    occurred before the tag battery is expected to expire (2x tag life)}
#' }
#' @source Data collected by the California Department of Water Resources 2021
"dat_orgfilt"

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

#' Example reference tags
#'
#' A vector of example reference tag codes
#'
#' @format A vector of example reference tag codes
"reftags"

#' Example Completely Filtered Detection Data
#'
#' An example dataset of real acoustic telemetry detections of fish at several
#' receivers within the California Central Valley from 2021. These detections
#' have already been been processed using `prefilter()` and `add_org()`.
#'
#' @format ## `filtered_detections`
#' A data frame with 41,000 rows and 26 columns:
#' \describe{
#'   \item{ReceiverSN}{The serial number of the detecting receiver}
#'   \item{DateTime_Local}{the local time of the detection (tz = America/Los_Angeles)}
#'   \item{Tag_Code}{The hexadecimal acoustic tag ID code}
#'   \item{CheckMBP}{A calculated field from the prefilter checking the
#'    time between acoustic transmissions from the same tag was >0.3secs}
#'   \item{TagInFile}{A calculated field from the add_fish filter which
#'    queries whether the tag code of the detection is associated with an
#'    organism.}
#'   \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'   \item{org_release_Date}{The release date and time of the fish}
#'   \item{release_location}{The coded name of the release site}
#'   \item{length}{The length of the fish in millimeters}
#'   \item{weight}{The weight of the fish in grams}
#'   \item{tag_weight}{The weight of the implanted acoustic tag}
#'   \item{tag_model}{The model number of the implanted acoustic tag}
#'   \item{tag_pulse_rate_interval_nominal}{The pulse rate interval (time
#'    between transmissions) of the implanted tag, as reported by the
#'    manufacturer}
#'   \item{tag_life}{The expected number of days the tag should continue to
#'    transmit, as reported by the manufacturer}
#'   \item{CheckDT}{A calculated field which checks whether the detection
#'    occurred after the release of the fish}
#'   \item{CheckBattLife}{A calculated field which checks whether the detection
#'    occurred before the tag battery is expected to expire (2x tag life)}
#'   \item{dep_id}{A unique id is created for each receiver deployment}
#'   \item{Make}{The brand of the acoustic receiver}
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
#' @source Data collected by the California Department of Water Resources 2021
"filtered_detections"

#' Example Multi-blanked Detection Data
#'
#' An example dataset of real acoustic telemetry detections of fish at several
#' receivers within the California Central Valley from 2021. These detections
#' have already been been processed using `blanking_event()` to create events
#' using maximum blanking periods from 3 to 1,500 seconds to reprocess the data.
#' Each row represents a single event which includes >=1 detection(s) per fish
#' per site which occur within the specified maximum blanking period `mbp_n`.
#'
#' @format ## `blanked_detects`
#' A data frame with 44,630 rows and 9 columns:
#' \describe{
#'  \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'  \item{Tag_Code}{The hexadecimal acoustic tag ID code}
#'  \item{mbp_n}{the maximum blanking period used to create this event}
#'  \item{event_change}{An increasing number which identifies the event number;
#'   one event per fish per site for all detections which occur within `mbp_n`
#'   seconds of the next.}
#'  \item{receiver_general_location}{The more general geographic name of the
#'   location of the receiver}
#'  \item{start_time}{The Date and Time of the first detection within the event}
#'  \item{end_time}{The Date and Time of the last detection within the event}
#'  \item{n_det}{The total number of detections contained within the event}
#'  \item{duration}{the total length of time of the event in seconds}
#' }
"blanked_detects"

#' Example Time Tests for Multi-blanked Detection Data
#'
#' Example output from the `duration_compare()` function, testing the duration
#' of detection events found in `blanked_detects`.
#'
#' @format ## `time_test`
#' A data frame with 333,400 rows and 4 columns:
#' \describe{
#'  \item{t}{The time (in seconds) against which the duration was compared}
#'  \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'  \item{mbp_n}{The maximum blanking period (in seconds) used to create a
#'   set of events}
#'  \item{prop_res}{The proportion of all events created with `mbp_n` which have
#'   a duration longer than time `t`.}
#' }
"time_test"

#' Example Renormalized Sum of Squares
#'
#' Example output from the `renorm_SSR()` function, calculating the
#' renormalized sum of squares for the "survival" data found in `time_test`.
#'
#' @format ## `ex_rSSR`
#' A data frame with 100 rows and 5 columns:
#' \describe{
#'  \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'  \item{mbp_n}{The maximum blanking period (in seconds) used to create a
#'   set of events}
#'  \item{SSR}{The sum of squared residuals between this `mbp_n` and the next}
#'  \item{n}{the total number of events created with this `mbp_n`}
#'  \item{rSSR}{the renormalized sum of squared residuals between this `mbp_n`
#'   and the next}
#' }
"ex_rSSR"

#' Example 95 Percent Convergence Threshold
#'
#' Example output from the `conv_thresholds()` function, calculating the 95%
#' convergence thresholds for the rSSR data found in `ex_rSSR`.
#'
#' @format ## `conv_thresh`
#' A data frame with 1 rows and 5 columns:
#' \describe{
#'  \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'  \item{min_val}{The minimum rSSR value}
#'  \item{max_val}{The maximum rSSR value}
#'  \item{threshold}{the rSSR value which represents the `thresh_level` cutoff
#'   for estimating convergence}
#'  \item{thresh_level}{The desired convergence level (100-x)}
#' }
"conv_thresh"

#' Example Optimum Maximum Blanking Period
#'
#' Example output from the `opt_mbp()` function, finding the optimal mbp for
#' each group and desired convergence threshold.
#'
#' @format ## `ex_opt`
#' A data frame with 1 rows and 5 columns:
#' \describe{
#'  \item{fish_type}{Generally a strain, run, and species of fish (e.g.
#'   Nimbus Fall Chinook = Fall-run Chinook Salmon from Nimbus Hatchery)}
#'  \item{min_val}{The minimum rSSR value}
#'  \item{max_val}{The maximum rSSR value}
#'  \item{threshold}{the rSSR value which represents the `thresh_level` cutoff
#'   for estimating convergence}
#'  \item{thresh_level}{The desired convergence level (100-x)}
#'  \item{opt_mbp}{The identified optimum mbp for the given threshold and group}
#' }
"ex_opt"
