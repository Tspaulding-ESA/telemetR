% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_receiver.R
\name{format_receivers}
\alias{format_receivers}
\title{Format for Receiver data for filteRjsats}
\usage{
format_receivers(
  data,
  var_receiver_serial,
  var_receiver_make,
  var_receiver_deploy,
  var_receiver_retrieve,
  local_time_zone
)
}
\arguments{
\item{data}{the detection dataframe with columns for individual receivers,
tag IDs,datetime, and the expected ping rate.}

\item{var_receiver_serial}{the column name, in quotes, which identifies the
serial number of the detection receiver}

\item{var_receiver_make}{the column name, in quotes, which identifies the
make or brand of the detection receiver. Must be one of "ATS", "Lotek", or
"Tekno"}

\item{var_receiver_deploy}{the column name, in quotes, which identifies the
date and time the receiver was deployed}

\item{var_receiver_retrieve}{the column name, in quotes, which identifies the
date and time the receiver was retrieved}

\item{local_time_zone}{the local timezone used for analyses. Uses tz database
names (e.g. "America/Los_Angeles" for Pacific Time)}
}
\value{
A dataframe which contains fields renamed to match those required by
add_receivers() function
}
\description{
This function takes a dataframe of receiver metadata and reformats specific
columns so that they can be read by the filtering functions in filteRjsats
package
}
