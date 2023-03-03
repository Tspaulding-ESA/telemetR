#' Setup a Detection Dataframe for Identifying the Optimal Blanking Period
#'
#' Takes a dataframe with telemetry detection data and a list of potential
#' Blanking Period multipliers (n_val) and crosses them, duplicating the entire
#' dataframe by the length of n_val. This function is contained in blanking
#' event.This function can be slow depending on the size of the dataframe.
#'
#' @param data the detection dataframe with columns for sites, tag IDs,
#' datetime, any grouping variables, and the expected ping rate.
#' @param var_site the column name, in quotes, which identifies unique residency
#' sites, these sites should be as distinct as possible, such that it is
#' infrequent that organisms can be detected at two sites at a given time.
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_datetime the column name, in quotes, which identifies the date and
#' time of the detection event. This column should already have been converted
#' to POSIXct format.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group animals. Common groupings are species and cohorts.
#' @param var_ping_rate the column name, in quotes, which identifies the
#' temporal frequency at which the transmitter emits a detectable signal.
#' @param n_val a vector sequence of integers which can be multiplied by the
#' ping rate to construct multiple potential blanking periods. The range and
#' step values for n should be selected based on prior knowledge about general
#' behavior habits of the study organism and the functionality of the equipment.
#' For more information, please refer to Capello et. al. 2015.
#' @return A dataframe which has been crossed with all integers in n_val
#' @export
setup_blanking <- function(data, var_site,var_Id,var_datetime,var_groups = NULL,
                           var_ping_rate, n_val){
  df <- data[,c(var_groups,var_site,var_Id,var_datetime, var_ping_rate)]
  cross <- tidyr::crossing(df, n_val)
  cross <- dplyr::arrange(cross, dplyr::across(dplyr::all_of(c(var_groups, var_Id,
                                                               "n_val",
                                                               var_datetime))))
  return(cross)
}

#' Create Potential Blanking Periods for Identifying Optimal Blanking Period
#'
#' Takes a dataframe with telemetry detection data and a list of potential
#' Blanking Period multipliers (n_val) and crosses them, duplicating the entire
#' dataframe by the length of n_val. Detections are grouped by individual,
#' site, and any supplied grouping variables. Then events are created by
#' collecting detections which occur within n_val*ping_rate from the next
#' detection. This function can be very slow depending on the size of the
#' dataframe.
#'
#' @param data the detection dataframe with columns for sites, tag IDs,
#' datetime, any grouping variables, and the expected ping rate.
#' @param var_site the column name, in quotes, which identifies unique residency
#' sites, these sites should be as distinct as possible, such that it is
#' infrequent that organisms can be detected at two sites at a given time.
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_datetime the column name, in quotes, which identifies the date and
#' time of the detection event. This column should already have been converted
#' to POSIXct format.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group animals. Common groupings are species and cohorts.
#' @param var_ping_rate the column name, in quotes, which identifies the
#' temporal frequency at which the transmitter emits a detectable signal.
#' @param n_val a vector sequence of integers which can be multiplied by the
#' ping rate to construct multiple potential blanking periods. The range and
#' step values for n should be selected based on prior knowledge about general
#' behavior habits of the study organism and the functionality of the equipment.
#' For more information, please refer to Capello et. al. 2015.
#' @param time_unit the preferred unit of time to calculate durations, this
#' should correspond to the ping_rate, (i.e. if the ping rate is 3 seconds, the
#' preferred time_unit is seconds). If the preferred time_unit is on the same
#' scale as the ping_rate, the ping rate should be converted to the same scale.
#' @return A dataframe which has been crossed with all integers in n_val, and
#' which has been condensed into events. Please refer to Capello et. al. 2015
#' for further detail about the creation of these events.
#' @export
blanking_event <- function(data,var_site,var_Id,var_datetime,var_groups = NULL,
                           var_ping_rate, n_val, time_unit){
  setup <- setup_blank(data, var_site,var_Id,var_datetime,var_groups,
                       n_val)

  event <- setup %>%
    ungroup() %>%
    group_by(across(all_of(c(var_Id, "n_val")))) %>%
    mutate(ping_rate = all_of(var_ping_rate),
           across(var_site, .fns = ~.x == lag(.x), .names = "same_loc"),
           across(c(var_datetime), .fns = ~ difftime(.x,
                                                     lag(.x),
                                                     units = time_unit) <= (n_val*ping_rate),
                  .names = "same_time"),
           event_change_binary = !(same_loc & same_time),
           event_change_binary = ifelse(is.na(event_change_binary),
                                           FALSE,
                                           event_change_binary),
           event_change = cumsum(event_change_binary))

  event_dur <- event %>%
    group_by(across(all_of(c(var_groups, var_Id, "n_val", "ping_rate",
                           "event_change", var_site)))) %>%
    summarise(across(all_of(var_datetime),
                     .fns = list(start_time = ~ min(.x, na.rm = TRUE),
                                 end_time = ~ max(.x, na.rm = TRUE),
                                 n_det = ~ n()),
                     .names = "{.fn}"
                     )) %>%
    mutate(duration = as.numeric(difftime(end_time,
                                          start_time,
                                          units = "sec"))
    ) %>%
    ungroup()

  return(event_dur)
}

#' Compare the duration of Potential Blanking Periods
#'
#' Takes a dataframe of detection data which has been condensed by potential
#' blanking periods and compares the duration of each event to a common sequence
#' of increasing times. If the event is longer than the duration it is
#' flagged as "survived". The proportion of events which survive for each
#' potential blanking period at each time (t) is then calculated.
#'
#' @param event_dur the detection dataframe which has been condensed into
#' discrete events using each potential blanking period.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group organisms. Common groupings are species and cohorts.
#' @param time_seq a vector of times on the same scale as the ping rate. The
#' largest value of the sequence should be greater that the longest duration
#' produced using blanking event, and the smallest should be shorter than the
#' smallest blanking period.
#' @return A dataframe which contains the proportion of "survived" events
#' created by each potential blanking period for each time (t).
#' @export
duration_compare <- function(event_dur, var_groups=NULL, time_seq){
  time_list <- list()

  for (t in time_seq){
    cat("\rt =", t)
    tmp <- event_dur
    tmp$resident <- tmp$duration >= t
    time_list[[as.character(t)]] <- tmp %>%
      group_by(across(all_of(c(var_groups, "n_val", "ping_rate")))) %>%
      summarise(prop_res = sum(resident)/n())
  }
  time_df = bind_rows(time_list, .id = "t") %>%
    mutate(t = as.numeric(t),
           mbp_n = ping_rate * n_val)
  return(time_df)
}

#' Residency Survival Plot
#'
#' Takes a dataframe of the proportion of events created by each potential
#' blanking period which "survived" a certain time (t) and creates a plot.
#' Used to visually look for convergences between survival lines.
#'
#' @param time_df a dataframe created by duration compare showing the proportion
#'  of events created by each potential blanking period which "survived" a
#'  certain time (t)
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group organisms. Common groupings are species and cohorts.
#' @param time_seq a vector of times on the same scale as the ping rate. The
#' largest value of the sequence should be greater that the longest duration
#' produced using blanking event, and the smallest should be shorter than the
#' smallest blanking period.
#' @param time_unit the unit of time used to calculate durations
#' @return A plot of the proportion of events created by each potential
#' blanking period at each time (t).
#' @export
residence_plot <- function(time_df, var_groups = NULL, time_unit){
  max_t <- time_df %>%
    group_by(t) %>%
    summarise(mean = mean(prop_res)) %>%
    filter(mean != 0) %>%
    ungroup %>%
    pull(t) %>%
    max()

  library(ggplot2)

  if(is.null(var_groups)){
  ggplot(time_df %>% filter(t < max_t), aes(x = t, y = prop_res, col = as.factor(mbp_n))) +
    geom_line() +
    scale_y_log10() +
    labs(x = paste("Time (",time_unit,")"), y = "Proportion of Event Lengths > Time")+
    coord_cartesian(ylim= c(0.0001, 1))+
    theme_classic()+
    theme(legend.position = "none")
  }else{
    ggplot(time_df %>% filter(t < max_t), aes(x = t, y = prop_res, col = as.factor(mbp_n))) +
      geom_line() +
      scale_y_log10() +
      labs(x = paste("Time (",time_unit,")"), y = "Proportion of Event Lengths > Time")+
      coord_cartesian(ylim= c(0.0001, 1))+
      facet_grid(as.formula(paste("~", var_groups)))+
      theme_classic()+
      theme(legend.position = "none")
  }
}

#' Calculate the Renormalized Sum of Squared Residuals
#'
#' Takes a dataframe of the proportion of events created by each potential
#' blanking period which "survived" a certain time (t) and calculates the sum
#' of squares of the residuals between one potential blanking period and the
#' next. This result is then renormalized by dividing the result by the number
#' of events created.
#'
#' @param time_df a dataframe created by duration compare showing the proportion
#'  of events created by each potential blanking period which "survived" a
#'  certain time (t)
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group organisms. Common groupings are species and cohorts.
#' @return A dataframe of the renormalized sum of squared residuals between each
#' potential blanking period and the subsequent one.
#' @export
renorm_SSR <- function(time_df, var_groups=NULL){
  SR <- time_df %>%
    ungroup() %>%
    group_by(across(all_of(var_groups))) %>%
    mutate(SR = ifelse(!is.na(lead(t)),
                       ifelse(lead(t) == t,
                              (lead(prop_res) - prop_res)^2,
                              0
                       ),
                       0
    ))

  rSSR <- SR %>%
    group_by(across(all_of(c(var_groups,"mbp_n")))) %>%
    summarise(SSR = sum(as.numeric(SR)), n = n()) %>%
    mutate(rSSR = SSR / n)

  return(rSSR)
}

#' Calculate Convergence Thresholds for the rSSR curve
#'
#' Takes a dataframe created by renorm_SSR and calculates the range in values
#' and then calculates thresholds given. Suggested values are
#' 0.5, 0.1, and 0.005. The rSSR calculated for each MBP should decrease
#' with each increasing blanking period until they reach close to zero, which
#' We consider convergence. Since the rSSR curve generally bounces around an
#' assymptote and often does not reach or stay at 0, we set a threshold a priori
#'  for identifying convergence.
#'
#' @param rSSR_df a dataframe created by created by renorm_SSRduration compare
#' showing the renormalized sum of squares of the residuals between one
#' potential blanking period and the next.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group organisms. Common groupings are species and cohorts.
#' @param thresh_levels a single value or vector of values used to set
#' thresholds for identifying convergence.
#' @return A dataframe of rSSR values corresponding to the given convergence
#' threshold
#' @export
conv_thresholds <- function(rSSR_df, var_groups, thresh_levels = c(0.05,0.01,0.005)){
  thresh_list <- list()

  for(s in 1:length(thresh_levels)){
    thresh_list[[s]] <- rSSR_df %>%
      group_by(across(all_of(var_groups)))%>%
      summarize(min_val = min(rSSR), max_val = max(rSSR)) %>%
      mutate(threshold = ((max_val - min_val) * thresh_levels[s]) + min_val,
             thresh_level = thresh_levels[s])
    }

  thresh_list <- bind_rows(thresh_list)

  return(thresh_list)
}

#' Idenitfy the Optimum MBP based on Convergence Threshold
#'
#' Takes dataframes created by renorm_SSR and conv_thresholds and determines the
#' corresponding "optimum" maximum blanking period (MBP) for each convergence
#' threshold.
#'
#' @param rSSR_df a dataframe created by created by renorm_SSRduration compare
#' showing the renormalized sum of squares of the residuals between one
#' potential blanking period and the next.
#' @param thresh_values a dataframe created by conv_thresholds corresponding to
#' the chosen convergence thresholds.
#' @return A dataframe showing the convergence value and corresponding maximum
#' blanking period.
#' @export
opt_mbp <- function(rSSR_df, thresh_values){
  thresh_values$opt_mbp <- NA
  for(s in thresh_values$threshold){
    thresh_values$opt_mbp[thresh_values$threshold == s] <- rSSR$mbp_n[max(which(rSSR$rSSR > s))+1]
  }

  return(thresh_values)
}

#' Plot the rSSR Curve and Convergence Thresholds and Optimum MBP
#'
#' Using the dataframes produced by renorm_SSR and opt_mbp, plots the rSSR
#' curve, and all the convergence thresholds (horizontal lines) and
#' corresponding optimum mbps (vertical lines).
#'
#' @param rSSR_df a dataframe created by created by renorm_SSRduration compare
#' showing the renormalized sum of squares of the residuals between one
#' potential blanking period and the next.
#' @param opt_mbp_df a dataframe created by opt_mbp showing the values for the
#' convergence thresholds and optimum mbps
#' @return A plot of the rSSR curve, convergence thresholds, and optimum mbps
#' @export
rSSR_plot <- function(rSSR, opt_mbp_df){

  rSSR_plot <- ggplot(rSSR) +
    geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1), color = "blue") +
    geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1), size = .5, color = "red") +
    geom_hline(data = opt_mbp_df, aes(yintercept = threshold, color = factor(sig_level)), linetype = "dashed") +
    geom_vline(data = opt_mbp_df, aes(xintercept = opt_mbp/60, color = factor(sig_level)), linetype = "dashed")+
    geom_line(aes(x = mbp_n / 60, y = zoo::rollmean(rSSR,7, na.pad = TRUE), group = 1), color = "grey50") +
    scale_color_discrete(name = "Significance Level")+
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = "MBP (minutes)", y = "renormalized Sum of Squared Residuals") +
    facet_grid(as.formula(paste("~", var_groups)))+
    theme_classic()

  Log_rSSR_plot <- ggplot(rSSR) +
    geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1), color = "blue") +
    geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1), size = .5, color = "red") +
    geom_hline(data = opt_mbp_df, aes(yintercept = threshold, color = factor(sig_level)), linetype = "dashed") +
    geom_vline(data = opt_mbp_df, aes(xintercept = opt_mbp/60, color = factor(sig_level)), linetype = "dashed")+
    geom_smooth(aes(x = mbp_n / 60, y = rSSR, group = 1),
                method = "loess",
                span = 0.7,
                color = "grey50",
                se = FALSE)+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_log10() +
    labs(x = "MBP (minutes)", y = "renormalized Sum of Squared Residuals") +
    facet_grid(as.formula(paste("~", var_groups)))+
    theme_classic()

  vp <- grid::viewport(width = unit(4,"inches"), height = unit(3,"inches"),
                       x = unit(4.5,"inches"), y = unit(2.75, "inches"))

  print(rSSR_plot)
  print(Log_rSSR_plot, vp = vp)
}

#' Build Continuous Residence Events
#'
#' Takes a dataframe with telemetry detection data and a single optimum blanking
#' period chosen from the output of `opt_mbp()`, and groups detections by
#' individual, site, and any supplied grouping variables into residence events.
#' The residence events are created by collecting detections which occur within
#' the selected optimum maximum blanking period from the next detection. This
#' function can be very slow depending on the size of the dataframe.
#'
#' @param data the detection dataframe with columns for sites, tag IDs,
#' datetime, any grouping variables, and the expected ping rate.
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group animals. Common groupings are species and cohorts.
#' @param var_Id the column name, in quotes, which identifies the individual
#' transmitter/tag/organism identifier.
#' @param var_datetime the column name, in quotes, which identifies the date and
#' time of the detection event. This column should already have been converted
#' to POSIXct format.
#' @param var_site the column name, in quotes, which identifies unique residency
#' sites, these sites should be as distinct as possible, such that it is
#' infrequent that organisms can be detected at two sites at a given time.
#' @param time_unit the unit of time used by the optimum maximum blanking
#' period, often on the same scale as the ping rate for the transmitter.
#' @param opt_mbp a single optimum blanking period chosen from the output of
#' `opt_mbp()`
#' @return A dataframe of detections which has been condensed into continuous
#' residence events based on the optimum maximum blanking period selected.
#' @export
build_residence <- function(data, var_groups, var_Id, var_datetime, var_site,
                            opt_mbp, time_unit){
  residence <- data %>%
    filter_at(all_of(c(var_groups, var_site, var_Id, var_datetime)),
              all_vars(!is.na(.))) %>%
    group_by(across(all_of(c(var_groups,var_Id)))) %>%
    arrange(across(all_of(c(var_groups, var_Id, var_datetime)))) %>%
    mutate(across(var_site,
                  .fns = ~.x == lag(.x),
                  .names = "same_loc"),
           across(var_datetime,
                  .fns = ~ difftime(.x,lag(.x), units = time_unit) <= opt_mbp,
                  .names = "same_time"),
           same_group = if_all(any_of(c(var_groups)),
                               .fns = ~.x == lag(.x)),
           across(.cols = same_loc:same_group,
                  .fns = ~ifelse(is.na(.x),TRUE,.x)),
           event_change_binary = if_any(.cols = same_loc:same_group,
                                        .fns = ~ifelse(.x == FALSE, TRUE, FALSE)),
           event_change_binary = ifelse(is.na(event_change_binary),
                                        FALSE,
                                        event_change_binary),
           event_number = cumsum(event_change_binary)) %>%
    ungroup() %>%
    group_by(across(all_of(c(var_groups, var_site,
                             "event_number", var_Id)))) %>%
    summarise(across(all_of(var_datetime),
                     .fns = list(start_time = ~ min(.x, na.rm = TRUE),
                                 end_time = ~ max(.x, na.rm = TRUE),
                                 n_det = ~ n()),
                     .names = "{.fn}")) %>%
    ungroup() %>%
    select(all_of(c(var_groups, var_Id, "event_number", var_site,
                    "start_time", "end_time", "n_det"))) %>%
    mutate(duration = difftime(end_time,start_time,units = time_unit)) %>%
    arrange(across(all_of(c(var_groups, var_Id, "start_time")))) %>%
    distinct()

  return(residence)
}
