options(dplyr.summarise.inform = FALSE)

setup_blanking <- function(data, var_site,var_Id,var_datetime,var_groups = NULL,
                           n_val, ping_rate){
  df <- data[,c(var_groups,var_site,var_Id,var_datetime)]
  cross <- tidyr::crossing(df, n_val)
  cross <- dplyr::arrange(cross, dplyr::across(dplyr::all_of(c(var_groups, var_Id,
                                                               "n_val",
                                                               var_datetime))))
  cross$mbp = ping_rate
  return(cross)
}

blanking_event <- function(data,var_site,var_Id,var_datetime,var_groups = NULL,
                           n_val, ping_rate, time_unit){
  setup <- setup_blank(data, var_site,var_Id,var_datetime,var_groups,
                       n_val)

  event <- setup %>%
    ungroup() %>%
    group_by(across(all_of(c(var_Id, "n_val")))) %>%
    mutate(ping_rate = ping_rate,
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

duration_compare <- function(event_dur, var_groups, time_seq){
  time_list <- list()

  for (t in time_seq){
    cat("\rt =", t)
    tmp <- event_dur
    tmp$resident <- tmp$duration >= t
    time_list[[as.character(t)]] <- tmp %>%
      group_by(across(all_of(var_groups, "n_val", "ping_rate"))) %>%
      summarise(prop_res = sum(resident)/n())
  }
  time_df = bind_rows(time_list, .id = "t") %>%
    mutate(t = as.numeric(t),
           mbp_n = ping_rate * n_val)
  return(time_df)
}

residence_plot <- function(time_df, var_groups){
  max_t <- time_df %>%
    group_by(t) %>%
    summarise(mean = mean(prop_res)) %>%
    filter(mean != 0) %>%
    ungroup %>%
    pull(t) %>%
    max()

  library(ggplot2)
  ggplot(time_df %>% filter(t < max_t), aes(x = t/60, y = prop_res, col = as.factor(mbp_n/60))) +
    geom_line() +
    scale_y_log10() +
    labs(x = "Time (minutes)", y = "Proportion of Event Lengths > Time")+
    coord_cartesian(ylim= c(0.0001, 1))+
    facet_grid(as.formula(paste("~", var_groups)))+
    theme_classic()+
    theme(legend.position = "none")
}

renorm_SSR <- function(time_df, var_groups){
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

sig_thresholds <- function(rSSR, var_groups, sig_levels){
  sig_list <- list()

  for(s in 1:length(sig_levels)){
    sig_list[[s]] <- rSSR %>%
      group_by(across(all_of(var_groups)))%>%
      summarize(min_val = min(rSSR), max_val = max(rSSR)) %>%
      mutate(threshold = ((max_val - min_val) * sig_levels[s]) + min_val,
             sig_level = sig_levels[s])
    }

  sig_list <- bind_rows(sig_list)

  return(sig_list)
}

opt_mbp <- function(rSSR, sig_list){
  sig_list$opt_mbp <- NA
  for(s in sig_list$threshold){
    sig_list$opt_mbp[sig_list$threshold == s] <- rSSR$mbp_n[max(which(rSSR$rSSR > s))+1]
  }

  return(sig_list)
}

rSSR_plot <- function(rSSR, opt_mbp){

  rSSR_plot <- ggplot(rSSR) +
    geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1), color = "blue") +
    geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1), size = .5, color = "red") +
    geom_hline(data = opt_mbp, aes(yintercept = threshold, color = factor(sig_level)), linetype = "dashed") +
    geom_vline(data = opt_mbp, aes(xintercept = opt_mbp/60, color = factor(sig_level)), linetype = "dashed")+
    geom_line(aes(x = mbp_n / 60, y = zoo::rollmean(rSSR,7, na.pad = TRUE), group = 1), color = "grey50") +
    scale_color_discrete(name = "Significance Level")+
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = "MBP (minutes)", y = "renormalized Sum of Squared Residuals") +
    facet_grid(as.formula(paste("~", var_groups)))+
    theme_classic()

  Log_rSSR_plot <- ggplot(rSSR) +
    geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1), color = "blue") +
    geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1), size = .5, color = "red") +
    geom_hline(data = opt_mbp, aes(yintercept = threshold, color = factor(sig_level)), linetype = "dashed") +
    geom_vline(data = opt_mbp, aes(xintercept = opt_mbp/60, color = factor(sig_level)), linetype = "dashed")+
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

  pdf(file_dir, width = 6.5, height = 4)
  print(rSSR_plot)
  print(Log_rSSR_plot, vp = vp)
  dev.off()
}

build_residence <- function(data, opt_mbp, var_groups, time_unit,
                            var_Id, var_datetime, var_site){
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
