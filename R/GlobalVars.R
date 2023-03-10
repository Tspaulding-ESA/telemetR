#' Add in Global Variables
#'
#' Sets all global variables to remove warnings in package build
#'
#' @name Set_GVs
globalVariables(c("%>%",".","DateTime_Local","Make","ReceiverSN","RefTag","SSR",
                  "Tag_Code","Tag_Hex","across","aes","all_of","all_vars",
                  "any_of","arrange","as.formula","bind_rows","coord_cartesian",
                  "distinct","end_time","event_change_binary","facet_grid",
                  "filter","filter_at","geom_hline","geom_line","geom_point",
                  "geom_smooth","geom_vline","ggplot","group_by","if_all",
                  "if_any","labs","lead","max_val","mbp_n","min_val",
                  "multipath","multipath_time","mutate","n","n_val",
                  "org_release_date","ping_rate","prop_res","pull","rSSR",
                  "receiver_retrieve","receiver_start","resident","same_group",
                  "same_loc","same_time","scale_color_discrete",
                  "scale_x_continuous","scale_y_log10","select","setup_blank",
                  "sig_level","start_time","summarise","summarize","tag_life",
                  "tag_pulse_rate_interval_nominal","theme","theme_classic",
                  "threshold","time_diff_lag","ungroup","unit","var_groups"))
