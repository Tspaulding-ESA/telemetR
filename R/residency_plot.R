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
#' @param time_unit the unit of time used to calculate durations
#' @returns A plot of the proportion of events created by each potential
#' blanking period at each time (t).
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' #Plot a comparison of the number of events longer than a given time `t`
#' residence_plot(time_df = time_test,
#'                var_groups = "fish_type",
#'                time_unit = "secs")
#' # Note: that the large number of lines extending past the largest Time
#' # indicates that a larger t is needed to ensure convergence
residence_plot <- function(time_df, var_groups = NULL, time_unit){
  max_t <- time_df |>
    dplyr::group_by(t) |>
    dplyr::summarise(mean = mean(prop_res)) |>
    dplyr::filter(mean != 0) |>
    dplyr::ungroup() |>
    dplyr::pull(t) |>
    max()

  if(is.null(var_groups)){
    plot <- ggplot(time_df |> dplyr::filter(t < max_t), aes(x = t, y = prop_res, col = as.factor(mbp_n))) +
      geom_line() +
      scale_y_log10() +
      labs(x = paste("Time (",time_unit,")"), y = "Proportion of Event Lengths > Time")+
      coord_cartesian(ylim= c(0.0001, 1))+
      theme_classic()+
      theme(legend.position = "none")
  }
  if(!is.null(var_groups)){
    plot <- ggplot(time_df |> dplyr::filter(t < max_t), aes(x = t, y = prop_res, col = as.factor(mbp_n))) +
      geom_line() +
      scale_y_log10() +
      labs(x = paste("Time (",time_unit,")"), y = "Proportion of Event Lengths > Time")+
      coord_cartesian(ylim= c(0.0001, 1))+
      facet_grid(as.formula(paste("~", var_groups)))+
      theme_classic()+
      theme(legend.position = "none")
  }
  print(plot)
}
