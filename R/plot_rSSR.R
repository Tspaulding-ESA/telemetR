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
#' @param var_groups a single string or vector of strings of the columns which
#' should be used to group organisms. Common groupings are species and cohorts.
#' @returns A plot of the rSSR curve, convergence thresholds, and optimum mbps
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' #plot the rSSR and log(rSSR) curves
#' rssr_plot(rSSR_df = ex_rSSR,
#'           opt_mbp_df = ex_opt,
#'           var_groups = "fish_type")
rSSR_plot <- function(rSSR_df, opt_mbp_df, var_groups=NULL){
  if(is.null(var_groups)){
    main_plot <- ggplot(rSSR_df) +
      geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1),
                color = "blue") +
      geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1),
                 size = .5, color = "red") +
      geom_hline(data = opt_mbp_df,
                 aes(yintercept = threshold,
                     color = factor(thresh_level)),
                 linetype = "dashed") +
      geom_vline(data = opt_mbp_df,
                 aes(xintercept = opt_mbp/60,
                     color = factor(thresh_level)),
                 linetype = "dashed")+
      geom_line(aes(x = mbp_n / 60,
                    y = zoo::rollmean(rSSR,7, na.pad = TRUE),
                    group = 1),
                color = "grey50") +
      scale_color_discrete(name = "Significance Level")+
      scale_x_continuous(expand = c(0, 0)) +
      labs(x = "MBP (minutes)", y = "renormalized Sum of Squared Residuals") +
      theme_classic()

    log_plot <- ggplot(rSSR_df) +
      geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1),
                color = "blue") +
      geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1),
                 size = .5, color = "red") +
      geom_hline(data = opt_mbp_df,
                 aes(yintercept = threshold, color = factor(thresh_level)),
                 linetype = "dashed") +
      geom_vline(data = opt_mbp_df,
                 aes(xintercept = opt_mbp/60, color = factor(thresh_level)),
                 linetype = "dashed")+
      geom_smooth(aes(x = mbp_n / 60, y = rSSR, group = 1),
                  method = "loess",
                  span = 0.7,
                  color = "grey50",
                  se = FALSE)+
      scale_color_discrete(name = "Significance Level")+
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_log10() +
      labs(x = "MBP (minutes)", y = "renormalized Sum of Squared Residuals") +
      theme_classic()+
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())
    }
  if(!is.null(var_groups)){
    main_plot <- ggplot(rSSR_df) +
      geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1),
                color = "blue") +
      geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1),
                 size = .5, color = "red") +
      geom_hline(data = opt_mbp_df,
                 aes(yintercept = threshold, color = factor(thresh_level)),
                 linetype = "dashed") +
      geom_vline(data = opt_mbp_df,
                 aes(xintercept = opt_mbp/60, color = factor(thresh_level)),
                 linetype = "dashed")+
      geom_line(aes(x = mbp_n / 60,
                    y = zoo::rollmean(rSSR,7, na.pad = TRUE),
                    group = 1),
                color = "grey50") +
      scale_color_discrete(name = "Significance Level")+
      scale_x_continuous(expand = c(0, 0)) +
      labs(x = "MBP (minutes)", y = "renormalized Sum of Squared Residuals") +
      facet_grid(as.formula(paste("~", var_groups)))+
      theme_classic()

    log_plot <- ggplot(rSSR_df) +
      geom_line(aes(x = mbp_n / 60, y = rSSR, group = 1),
                color = "blue") +
      geom_point(aes(x = mbp_n / 60, y = rSSR, group = 1),
                 size = .5, color = "red") +
      geom_hline(data = opt_mbp_df,
                 aes(yintercept = threshold, color = factor(thresh_level)),
                 linetype = "dashed") +
      geom_vline(data = opt_mbp_df,
                 aes(xintercept = opt_mbp/60, color = factor(thresh_level)),
                 linetype = "dashed")+
      geom_smooth(aes(x = mbp_n / 60, y = rSSR, group = 1),
                  method = "loess",
                  span = 0.7,
                  color = "grey50",
                  se = FALSE)+
      scale_x_continuous(expand = c(0, 0)) +
      scale_color_discrete(name = "Significance Level")+
      scale_y_log10() +
      labs(x = "MBP (minutes)", y = "renormalized Sum of Squared Residuals") +
      facet_grid(as.formula(paste("~", var_groups)))+
      theme_classic()+
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())
  }

  print(main_plot)
  print(log_plot)
}
