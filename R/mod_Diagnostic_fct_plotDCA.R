#' plotDCA
#'
#' @param data data.frame input
#' @param model brms model object
#' @param col_val
#' ue character string representing column of values
#' @param col_label character string representing column of labels
#' @param positive character string representing a "positive" case
#' @param ci_interval numeric CI interval
#' @param plot_dim clientData from session
#'
#' @return plotly object
#' @export
#'
#' @examples
plotDCA <- function(data
                    ,model
                    ,col_value
                    ,col_label
                    ,positive
                    ,ci_interval
                    ,plot_dim) {

  set.seed(1234) # for reproducibility

  ci_lwr <- (0 + ((1 - (ci_interval / 100)) / 2))
  ci_upr <- (1 - ((1 - (ci_interval / 100)) / 2))

  # Change outcome to binary integer
  data <- data %>%
    dplyr::mutate(
      outcome_ = factor(
        dplyr::if_else(!!rlang::sym(col_label) == positive, true = 1, false = 0)
        ,levels = c(0, 1)
      )
    )

  # Get number of positives and negatives
  tp <- sum(data$outcome_ == 1) / nrow(data)
  tn <- sum(data$outcome_ == 0) / nrow(data)

  centred_vector <- data[[col_value]] - median(data[[col_value]], na.rm = TRUE)
  new_data <- data.frame(
    value_centred = seq(min(centred_vector), max(centred_vector), length.out = 50)
  )

  # Get draws
  list_draws <- fitted(
    model
    ,newdata = new_data
    ,ndraws = 100
    ,summary = FALSE
  ) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      value_centred = new_data$value_centred
      ,value_scaled = value_centred + median(data[[col_value]], na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(-c(value_centred, value_scaled)) %>%
    dplyr::rename(iter = name, probability = value) %>%
    dplyr::mutate(prob_ratio = probability / (1 - probability)) %>%
    dplyr::group_split(iter)

  # Perform DCA across all probability thresholds (as fast as I could get it!)
  performDCA <- function(x, data) {

    df_pred <- data %>%
      dplyr::mutate(prediction = dplyr::if_else(!!rlang::sym(col_value) > as.numeric(x[2]), 1, 0))

    conf_mat <- table(df_pred$prediction, df_pred$outcome_) %>%
      as.data.frame() %>%
      dplyr::rename(pred = Var1, ref = Var2)

    tp_rate <- conf_mat$Freq[4] / sum(conf_mat$Freq)
    fp_rate <- conf_mat$Freq[2] / sum(conf_mat$Freq)
    net_benefit <- tp_rate - as.numeric(x[5]) * fp_rate
    return(net_benefit)

  }

  df_results <- lapply(
    seq_along(list_draws)
    ,function(i) {
      results <- list_draws[[i]]
      results$net_benefit <- apply(
        results
        ,MARGIN = 1
        ,performDCA # function defined above
        ,data = data
      )
      return(results)
    }
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(bin = cut(probability, seq(0, 1, 0.01)))

  # Summarise net benefits across draws
  df_results_sum <- df_results %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      probability = median(probability)
      ,median_benefit = median(net_benefit, na.rm = TRUE)
      ,lwr_benefit = quantile(net_benefit, ci_lwr, na.rm = TRUE)
      ,upr_benefit = quantile(net_benefit, ci_upr, na.rm = TRUE)
      ,n = dplyr::n()
    ) %>%
    dplyr::mutate(
      prob_ratio = probability / (1 - probability)
      ,all_benefit = tp - tn * prob_ratio # net benefit for intervening in all cases
    )

  # Make plot
  p <- ggplot2::ggplot(df_results_sum, ggplot2::aes(x = probability, y = median_benefit))+
    ggplot2::geom_line(data = df_results, ggplot2::aes(y = net_benefit, group = iter), alpha = 0.1, colour = "grey70")+
    ggplot2::geom_hline(yintercept = 0, colour = "forestgreen", linewidth = 1, alpha = 0.75)+
    ggplot2::geom_line(ggplot2::aes(y = all_benefit), colour = "mediumorchid4", linewidth = 1, alpha = 0.75)+
    ggplot2::geom_line(colour = "blue2", linewidth = 1, alpha = 0.75)+
    plotTheme(font_size = 12)+
    ggplot2::xlab(paste0("Threshold probability of ", positive))+
    ggplot2::ylab("Net benefit of intervention")+
    ggplot2::ylim(c(0, 1))

  gg <- plotly::ggplotly(p, width = plot_dim$output_pid_width, height = plot_dim$output_pid_height)

  # geom_line
  gg$x$data[[1]]$text <- NA
  # gg$x$data[[1]]$text <- paste0(
  #   "Posterior sample: ", sub(".", "", df_results$iter), "\n"
  #   ,"Threshold probability of ", positive, ": ", round(df_results$probability, 2), "\n"
  #   ,"Net benefit of intervening: ", round(df_results$net_benefit, 2)
  # )

  # geom_hline
  gg$x$data[[2]]$text <- paste0(
    "Threshold probability of ", positive, ": ", round(df_results$probability, 2), "\n"
    ,"Net benefit of no intervention: 0"
  )

  # geom_line
  gg$x$data[[3]]$text <- paste0(
    "Threshold probability of ", positive, ": ", round(df_results_sum$probability, 2), "\n"
    ,"Net benefit of intervening in all cases: ", round(df_results_sum$all_benefit, 2)
  )

  # geom_line
  gg$x$data[[4]]$text <- paste0(
    "Threshold probability of ", positive, ": ", round(df_results_sum$probability, 2), "\n"
    ,"Median net benefit of intervention: ", round(df_results_sum$median_benefit, 2)
  )

  return(gg)
}
