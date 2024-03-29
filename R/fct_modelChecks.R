#' fitModelPrec
#'
#' @param model fitted brms or rstanarm model object
#' @param model_type character string denoting the type of model ("imprecision", "regression", or "reference")
#' @param ci_interval numeric interval width
#'
#' @return list containing ggplot objects
#' @export
#'
#' @examples
modelChecks <- function(model, model_type, ci_interval) {

  ci_lwr <- 0 + ((1 - (ci_interval / 100)) / 2)
  ci_upr <- (ci_interval / 100) + ((1 - (ci_interval / 100)) / 2)

  if (model_type == "imprecision") {

    posterior_samples <- brms::as_draws_df(model) %>%
      dplyr::select(1:3) %>%
      dplyr::rename(Mean = 1, `Between-day SD (intermediate)` = 2, `Within-day SD (repeatability)` = 3) %>%
      dplyr::mutate(
        `Total laboratory CV (%)` = sqrt(`Within-day SD (repeatability)`^2 + `Between-day SD (intermediate)`^2) / Mean * 100
      ) %>%
      dplyr::mutate(
        .iteration = rep(1:(nrow(as.matrix(model)) / 4), times = 4)
        ,.chain = rep(1:4, each = nrow(as.matrix(model)) / 4)
      ) %>%
      tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
      dplyr::mutate(
        parameter = factor(
          parameter
          ,levels = c("Mean", "Between-day SD (intermediate)", "Within-day SD (repeatability)", "Total laboratory CV (%)")
        )
      )

  } else if (model_type == "logistic") {

    posterior_samples <- as.data.frame(model) %>%
      dplyr::select(1, 2) %>%
      dplyr::rename(`Intercept (probability)`= 1, `Coefficient (log-odds)` = 2) %>%
      dplyr::mutate(`Intercept (probability)` = plogis(`Intercept (probability)`)) %>%
      dplyr::mutate(
        .iteration = rep(1:(nrow(as.matrix(model)) / 4), times = 4)
        ,.chain = rep(1:4, each = nrow(as.matrix(model)) / 4)
      ) %>%
      tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
      dplyr::mutate(
        parameter = factor(
          parameter
          ,levels = c("Intercept (probability)", "Coefficient (log-odds)")
        )
      )

  } else if (model_type == "reference") {

    posterior_samples <- brms::as_draws_df(model) %>%
      dplyr::select(1:2, .chain, .iteration) %>%
      dplyr::rename(Mean = 1, `Between-replicate SD` = 2) %>%
      tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
      dplyr::mutate(parameter = factor(parameter, levels = c("Mean", "Between-replicate SD")))

  } else if (model_type == "reference_varying") {

    posterior_samples <- brms::as_draws_df(model) %>%
      dplyr::select(1:3, .chain, .iteration) %>%
      dplyr::rename(Mean = 1, `Within-replicates SD` = 2, `Between-replicate SD` = 3) %>%
      tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
      dplyr::mutate(parameter = factor(parameter, levels = c("Mean", "Within-replicates SD", "Between-replicate SD")))

  } else if (model_type == "regression") {

    posterior_samples <- brms::as_draws_df(model) %>%
      dplyr::select(1:3, .chain, .iteration) %>%
      dplyr::rename(Intercept = 1, Slope = 2, Residual = 3) %>%
      tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
      dplyr::mutate(
        parameter = factor(parameter, levels = c("Intercept", "Slope", "Residual"))
        ,vline = dplyr::case_when(
          parameter == "Intercept" ~ 0
          ,parameter == "Slope" ~ 1
          ,TRUE ~ NA_real_
        )
      )

  }

  if (model_type != "regression") {

    densities <- posterior_samples %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(
        lwr = quantile(value, ci_lwr)
        ,upr = quantile(value, ci_upr)
        ,within_interval = dplyr::if_else(value >= lwr & value <= upr, TRUE, FALSE)
      ) %>%
      dplyr::summarise(
        density_x = density(value, adjust = 2)$x
        ,density_y = density(value, adjust = 2)$y
        ,lwr = mean(lwr)
        ,upr = mean(upr)
      )

    posteriors <- densities %>%
      ggplot2::ggplot(ggplot2::aes(x = density_x, y = density_y))+
      ggplot2::geom_area(colour = "black", fill = "dodgerblue", alpha = 0.25)+
      ggplot2::geom_area(data = dplyr::filter(densities, density_x >= lwr & density_x <= upr), fill = "dodgerblue4", alpha = 0.5)+
      ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
      ggplot2::xlab("Parameter value")+
      ggplot2::ylab("Density (A.U.)")+
      plotTheme(12)

  } else {

    densities <- posterior_samples %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(
        lwr = quantile(value, ci_lwr)
        ,upr = quantile(value, ci_upr)
        ,within_interval = dplyr::if_else(value >= lwr & value <= upr, TRUE, FALSE)
      ) %>%
      dplyr::summarise(
        density_x = density(value, adjust = 2)$x
        ,density_y = density(value, adjust = 2)$y
        ,lwr = mean(lwr)
        ,upr = mean(upr)
        ,vline = mean(vline)
      )

    posteriors <- densities %>%
      ggplot2::ggplot(ggplot2::aes(x = density_x, y = density_y))+
      ggplot2::geom_area(colour = "black", fill = "dodgerblue", alpha = 0.25)+
      ggplot2::geom_area(data = dplyr::filter(densities, density_x >= lwr & density_x <= upr), fill = "dodgerblue4", alpha = 0.5)+
      ggplot2::geom_vline(ggplot2::aes(xintercept = vline))+
      ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
      ggplot2::xlab("Parameter value")+
      ggplot2::ylab("Density (A.U.)")+
      plotTheme(12)

  }

  # Gather MCMC traces
  binwidth <- max(posterior_samples$.iteration) * 0.02

  traces <- posterior_samples %>%
    dplyr::filter(parameter != "Total laboratory CV (%)") %>%
    ggplot2::ggplot(ggplot2::aes(x = .iteration, y = value))+
    ggplot2::geom_step(
      ggplot2::aes(colour = as.factor(.chain))
      ,alpha = 0.5
      ,stat = "summary_bin"
      ,binwidth = binwidth
      ,fun = "median", linewidth = 1
    )+
    ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
    ggplot2::xlab("Iteration")+
    ggplot2::ylab("Parameter value")+
    ggplot2::labs(colour = "Chain")+
    ggplot2::scale_colour_manual(values = paste0("dodgerblue", 1:4))+
    plotTheme(12)

  plots <- list(traces, posteriors)
  return(plots)

}
