#' fitModelPrec
#'
#' @param model fitted brms or rstanarm model object
#' @param model_type character string denoting the type of model ("imprecision", "regression", or "reference")
#'
#' @return list containing ggplot objects
#' @export
#'
#' @examples
modelChecks <- function(model, model_type) {

 if (model_type == "imprecision") {

   posterior_samples <- as.data.frame(model) %>%
     dplyr::select(1, ncol(as.data.frame(model)) - 1, ncol(as.data.frame(model))) %>%
     rename(Intercept = 1, `Within-day SD` = 2, `Between-day SD` = 3) %>%
     dplyr::mutate(`CV (%)` = sqrt(`Within-day SD`^2 + `Between-day SD`^2) / Intercept * 100) %>%
     dplyr::mutate(
       .iteration = rep(1:(nrow(as.matrix(model)) / 4), times = 4)
       ,.chain = rep(1:4, each = nrow(as.matrix(model)) / 4)
     ) %>%
     tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
     dplyr::mutate(
       parameter = factor(
         parameter
         ,levels = c("Intercept", "Between-day SD", "Within-day SD", "CV (%)")
         )
     )

   posteriors <- posterior_samples %>%
     ggplot2::ggplot(aes(x = value))+
     ggplot2::geom_density(adjust = 2, fill = "dodgerblue4", alpha = 0.5)+
     ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
     ggplot2::xlab("Parameter value")+
     ggplot2::ylab("Density (A.U.)")+
     plotTheme(12)

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

   posteriors <- posterior_samples %>%
     ggplot2::ggplot(aes(x = value))+
     ggplot2::geom_vline(aes(xintercept = vline))+
     ggplot2::geom_density(adjust = 2, fill = "dodgerblue4", alpha = 0.5)+
     ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
     ggplot2::xlab("Parameter value")+
     ggplot2::ylab("Density (A.U.)")+
     plotTheme(12)

 } else if (model_type == "reference") {

   posterior_samples <- as.data.frame(model) %>%
     dplyr::rename(Intercept = 1, Residual = 2) %>%
     dplyr::mutate(
       .iteration = rep(1:(nrow(as.matrix(model)) / 4), times = 4)
       ,.chain = rep(1:4, each = nrow(as.matrix(model)) / 4)
     ) %>%
     tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
     dplyr::mutate(parameter = factor(parameter, levels = c("Intercept", "Residual")))

   posteriors <- posterior_samples %>%
     ggplot2::ggplot(aes(x = value))+
     ggplot2::geom_density(adjust = 2, fill = "dodgerblue4", alpha = 0.5)+
     ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
     ggplot2::xlab("Parameter value")+
     ggplot2::ylab("Density (A.U.)")+
     plotTheme(12)

 } else if (model_type == "reference_varying") {

   posterior_samples <- as.data.frame(model) %>%
     dplyr::select(1, ncol(as.data.frame(model)) - 1, ncol(as.data.frame(model))) %>%
     rename(Intercept = 1, `Within-replicate SD` = 2, `Between-replicate SD` = 3) %>%
     dplyr::mutate(
       .iteration = rep(1:(nrow(as.matrix(model)) / 4), times = 4)
       ,.chain = rep(1:4, each = nrow(as.matrix(model)) / 4)
     ) %>%
     tidyr::pivot_longer(cols = -c(.iteration, .chain), names_to = "parameter") %>%
     dplyr::mutate(
       parameter = factor(
         parameter
         ,levels = c("Intercept", "Between-replicate SD", "Within-replicate SD")
       )
     )

   posteriors <- posterior_samples %>%
     ggplot2::ggplot(aes(x = value))+
     ggplot2::geom_density(adjust = 2, fill = "dodgerblue4", alpha = 0.5)+
     ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
     ggplot2::xlab("Parameter value")+
     ggplot2::ylab("Density (A.U.)")+
     plotTheme(12)

 }

  # Gather MCMC traces
  traces <- posterior_samples %>%
    dplyr::filter(parameter != "CV (%)") %>%
    ggplot2::ggplot(aes(x = .iteration, y = value))+
    ggplot2::geom_line(aes(colour = as.factor(.chain)), alpha = 0.5)+
    ggplot2::facet_wrap(~parameter, ncol = 1, scale = "free")+
    ggplot2::xlab("Iteration")+
    ggplot2::ylab("Parameter value")+
    ggplot2::labs(colour = "Chain")+
    ggplot2::scale_colour_manual(values = paste0("dodgerblue", 1:4))+
    plotTheme(12)

  plots <- list(traces, posteriors)
  return(plots)

}
