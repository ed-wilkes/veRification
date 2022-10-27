#' fitModelPrec
#'
#' @param data data.frame input
#' @param x_var character string denoting column containing x values
#' @param y_var character string denoting column containing y values
#' @param qc_level character string/numeric denoting QC level
#' @param col_level character string denoting colour for output
#' @param colours character vector containing colours
#' @param test_claims boolean denoting whether to test against target values
#' @param claims_data numeric vector containing target values
#'
#' @return list of shiny valueBoxes
#' @export
#'
#' @examples
fitModelPrec <- function(data
                         ,x_var
                         ,y_var
                         ,qc_level
                         ,col_level
                         ,colours
                         ,test_claims = FALSE
                         ,claims_data = NULL) {

  colour <- colours[qc_level]
  width <- 3
  if (!is.null(col_level)) {
    df_filter <- dplyr::filter(data, .data[[col_level]] == qc_level)
  } else {
    df_filter <- data
  }

  # Fit model
  form <- as.formula(paste0(y_var, "~ 1 + (1|", x_var, ")"))
  mean_y <- mean(data[[y_var]])
  sd_y <- sd(data[[y_var]])

  # Use rstanarm due to greater speed compared to brms
  fit <- rstanarm::stan_lmer(
    formula = form
    ,prior_intercept = normal(mean_y, 2.5 * sd_y, autoscale = FALSE) # rstanarm uninformative default priors
    ,prior_aux = exponential(1 / sd_y, autoscale = FALSE)
    ,data = df_filter
    ,seed = 1234
    ,iter = 4000 # arbitrary number of samples to get stable estimates
    ,adapt_delta = 0.999 # arbitrarily high to fit most models
  )

  # Basic model checks
  if (any(summary(fit)[,"Rhat"] > 1.1) || any(summary(fit)[,"n_eff"] / 4000 < 0.1)) {
    value_check <- "Fail"
    sub_check <- "Something has gone wrong with the Bayesian model. Interpret results with caution."
    colour_check <- "red"
    icon_check <- "triangle-exclamation"
  } else {
    value_check <- "Pass"
    sub_check <- "Bayesian model checks passed"
    colour_check <- "green"
    icon_check <- "circle-check"
  }

  # Get parameters
  post_mean <- quantile(as.matrix(fit)[,1], 0.5)
  post_cv_within <- quantile((as.matrix(fit)[,"sigma"] / as.matrix(fit)[,1]) * 100, 0.5)
  post_cv_between <- quantile((as.matrix(fit)[,paste0("Sigma[", x_var, ":(Intercept),(Intercept)]")] / as.matrix(fit)[,1]) * 100, 0.5)
  dist_sd_total <- sqrt(as.matrix(fit)[,"sigma"]^2 + as.matrix(fit)[,paste0("Sigma[", x_var, ":(Intercept),(Intercept)]")]^2)
  dist_cv_total <- (dist_sd_total / as.matrix(fit)[,1]) * 100
  post_sd_total <- quantile(dist_sd_total, 0.5)
    post_total_cv <- format(round(quantile(dist_cv_total, c(0.025, 0.5, 0.975)), 1), nsmall = 1)
  post_cv_str <- paste0(post_total_cv[2], " (", post_total_cv[1], ", ", post_total_cv[3], ")")

  # Test against claims
  if (test_claims == TRUE && !is.null(claims_data)) {

    claim <- claims_data[qc_level]

    ## Legacy code back-calculating claimed CV to SD
    # sd_claim <- (claim / 100) * post_mean # ratio to posterior median
    # prob_above <- sum(dist_sd_total > sd_claim) / length(dist_sd_total)
    # colour_tested <- ifelse(prob_above > 0.9, yes = "red", no = "green")

    # Check if 90% probability that posterior total CV is above claimed value
    prob_above <- sum(dist_cv_total > claim) / length(dist_cv_total)
    colour_tested <- ifelse(prob_above > 0.9, yes = "red", no = "green")
    icon_tested <- ifelse(prob_above > 0.9, yes = "triangle-exclamation", no = "circle-check")
    text_tested <- ifelse(
      prob_above > 0.9
      ,yes = paste0("does not meet claim of ", claim, "%")
      ,no = paste0("meets claim of ", claim, "%")
    )

    ## Return boxes
    boxes <- list(
      fluidRow(
        valueBox(
          value = format(round(post_mean, 2), nsmall = 2)
          ,color = colour
          ,subtitle = "Mean"
          ,width = width
        )
        ,valueBox(
          value = format(round(post_sd_total, 2), nsmall = 2)
          ,color = colour
          ,subtitle = "Total laboratory SD"
          ,width = width
        )
        ,valueBox(
          value = paste0(
            format(round(post_mean - post_sd_total, 2), nsmall = 2),  " - "
            ,format(round(post_mean + post_sd_total, 2), nsmall = 2)
          )
          ,color = colour
          ,subtitle = "2SD control range"
          ,width = width
        )
        ,valueBox(
          value = value_check
          ,color = colour_check
          ,subtitle = sub_check
          ,icon = icon(icon_check)
          ,width = width
        )
      )
      ,HTML("<hr style='margin-top: 0'>")
      ,fluidRow(
        valueBox(
          value = format(round(post_cv_within, 1), nsmall = 1)
          ,color = colour
          ,subtitle = "Repeatability CV (%)"
          ,width = width
        )
        ,valueBox(
          value = format(round(post_cv_between, 1), nsmall = 1)
          ,color = colour
          ,subtitle = "Intermediate precision CV (%)"
          ,width = width
        )
        ,valueBox(
          value = post_cv_str
          ,color = colour_tested
          ,subtitle = paste0("Total laboratory CV (%) - ", text_tested)
          ,icon = icon(icon_tested)
          ,width = width
        )
        ,column(
          width = 3
          ,tags$i(
            "NB: The model checks performed here cover the basics and do not
            necessarily account for all the possible errors that might occur."
          )
        )
      )
    )

  } else {

    boxes <- list(
      fluidRow(
        valueBox(
          value = format(round(post_mean, 2), nsmall = 2)
          ,color = colour
          ,subtitle = "Mean"
          ,width = width
        )
        ,valueBox(
          value = format(round(post_sd_total, 2), nsmall = 2)
          ,color = colour
          ,subtitle = "Total laboratory SD"
          ,width = width
        )
        ,valueBox(
          value = paste0(
            format(round(post_mean - post_sd_total, 2), nsmall = 2),  " - "
            ,format(round(post_mean + post_sd_total, 2), nsmall = 2)
          )
          ,color = colour
          ,subtitle = "2SD control range"
          ,width = width
        )
        ,valueBox(
          value = value_check
          ,color = colour_check
          ,subtitle = sub_check
          ,icon = icon(icon_check)
          ,width = width
        )
      )
      ,HTML("<hr style='margin-top: 0'>")
      ,fluidRow(
        valueBox(
          value = format(round(post_cv_within, 1), nsmall = 1)
          ,color = colour
          ,subtitle = "Repeatability CV (%)"
          ,width = width
        )
        ,valueBox(
          value = format(round(post_cv_between, 1), nsmall = 1)
          ,color = colour
          ,subtitle = "Intermediate precision CV (%)"
          ,width = width
        )
        ,valueBox(
          value = post_cv_str
          ,color = colour
          ,subtitle = "Total laboratory CV (%)"
          ,width = width
        )
        ,column(
          width = 3
          ,tags$i(
            "NB: The model checks performed here cover the basics and do not
            necessarily account for all the possible errors that might occur."
          )
        )
      )
    )

  }
  return(boxes)

}
