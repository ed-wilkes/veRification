#' boxesPrec
#'
#' @param model rstanarm model object
#' @param x_var character denoting column containing x values
#' @param colours character vector of colours
#' @param qc_level numeric or character denoting QC level
#' @param test_claims boolean denoting whether to test against claims
#' @param claims_data numeric vector of manufacturer's claims
#' @param ci_interval numeric CI interval
#'
#' @return list of shiny fluidRows -> boxes -> valueBoxes
#' @export
#'
#' @examples
boxesPrec <- function(model, x_var, colours, qc_level, test_claims, claims_data, ci_interval) {

  colour <- colours[qc_level]
  width <- 3

  # Basic model checks
  if (any(summary(model)[,"Rhat"] > 1.1) || any(summary(model)[,"n_eff"] / 4000 < 0.1)) {
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

  # Get CI
  ci_lwr <- 0 + ((1 - (ci_interval / 100)) / 2)
  ci_upr <- (ci_interval / 100) + ((1 - (ci_interval / 100)) / 2)
  ci_one_tail <- (ci_interval / 100) - (1 - (ci_interval / 100))

  # Get parameters
  post_mean <- median(as.matrix(model)[,1])
  post_cv_within <- median((as.matrix(model)[,"sigma"] / as.matrix(model)[,1]) * 100)
  post_cv_between <- median((as.matrix(model)[,paste0("Sigma[", x_var, ":(Intercept),(Intercept)]")] / as.matrix(model)[,1]) * 100)
  dist_sd_total <- sqrt(as.matrix(model)[,"sigma"]^2 + as.matrix(model)[,paste0("Sigma[", x_var, ":(Intercept),(Intercept)]")]^2)
  dist_cv_total <- (dist_sd_total / as.matrix(model)[,1]) * 100
  post_sd_total <- median(dist_sd_total)
  post_total_cv <- format(round(quantile(dist_cv_total, c(ci_lwr, 0.5, ci_upr)), 1), nsmall = 1)
  post_cv_str <- paste0(post_total_cv[2], " (", post_total_cv[1], ", ", post_total_cv[3], ")")

  # Test against claims
  if (test_claims == TRUE && !is.null(claims_data)) {

    claim <- claims_data[qc_level]

    # Check if given X% probability that posterior total CV is above claimed value
    prob_above <- sum(dist_cv_total > claim) / length(dist_cv_total)
    colour_tested <- ifelse(prob_above > ci_one_tail, yes = "red", no = "green")
    icon_tested <- ifelse(prob_above > ci_one_tail, yes = "triangle-exclamation", no = "circle-check")
    text_tested <- ifelse(
      prob_above > ci_one_tail
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
