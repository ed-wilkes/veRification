#' boxesPrec
#'
#' @param model rstanarm model object
#' @param x_var character denoting column containing x values
#' @param colours character vector of colours
#' @param qc_level numeric or character denoting QC level
#' @param test_claims boolean denoting whether to test against claims
#' @param claims_data numeric vector of manufacturer's claims
#' @param ci_interval numeric CI interval
#' @param method character string denoting model type
#'
#' @return list of shiny fluidRows -> boxes -> valueBoxes
#' @export
#'
#' @examples
boxesPrec <- function(model
                      ,x_var
                      ,colours
                      ,qc_level
                      ,test_claims
                      ,claims_data
                      ,ci_interval
                      ,method) {

  colour <- colours[qc_level]

  # Get CI
  ci_lwr <- 0 + ((1 - (ci_interval / 100)) / 2)
  ci_upr <- (ci_interval / 100) + ((1 - (ci_interval / 100)) / 2)
  ci_one_tail <- (ci_interval / 100) - (1 - (ci_interval / 100))

  # Get parameters
  if (method != "Bayesian") {

    width <- 4
    bayesian_check_box <- NULL
    bayesian_check_str <- NULL

    # Get VCAinference object
    test_obj <- VCA::VCAinference(
      model
      ,alpha = 1 - (ci_interval / 100)
    )$ConfInt$CV

    parameters <- as.data.frame(model$aov.tab)

    # Mean
    post_mean <- model$Mean

    # Within-day CV
    post_cv_with <- format(round(parameters$`CV[%]`[3], 1), nsmall = 1)
    post_cv_with_lwr <- format(round(test_obj$TwoSided$LCL[3], 1), nsmall = 1)
    post_cv_with_upr <- format(round(test_obj$TwoSided$UCL[3], 1), nsmall = 1)
    post_cv_with_str <- paste0(post_cv_with, " (", post_cv_with_lwr, ", ", post_cv_with_upr, ")")

    # Between-day CV
    post_cv_bet <- format(round(parameters$`CV[%]`[2]), 1, nsmall = 1)
    post_cv_bet_lwr <- format(round(test_obj$TwoSided$LCL[2], 1), nsmall = 1)
    post_cv_bet_upr <- format(round(test_obj$TwoSided$UCL[2], 1), nsmall = 1)
    post_cv_bet_str <- paste0(post_cv_bet, " (", post_cv_bet_lwr, ", ", post_cv_bet_upr, ")")

    # Total SD/CV
    post_sd_total <- parameters$SD[1]
    post_cv_total <- format(round(parameters$`CV[%]`[1], 1), nsmall = 1)
    post_cv_total_lwr <- format(round(test_obj$TwoSided$LCL[1], 1), nsmall = 1)
    post_cv_total_upr <- format(round(test_obj$TwoSided$UCL[1], 1), nsmall = 1)
    post_cv_total_str <- paste0(post_cv_total, " (", post_cv_total_lwr, ", ", post_cv_total_upr, ")")

    # Check if testing against claimed value
    if (test_claims == TRUE && !is.null(claims_data)) {

      claim <- claims_data[qc_level]

      # Check if total CV one-sided confidence interval is above claimed value
      colour_tested <- ifelse(test_obj$OneSided$LCL[1] > claim, yes = "red", no = "green")
      icon_tested <- ifelse(test_obj$OneSided$LCL[1] > claim, yes = "triangle-exclamation", no = "circle-check")
      text_tested <- ifelse(
        test_obj$OneSided$LCL[1] > claim
        ,yes = paste0("does not meet claim of ", claim, "%")
        ,no = paste0("meets claim of ", claim, "%")
      )

    }

  } else if (method == "Bayesian") {

    width <- 3

    # Basic model checks
    if (any(brms::rhat(model) > 1.1) || any(brms::neff_ratio(model)[1:2] < 0.1)) {
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

    bayesian_check_box <- valueBox(
      value = value_check
      ,color = colour_check
      ,subtitle = sub_check
      ,icon = icon(icon_check)
      ,width = width
    )

    bayesian_check_str <- column(
      width = 3
      ,tags$i(
        "NB: The model checks performed here cover the basics and do not
            necessarily account for all the possible errors that might occur."
      )
    )

    # Mean
    post_mean <- median(as.matrix(model)[,1])

    # Within-day CV
    dist_cv_with <- (as.matrix(model)[,"sigma"] / as.matrix(model)[,1]) * 100
    post_cv_with <- gsub(" ", "", format(round(quantile(dist_cv_with, c(ci_lwr, 0.5, ci_upr)), 1), nsmall = 1))
    post_cv_with_str <- paste0(post_cv_with[2], " (", post_cv_with[1], ", ", post_cv_with[3], ")")

    # Between-day CV
    dist_cv_bet <- (as.matrix(model)[,paste0("sd_", x_var, "__Intercept")] / as.matrix(model)[,1]) * 100
    post_cv_bet <- gsub(" ", "", format(round(quantile(dist_cv_bet, c(ci_lwr, 0.5, ci_upr)), 1), nsmall = 1))
    post_cv_bet_str <- paste0(post_cv_bet[2], " (", post_cv_bet[1], ", ", post_cv_bet[3], ")")

    # Total SD/CV
    dist_sd_total <- sqrt(as.matrix(model)[,"sigma"]^2 + as.matrix(model)[,paste0("sd_", x_var, "__Intercept")]^2)
    dist_cv_total <- (dist_sd_total / as.matrix(model)[,1]) * 100
    post_sd_total <- median(dist_sd_total)
    post_cv_total <- gsub(" ", "", format(round(quantile(dist_cv_total, c(ci_lwr, 0.5, ci_upr)), 1), nsmall = 1))
    post_cv_total_str <- paste0(post_cv_total[2], " (", post_cv_total[1], ", ", post_cv_total[3], ")")

    # Check if testing against claimed value
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

    }

  }

  font_size <- "font-size: 90%;"

  # If tested against claims
  if (test_claims == TRUE && !is.null(claims_data)) {

    # Return boxes
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
        # valueBox for Bayesian model checks
        ,bayesian_check_box
      )
      ,HTML("<hr style='margin-top: 0'>")
      ,fluidRow(
        valueBox(
          value = tags$p(post_cv_with_str, style = font_size)
          ,color = colour
          ,subtitle = "Repeatability CV (%)"
          ,width = width
        )
        ,valueBox(
          value = tags$p(post_cv_bet_str, style = font_size)
          ,color = colour
          ,subtitle = "Intermediate precision CV (%)"
          ,width = width
        )
        ,valueBox(
          value = tags$p(post_cv_total_str, style = font_size)
          ,color = colour_tested
          ,subtitle = paste0("Total laboratory CV (%) - ", text_tested)
          ,icon = icon(icon_tested)
          ,width = width
        )
        # Bayesian model check string
        ,bayesian_check_str
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
        # valueBox for Bayesian model checks
        ,bayesian_check_box
      )
      ,HTML("<hr style='margin-top: 0'>")
      ,fluidRow(
        valueBox(
          value = tags$p(post_cv_with_str, style = font_size)
          ,color = colour
          ,subtitle = "Repeatability CV (%)"
          ,width = width
        )
        ,valueBox(
          value = tags$p(post_cv_bet_str, style = font_size)
          ,color = colour
          ,subtitle = "Intermediate precision CV (%)"
          ,width = width
        )
        ,valueBox(
          value = tags$p(post_cv_total_str, style = font_size)
          ,color = colour
          ,subtitle = "Total laboratory CV (%)"
          ,width = width
        )
        # Bayesian model check string
        ,bayesian_check_str
      )
    )

  }
  return(boxes)

}
