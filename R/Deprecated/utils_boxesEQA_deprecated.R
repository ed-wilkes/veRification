#' boxesEQA
#'
#' @param model rstanarm model object
#'
#' @return list of shiny valueBoxes
#' @export
#'
#' @examples
boxesEQA <- function(model) {

  # Summarise parameter values
  additive <- paste0(
    round(median(as.matrix(model)[,1]),2), " ("
    ,round(quantile(as.matrix(model)[,1], 0.025), 2), ", "
    ,round(quantile(as.matrix(model)[,1], 0.975), 2), ")"
  )

  add_zero <- sum(as.matrix(model)[,1] > 0) / length(as.matrix(model)[,1])
  icon_tested_one <- ifelse(
    add_zero > 0.95
    ,yes = "triangle-exclamation"
    ,no = ifelse(
      add_zero < 0.05
      ,yes = "exclamation_triangle"
      ,no = "circle-check"
    )
  )
  colour_tested_one <- ifelse(
    add_zero > 0.95
    ,yes = "red"
    ,no = ifelse(
      add_zero < 0.05
      ,yes = "red"
      ,no = "green"
    )
  )

  proportional <- paste0(
    round(median(as.matrix(model)[,2]),2), " ("
    ,round(quantile(as.matrix(model)[,2], 0.025), 2), ", "
    ,round(quantile(as.matrix(model)[,2], 0.975), 2), ")"
  )

  prop_zero <- sum(as.matrix(model)[,2] > 1) / length(as.matrix(model)[,2])
  icon_tested_two <- ifelse(
    prop_zero > 0.95
    ,yes = "triangle-exclamation"
    ,no = ifelse(
      prop_zero < 0.05
      ,yes = "exclamation_triangle"
      ,no = "circle-check"
    )
  )
  colour_tested_two <- ifelse(
    prop_zero > 0.95
    ,yes = "red"
    ,no = ifelse(
      prop_zero < 0.05
      ,yes = "red"
      ,no = "green"
    )
  )
  width <- 12

  # Generate value boxes
  boxes <- list(
    p()
    ,hr()
    ,fluidRow(
      valueBox(
        value = additive
        ,subtitle = "Additive difference"
        ,color = colour_tested_one
        ,icon = icon(icon_tested_one)
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          "NB: The numbers within parentheses represent the 95% credible interval for this parameter
          based on scaled, weakly informative prior distributions."
        )
      )
    )
    ,p()
    ,hr()
    ,fluidRow(
      valueBox(
        value = proportional
        ,subtitle = "Proportional difference"
        ,color = colour_tested_two
        ,icon = icon(icon_tested_two)
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          "NB: The numbers within parentheses represent the 95% credible interval for this parameter
          based on scaled, weakly informative prior distributions."
        )
      )
    )
  )
  return(boxes)
}
