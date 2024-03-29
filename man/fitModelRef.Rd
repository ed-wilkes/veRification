% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_Reference_fct_fitModelRef.R
\name{fitModelRef}
\alias{fitModelRef}
\title{fitModelRef}
\usage{
fitModelRef(
  data,
  prior_location,
  prior_scale,
  prior_n = NULL,
  col_sample,
  col_value_1,
  col_value_2 = NULL,
  var_option
)
}
\arguments{
\item{data}{data.frame input}

\item{prior_location}{numeric mean of prior distribution}

\item{prior_scale}{numeric variation of prior distribution}

\item{prior_n}{numeric number of observations used to determine prior mean}

\item{col_sample}{character string denoting column containing sample IDs}

\item{col_value_1}{character string denoting column containing measurements}

\item{col_value_2}{character string denoting column containing duplicate measurements}

\item{var_option}{character string denoting variance type}
}
\value{
brms model object
}
\description{
fitModelRef
}
