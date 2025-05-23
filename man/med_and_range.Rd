% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/med_range.R
\name{med_and_range}
\alias{med_and_range}
\title{Print text of the median survival value and the range of survival time}
\usage{
med_and_range(
  model_name,
  digits_all = 1,
  digits_med = NULL,
  digits_range = NULL,
  unit_text = "",
  print_result = TRUE
)
}
\arguments{
\item{model_name}{the survival model.}

\item{digits_all}{number of digits to round the median and range too.}

\item{digits_med}{number of digits to round mean to. Must specify digits_range .}

\item{digits_range}{number of digits to round range to. Must specify digits_med.}

\item{unit_text}{unit for the median survival. Comes right after the median in the text.}
}
\value{
text with the median survival value and the range of survival time
}
\description{
Print text of the median survival value and the range of survival time
}
\examples{
fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
med_and_range(fit, digits_med = 0, digits_range = 1, unit_text = "years")
}
