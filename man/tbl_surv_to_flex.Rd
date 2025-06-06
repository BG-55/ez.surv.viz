% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_surv_to_flex.R
\name{tbl_surv_to_flex}
\alias{tbl_surv_to_flex}
\title{Turn tbl_survfit or tbl_cuminc tables into flextables}
\usage{
tbl_surv_to_flex(
  table_name,
  label_names = NULL,
  label_header_names = NULL,
  text_font_size = 9,
  table_title = "",
  bold_title = TRUE,
  title_font_size = 12,
  table_footnote = "",
  footnote_font_size = 9,
  missing_value_replace = "—",
  border_color = "#D3D3D3",
  header_alignment = "center",
  table_layout_type = "autofit"
)
}
\arguments{
\item{table_name}{An object of the class tbl_survfit, tbl_cuminc or a list of tbl_survfits that is to be turned into a flextbale. Tables in a list will be merged together in the order of the list. Lists of tbl_cuminc are not allowed.}

\item{label_names}{A named character vector the consists of the labels for the variables or outcomes in the "Characteristic" column.}

\item{label_header_names}{A named character vector the consists of the labels for the headers.}

\item{text_font_size}{A numeric that defines the size of the text in the table. Default is 9.}

\item{table_title}{A character that defines the title of the table. Default is empty.}

\item{bold_title}{A boolean that defines if the title of the table will be bold or not. Default is TRUE, which gives a bold title.}

\item{title_font_size}{A numeric that defines the size of the text in the title. Default is 12.}

\item{table_footnote}{A character that defines the footnote of the table. Default is empty.}

\item{footnote_font_size}{A numeric that defines the size of the text in the footnote Default is 9.}

\item{missing_value_replace}{A character that defines what values will replace missing values in the table. Example when the survival never gets below 50\%, the median survival is missing. Default is "—".}

\item{border_color}{A character the defines the the color of the borders in the table. Default is #D3D3D3.}

\item{header_alignment}{A character value that is either 'left', 'right', 'center', or 'justify' that defines the alignment of the headers.}

\item{table_layout_type}{A charatcer value that is either 'fixed' or 'autofit' to determine the layour of the table. Default is 'autofit'}
}
\value{
A flextable object with the results of the tbl_survfit, tbl_cuminc or a  list of tbl_survfits
}
\description{
Turn tbl_survfit or tbl_cuminc tables into flextables
}
\examples{
#Simple non stratified analysis with median survival time
fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
#Change header labels in gtsummary
med_surv <- gtsummary::tbl_survfit(fit, probs = 0.5, label_header = "Median Survial")
tbl_surv_to_flex(med_surv, table_title = "Table 1A. Median Survival Time")
#Add survival times and use a list
times_surv <- gtsummary::tbl_survfit(fit, times = c(10,25,50))
#The median survival time table will come first as it is first in the list
list_surv <- list(med_surv,times_surv)
tbl_surv_to_flex(list_surv, table_title = "Combined Table")

#Stratified Analysis
fit2 <- survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)
med_surv2 <- gtsummary::tbl_survfit(fit2, probs = 0.5, label_header = "Median Survial")
#We can change the missing values for the upper confidence intervals to something else
tbl_surv_to_flex(med_surv2, missing_value_replace = "NR", table_title = "Stratified Analysis")
#Create a times table as well
times2 <- gtsummary::tbl_survfit(fit2, times = c(10,25,50))
#We can change the label and headers
tbl_surv_to_flex(list(med_surv2,times2), missing_value_replace = "NR", table_title = "Stratified Analysis",
                   label_names = c("Maintained" = "New Maintain", "x" = "New Name"),
                   label_header_names = c("Time 10" = "New Time Name",
                                          "Median Survial" = "The Median Survival Time"))
#Tbl_cuminc example
cuminc_ex <- tidycmprsk::cuminc(survival::Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial)
ex_tab <- tidycmprsk::tbl_cuminc(cuminc_ex, outcomes = c("death from cancer","death other causes"))
tbl_surv_to_flex(ex_tab, label_names = c("death from cancer" = "Cancer Death"))
                                       
}
