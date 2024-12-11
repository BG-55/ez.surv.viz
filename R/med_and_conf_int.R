#' Print text of the median survival value and its subsuquent 95% confidence interval
#'
#' @importFrom dplyr %>%
#'
#' @param model_name the unstratified survival model
#' @param digits_all number of digits to round the median and CI too
#' @param digits_med number of digits to round mean to. Must specify digits_ci 
#' @param digits_ci number of digits to round CI to. Must specify digits_med
#' @param unit_text unit for the median survival. Comes right after the median in the text.
#'
#' @return text with the median survival value and its subsuquent 95% confidence interval
#' @export
#'
#' @examples 
#' fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
#' med_and_conf_int(fit, digits_med = 0, digits_ci = 1, unit_text = "years")
med_and_conf_int <- function(model_name, digits_all = 1, digits_med = NULL,
                             digits_ci = NULL, unit_text = "") {
  #Class check
  if (class(model_name) != "survfit") {
    stop("model_name must be of class survfit")
  }
  if(is.null(digits_med) & !is.null(digits_ci) | !is.null(digits_med) & is.null(digits_ci)) {
    stop("Either none or both of digits_med and digits_ci must be NULL or have a numeric")
  }
  #Assign digits_med and ci to overall digits if empty
  if(is.null(digits_med) & is.null(digits_ci)) {
    digits_med <- digits_all
    digits_ci <- digits_all
  }
  #Define model matrix
  s1 <- survival:::survmean(model_name, rmean="none")$matrix
  #Return the pasted text
  paste(round(unname(s1["median"]),digits_med),unit_text,
        paste0("(",round(unname(s1["0.95LCL"]),digits_ci),","),
        paste0(round(unname(s1["0.95UCL"]),digits_ci),")"))
}

