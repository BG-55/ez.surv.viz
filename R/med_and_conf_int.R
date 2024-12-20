#' Print text of the median survival value and its subsequent 95% confidence interval
#'
#' @importFrom dplyr %>%
#'
#' @param model_name the survival model.
#' @param digits_all number of digits to round the median and CI too.
#' @param digits_med number of digits to round mean to. Must specify digits_ci .
#' @param digits_ci number of digits to round CI to. Must specify digits_med.
#' @param unit_text unit for the median survival. Comes right after the median in the text.
#' 
#' @return text with the median survival value and its subsequent 95% confidence interval
#' @export
#'
#' @examples 
#' fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
#' med_and_conf_int(fit, digits_med = 0, digits_ci = 1, unit_text = "years")
med_and_conf_int <- function(model_name, digits_all = 1, digits_med = NULL,
                             digits_ci = NULL, unit_text = "",
                             print_result = TRUE) {
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
  #If strata or not
  if("strata" %in% names(model_name)) {
    #Convert to dataframe
    mat_to_dat <- base::as.data.frame.matrix(survival:::survmean(model_name, rmean="none")$matrix)
    #Create nice strata names and extract them
    all_set_dat <- mat_to_dat %>%  tibble::rownames_to_column(., var = "strat") %>%
      dplyr::mutate(strat_new = stringr::str_extract(strat, '(?<==).*'))
    strata_names <- dplyr::pull(all_set_dat,strat_new)
    #For each strata pull the median survival time 
    stats_for_each_strata <- c()
    #Strata names in order from loop
    strt_order <- c()
    for (z in strata_names){
      the_filt_dat <- all_set_dat %>% dplyr::filter(strat_new == z)
      stats_for_each_strata <- c(stats_for_each_strata, 
                                 paste(
                                   round(the_filt_dat$median,digits_med),unit_text,
                                   paste0("(",round(the_filt_dat$`0.95LCL`,digits_ci),","),
                                   paste0(round(the_filt_dat$`0.95UCL`,digits_ci),").")))
      strt_order <- c(strt_order, z)
    }
    names(stats_for_each_strata) <- strt_order
    #Save the text and keep names
    final_text <- stats_for_each_strata
    final_text[] <- paste(stats_for_each_strata)
  } else {
    #Define model matrix
    s1 <- survival:::survmean(model_name, rmean="none")$matrix
    #Return the pasted text
    non_strat <- c(Overall = paste(round(unname(s1["median"]),digits_med),unit_text,
          paste0("(",round(unname(s1["0.95LCL"]),digits_ci),","),
          paste0(round(unname(s1["0.95UCL"]),digits_ci),")")))
    final_text <- non_strat
    final_text[] <- paste(non_strat)
  }
  #Print result or save it as an object
  if(print_result) {
    print(final_text)
  } else {
    return(final_text)
  }
  
}

