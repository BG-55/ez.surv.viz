#' Print text of the median survival value and the range of survival time
#'
#' @importFrom dplyr %>%
#'
#' @param model_name the survival model.
#' @param digits_all number of digits to round the median and range too.
#' @param digits_med number of digits to round mean to. Must specify digits_range .
#' @param digits_range number of digits to round range to. Must specify digits_med.
#' @param unit_text unit for the median survival. Comes right after the median in the text.
#' 
#' @return text with the median survival value and the range of survival time
#' @export
#'
#' @examples 
#' fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
#' med_and_range(fit, digits_med = 0, digits_range = 1, unit_text = "years")
med_and_range <- function(model_name, digits_all = 1, digits_med = NULL,
                          digits_range = NULL, unit_text = "",
                          print_result = TRUE) {
  #Class check
  if (class(model_name) != "survfit") {
    stop("model_name must be of class survfit")
  }
  if(is.null(digits_med) & !is.null(digits_range) | !is.null(digits_med) & is.null(digits_range)) {
    stop("Either none or both of digits_med and digits_range must be NULL or have a numeric")
  }
  #Assign digits_med and range to overall digits if empty
  if(is.null(digits_med) & is.null(digits_range)) {
    digits_med <- digits_all
    digits_range <- digits_all
  }
  #If strata or not
  if("strata" %in% names(model_name)) {
    #Convert to dataframe
    tidy_dat <- broom::tidy(model_name)
    #Create nice strata names and extract them
    all_set_dat <- tidy_dat %>%
      dplyr::mutate(strat_new = stringr::str_extract(strata, '(?<==).*'))
    strata_names <- unique(all_set_dat$strat_new)
    #Convert to dataframe
    mat_to_dat <- base::as.data.frame.matrix(survival:::survmean(model_name, rmean="none")$matrix)
    #Create nice strata names and extract them
    med_dat <- mat_to_dat %>%  tibble::rownames_to_column(., var = "strat") %>%
      dplyr::mutate(strat_new = stringr::str_extract(strat, '(?<==).*'))
    #For each strata pull the median survival time 
    stats_for_each_strata <- c()
    #Strata names in order from loop
    strt_order <- c()
    for (z in strata_names){
      #For range
      the_filt_dat <- all_set_dat %>% dplyr::filter(strat_new == z)
      #For median
      med_filt_dat <- med_dat %>% dplyr::filter(strat_new == z)
      filt_range <- base::range(the_filt_dat$time, na.rm = TRUE)
      stats_for_each_strata <- c(stats_for_each_strata, 
                                 paste(
                                   round(med_filt_dat$median,digits_med),unit_text,
                                   paste0("(",round(filt_range[1],digits_range),","),
                                   paste0(round(filt_range[2],digits_range),").")))
      strt_order <- c(strt_order, z)
    }
    names(stats_for_each_strata) <- strt_order
    #Save the text and keep names
    final_text <- stats_for_each_strata
    final_text[] <- paste(stats_for_each_strata)
  } else {
    #Define model matrix
    s1 <- survival:::survmean(model_name, rmean="none")$matrix
    #Define range
    r1 <- base::range(model_name[["time"]], na.rm = TRUE)
    #Return the pasted text
    non_strat <- c(Overall = paste(round(unname(s1["median"]),digits_med),unit_text,
                                   paste0("(",round(r1[1],digits_range),","),
                                   paste0(round(r1[2],digits_range),")")))
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

