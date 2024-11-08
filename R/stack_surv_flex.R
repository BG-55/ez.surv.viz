#' Stack tbl_surv_to_flex tables
#'
#' @importFrom dplyr %>%
#'
#' @param table_list A list of flextables created by tbl_surv_to_flex. Formatting in these tables will be lost.
#' @param label_names A named character vector the consists of the labels for the variables or outcomes in the "Characteristic" column.
#' @param label_header_names A named character vector the consists of the labels for the headers.
#' @param text_font_size A numeric that defines the size of the text in the table. Default is 9.
#' @param table_title A character that defines the title of the table. Default is empty.
#' @param bold_title A boolean that defines if the title of the table will be bold or not. Default is TRUE, which gives a bold title.
#' @param title_font_size A numeric that defines the size of the text in the title. Default is 12.
#' @param table_footnote A character that defines the footnote of the table. Default is empty.
#' @param footnote_font_size A numeric that defines the size of the text in the footnote Default is 9.
#' @param missing_value_replace A character that defines what values will replace missing values in the table. Example when the survival never gets below 50%, the median survival is missing. Default is "—".
#' @param border_color A character the defines the the color of the borders in the table. Default is #D3D3D3.
#' @param header_alignment A character value that is either 'left', 'right', 'center', or 'justify' that defines the alignment of the headers.
#'
#' @return A flextable consisting of the stacked flextables
#' @export
#'
#' @examples
#' Melanoma <- MASS::Melanoma
#' library(survival)
#' library(dplyr)
#' Melanoma2 <- 
#'   Melanoma %>% 
#'   mutate(status = as.factor(recode(status, `2` = 0, `1` = 1, `3` = 2)))
#' #Make the status a labeled factor
#' Melanoma2 <- Melanoma2 %>% mutate(status = factor(status,levels = c("0","1","2"),
#'                                                   labels = c("Censored","Melanoma","Other Causes"))) %>%
#'   mutate(sex = factor(sex,levels = c("0","1"), labels = c("Female","Male")))
#' 
#' #Fit the models
#' ajfit <- survfit(Surv(time, status) ~1, data = Melanoma2)
#' ajfit_strata <- survfit(Surv(time, status) ~sex, data = Melanoma2)
#' 
#' #Create the tables for the specfic times
#' times2 <- gtsummary::tbl_survfit(ajfit, times = c(1000,2000,3000))
#' times2_strata  <- gtsummary::tbl_survfit(ajfit_strata, times = c(1000,2000,3000))
#' 
#' #Use the tbl_surv_to_flex function
#' xv <- ez.surv.viz::tbl_surv_to_flex(times2)
#' xv_strat <- ez.surv.viz::tbl_surv_to_flex(times2_strata)
#' 
#' #Combine in a list
#' comb_list <- list(xv,xv_strat)
#' 
#' #Stack them
#' stack_surv_flex(comb_list, label_names = c("Overall" = "All People"))
#' 
stack_surv_flex <- function(table_list, label_names = NULL, label_header_names = NULL,
                            text_font_size = 9,
                            table_title = "", bold_title = TRUE,
                            title_font_size = 12,
                            table_footnote = "", footnote_font_size = 9,
                            missing_value_replace = "—", border_color = "#D3D3D3",
                            header_alignment = "center") {
  #Bind all the dataframes together
  table_list_new <- data.frame()
  for (z in 1:length(table_list)) {
    table_list_new <- rbind(table_list_new, test_l[[z]][['body']][['dataset']])
  }
  #Make the output table
  table_list_new <- table_list_new %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                ~ stringr::str_replace_all(.,"—",missing_value_replace))) %>%
    flextable::flextable() %>%
    flextable::add_header_lines(top = TRUE,
                                values = c(table_title))   %>% 
    flextable::border_outer(border = officer::fp_border(color = border_color), part = "body") %>%
    flextable::border_outer(border = officer::fp_border(color = border_color), part = "header") %>%
    flextable::border_inner(border = officer::fp_border(color = border_color)) %>%
    flextable::fontsize(size = text_font_size) %>%
    flextable::fontsize(size = text_font_size, part = "header") %>%
    #Title is always first header row
    flextable::fontsize(size = title_font_size, i = 1, part = "header") %>%
    flextable::set_table_properties(layout = "autofit") %>%
    flextable::add_footer_lines(table_footnote) %>%
    flextable::fontsize(size = footnote_font_size, part = "footer") %>%
    flextable::bold(part = 'header', i = 1, bold = bold_title)  %>%
    flextable::align(align = header_alignment, part = "header")
  #If named vector use the labellor to label them for both headings and variables when appropriate
  if (is.null(label_names)) {
    table_list_new <- table_list_new
  } else {
    table_list_new <- table_list_new %>%
      flextable::labelizor(part = "body", labels = label_names)
  }
  if(is.null(label_header_names)) {
    table_list_new
  } else {
    table_list_new %>%
      flextable::labelizor(part = "header", labels = label_header_names)
  }
}