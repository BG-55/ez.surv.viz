#' Turn tbl_survfit or tbl_cuminc tables into flextables
#'
#' @importFrom dplyr %>%
#'
#' @param table_name An object of the class tbl_survfit, tbl_cuminc or a list of tbl_survfits that is to be turned into a flextbale. Tables in a list will be merged together in the order of the list. Lists of tbl_cuminc are not allowed.
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
#' @param table_layout_type A charatcer value that is either 'fixed' or 'autofit' to determine the layour of the table. Default is 'autofit'
#'
#' @return A flextable object with the results of the tbl_survfit, tbl_cuminc or a  list of tbl_survfits
#' @export
#'
#' @examples
#' #Simple non stratified analysis with median survival time
#' fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
#' #Change header labels in gtsummary
#' med_surv <- gtsummary::tbl_survfit(fit, probs = 0.5, label_header = "Median Survial")
#' tbl_surv_to_flex(med_surv, table_title = "Table 1A. Median Survival Time")
#' #Add survival times and use a list
#' times_surv <- gtsummary::tbl_survfit(fit, times = c(10,25,50))
#' #The median survival time table will come first as it is first in the list
#' list_surv <- list(med_surv,times_surv)
#' tbl_surv_to_flex(list_surv, table_title = "Combined Table")
#' 
#' #Stratified Analysis
#' fit2 <- survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)
#' med_surv2 <- gtsummary::tbl_survfit(fit2, probs = 0.5, label_header = "Median Survial")
#' #We can change the missing values for the upper confidence intervals to something else
#' tbl_surv_to_flex(med_surv2, missing_value_replace = "NR", table_title = "Stratified Analysis")
#' #Create a times table as well
#' times2 <- gtsummary::tbl_survfit(fit2, times = c(10,25,50))
#' #We can change the label and headers
#' tbl_surv_to_flex(list(med_surv2,times2), missing_value_replace = "NR", table_title = "Stratified Analysis",
#'                    label_names = c("Maintained" = "New Maintain", "x" = "New Name"),
#'                    label_header_names = c("Time 10" = "New Time Name",
#'                                           "Median Survial" = "The Median Survival Time"))
#' #Tbl_cuminc example
#' cuminc_ex <- tidycmprsk::cuminc(survival::Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial)
#' ex_tab <- tidycmprsk::tbl_cuminc(cuminc_ex, outcomes = c("death from cancer","death other causes"))
#' tbl_surv_to_flex(ex_tab, label_names = c("death from cancer" = "Cancer Death"))
#'                                        
tbl_surv_to_flex <- function(table_name, label_names = NULL, label_header_names = NULL,
                                text_font_size = 9,
                                table_title = "", bold_title = TRUE,
                                title_font_size = 12,
                                table_footnote = "", footnote_font_size = 9,
                                missing_value_replace = "—", border_color = "#D3D3D3",
                                header_alignment = "center", table_layout_type = "autofit") {

  #In case wrong table type
  if(class(table_name)[1] != "tbl_survfit" & class(table_name)[1] != "list" &
     class(table_name)[1] != "tbl_cuminc") {
    stop("The table must be of the class tbl_survfit, tbl_cuminc, or a list of tbl_survfits")
  }
  #In case wrong table type in list
  if(class(table_name)[1] == "list") {
  if(sum(unlist(lapply(table_name, function(x) class(x)[1]))!= "tbl_survfit", na.rm = TRUE) != 0) {
    stop("All tables in list must be of the class tbl_survfit")
  }
  }
  #Check if the table or list of tables is a cuminc
  cuminc_true <- base::ifelse(class(table_name)[1] == "tbl_cuminc" |
                                class(table_name)[1] == "list" &
  sum(unlist(lapply(table_name, function(x) class(x)[1]))== "tbl_cuminc", na.rm = TRUE) >= 1,"yes","no")
  #In case not a boolean for bold_title
  if(bold_title != TRUE & bold_title != FALSE) {
    stop("bold_title must be either TRUE or FALSE")
  }
  #In case wrong alignment type
  if(header_alignment %in% c('left', 'right', 'center','justify') == FALSE) {
    stop("header_alignment must be one of 'left', 'right', 'center', or 'justify'")
  }
  #In case non named vector for label_names
  if(!is.null(label_names) & is.null(names(label_names))) {
    stop("label_names must be a named vector")
  }
  #Remove bold or italic stars from labels because it wont make sense when going to df to flextable
  if(class(table_name)[1] == "tbl_survfit" | class(table_name)[1] == "tbl_cuminc") {
  table_name[["table_styling"]][["header"]] <- table_name[["table_styling"]][["header"]]  %>%
    dplyr::mutate(label = stringr::str_replace_all(label,"\\*",""))
  } else if(class(table_name)[1] == "list") {
    #Remove bold or italic stars from labels because it wont make sense when going to df to flextable
    jkl <- length(table_name)
    for (z in 1:jkl) {
      table_name[[z]][["table_styling"]][["header"]] <- table_name[[z]][["table_styling"]][["header"]] %>%
        dplyr::mutate(label = stringr::str_replace_all(label,"\\*",""))
    }
#Join all the tables together
table_name <- lapply(table_name, function(x) gtsummary::as_tibble(x)) %>%
  purrr::reduce(dplyr::full_join, by = "Characteristic")
  }
#Fix cuminc table to be like normal table
 tab1 <-  table_name %>% gtsummary::as_tibble()
 if(cuminc_true == "yes") {
   tab1 <- tab1 %>%
     #For stratified analysis as the group column lacks key info
     dplyr::mutate(Group = dplyr::case_when(
       is.na(Group) ~ Characteristic,
       TRUE ~ Group
     )) %>%
     dplyr::select(-c(Characteristic)) %>% dplyr::rename(Characteristic = Group)
 }
 #Create the output table
 tab1 <- tab1 %>%
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
    flextable::set_table_properties(layout = table_layout_type) %>%
    flextable::add_footer_lines(table_footnote) %>%
    flextable::fontsize(size = footnote_font_size, part = "footer") %>%
    flextable::bold(part = 'header', i = 1, bold = bold_title)  %>%
    flextable::align(align = header_alignment, part = "header")
 #If named vector use the labellor to label them for both headings and variables when appropriate
 if (is.null(label_names)) {
   tab1 <- tab1
 } else {
   tab1 <- tab1 %>%
     flextable::labelizor(part = "body", labels = label_names)
 }
 if(is.null(label_header_names)) {
   tab1
 } else {
   tab1 %>%
     flextable::labelizor(part = "header", labels = label_header_names)
 }
}