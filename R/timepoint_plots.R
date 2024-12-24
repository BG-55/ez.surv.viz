#' Create subject timepoint plots
#'
#' @importFrom dplyr %>%
#'
#' @param data_name a dataframe in long format consisting of 1 id column where each subject has a unique indentifier, and the rest of the columns should be event columns with the time of the event for the subject and NA if they did not have the event 
#' @param id a charatcer that is the name of the id column. Default is "id".
#' @param start_time a number that gives the time that lines should start at. Default is 0.
#' @param outcome_colors A vector consisting of the color names/hexadecimals. Default is colors from Dark 3 palette.
#' @param col_palette A string containing the name of the hcl palette to use. See hcl.pals() for names. Default is Dark 3.
#' @param xlab_name Label for the x-axis. Default is "Time".
#' @param ylab_name Label for the y-axis. Default is "ID".
#' @param legend_name Label for the legend. Default is "outcome".
#' @param line_size A numeric containing the value of the line thickness. Default is 0.5.
#' @param point_size A numeric containing the value of the point size Default is 3.
#' @param legend_text_size A number that determines the size of the text in the legend. Default is 10.
#' @param legend_title_text_size A number that determines the size of the title in the legend. Default is 12.
#' @param legend_size A number that determines the size of the legend in centimeters. Default is 0.5.
#'
#' @return a GGPlot object with the time point plots for each subject
#' @export
#'
#' @examples
#' var_labels <- paste("Event",1:5)
#' #Dataset setup with id and event columns
#' toy <- data.frame(id = c("1","2","3"), e_1 = c(1,4,8), e_2 = c(NA,6,NA),
#' e_3 = c(4,NA,6), e_4 = c(2,10,NA), e_5 = c(10,12,14))
#' #Because of missing values for people with missing events ggplot gives useless warnings
#' #So we run the code like this to avoid seeing them
#' base::suppressWarnings(print(timepoint_plot(toy)))
#' #Example with multiple events for a person
#' toy2 <- data.frame(id = c("1","2","3"), e_1 = c(1,4,8), e_1_2 = c(3,NA,NA), e_2 = c(NA,6,NA),
#' e_3 = c(4,NA,6), e_4 = c(2,10,NA), e_4_2 = c(4,17,NA), e_5 = c(10,12,14)) %>%
#' # For each variable we pivot longer all of the columns where the person could have an event
#' # and put them back into one column with the event name
#' tidyr::pivot_longer(cols = c(e_1,e_1_2), names_to = NULL, values_to = 'e_1') %>%
#' tidyr::pivot_longer(cols = c(e_4,e_4_2), names_to = NULL, values_to = 'e_4')
#' base::suppressWarnings(print(timepoint_plot(toy2)))
timepoint_plot <- function(data_name = NULL, id = "id", start_time = 0,
                           outcome_colors=NULL, col_palette = "Dark 3",
                           xlab_name="Time", ylab_name="ID", legend_name = "Outcome",
                           line_size = 0.5, point_size = 3,legend_text_size = 10,
                           legend_title_text_size = 12, legend_size = 0.5) {
#Remove irrelevant warnings about values out of scale range (Its because of missing values for ppl w/o events)
  suppressWarnings({
  transform_dat <- data_name %>%
    #Rename id column to id
    dplyr::rename('id' = id) %>%
    #Set a start time
    dplyr::mutate(start_time = start_time) %>%
    tidyr::pivot_longer(cols = c(tidyselect::everything(), -c(id)),
                        names_to = 'var', values_to = 'time_all')
  tp_plot <- ggplot2::ggplot(data = transform_dat, ggplot2::aes(x=time_all,y=id)) +
    ggplot2::geom_line(linewidth = line_size)
  unique_events <- base::unique((transform_dat %>% dplyr::filter(var != "start_time"))$var)
  #Make sure color palette exists
  if(!(col_palette %in% grDevices::hcl.pals())) {
    stop("The color palette must be one of the hcl.pals()")
  }
  #Set outcome colors
  if(is.null(outcome_colors)) {
    outcome_colors <- grDevices::hcl.colors(length(unique_events), col_palette)
  }
  for (z in unique_events) {
    tp_plot <- tp_plot +
      ggplot2::geom_point(data = transform_dat %>% dplyr::filter(var == z),
                          ggplot2::aes(x=time_all, y = id, color = var), size = point_size)
  }
  tp_plot <- tp_plot +
    ggplot2::labs(x = xlab_name, y = ylab_name) +
    ggplot2::scale_color_manual(name = legend_name, values = outcome_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_text(size = legend_title_text_size), 
                   legend.text = ggplot2::element_text(size = legend_text_size),
                   legend.key.size = grid::unit(legend_size, 'cm'))
  return(tp_plot)
  })
}