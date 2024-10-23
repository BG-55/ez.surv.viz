#' Create cumulative incidence plots
#' 
#' @importFrom dplyr %>%
#'
#' @param model_name A competing risks object of the class survfitms survfit
#' @param line_size A numeric containing the value of the line thickness. Default is 1.
#' @param outcome_colors A vector consisting of the color names/hexadecimals. Default is colors from Dark 3 palette
#' @param col_palette A string containing the name of the hcl palette to use. See hcl.pals() for names. Default is Dark 3
#' @param xlab_name Label for the x-axis. Default is "Time"
#' @param ylab_name Label for the y-axis. Default is "Cumulative Incidence"
#' @param conf_int Determines whether a confidence interval should be drawn for each curve. Default is TRUE.
#' @param conf_int_alpha A value between 0 and 1 that determines the alpha of the ribbon confidence interval
#'
#' @return A GGPlot object with the graph of the cumulative incidence curves
#' @export
#'
#' @examples
#' Melanoma <- MASS::Melanoma
#' library(survival)
#' library(dplyr)
#' #Make the variable in the format for competing risks
#' Melanoma2 <- 
#' Melanoma %>% 
#' mutate(status = as.factor(recode(status, `2` = 0, `1` = 1, `3` = 2)))
#' #Make the status a labeled factor
#' Melanoma2 <- Melanoma2 %>% mutate(status = factor(status,levels = c("0","1","2"),
#'    labels = c("Censored","Melanoma","Other Causes")))
#' #Define the model
#' ajfit <- survfit(Surv(time, status) ~1, data = Melanoma2)
#' #Plot it
#' ci_graph(ajfit)
#' 
ci_graph <- function(model_name, line_size = 1, outcome_colors=NULL, col_palette = "Dark 3",
                     xlab_name="Time",ylab_name="Cumlative Incidence", conf_int = TRUE,
                     conf_int_alpha = 0.2) {
  #Check if missing model
  if(missing(model_name)) {
    "You must input a survfit model for model_name"
  }
  #Get model type
  #Check if wrong model type
  if(class(model_name)[1] != "survfitms" | class(model_name)[2] != "survfit") {
    stop("Model must be a competing risks model. See ?survfit.formula to see how to make that model")
  }
  #Tidy the the data
  dat2 <- broom::tidy(model_name)
  #Make sure color palette exists
  if(!(col_palette %in% grDevices::hcl.pals())) {
    stop("The color palette must be one of the hcl.pals()")
  }
  #Set outcome colors
  if(is.null(outcome_colors)) {
    outcome_colors <- grDevices::hcl.colors(length(unique((dat2 %>%
                                                             dplyr::filter(state != "(s0)"))$state)), col_palette)
  }
  #Make conf_int correct
  if(conf_int != TRUE & conf_int != FALSE) {
    stop("conf_int must be either true or false")
  }
  #Make sure alpha is a number
  if(is.character(conf_int_alpha)) {
    stop("Alpha must be a number between 0 and 1")
  }
  #Make sure its between 0 and 1
  if(conf_int_alpha > 1 | conf_int_alpha < 0) {
    stop("Alpha must be a number between 0 and 1")
  }
  #Make the plot
  if(conf_int == TRUE) {
    dat2 %>% dplyr::filter(state != "(s0)") %>% ggplot2::ggplot() +
      ggplot2::geom_step(ggplot2::aes(x = time, y = estimate, color = state, linetype = state),
                         size = line_size) +
      ggplot2::geom_ribbon(ggplot2::aes(x = time, y = estimate, fill = state, ymin = conf.low, ymax = conf.high),
                  alpha = conf_int_alpha) +
      ggplot2::theme_classic() +  ggplot2::theme(legend.position="bottom") +
      ggplot2::labs(x = xlab_name, y = ylab_name) +
      ggplot2::scale_color_manual(name = "Outcome", values = outcome_colors) +
      ggplot2::scale_fill_manual(name = "Outcome", values = outcome_colors) +
      ggplot2::scale_linetype_discrete(name = "Outcome") +
      ggplot2::theme(legend.position = "inside",
                     legend.position.inside = c(0.15,0.8))
  } else {
   dat2 %>% dplyr::filter(state != "(s0)") %>% ggplot2::ggplot() +
      ggplot2::geom_step(ggplot2::aes(x = time, y = estimate, color = state, linetype = state),
                         size = line_size) +
      ggplot2::theme_classic() +  ggplot2::theme(legend.position="bottom") +
      ggplot2::labs(x = xlab_name, y = ylab_name) +
      ggplot2::scale_color_manual(name = "Outcome", values = outcome_colors) +
      ggplot2::scale_linetype_discrete(name = "Outcome") +
      ggplot2::theme(legend.position = "inside",
                     legend.position.inside = c(0.15,0.8))
  }
}