#' Create risk tables for competing risks
#' 
#' @importFrom dplyr %>%
#'
#' @param model_name A competing risks object of the class survfitms survfit
#' @param factor_order A vector of the outcomes in the order that the user wants them to be listed in the legend. First vector will be at the bottom, second will be above it and so on and so forth. Please be careful and only use with a labeled factor of outcomes.
#' @param text_size A numeric containing the size of the text in the risk table. Default is 4.
#' @param xlab_name Label for the x-axis. Default is "Time"
#' @param ylab_name Label for the y-axis. Default is "State"
#'
#' @return A GGPlot object with the risk table for competing risks
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
#' ci_risk(ajfit)
#' 
ci_risk <- function(model_name, factor_order = NULL, text_size = 4, xlab_name = "Time", ylab_name = "State") {
  #Check if missing model
  if(missing(model_name)) {
    "You must input a survfit model for model_name"
  }
  #Get model type
  #Check if wrong model type
  if(class(model_name)[1] != "survfitms" | class(model_name)[2] != "survfit") {
    stop("Model must be a competing risks model. See ?survfit.formula to see how to make that model")
  }
  dat2 <- broom::tidy(model_name) %>% dplyr::group_by(state) %>%
    dplyr::mutate(csm = cumsum(n.event)) 
  
  brks <- pretty(dat2$time)
  # brks <- brks[brks <= max(dat2$time)]
  #Get values that are the closest to breakpoints but not past them
  minz <- c()
  minz_past_point <- c()
  for (i in brks) {
    
    #The purpose of the second ifelse is to make sure that we don't end up with
    #two of the same times if there are no events past the second breakpoint
    minz <- c(minz,ifelse(i==0,min(dat2$time[dat2$time >= i]),
                          base::suppressWarnings(ifelse(min(dat2$time[dat2$time >= 0]) == max(dat2$time[dat2$time <= i]) |
                                                          max(dat2$time[dat2$time <= i]) == -Inf,
                                                        min(dat2$time[dat2$time >= i]),max(dat2$time[dat2$time <= i])))))
    #Create vector for this as well
    minz_past_point <- c(minz_past_point,
                         base::suppressWarnings(ifelse(min(dat2$time[dat2$time >= 0]) == max(dat2$time[dat2$time <= i]),
                                                       TRUE,FALSE)))
    
  }
  #past points
  intr <- data.frame(minz, minz_past_point) %>% dplyr::filter(minz_past_point == TRUE) %>% pull(.,minz)
  #Subtract 1 from csm if its a past breakpoint time
  dat3 <- dat2 %>% dplyr::filter(time %in% minz) %>% dplyr::mutate(minz_past_point = time %in% minz_past_point) %>%
    dplyr::mutate(csm = dplyr::case_when(
      n.event == 1 & minz_past_point == TRUE ~ csm - 1,
      TRUE ~ csm
    )) %>%
    #Add 1 to number at risk if its past a breakpoint time
    dplyr::mutate(n.risk = dplyr::case_when(
      minz_past_point == TRUE ~ n.risk + 1,
      TRUE ~ n.risk
    ))
  #Add zero row if no zero break
  if(0 %in% brks == FALSE) {
    #Add 0 into breaks
    brks <- c(0,brks)
    #Create zero row
    all_state_here <- unique(dat3$state)
    zero_row_data <- data.frame()
    for (j in all_state_here) {
      ifelse(j=="(s0)",zero_row_data <- rbind(zero_row_data,c(max(dat2$n.risk, na.rm = TRUE),0,"(s0)",0,TRUE)),
             zero_row_data <- rbind(zero_row_data,c(0,0,j,0,TRUE)))
    }
    colnames(zero_row_data) <- c("n.risk","csm","state","time","art_zero")
    zero_row_data$n.risk <- as.numeric(zero_row_data$n.risk)
    zero_row_data$csm <- as.numeric(zero_row_data$csm)
    zero_row_data$time <- as.numeric(zero_row_data$time)
    dat3 <- dat3 %>% bind_rows(zero_row_data) %>% dplyr::mutate(art_zero = case_when(
      is.na(art_zero) ~ "FALSE",
      TRUE ~ art_zero
    ))
  } else {
    dat3 <- dat3 %>% dplyr::mutate(art_zero = "FALSE")
  }
  
  #Set cumsum to zero for the non overall risk because no events actually happened at time 0
  #Unless there is an event actually at 0
  #Verified
  dat3 <- dat3 %>% dplyr::mutate(csm = dplyr::case_when(
    time == min(time, na.rm = TRUE) & min(time, na.rm = TRUE) != 0 ~ 0,
    TRUE ~ csm
  )) %>% dplyr::mutate(state = ifelse(state == "(s0)","Number At Risk",state)) %>%
    dplyr::mutate(state2 = factor(state, levels = c(dat3 %>%
                                                      dplyr::filter(state != "Number At Risk") %>%
                                                      dplyr::pull(.,state) %>% base::unique(.),"Number At Risk"),
                                  ordered = TRUE))
    #The purpose of this is to subtract the person that had the event at that time
    #from the number of people at risk at that time. For zero, unless there is an an event
    #at time zero, we don't modify it because no events at time zero happened
    #and we want the total number of people at time zero unless an event happened
    #at time zero.
    #Maybe change to sum of n.censor + n.event?
  dat3 <- dat3 %>%  dplyr::mutate(n.risk.new =
                    ifelse(time == min(time,na.rm = TRUE) & min(time, na.rm = TRUE) != 0 | art_zero == TRUE,
                           n.risk,n.risk-((dat3 %>% dplyr::group_by(time) %>%
                                             dplyr::summarise(all_s = sum(n.event+n.censor, na.rm = TRUE)) %>%
                                             dplyr::filter(time == time) %>% dplyr::pull(.,all_s)))))
  #Make into a factor when told to
  if(!is.null(factor_order)) {
    new_fac <- c("Number At Risk")
    new_fac <- c(factor_order,new_fac)
    dat3$state2 <- factor(dat3$state2, levels = new_fac, ordered = TRUE)
  }
  #Verified
  #Set times to be equal to breaks 
  #Get unique times
  bn <- dat3 %>% arrange(time) %>% pull(.,time) %>% base::unique(.)
  #Named vector with breaks and times
  vvvv <- c(bn=brks)
  #Column where the times correspond to the break times
  dat3$new_time <- vvvv[as.factor(dat3$time)]
  ggplot2::ggplot(data = dat3,
                  ggplot2::aes(x=new_time,y=state2,label = ifelse(state == "Number At Risk",n.risk.new,csm))) + 
    ggplot2::geom_text(size = 4) +
    ggplot2::scale_x_continuous(breaks = brks) +
    ggplot2::labs(y=ylab_name, x = xlab_name) +
    ggplot2::theme_classic()
  
}