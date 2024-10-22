#' Create risk tables for competing risks
#' 
#' @importFrom dplyr %>%
#'
#' @param model_name A competing risks object of the class survfitms survfit
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
ci_risk <- function(model_name) {
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
brks <- brks[brks <= max(dat2$time)]
#Get values that are the closest to breakpoints but not past them
minz <- c()
minz_past_point <- c()
for (i in brks) {
  
  #The purpose of the second ifelse is to make sure that we don't end up with
  #two of the same times if there are no events past the second breakpoint
  minz <- c(minz,ifelse(i==0,min(dat2$time[dat2$time >= i]),
                        ifelse(min(dat2$time[dat2$time >= 0]) == max(dat2$time[dat2$time <= i]),
                               min(dat2$time[dat2$time >= i]),max(dat2$time[dat2$time <= i]))))
  #Create column for this as well
  minz_past_point <- c(minz_past_point,
                       base::suppressWarnings(ifelse(min(dat2$time[dat2$time >= 0]) == max(dat2$time[dat2$time <= i]),
                                               TRUE,FALSE)))
  
}
#Subtract 1 from csm if its a past breakpoint time
dat3 <- dat2 %>% dplyr::filter(time %in% minz) %>% dplyr::mutate(minz_past_point = minz_past_point) %>%
  dplyr::mutate(csm = dplyr::case_when(
    n.event == 1 & minz_past_point == TRUE ~ csm - 1,
    TRUE ~ csm
  )) %>%
  #Add 1 to number at risk if its past a breakpoint time
  dplyr::mutate(n.risk = dplyr::case_when(
    minz_past_point == TRUE ~ n.risk + 1,
    TRUE ~ n.risk
  ))
#Set cumsum to zero for the non overall risk because no events actually happened at time 0
#Unless there is an event actually at 0
#Verified
dat3 <- dat3 %>% dplyr::mutate(csm = dplyr::case_when(
  time == min(time, na.rm = TRUE) & min(time, na.rm = TRUE) != 0 ~ 0,
  TRUE ~ csm
)) %>% dplyr::mutate(state = ifelse(state == "(s0)","Number At Risk",state)) %>%
  dplyr::mutate(state2 = factor(state, levels = c(dat3 %>%
      dplyr::filter(state != "Number At Risk") %>% dplyr::pull(.,state) %>% base::unique(.),"Number At Risk"),
                         ordered = TRUE)) %>%
  #The purpose of this is to subtract the person that had the event at that time
  #from the number of people at risk at that time. For zero, unless there is an an event
  #at time zero, we don't modify it because no events at time zero happened
  #and we want the total number of people at time zero unless an event happened
  #at time zero.
  dplyr::mutate(n.risk.new = ifelse(time == min(time,na.rm = TRUE) & min(time, na.rm = TRUE) != 0,
                             n.risk,n.risk-1))


ggplot2::ggplot(data = dat3,
                ggplot2::aes(x=time,y=state2,label = ifelse(state == "Number At Risk",n.risk.new,csm))) + 
  ggplot2::geom_text() +
  ggplot2::scale_x_continuous(breaks = brks) +
  ggplot2::theme_classic()
}
