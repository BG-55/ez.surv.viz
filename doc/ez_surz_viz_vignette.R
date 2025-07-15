## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ----setup, message=FALSE,warning=FALSE---------------------------------------
library(survival)
library(dplyr)
library(ez.surv.viz)

## -----------------------------------------------------------------------------
Melanoma <- MASS::Melanoma

## -----------------------------------------------------------------------------
#Make the variable in the format for competing risks
Melanoma2 <- 
Melanoma %>% 
mutate(status = as.factor(recode(status, `2` = 0, `1` = 1, `3` = 2)))

## -----------------------------------------------------------------------------
Melanoma2 <- Melanoma2 %>% mutate(status = factor(status,levels = c("0","1","2"),
   labels = c("Censored","Melanoma","Other Causes")))

## -----------------------------------------------------------------------------
ajfit <- survfit(Surv(time, status) ~1, data = Melanoma2)

## -----------------------------------------------------------------------------
ci_graph(ajfit)

## -----------------------------------------------------------------------------
ci_graph(ajfit, outcome_colors = c("red","blue"), conf_int = FALSE)

## -----------------------------------------------------------------------------
ci_graph(ajfit, col_palette = "Green-Brown")

## -----------------------------------------------------------------------------
ci_graph(ajfit, factor_order = c("Other Causes", "Melanoma"))

## -----------------------------------------------------------------------------
library(ggplot2)
ci_graph(ajfit, conf_int = FALSE) + geom_vline(xintercept = 2000) + scale_x_continuous(breaks = c(0,1000,2000,3000,4000,5000))

## -----------------------------------------------------------------------------
ci_risk(ajfit)

## -----------------------------------------------------------------------------
ci_risk(ajfit, factor_order = c("Other Causes", "Melanoma"))

## -----------------------------------------------------------------------------
ci_risk(ajfit) + 
  ggplot2::theme(axis.text.y = ggtext::element_markdown(colour = c(hcl.colors(2,palette = "Dark 3"),'black')))

## ----fig.height=7-------------------------------------------------------------
library(patchwork)
graph1 <- ci_graph(ajfit)
table1 <- ci_risk(ajfit) + 
  ggplot2::theme(axis.text.y = ggtext::element_markdown(colour = c(hcl.colors(2,palette = "Dark 3"),'black')))
graph1/table1 + plot_layout(heights = c(3,1)) +
  plot_annotation('This is a title',theme=ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)))

## -----------------------------------------------------------------------------
Melanoma2 <- 
  Melanoma %>% 
  mutate(status = as.factor(recode(status, `2` = 0, `1` = 1, `3` = 2)))
#Make the status a labeled factor
Melanoma2 <- Melanoma2 %>% mutate(status = factor(status,levels = c("0","1","2"),
                                                  labels = c("Censored","Melanoma","Other Causes"))) %>%
  mutate(sex = factor(sex,levels = c("0","1"), labels = c("Female","Male")))
#Define the model
ajfit <- survfit(Surv(time, status) ~sex, data = Melanoma2)

## -----------------------------------------------------------------------------
strat_ci_graph(ajfit)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
Melanoma <- MASS::Melanoma
library(survival)
library(dplyr)
Melanoma2 <- 
  Melanoma %>% 
  mutate(status = as.factor(recode(status, `2` = 0, `1` = 1, `3` = 2)))
#Make the status a labeled factor
Melanoma2 <- Melanoma2 %>% mutate(status = factor(status,levels = c("0","1","2"),
                                                  labels = c("Censored","Melanoma","Other Causes"))) %>%
  mutate(sex = factor(sex,levels = c("0","1"), labels = c("Female","Male")))

#Fit the models
ajfit <- survfit(Surv(time, status) ~1, data = Melanoma2)
ajfit_strata <- survfit(Surv(time, status) ~sex, data = Melanoma2)

## -----------------------------------------------------------------------------
#Create the tables for the specfic times
times2 <- gtsummary::tbl_survfit(ajfit, times = c(1000,2000,3000))
times2_strata  <- gtsummary::tbl_survfit(ajfit_strata, times = c(1000,2000,3000))

## -----------------------------------------------------------------------------
#Use the tbl_surv_to_flex function
xv <- ez.surv.viz::tbl_surv_to_flex(times2)
xv_strat <- ez.surv.viz::tbl_surv_to_flex(times2_strata)

#Combine in a list
comb_list <- list(xv,xv_strat)

## -----------------------------------------------------------------------------
stack_surv_flex(comb_list, label_names = c("Overall" = "All People"))

## -----------------------------------------------------------------------------
toyx <- data.frame(subj_id = c("1","2","3"), e_1 = c(1,4,8), e_1_2 = c(3,8,NA), e_2 = c(NA,6,NA),
                   e_3 = c(4,NA,6), e_4 = c(2,10,NA),
                   e_5 = c(10,2,14), e_6_at_4 = c(4,3,8), e_6_at_8 = c(4,7,9))

## -----------------------------------------------------------------------------
toyx_new <- toyx %>% tidyr::pivot_longer(cols = c(e_1,e_1_2), names_to = NULL, values_to = 'e_1')

## -----------------------------------------------------------------------------
base::suppressWarnings(print(timepoint_plot(toyx_new %>% dplyr::select(-c(e_6_at_4, e_6_at_8)), id = "subj_id")))

## -----------------------------------------------------------------------------
toyx_new_2 <- toyx_new %>% dplyr::rename("Event 1" = e_1, "Event 2" = e_2, "Cancer" = e_3, "Common Cold" = e_4, "Flu" = e_5)

base::suppressWarnings(print(timepoint_plot(toyx_new_2 %>% dplyr::select(-c(e_6_at_4, e_6_at_8)), id = "subj_id", col_palette = "Viridis",
               xlab_name = "Years", point_size = 4, line_size = 1)))

## -----------------------------------------------------------------------------
base::suppressWarnings(print(timepoint_plot(toyx_new_2 %>% dplyr::select(-c(e_6_at_4,e_6_at_8)),
                                            id = "subj_id") +
                              geom_point(data = toyx_new_2 %>% rename(id = "subj_id"),
                                aes(x = case_when(e_6_at_4 > 5 ~ 4, e_6_at_8 > 5 ~ 8, TRUE ~ NA),
  shape = case_when(e_6_at_4 > 5 ~ "Yep", e_6_at_8 > 5 ~ "Yep", TRUE ~ "NA")), size = 3,
                                          position = position_nudge(y = 0.19)) +
  scale_shape_manual(name = "Counts", values = c("Yep" = 25, "NA" = NA), limits = c("Yep"),
                                                  labels = c("Yep" = "Correct Counts"))))

