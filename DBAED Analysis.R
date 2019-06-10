
# Author: Michael Gordon
# Purpose: Analysis for poster and working paper for Deliberation, Beleif Aggregation and Epistemic Democracy II



# Packages ----------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(ROCR)
library(pROC)
library(scoring)
library(s2dverification)
#library(tidyverse)



# Options -----------------------------------------------------------------
options(scipen = 999)


# Data Import -------------------------------------------------------------

#Pooled_Market_Data <- read_rds('Final Data/Pooled_Market_Data_common_core.rds')
Pooled_Market_Data <- read_rds('Final Data/Pooled_Market_Data_full_set.rds')

ssrp_demographics_raw <- read_csv('SSRP Demographics.csv')
eerp_demographics_raw <-  read_csv('EERP Demographics.csv')

# Data Collation ----------------------------------------------------------
outcome_data <- bind_rows(Pooled_Market_Data$SSRP$outcomes,Pooled_Market_Data$EERP$outcomes, Pooled_Market_Data$ML2$outcomes, Pooled_Market_Data$RPP$outcomes)

ssrp_survey <- Pooled_Market_Data$SSRP$survey %>% 
  filter(question_number == 3) %>% 
  mutate(response = 100- response)

survey_data <- bind_rows(Pooled_Market_Data$SSRP$survey,Pooled_Market_Data$EERP$survey, Pooled_Market_Data$ML2$survey, Pooled_Market_Data$RPP$survey) %>% 
  filter((project == 'ML2'& question_number== 2) | question_number == '1a' | question_number == '1'|question_number == 'a') %>% 
  filter(!(project == 'ML2'& question_number== 1)) %>% 
  filter(project != 'SSRP 3 Outcome Market') %>% 
  bind_rows(ssrp_survey) %>% 
  left_join(outcome_data)

market_data <- bind_rows(Pooled_Market_Data$SSRP$market,Pooled_Market_Data$EERP$market, Pooled_Market_Data$ML2$market, Pooled_Market_Data$RPP$market) %>%
  left_join(outcome_data)

t <- survey_data %>% 
  filter(project == 'RPP Stage 2', study_id == '2')
  


market_summary <- market_data %>% 
  group_by(project, study_id) %>% 
  summarise(market_belief = last(price),
            replicated = max(replicated)) %>% 
  mutate(market_belief_binary = round(market_belief,0)) %>% 
  mutate(correct_prediction_market = market_belief_binary == replicated) %>% 
  drop_na() 
# Survey Aggregation ------------------------------------------------------

# Survey Data needs some new columns to compute majority voting and mean cut off voting

survey_expanded <- survey_data %>% 
  mutate(response = case_when(
    project == 'EERP' ~ response,
    TRUE ~ response/100
  )) %>% 
  group_by(project, study_id) %>% 
  mutate(mean = mean(response, na.rm=T),
         median = median(response, na.rm = T)) %>% 
  ungroup() %>%
  group_by(project, user_id) %>% 
  mutate(user_mean = mean(response, na.rm = T)) %>% 
  mutate(majority_voting = ifelse(response > 0.5,1,0),
         mean_cut_off_voting = ifelse(response > user_mean, 1, 0)) %>% 
  ungroup() %>% 
  group_by(project, user_id) %>% 
  mutate(user_distributed = (response-min(response, na.rm = T))/(max(response, na.rm = T)-min(response, na.rm = T))) 
  


aggregation_survey <- survey_expanded %>% 
  group_by(project, study_id) %>% 
  summarise(mean = mean(response, na.rm = T),
            median = median(response, na.rm = T),
            majority_voting = sum(majority_voting, na.rm = T)/n(),
            mean_cut_off_voting = sum(mean_cut_off_voting, na.rm = T)/n(),
            user_normalised = mean(user_distributed, na.rm = T)) %>% 
  left_join(outcome_data) %>% 
  drop_na() %>% 
  mutate(group = 'mean_brier') %>% 
  left_join(market_summary)

cor.test(aggregation_survey$mean, aggregation_survey$market_belief, method=c("spearman"))

tmp <- aggregation_survey %>% 
  select(mean, replicated) %>% 
  mutate(error = abs(mean - replicated)^2) %>% 
  ungroup() %>% 
  summarise(mean = mean(error))
max(aggregation_survey$mean)

0.4227623^2

extreme <- aggregation_survey %>% 
  ungroup() %>% 
  select(mean, market_belief) %>% 
  mutate(mean = abs(0.5-mean),
         market_belief = abs(0.5-market_belief)) %>% 
  summarise_all(mean)

sum(aggregation_survey$correct_prediction_market)


75/103
replicated <- aggregation_survey %>% 
  filter(replicated == 1) %>% 
  sample_frac(0.3)

overll_accuracy <- aggregation_survey %>% 
  ungroup() %>% 
  mutate_at(vars(mean:user_normalised), list( ~round(.,0))) %>% 
  mutate_at(vars(mean:user_normalised), list( ~(.==replicated))) %>% 
  summarise_at(vars(mean:user_normalised), sum)

nonreplicated <- aggregation_survey %>% 
  filter(replicated == 0) 
unbalanced_survey_aggregation <- bind_rows(replicated, nonreplicated)

#aggregation_survey <- unbalanced_survey_aggregation

aggregations_summary_statistics <- aggregation_survey %>% 
  ungroup() %>% 
  mutate


b_mean <- BrierScore(aggregation_survey$replicated, aggregation_survey$mean)[1:4] %>% as_tibble()
b_median <- BrierScore(aggregation_survey$replicated, aggregation_survey$median)[1:4] %>% as_tibble()
b_majority_voting <- BrierScore(aggregation_survey$replicated, aggregation_survey$majority_voting)[1:4] %>% as_tibble()
b_mean_cut_off_voting <- BrierScore(aggregation_survey$replicated, aggregation_survey$mean_cut_off_voting)[1:4] %>% as_tibble()
b_user_normalised <- BrierScore(aggregation_survey$replicated, aggregation_survey$user_normalised)[1:4] %>% as_tibble()
b_market <- BrierScore(aggregation_survey$replicated, aggregation_survey$market_belief)[1:4] %>% as_tibble()



mean_spearman <- cor.test(aggregation_survey$replicated, aggregation_survey$mean, method=c("spearman"))
median_spearman <- cor.test(aggregation_survey$replicated, aggregation_survey$median, method=c("spearman"))
majority_voting_spearman <- cor.test(aggregation_survey$replicated, aggregation_survey$majority_voting, method=c("spearman"))
mean_cut_off_spearman <- cor.test(aggregation_survey$replicated, aggregation_survey$mean_cut_off_voting, method=c("spearman"))
user_normalised_spearman <- cor.test(aggregation_survey$replicated, aggregation_survey$user_normalised, method=c("spearman"))
market_spearman <- cor.test(aggregation_survey$replicated, aggregation_survey$market_belief, method=c("spearman"))

spearman_correlations <- c(mean_spearman$estimate, median_spearman$estimate, majority_voting_spearman$estimate, mean_cut_off_spearman$estimate,
                           user_normalised_spearman$estimate, market_spearman$estimate)

library('pROC')
a_mean <- auc(roc(aggregation_survey$replicated, aggregation_survey$mean))
a_median <- auc(roc(aggregation_survey$replicated, aggregation_survey$median))
a_majority_voting <- auc(roc(aggregation_survey$replicated, aggregation_survey$majority_voting))
a_mean_cut_off_voting <- auc(roc(aggregation_survey$replicated, aggregation_survey$mean_cut_off_voting))
a_user_normalised <- auc(roc(aggregation_survey$replicated, aggregation_survey$user_normalised))
a_market <- auc(roc(aggregation_survey$replicated, aggregation_survey$market_belief))

auc <- c(a_mean,a_median,a_majority_voting,a_mean_cut_off_voting,a_user_normalised,a_market)

wmw <- function(labels, scores){
  labels <- as.logical(labels)
  pos <- scores[labels]
  neg <- scores[!labels]
  U <- as.numeric(wilcox.test(pos, neg)$statistic)
  
}
 
w_mean <- wmw(aggregation_survey$replicated, aggregation_survey$mean)
w_median <- wmw(aggregation_survey$replicated, aggregation_survey$median)
w_majority_voting <- wmw(aggregation_survey$replicated, aggregation_survey$majority_voting)
w_mean_cut_off_voting <- wmw(aggregation_survey$replicated, aggregation_survey$mean_cut_off_voting)
w_user_normalised <- wmw(aggregation_survey$replicated, aggregation_survey$user_normalised)
w_market <- wmw(aggregation_survey$replicated, aggregation_survey$market_belief)

w <- c(w_mean,w_median,w_majority_voting,w_mean_cut_off_voting,w_user_normalised,w_market)

brier_scores <- bind_rows(b_mean, b_median, b_majority_voting, b_mean_cut_off_voting, b_user_normalised, b_market) %>% 
  mutate(aggregation = c('Mean', 'Median', 'Majority Voting', 'Mean Cut Off Voting', 'User Normalised', 'Prediction Market')) %>% 
  select(aggregation, reliability = rel, resolution = res, uncertainty = unc, brier_score = bs) %>% 
  mutate(Spearman_Correlations =spearman_correlations,
         AUC = auc,
         Wilcoxon_Mann_Whitney_U_test = w)

plot_data <- aggregation_survey %>% 
  ungroup() %>% 
  select(3:8,10) %>% 
  gather(-replicated,key = 'aggregation', value = 'beliefs' ) %>% 
  mutate(aggregation = case_when(
    aggregation == 'majority_voting' ~ 'Majority Voting', 
    aggregation == 'market_belief' ~ 'Prediction Market', 
    aggregation == 'mean' ~ 'Mean', 
    aggregation == 'median' ~ 'Median', 
    aggregation == 'mean_cut_off_voting' ~ 'Participant Mean Cut Off Voting', 
    aggregation == 'user_normalised' ~ 'Participant Normalized' 
    
  )) %>% 
  mutate(replicated = ifelse(replicated ==1, 'Successful Replication', 'Unsuccessful Replication')) 

ggplot(plot_data) +
  geom_density(data = filter(plot_data, replicated == 'Unsuccessful Replication'),aes(x=beliefs, colour = replicated)) +
  geom_density(data = filter(plot_data, replicated == 'Successful Replication'),aes(x=beliefs, colour = replicated) ) +
  scale_color_brewer(name = 'Replication Outcome',palette = "Set1",direction = -1)+
  facet_wrap(~aggregation)+
  #geom_histogram(aes(x=survey_belief))+
  xlab('Forecasted Probability of Replication') +
 
  ggtitle('Forecast Distribution for Successful and Unsuccessful Replications') +
  theme_minimal()+
  theme(axis.text.x =  element_text(angle = 90), 
        legend.position = 'bottom') 
  
# Demographics ------------------------------------------------------------

eerp_dem_formatted <- eerp_demographics_raw %>% 
  select(user_id = userid, 
         position = Position,
         years_academia = 'Years in Academia',
        # age = Age,
         gender = Gender,
         country = 'Country of Residence',
         fields = 'Reserach fields') %>% 
  mutate(project = 'EERP',
         years_academia = as.numeric(years_academia))

ssrp_dem_formatted <- ssrp_demographics_raw  %>% 
  select(user_id = uid, 
         position = d_positionPosition,
         years_academia = "d_sinceYearsinAcademiaafte",
         #age = Age,
         gender = d_genderGender,
         country = d_residence_1NoName,
         fields = "d_researchCoreFieldsofRese") %>% 
  mutate(project = 'SSRP 2 Outcome Market')

demographics <- bind_rows(eerp_dem_formatted, ssrp_dem_formatted) %>% 
  distinct() %>% 
  group_by(project, user_id) %>% 
  summarise_all(first)

survey_demographics <- survey_data %>% 
  left_join(demographics) %>% 
  mutate(response = case_when(
    project == 'EERP' ~ response,
    TRUE ~ response/100
  )) %>% 
 left_join(outcome_data)  

# Gender Model
survey_gender  <- survey_demographics %>% 
  filter(gender == 'Male' | gender == 'Female' ) %>% 
 mutate(brier_score = (replicated - response)^2) %>% 
  group_by(project, user_id, gender) %>% 
  summarise(brier_score = mean(brier_score)) %>% 
  mutate(male = ifelse(gender=='Male', 1,0)) %>% 
  ungroup() %>% 
  select(male, brier_score)



gender_model <- lm(brier_score ~ male, data = survey_gender)
summary(gender_model)

# Experience Model
survey_exp <- survey_demographics %>% 
  filter(!is.na(years_academia)) %>% 
  mutate(brier_score = (replicated - response)^2) %>% 
  group_by(project, user_id, years_academia) %>% 
  summarise(brier_score = mean(brier_score)) 

exp_model <- lm(brier_score ~ years_academia, data = survey_exp)
summary(exp_model)


# Combined Model 
survey_combined  <- survey_demographics %>% 
  filter(gender == 'Male' | gender == 'Female' ) %>% 
  filter(!is.na(years_academia)) %>% 
  mutate(brier_score = (replicated - response)^2) %>% 
  group_by(project, user_id, gender, years_academia) %>% 
  summarise(brier_score = sqrt(mean(brier_score))) %>% 
  mutate(male = ifelse(gender=='Male', 1,0)) %>% 
  ungroup() %>% 
  select(male, years_academia, brier_score)

combined_model <- lm(brier_score ~ ., data = survey_combined )
summary(combined_model)

# variance 

variance <- survey_demographics %>% 
  filter(gender == 'Male' | gender == 'Female' ) %>% 
  filter(!is.na(years_academia)) %>% 
  group_by(gender, user_id, project, years_academia) %>%
  summarise(variance = var(response)) %>% 
  mutate(male = ifelse(gender=='Male', 1,0)) %>% 
  ungroup() %>% 
  select(variance, male, years_academia)
  
variance_model <-  lm(variance ~ ., data= variance)
summary(variance_model)

# Optimism

optim <- survey_demographics  %>% 
  filter(gender == 'Male' | gender == 'Female' ) %>% 
  filter(!is.na(years_academia)) %>% 
  group_by(gender, user_id, project, years_academia) %>% 
  summarise(mean = mean(response, na.rm = T))

optim_model <- lm(mean ~ gender , data= optim)
summary(optim_model)
