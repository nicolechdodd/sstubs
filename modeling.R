library(tidyverse)
library(brms)

#### Filter down data for modeling ############################################
## load data
## NOTE: have to unlock data, not just save-as to another CSV, cause there's
## some weird corruption thing happening
merged_df <- read.csv("data/Anjini-data-unlocked.csv")

## cleaning
# convert to tibble
merged_df <- as_tibble(merged_df)

# clean up columns and filter data
merged_df <- merged_df %>% 
  select(-`X`) %>% 
  filter(number_of_commits_oldest > 0)

# get rid of unwanted columns
merged_df <- merged_df %>% 
  select(bugType, bugFilePath, projectName, bugNodeLength, fixNodeLength, 
         author_before:author_after, author_oldest_commit:oldest_entropy.entropy)


# create column for same or different fixing author
merged_df <- merged_df %>% 
  mutate(same_before_after = rep(0))

merged_df$same_before_after[merged_df$author_before == merged_df$author_after] <- 1

# reorganize to get author columns next to each other
merged_df <- merged_df %>% 
  select(bugType:author_after, same_before_after, author_oldest_commit:oldest_entropy.entropy)

# write to new file
write.csv(merged_df, "data/data-cleaned.csv")


## Load cleaned data
data <- read.csv("data/data-cleaned.csv")

# make things factors and get rid of pesky index column
data <- data %>% 
  select(-X) %>% 
  mutate(bugType = as.factor(bugType),
         projectName = as.factor(projectName),
         bugFilePath = as.factor(bugFilePath))


#### DATA EXPLORATION ##########################################################
mean(data$number_of_commits_oldest)
sd(data$number_of_commits_oldest)
var(data$number_of_commits_oldest)

## in this case, mean does not equal variance, meaning we should use neg binomial
## distribution instead of Poisson due to overdispersion

mean(data$latencies)
sd(data$latencies)
var(data$latencies)

## when log transforming, these numbers get a lot better
mean(log(data$latencies))
sd(log(data$latencies))
var(log(data$latencies))

## can use Gaussian distribution with log-transformed values


#### ANJINI MODELS for reference ###############################################

# model_beforeAfterEntropiesPred_timeLatencyOutcome = lmer(log(merged_df$latencies) ~ merged_df$after_entropy.entropy + 
#                                                            merged_df$before_entropy.entropy + 
#                                                            ((1+merged_df$after_entropy.entropy + merged_df$before_entropy.entropy ) || merged_df$projectName) + 
#                                                            ((1+merged_df$after_entropy.entropy + merged_df$before_entropy.entropy ) || merged_df$bugType) + 
#                                                            ((1+merged_df$after_entropy.entropy + merged_df$before_entropy.entropy ) || merged_df$bugFilePath),
#                                                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))

# model_oldestAfterEntropiesPred_timeLatencyOutcome = lmer(log(merged_df$latencies) ~ merged_df$after_entropy + 
#                                                            merged_df$oldest_entropy + 
#                                                            ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$projectName) + 
#                                                            ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugType) + 
#                                                            ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugFilePath), 
#                                                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))


# model_beforeAfterEntropiesPred_commitLatencyOutcome = lmer((merged_df$number_of_commits_oldest) ~ merged_df$after_entropy + 
#                                                              merged_df$before_entropy + 
#                                                              ((1+merged_df$after_entropy + merged_df$before_entropy ) || merged_df$projectName) + 
#                                                              ((1+merged_df$after_entropy + merged_df$before_entropy ) || merged_df$bugType) + 
#                                                              ((1+merged_df$after_entropy + merged_df$before_entropy ) || merged_df$bugFilePath), 
#                                                            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))

## this was the only one with significant effects
# model_oldestAfterEntropiesPred_commitLatencyOutcome = lmer((merged_df$number_of_commits_oldest) ~ merged_df$after_entropy + 
#                                                              merged_df$oldest_entropy + 
#                                                              ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$projectName) + 
#                                                              ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugType) + 
#                                                              ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugFilePath),
#                                                            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))




## talk with Emily:
## def keep bug file path because the bug could be in a particular file within a bigger project;
## not the same as project name
## definitely no random intercepts by author because that's insane
## should probably avoid SD cutoffs because the data are overdispersed


#### NEW MODELS ################################################################

# set sum coding and seed
options(contrasts = c("contr.sum", "contr.sum"))
set.seed(10)


#### MODEL 1: latencies by before and after entropy

## re-creating first Anjini model with brms
orig1 <- brm(log(latencies) ~ before_entropy.entropy + after_entropy.entropy +
               (1 + before_entropy.entropy + after_entropy.entropy || projectName) +
               (1 + before_entropy.entropy + after_entropy.entropy || bugType) +
               (1 + before_entropy.entropy + after_entropy.entropy || bugFilePath),
             data = data,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000,
             control = list(adapt_delta = 0.99))
# converged, no significant effects

## same model minus bugFilePath random intercepts (over 900 of them)
new1 <- brm(log(latencies) ~ before_entropy.entropy + after_entropy.entropy +
              (1 + before_entropy.entropy + after_entropy.entropy || projectName) +
              (1 + before_entropy.entropy + after_entropy.entropy || bugType),
            data = data,
            chains = 4,
            cores = 4,
            family = gaussian,
            warmup = 1000,
            iter = 6000,
            control = list(adapt_delta = 0.99))
# converged, no significant effects

## LOO COMPARISON
# using loo instead of waic per warning message about p_waic estimates being too high for some points
orig1 <- add_criterion(orig1, "loo")
new1 <- add_criterion(new1, "loo")

loo_compare(orig1, new1, criterion = "loo")
## better fit when including random intercepts by bugFilePath


## orig1 but with a predictor variable by same_author
orig1same <- brm(log(latencies) ~ before_entropy.entropy + after_entropy.entropy + same_before_after +
                   (1 + before_entropy.entropy + after_entropy.entropy || projectName) +
                   (1 + before_entropy.entropy + after_entropy.entropy || bugType) +
                   (1 + before_entropy.entropy + after_entropy.entropy || bugFilePath),
                 data = data,
                 chains = 4,
                 cores = 4,
                 family = gaussian,
                 warmup = 1000,
                 iter = 6000,
                 control = list(adapt_delta = 0.99))
# converged, significant effect for same_before_after

#### LOO COMPARISON all three models
orig1same <- add_criterion(orig1same, "loo")

loo_compare(orig1, new1, orig1same, criterion = "loo")
## best model was orig1same, including a predictor variable for same_before_after


## run one more model but including same_before_after as a random slope as well
orig1same2 <- brm(log(latencies) ~ before_entropy.entropy + after_entropy.entropy + same_before_after +
                    (1 + before_entropy.entropy + after_entropy.entropy + same_before_after || projectName) +
                    (1 + before_entropy.entropy + after_entropy.entropy + same_before_after || bugType) +
                    (1 + before_entropy.entropy + after_entropy.entropy + same_before_after || bugFilePath),
                  data = data,
                  chains = 4,
                  cores = 4,
                  family = gaussian,
                  warmup = 1000,
                  iter = 6000,
                  control = list(adapt_delta = 0.99))

#### LOO COMPARISON of orig1, orig1same, and orig1same2
orig1same2 <- add_criterion(orig1same2, "loo")

loo_compare(orig1, orig1same, orig1same2, criterion = "loo")
## best model was origsame2, which included random slopes by same_before_after




#### MODEL 2: latencies by oldest and after entropy

## re-creating second Anjini model with brms
orig2 <- brm(log(latencies) ~ oldest_entropy.entropy + after_entropy.entropy +
               (1 + oldest_entropy.entropy + after_entropy.entropy || projectName) +
               (1 + oldest_entropy.entropy + after_entropy.entropy || bugType) +
               (1 + oldest_entropy.entropy + after_entropy.entropy || bugFilePath),
             data = data,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000,
             control = list(adapt_delta = 0.99))
# converged, no significant effects

## creating new model using optimizations identified above
new2 <- brm(log(latencies) ~ oldest_entropy.entropy + after_entropy.entropy + same_before_after +
              (1 + oldest_entropy.entropy + after_entropy.entropy + same_before_after || projectName) +
              (1 + oldest_entropy.entropy + after_entropy.entropy + same_before_after || bugType) +
              (1 + oldest_entropy.entropy + after_entropy.entropy + same_before_after || bugFilePath),
            data = data,
            chains = 4,
            cores = 4,
            family = gaussian,
            warmup = 1000,
            iter = 6000,
            control = list(adapt_delta = 0.99))
# converged, significant effect for same_before_after


#### LOO COMPARISON of models
orig2 <- add_criterion(orig2, "loo")
new2 <- add_criterion(new2, "loo")

loo_compare(orig2, new2, criterion = "loo")
## best model was new2, which included random slopes by same_before_after


#### LOO COMPARISON of all latency models
loo_compare(orig1, orig1same2, orig2, new2, criterion = "loo")
## best model was new2 (oldest + after entropy), with orig1same2 (before + after entropy) as a close second





#### MODEL 3: number of commits by before and after entropy

## re-creating third Anjini model with brms
orig3 <- brm(number_of_commits_oldest ~ before_entropy.entropy + after_entropy.entropy +
               (1 + before_entropy.entropy + after_entropy.entropy || projectName) +
               (1 + before_entropy.entropy + after_entropy.entropy || bugType) +
               (1 + before_entropy.entropy + after_entropy.entropy || bugFilePath),
             data = data,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000,
             control = list(adapt_delta = 0.99))
# converged, no significant effects

## re-creating third Anjini model, but with negbinomial just for shits
orig3neg <- brm(number_of_commits_oldest ~ before_entropy.entropy + after_entropy.entropy +
                  (1 + before_entropy.entropy + after_entropy.entropy || projectName) +
                  (1 + before_entropy.entropy + after_entropy.entropy || bugType) +
                  (1 + before_entropy.entropy + after_entropy.entropy || bugFilePath),
                data = data,
                chains = 4,
                cores = 4,
                family = negbinomial,
                warmup = 1000,
                iter = 6000,
                control = list(adapt_delta = 0.99))
# converged, no significant effects

## creating new model using optimizations identified above
new3 <- brm(number_of_commits_oldest ~ before_entropy.entropy + after_entropy.entropy + same_before_after +
              (1 + before_entropy.entropy + after_entropy.entropy + same_before_after || projectName) +
              (1 + before_entropy.entropy + after_entropy.entropy + same_before_after || bugType) +
              (1 + before_entropy.entropy + after_entropy.entropy + same_before_after || bugFilePath),
            data = data,
            chains = 4,
            cores = 4,
            family = negbinomial,
            warmup = 1000,
            iter = 6000,
            control = list(adapt_delta = 0.99))
# converged, significant effect for same_before_after


#### LOO COMPARISON of models
orig3 <- add_criterion(orig3, "loo")
new3 <- add_criterion(new3, "loo")
orig3neg <- add_criterion(orig3neg, "loo")

loo_compare(orig3, new3, orig3neg, criterion = "loo")
## negbinomial distribution definitely improved the fit of the model, but the best one
## is the one that used negbinomial AND included a predictor variable for same_before_after


#### MODEL 4: number of commits by oldest and after entropy

## re-creating fourth Anjini model with brms
orig4 <- brm(number_of_commits_oldest ~ oldest_entropy.entropy + after_entropy.entropy +
               (1 + oldest_entropy.entropy + after_entropy.entropy || projectName) +
               (1 + oldest_entropy.entropy + after_entropy.entropy || bugType) +
               (1 + oldest_entropy.entropy + after_entropy.entropy || bugFilePath),
             data = data,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000,
             control = list(adapt_delta = 0.99))
# converged, no significant effects

## creating new model using optimizations identified above
new4 <- brm(number_of_commits_oldest ~ oldest_entropy.entropy + after_entropy.entropy + same_before_after +
              (1 + oldest_entropy.entropy + after_entropy.entropy + same_before_after || projectName) +
              (1 + oldest_entropy.entropy + after_entropy.entropy + same_before_after || bugType) +
              (1 + oldest_entropy.entropy + after_entropy.entropy + same_before_after || bugFilePath),
            data = data,
            chains = 4,
            cores = 4,
            family = negbinomial,
            warmup = 1000,
            iter = 6000,
            control = list(adapt_delta = 0.99))
# converged, significant effect for same_before_after

#### LOO COMPARISON of models
orig4 <- add_criterion(orig4, "loo")
new4 <- add_criterion(new4, "loo")

loo_compare(orig4, new4, criterion = "loo")
## best model is new4



#### LOO COMPARISON of all number of commits models
loo_compare(orig3, new3, orig4, new4, criterion = "loo")

## best model is new3, which models before and after entropy, not oldest and after entropy
## interesting that latencies were best modeled by oldest and after entropy, but
## number of commits were best modeled by before and after entropy
## NB: elpd_diff between before and oldest entropy models is small in both cases


#### SAVE MODELS ###############################################################
# write_rds(orig1, "models/original-loglatencies-before-after.RDS")
# write_rds(new1, "models/original-loglatencies-before-after-nofilepath.RDS")
# write_rds(orig1same2, "models/new-loglatencies-before-after-same.RDS")
# write_rds(orig2, "models/original-loglatencies-oldest-after.RDS")
# write_rds(new2, "models/new-loglatencies-oldest-after-same.RDS")
# write_rds(orig3, "models/original-commits-before-after.RDS")
# write_rds(orig3neg, "models/original-neg-commits-before-after.RDS")
# write_rds(new3, "models/new-commits-before-after-same.RDS")
# write_rds(orig4, "models/original-commits-oldest-after.RDS")
# write_rds(new4, "models/new-commits-oldest-after-same.RDS")
