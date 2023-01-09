library(tidyverse)
library(brms)
library(lme4)

#### Filter down data for modeling ############################################
## load data
merged_df <- read.csv("data/AnjiniV_ThesisFiles_truncated_df.csv")

## cleaning
# convert to tibble
merged_df <- as_tibble(merged_df)

# clean up columns and filter data
merged_df <- merged_df %>% 
  select(-c(`X.1`:`X`)) %>% 
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


#### Load cleaned data #########################################################
data <- read.csv("data/data-cleaned.csv")

# make things factors and get rid of index column
data <- data %>% 
  select(-X) %>% 
  mutate(bugType = as.factor(bugType),
         projectName = as.factor(projectName),
         author_after = as.factor(author_after))


#### Some basic data exploration ###############################################
mean(data$number_of_commits_oldest)
sd(data$number_of_commits_oldest)

# in this case, mean does not equal variance, meaning we should use neg binomial
# distribution instead of Poisson due to overdispersion 

twoFix <- data %>% 
  filter(number_of_commits_oldest <= 2 )



#### Modeling ##################################################################

## Anjini models for reference
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

# model_oldestAfterEntropiesPred_commitLatencyOutcome = lmer((merged_df$number_of_commits_oldest) ~ merged_df$after_entropy + 
#                                                              merged_df$oldest_entropy + 
#                                                              ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$projectName) + 
#                                                              ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugType) + 
#                                                              ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugFilePath),
#                                                            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))


## simplified so I can actually read them
model_beforeAfterEntropiesPred_timeLatencyOutcome = lmer(log(latencies) ~ after_entropy.entropy + before_entropy.entropy + 
                                                           (1 | projectName) + 
                                                           (1 | bugType) + 
                                                           (1 | bugFilePath),
                                                         data = merged_df,
                                                         control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))




## probably need random intercepts by author, and also random intercepts by whether it was the same author given previous research
## why do we need random intercepts by bugFilePath? Kevin agrees to remove
## what about bug node length or fix node length?

# set sum coding
options(contrasts = c("contr.sum", "contr.sum"))

## first proposed model
model1 <- brm(number_of_commits_oldest ~ after_entropy.entropy * oldest_entropy.entropy +
                 (1 | projectName) +
                 (1 | bugType) +
                 (1 + same_before_after | author_after),
               data = data,
               chains = 4,
               cores = 2,
               family = negbinomial,
               warmup = 400,
               iter = 2000,
               thin = 10)
