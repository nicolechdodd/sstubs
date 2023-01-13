library(lme4)

merged_df <- read.csv("data/Anjini-data-unlocked.csv")
merged_df <- merged_df[(merged_df$number_of_commits_oldest>0),]


model_beforeAfterEntropiesPred_timeLatencyOutcome=lmer(log(merged_df$latencies) ~ merged_df$after_entropy.entropy + merged_df$before_entropy.entropy + ((1+merged_df$after_entropy.entropy + merged_df$before_entropy.entropy ) || merged_df$projectName)+ ((1+merged_df$after_entropy.entropy + merged_df$before_entropy.entropy ) || merged_df$bugType) + ((1+merged_df$after_entropy.entropy + merged_df$before_entropy.entropy ) || merged_df$bugFilePath),control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000) ))

model_oldestAfterEntropiesPred_timeLatencyOutcome=lmer(log(merged_df$latencies) ~ merged_df$after_entropy + merged_df$oldest_entropy + ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$projectName)+ ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugType) + ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugFilePath),control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000) ))


model_beforeAfterEntropiesPred_commitLatencyOutcome=lmer((merged_df$number_of_commits_oldest) ~ merged_df$after_entropy + merged_df$before_entropy + ((1+merged_df$after_entropy + merged_df$before_entropy ) || merged_df$projectName)+ ((1+merged_df$after_entropy + merged_df$before_entropy ) || merged_df$bugType) + ((1+merged_df$after_entropy + merged_df$before_entropy ) || merged_df$bugFilePath),control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000) ))

model_oldestAfterEntropiesPred_commitLatencyOutcome=lmer((merged_df$number_of_commits_oldest) ~ merged_df$after_entropy + merged_df$oldest_entropy + ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$projectName)+ ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugType) + ((1+merged_df$after_entropy + merged_df$oldest_entropy ) || merged_df$bugFilePath),control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000) ))
