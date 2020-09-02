# Training spatial cognition enhances mathematical learning - a randomized study in 17,000 children
# Nicholas Judd & Torkel Klingberg
# 02/09/2020
# nickkjudd@gmail.com


# This script will replicate the main finding using mixed effects models 
# it is not the original script of the paper and was recoded just to double check the finding
# this model is the bedrock of the paper therefore we thought it best to release it so people can check it out themselves
# if you find any errors please email: nickkjudd@gmail.com

#### #### #### #### #### #### ####
#### #### ## packages ## #### ####
#### #### #### #### #### #### ####

# mandatory
# install.packages("ggplot2"); install.packages("data.table"); install.packages("lmer")
# optional # install.packages("sjPlot"); install.packages("lattice");

# Notes on strict CFA; important parts of the code; missing fiml (there was very little missing data, see methods)
# fit_strict <- cfa(strict, data = tests, mimic = "mplus", estimator='mlr', missing='fiml')
# factor_scores <- as.data.frame(predict(fit_strict)) # getting factor scores
# factor_scores$t2 <- factor_scores$t2/sd(factor_scores$t1)
# factor_scores$t3 <- factor_scores$t3/sd(factor_scores$t1)
# factor_scores$t1 <- factor_scores$t1/sd(factor_scores$t1)



f <- fread("~/Projects/R_projects/spatialcognition/factor_data.csv")
str(f)

# things that should be dummy coded in the model
f$account_id <- as.character(f$account_id)
f$training_plan_id <- as.character(f$training_plan_id)
f$training_time <- as.character(f$training_time)

unique(f$time) # double checking time is coded 0, 4, 6
f$time <- as.numeric(as.character(f$time)) # making time numeric for random slopes

#### #### #### #### #### #### ####
#### #### model building #### ####
#### #### #### #### #### #### ####

# adding random intercept
m1.1 <- lmer(Math_Factor ~ (1 | account_id), f)
# adding random slope
m1.2 <- lmer(Math_Factor ~ (time | account_id), f)
# adding fixed effect of time
m1.3 <- lmer(Math_Factor ~ time + (time | account_id), f, REML = F) 
# adding age brackets
m1.4 <- lmer(Math_Factor ~ time + age + (time | account_id), f, REML = F) 
# adding cohort
m1.5 <- lmer(Math_Factor ~ time + age + cohort + (time | account_id), f, REML = F) 

# adding training time
m2.1 <- lmer(Math_Factor ~ time + age + cohort + training_time + (time | account_id), f, REML = F)
# adding the interaction of training time with math test improvement
m2.2 <- lmer(Math_Factor ~ time + age + cohort + training_time + training_time:time + (time | account_id), f, REML = F)

# comparing the AIC's
results_lmer_basemods <- AICcmodavg::aictab(c(m1.3, m1.4, m1.5, m2.1, m2.2), modnames = c('m1.3', 'm1.4', 'm1.5','m2.1', 'm2.2'))
results_lmer_basemods; # clearly showing the model fit imporving, yet these are all expect
# the models fail to converge, this is not an issue, we will come back to it.

# adding the fixed effects of different amounts of NVR and rotation
m3.1 <- lmer(Math_Factor ~ time + age + cohort + training_time + training_time:time + nvr_porportions + rotcombi_porportions + (time | account_id), f, REML = F)
# seeing how these effect math improvement
m3.2 <- lmer(Math_Factor ~ time + age + cohort + training_time + training_time:time + nvr_porportions + rotcombi_porportions + nvr_porportions:time + rotcombi_porportions:time + (time | account_id), f, REML = F)

# sjPlot::plot_model(m3.2, type = "diag") # looks a lot like m2.2 which is okay (not the end of the world diagnostics) More tasks &/or more timepoints would've helped
# lattice::qqmath(m3.2, id = .05)

results_spatialcognition <- AICcmodavg::aictab(c(m2.2, m3.1, m3.2), modnames = c('Default (inter)', 'nvr/rot fixed','nvr/rot interaction'))
results_spatialcognition;
# we see the interaction model as a much better fit than the default or the fixed effects

sjPlot::plot_model(m3.2) + theme_minimal()

# for the psychologists, dummy coded training plans
dcoded_tplan <- lmer(Math_Factor ~ time + training_plan_id + age + cohort + training_time + training_time:time + (time | account_id), f, REML = F)
dcoded_tplan_inter <- lmer(Math_Factor ~ time*training_plan_id + age + cohort + training_time + training_time:time + (time | account_id), f, REML = F)
AIC(dcoded_tplan_inter) - AIC(dcoded_tplan) # -28 AIC dif better
AIC(dcoded_tplan_inter) - AIC(m3.2) # a little bit worse 2 AIC, probably because of the unnecessary extra terms

# wait a secound!!! your mods are failing to converge

#### #### #### #### #### #### ####
#### convergence & diagnostics ####
#### #### #### #### #### #### ####

#### short story on convergence ####
# it is our coding of time, if we use 0, 1, 2; which is common in the literature the problem disappears

# f$time_recoded <- recode(f$time,`0` = 0L, `4` = 1L, `6` = 2L)
# m2.1_recodedT <- lmer(Math_Factor ~ time_recoded + training_time + age + cohort + (time_recoded | account_id), f, REML = F)
# m3.2_recodedT <- lmer(Math_Factor ~ time_recoded*rotcombi_porportions*nvr_porportions + age + cohort + training_time + (time_recoded | account_id), f, REML = F)
# and the 3) results stay the same
# AIC(m3.2_recodedT) - AIC(m2.1_recodedT)

#### long story on convergence; it was never actually a problem ####
# 1) convergence warnings in this case are not an issue according to Ben Blocker
# https://github.com/lme4/lme4/issues/120
# yet in some cases they can be!!! see: https://stats.stackexchange.com/questions/110004/how-scared-should-we-be-about-convergence-warnings-in-lme4
# relgrad2.1 <- with(m2.1@optinfo$derivs,solve(Hessian,gradient)); max(abs(relgrad2.1))
# relgrad4.2 <- with(m3.2@optinfo$derivs,solve(Hessian,gradient)); max(abs(relgrad3.2))

#### some diagnostics
# https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html#diagnostics
# normality of residuals isn't as important as people put it out to be (Gelman & Hill + Ben Blocker)
# influ <- influence.ME::influence(m4.2, "account_id") # data is too big for this function

# plot(m2.2); qqnorm(resid(m2.2)); qqline(resid(m2.2)) # not fantastic, yet not horrible since its platykurtotic
# plot_model(m2.2, type = "diag") # non-normality of residuals isn't horrible, see graph 3
# lattice::qqmath(m2.2, id = .05)

#plot_model(m3.2, type = "diag") # looks a lot like m2.2 which is okay (not the end of the world diagnostics) More tasks &/or more timepoints would've helped
# lattice::qqmath(m3.2, id = .05)
#plt_3wayinter <- plot_model(m3.2, type = "pred", terms = c("time", "rot [0, 1]", "nvr_dummy"), ci.lvl = NA, 
#                            legend.title = "Rotation Group")

# lastly because NVR has so little variance it could be argued it should be dummy coded instead
f$nvr_dummy <- f$nvr_porportions > 0

dcoded_nvr <- lmer(Math_Factor ~ time + age + cohort + training_time + training_time:time + nvr_dummy + rotcombi_porportions + (time | account_id), f, REML = F)
dcoded_nvr_inter <- lmer(Math_Factor ~ time + age + cohort + training_time + training_time:time + nvr_dummy + rotcombi_porportions + nvr_dummy:time + rotcombi_porportions:time + (time | account_id), f, REML = F)

AICcmodavg::aictab(c(m2.2, dcoded_nvr, dcoded_nvr_inter), modnames = c('Default (inter)', 'nvr/rot fixed (dummy)','nvr/rot interaction (dummy)'))
# result still holds



