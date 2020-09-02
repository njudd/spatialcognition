# Training spatial cognition enhances mathematical learning - a randomized study in 17,000 children
# Nicholas Judd & Torkel Klingberg
# 02/09/2020

# This script will replicate the main finding using mixed effects models.

# important parts of the code for the strict cfa; missing fiml (there was very little missing data, see methods)
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

#### #### model building #### #### 
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











######## Diagnostics ######## 
# https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html#diagnostics
# normality of residuals isn't as important as people put it out to be (Gelman & Hill + Ben Blocker)
# influ <- influence.ME::influence(m4.2, "account_id") # data is too big for this function

# plot(m2.2); qqnorm(resid(m2.2)); qqline(resid(m2.2)) # not fantastic, yet not horrible since its platykurtotic
# plot_model(m2.2, type = "diag") # non-normality of residuals isn't horrible, see graph 3
# lattice::qqmath(m2.2, id = .05)

# need to work on these plots from Ben Blocker*
# colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
# grid.arrange(plot(cmod2_lme,type=c("p","smooth")),
#             plot(cmod2_lme,sqrt(abs(resid(.)))~fitted(.),
#      col=ifelse(mc2$Site=="Toolik, AK",colvec[1],colvec[2]),
#      type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
# ## "sqrt(abs(resid(x)))"),
# plot(cmod2_lme,resid(.,type="pearson")~cYear,
#      type=c("p","smooth")),
# qqnorm(cmod2_lme,abline=c(0,1),
#        col=ifelse(mc2$Site=="Toolik, AK",colvec[1],colvec[2])))

