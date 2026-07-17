# Setup -------------------------------------------------------------------

library(haven)
allbus_2021_cda <- read_spss("./data/allbus_2021/ZA5280_v1-0-0.sav")

library(dplyr)
library(forcats)
library(sjlabelled)
allbus_2021_cda <-
  allbus_2021_cda %>%
  remove_all_labels() %>%
  mutate(
    sex = recode_factor(sex, `1` = "Male", `2` = "Female", `3` = "Non-binary"),
    region = recode_factor(
      eastwest, `1` = "Western Germany", `2` = "Eastern Germany"
    ),
    xenophobia = rowMeans(across(ma01b:ma04, ~as.numeric(.x))),
    trust = ifelse(st01 == 1, 1, 0),
    contact = rowSums(across(mc01:mc04, ~as.numeric(.x))),
    party = recode_factor(
      pv01,
      `1`= "CDU-CSU", `2`= "SPD", `3` = "FDP", `4` = "Gruene", `6` = "Linke",
      `42` = "AfD", `90` = "Other party", `91` = "Would not vote"),
    person_weight = as.numeric(wghtpew)
  )


# T-test example ----------------------------------------------------------
# Relationship between region in Germany (dummy) and xenophobia (continuous)
# t-test straightforward implemented in R
t.test(xenophobia ~ region, data = allbus_2021_cda)

?t.test
# Check assumptions
# We need to check if our dependent variable is normally distributed

# Shapiro-Wilk test for whole sample
shapiro.test(allbus_2021_cda$xenophobia)

# Shapiro-Wilk test for each region group
by(allbus_2021_cda$xenophobia, allbus_2021_cda$region, shapiro.test)

# The results show that the p-values of the Shapiro-Wilk test are smaller than .05, indicating that the
# normality assumption is violated for both gender groups.

# Visual inspection with quantile plots (qqplots)
library(tidyverse)
ggplot(allbus_2021_cda, aes(sample = xenophobia, colour = region)) +
  stat_qq() +
  stat_qq_line()+
  facet_wrap(~region)

# The dots appear to follow the line reasonably well but deviations are
# detectable. Visual inspection unclear but Shapiro-Wilk test is definitive.

# As we find no support for normality, we may have to use the independent
# samples t-test or the Mann-Whitney U test. The decision on which to choose
# depends on whether the variances are equal.
# We inspect this with Levene's test
library(car) # For Levene's test
leveneTest(allbus_2021_cda$xenophobia, allbus_2021_cda$region)
leveneTest(allbus_2021_cda$xenophobia, allbus_2021_cda$region, center=mean) ## By default, the test utilizes
# the median. We can switch to mean with the option "center=mean"

# The high F-value (F) suggests that we can reject the null hypothesis
# that the population variances are equal. Because we obtained evidence that the variances
# are not equal, we should utilize the Wilcoxon/Mann-Whitney test.
wilcox.test(xenophobia ~ region, data = allbus_2021_cda)

# Furthermore, we can calculate the effect size: Cohen's D
library(lsr) # several useful functions, we use it for calculating Cohen's D
cohensD(xenophobia ~ region, data=allbus_2021_cda)
# How to interpret Cohen's D: A d of 1 indicates that the group means differ by
# 1 standard deviation; A d of of 2 -> differ by 2 standard deviations, and so on...
# So an effect size of 0.5 means the value of the average person in group 1 is 0.5
# standard deviations above the average person in group 2.

# Rule of thumb conventions:
# <0.2 negligible
# 0.2 = small effect
# 0.5 = medium effect size
# 0.8 >= large effect


# ANOVA example -----------------------------------------------------------

# But before we apply ANOVA, we should check first whether assumptions are fulfilled

# Check assumptions -------------------------------------------------------

# Next, we need to check if our dependent variable is normally distributed

# Shapiro-Wilk test for whole sample
shapiro.test(allbus_2021_cda$xenophobia)

# Shapiro-Wilk test for each group
by(allbus_2021_cda$xenophobia, allbus_2021_cda$party, shapiro.test)
# The results show that the p-values of the Shapiro-Wilk test are smaller than .05, indicating that the
# normality assumption is violated for all three status groups.

# Visual inspection with quantile plots (qqplots)
ggplot(allbus_2021_cda, aes(sample = xenophobia, colour = party)) +
  stat_qq() +
  stat_qq_line()+
  facet_wrap(~party)

# The dots appear to follow the line reasonably well but deviations are
# detectable. Visual inspection unclear but Shapiro-Wilk test is definitive.

# As we find no support for normality, we may have to use the a one-way ANOVA F-test when
# the variances are equal or a Kruskal-Wallis rank test when the variances are not equal.
# So the decision on which to choose depends on whether the variances are equal.
# We inspect this with Levene's test
leveneTest(allbus_2021_cda$xenophobia, allbus_2021_cda$party)
leveneTest(allbus_2021_cda$xenophobia, allbus_2021_cda$party, center=mean) ## By default, the test utilizes
# the median. We can switch to mean with the option "center=mean"

# The Levene's test statistic clearly suggests that the population variances are not equal.

# Calculate test statistic and effect size --------------------------------

# Instead of using ANOVA, we should utilize Kruskal-Wallis rank test
# In order to exemplify process for ANOVA, we will ignore this for a moment and go through
# ANOVA. Implementation of Kruskal-Wallis test is very similar and documented for example
# here: https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#google_vignette

# We will use the aov-function to calculate the test
anova <- aov(formula = xenophobia ~ party,
             data = allbus_2021_cda)

summary(anova)
# R doesn't use the terms "between-group" and "within-group". The between groups variance
# corresponds to the effect that the party affiliation has on the outcome variable and the within
# groups variance corresponds to the ``leftover'' variability, so it calls that the
# residuals.
# Between group sum of squares SSb = 1135
# Within group sum of squares SSw = 3844

# The model has an F-value of 103.8. We also learn that the p-value is smaller than the
# significance level (0.001). Therefore,
# we can reject the null hypothesis that there is no difference
# in xenophobia between groups and conclude that xenophobia differs significantly
# across at least two of the groups.

# Post-hoc test to identify which groups differ
# The textbooks suggest various post hoc tests depending on whether population variances
# and sample sizes per group differ. The available post-hoc tests in SPPS exceed the
# available approaches in R. For our purposes, we can use the widely used Holm
# correction or the conservative Tukey's honestly significant difference test (Tukey's HSD)

# We use the posthocPairwiseT-function from the lsr-package
posthocPairwiseT(anova, p.adjust.method="holm")

# Alternative TukeyHSD
TukeyHSD(anova)
par(mai=c(1.5,2,1,1))
plot(TukeyHSD(anova, conf.level=.95, ordered=TRUE), las = 1)
dev.off()
# Furthermore, we can calculate the effect size.
# For this, we use the etaSquared-function from the lsr-package
etaSquared(x=anova)
# The first output corresponds to the eta-squared statistic. The second one is a partial
# eta-squared and only relevant for more complicated ANOVAs (not covered in this course).

# Remember, the eta-squared is the between sum of squares divided by the total sum of squares.
# The interpretation is straightforward: it refers to the proportion of the variability
# in the outcome variable than can be explained in terms of the predictor.
# Eta-squared is closely related to the squared correlation and the square-root of eta-squared
# can be interpreted as if it referred to the magnitude of a Pearson correlation.
sqrt(0.2280118)

## 0.478 is a moderate effect.


# Simple linear regression ------------------------------------------------

simple_linear_model <-
  lm(
    xenophobia ~ age + sex + region + trust + contact + party,
    data = allbus_2021_cda
  )

summary(simple_linear_model)

contrasts(allbus_2021_cda$party)

# Relevel within the variable
lm_relevel <-
  lm(
    xenophobia ~ age + sex + region + trust + contact + relevel(party, ref = 6),
    data = allbus_2021_cda
  )
summary(lm_relevel)

#
weighted_regression <-
  lm(
    xenophobia ~ age + sex + trust + contact + relevel(party, ref = 6),
    weights = person_weight,
    data = allbus_2021_cda
  )
summary(weighted_regression)


# GLMs --------------------------------------------------------------------
table(allbus_2021_cda$trust)

# Logit model
simple_model_logistic <-
  glm(
    trust ~ age + sex + region + contact + party,
    family = binomial(link = "logit"),
    data = allbus_2021_cda
  )

summary(simple_model_logistic)

?glm

# Assessing model quality
library(performance)
r2_nagelkerke(simple_model_logistic)

# Probit instead of logit
simple_model_probit <-
  glm(
    trust ~ age + sex + region + contact + party,
    family = binomial(link = "probit"),
    data = allbus_2021_cda
  )
summary(simple_model_probit)

# Compare models
anova(simple_model_logistic, simple_model_probit)

# use performance package to compare more thoroughly
compare_performance(
  simple_model_logistic, simple_model_probit,
  metrics = c("AIC", "BIC", "R2")
)

# Access model results
names(simple_linear_model)

simple_linear_model$coefficients

summary(simple_linear_model)$coefficients[,2]

confint(simple_linear_model)

library(parameters)
model_parameters(simple_linear_model)


# Interactions ------------------------------------------------------------
library(interactions)
library(viridis)
interaction_model <- lm(xenophobia ~ age*region, data=allbus_2021_cda)
summary(interaction_model)

interact_plot(interaction_model, pred = age, modx = region,
                          interval=TRUE, x.label="Age",
                          y.label="Predicted xenophobia",
                          colors=viridis(n=2, direction=-1), rug=FALSE, jitter=.5)


# Predictions -------------------------------------------------------------


# Predictions with linear model
simple_linear_model_new <-
  lm(xenophobia ~ age + region + party, data = allbus_2021_cda)
predictions_data <-
  data.frame(
    age = rep(mean(allbus_2021_cda$age, na.rm = TRUE), times = 4),
    region = as.factor(rep(c("Western Germany", "Eastern Germany"), each = 2)),
    party = as.factor(rep(c("CDU-CSU", "AfD"), times = 2))
  )
predictions_data

predict(object = simple_linear_model_new,
        newdata = predictions_data,
        interval = "confidence")

# Predictions with logit model
simple_model_logistic_new <-
  glm(
    trust ~ age + region + party,
    family = binomial(link = "logit"),
    data = allbus_2021_cda
  )

summary(simple_model_logistic_new)

predictions <-
  predict(object = simple_model_logistic_new, newdata = predictions_data)

# Predictions have to be converted into probabilities = predicted probabilities
exp(predictions) / (1 + exp(predictions))

# Average Marginal Effects
library(marginaleffects)
plot_cme(
  simple_model_logistic_new,
  variables = "region",
  condition = "party"
)+
  ylab("Marginal effect of region on trust")


# Regression output
library(stargazer)
reg_1 <- lm(xenophobia ~ age + sex + region, data = allbus_2021_cda)
reg_2 <- lm(xenophobia ~ age + sex + region + trust, data = allbus_2021_cda)
reg_3 <- glm(trust ~ age + sex + region, data = allbus_2021_cda, family = binomial(link = "logit"))
reg_4 <- glm(trust ~ age + sex + region, data = allbus_2021_cda, family = binomial(link = "probit"))
stargazer(
  reg_1, reg_2, reg_3, reg_4,
  type = "latex",
  dep.var.labels = c("Xenophobia", "Trust"),
  covariate.labels =
    c("Age", "Female", "Non-binary", "Region (Reference = Western Germany", "Social Trust")
)

library(huxtable)
huxreg_table <-
  huxreg(
    reg_1, reg_2, reg_3, reg_4,
    coefs = c(
      "(Intercept)", "Age" = "age", "Female (Reference: Male)" = "sexFemale",
      "Non-binary (Reference: Male)" = "sexNon-binary",
      "Eastern Germany (Reference: Western Germany)" = "regionEastern Germany",
      "Trust" = "trust"
    ),
    statistics = c("N" = "nobs", "R squared" = "r.squared", "BIC")
  )
huxreg_table
?huxreg

library(report)
t.test(xenophobia ~ region, data = allbus_2021_cda) %>%
  report()

library(broom)
tidy(simple_linear_model)

augment(simple_linear_model)
