context("resid_panel")

# Load libraries
library(lme4)
library(nlme)
library(vdiffr)

test_that("formatting options", {

  # model
  lm_model1 <- lm(Volume ~ Girth, data = trees)

  # tests for formatting options
  expect_doppelganger(title = "formatting options - bins = 10", fig = resid_panel(lm_model1, plots = "hist", bins = 10))
  expect_doppelganger(title = "formatting options - smoother = TRUE", fig = resid_panel(lm_model1, plots = "all", smoother = TRUE))
  expect_doppelganger(title = "formatting options - qqlines = FALSE", fig = resid_panel(lm_model1, plots = "qq", qqline = FALSE))
  expect_doppelganger(title = "formatting options - qqbands = TRUE", fig = resid_panel(lm_model1, plots = "qq", qqbands = TRUE))
  expect_doppelganger(title = "formatting options - scale = .8", fig = resid_panel(lm_model1, plots = "all", scale = .8))
  expect_doppelganger(title = "formatting options - theme = classic", fig = resid_panel(lm_model1, plots = "all", theme = "classic"))
  expect_doppelganger(title = "formatting options - theme = gray", fig = resid_panel(lm_model1, plots = "all", theme = "gray"))
  expect_doppelganger(title = "formatting options - theme = grey", fig = resid_panel(lm_model1, plots = "all", theme = "grey"))
  expect_doppelganger(title = "formatting options - theme = bw", fig = resid_panel(lm_model1, plots = "all", theme = "bw"))
  expect_doppelganger(title = "formatting options - axis.text.size = 6", fig = resid_panel(lm_model1, plots = "all", axis.text.size = 6))
  expect_doppelganger(title = "formatting options - title.text.size = 6", fig = resid_panel(lm_model1, plots = "all", title.text.size = 6))
  expect_doppelganger(title = "formatting options - title.opt = FALSE", fig = resid_panel(lm_model1, plots = "all", title.opt = FALSE))
  expect_doppelganger(title = "formatting options - nrow = 4", fig = resid_panel(lm_model1, plots = "all", nrow = 4))
  expect_doppelganger(title = "formatting options - nrow = 2", fig = resid_panel(lm_model1, plots = "all", nrow = 2))
  expect_doppelganger(title = "formatting options - nrow = 1", fig = resid_panel(lm_model1, plots = "all", nrow = 1))

})

test_that("panel options", {

  # model
  lm_model1 <- lm(Volume ~ Girth, data = trees)

  # tests for user specified panels
  expect_doppelganger(title = "panel options - plots = c(boxplot, cookd)", fig = resid_panel(lm_model1, plots = c("boxplot", "cookd")))
  expect_doppelganger(title = "panel options - plots = c(qq, ls, index)", fig = resid_panel(lm_model1, plots = c("qq", "ls", "index")))

  # tests for package prespecified panels
  expect_doppelganger(title = "panel options - no plots specified", fig = resid_panel(lm_model1))
  expect_doppelganger(title = "panel options - plots = default", fig = resid_panel(lm_model1, plots = "default"))
  expect_doppelganger(title = "panel options - plots = SAS", fig = resid_panel(lm_model1, plots = "SAS"))
  expect_doppelganger(title = "panel options - plots = R", fig = resid_panel(lm_model1, plots = "R"))
  expect_doppelganger(title = "panel options - plots = all", fig = resid_panel(lm_model1, plots = "all"))

})

test_that("lm with one continuous X", {

  # model
  lm_model1 <- lm(Volume ~ Girth, data = trees)

  # tests for individual plots
  expect_doppelganger(title = "lm with one continuous X - plots = boxplot", fig = resid_panel(lm_model1, plots = "boxplot"))
  expect_doppelganger(title = "lm with one continuous X - plots = cookd", fig = resid_panel(lm_model1, plots = "cookd"))
  expect_doppelganger(title = "lm with one continuous X - plots = index", fig = resid_panel(lm_model1, plots = "index"))
  expect_doppelganger(title = "lm with one continuous X - plots = hist", fig = resid_panel(lm_model1, plots = "hist"))
  expect_doppelganger(title = "lm with one continuous X - plots = lev", fig = resid_panel(lm_model1, plots = "lev"))
  expect_doppelganger(title = "lm with one continuous X - plots = ls", fig = resid_panel(lm_model1, plots = "ls"))
  expect_doppelganger(title = "lm with one continuous X - plots = qq", fig = resid_panel(lm_model1, plots = "qq"))
  expect_doppelganger(title = "lm with one continuous X - plots = resid", fig = resid_panel(lm_model1, plots = "resid"))
  expect_doppelganger(title = "lm with one continuous X - plots = yvp", fig = resid_panel(lm_model1, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "lm with one continuous X - type = pearson", fig = resid_panel(lm_model1, plots = "all", type = "pearson"))
  expect_doppelganger(title = "lm with one continuous X - type = response", fig = resid_panel(lm_model1, plots = "all", type = "response"))
  expect_doppelganger(title = "lm with one continuous X - type = standardized", fig = resid_panel(lm_model1, plots = "all", type = "standardized"))

})

test_that("lm with one categorical X", {

  # model
  lm_model2 <- lm(weight ~ group, data = PlantGrowth)

  # tests for individual plots
  expect_doppelganger(title = "lm with one categorical X - plots = boxplot", fig = resid_panel(lm_model2, plots = "boxplot"))
  expect_doppelganger(title = "lm with one categorical X - plots = cookd", fig = resid_panel(lm_model2, plots = "cookd"))
  expect_doppelganger(title = "lm with one categorical X - plots = index", fig = resid_panel(lm_model2, plots = "index"))
  expect_doppelganger(title = "lm with one categorical X - plots = hist", fig = resid_panel(lm_model2, plots = "hist"))
  expect_doppelganger(title = "lm with one categorical X - plots = lev", fig = resid_panel(lm_model2, plots = "lev"))
  expect_doppelganger(title = "lm with one categorical X - plots = ls", fig = resid_panel(lm_model2, plots = "ls"))
  expect_doppelganger(title = "lm with one categorical X - plots = qq", fig = resid_panel(lm_model2, plots = "qq"))
  expect_doppelganger(title = "lm with one categorical X - plots = resid", fig = resid_panel(lm_model2, plots = "resid"))
  expect_doppelganger(title = "lm with one categorical X - plots = yvp", fig = resid_panel(lm_model2, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "lm with one categorical X - type = pearson", fig = resid_panel(lm_model2, plots = "all", type = "pearson"))
  expect_doppelganger(title = "lm with one categorical X - type = response", fig = resid_panel(lm_model2, plots = "all", type = "response"))
  expect_doppelganger(title = "lm with one categorical X - type = standardized", fig = resid_panel(lm_model2, plots = "all", type = "standardized"))

})

test_that("lm with multiple continuous X and categorical X", {

  # data
  d <- ggplot2::diamonds[1:50,]
  d <- d[-which(d$cut == "Fair"),]
  d <- d[-which(d$color == "G"),]
  d <- d[-which(d$clarity == "I1"),]
  d <- d[-which(d$clarity == "VVS2"),]
  d <- d[-which(d$clarity == "VVS1"),]

  # model
  lm_model3 <- lm(price ~ carat + cut + color + depth, data = d)

  # tests for individual plots
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = boxplot", fig = resid_panel(lm_model3, plots = "boxplot"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = cookd", fig = resid_panel(lm_model3, plots = "cookd"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = index", fig = resid_panel(lm_model3, plots = "index"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = hist", fig = resid_panel(lm_model3, plots = "hist"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = lev", fig = resid_panel(lm_model3, plots = "lev"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = ls", fig = resid_panel(lm_model3, plots = "ls"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = qq", fig = resid_panel(lm_model3, plots = "qq"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = resid", fig = resid_panel(lm_model3, plots = "resid"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - plots = yvp", fig = resid_panel(lm_model3, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "lm with multiple continous X and categorical X - type = pearson", fig = resid_panel(lm_model3, plots = "all", type = "pearson"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - type = response", fig = resid_panel(lm_model3, plots = "all", type = "response"))
  expect_doppelganger(title = "lm with multiple continous X and categorical X - type = standardized", fig = resid_panel(lm_model3, plots = "all", type = "standardized"))

})

test_that("lm with multiple categorical X", {

  # data
  d <- ggplot2::diamonds[1:50,]
  d <- d[-which(d$cut == "Fair"),]
  d <- d[-which(d$color == "G"),]
  d <- d[-which(d$clarity == "I1"),]
  d <- d[-which(d$clarity == "VVS2"),]
  d <- d[-which(d$clarity == "VVS1"),]

  # model
  lm_model4 <- lm(price ~ cut + color + clarity, data = d)

  # tests for individual plots
  expect_doppelganger(title = "lm with multiple categorical X - plots = boxplot", fig = resid_panel(lm_model4, plots = "boxplot"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = cookd", fig = resid_panel(lm_model4, plots = "cookd"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = index", fig = resid_panel(lm_model4, plots = "index"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = hist", fig = resid_panel(lm_model4, plots = "hist"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = lev", fig = resid_panel(lm_model4, plots = "lev"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = ls", fig = resid_panel(lm_model4, plots = "ls"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = qq", fig = resid_panel(lm_model4, plots = "qq"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = resid", fig = resid_panel(lm_model4, plots = "resid"))
  expect_doppelganger(title = "lm with multiple categorical X - plots = yvp", fig = resid_panel(lm_model4, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "lm with multiple categorical X - type = pearson", fig = resid_panel(lm_model4, plots = "all", type = "pearson"))
  expect_doppelganger(title = "lm with multiple categorical X - type = response", fig = resid_panel(lm_model4, plots = "all", type = "response"))
  expect_doppelganger(title = "lm with multiple categorical X - type = standardized", fig = resid_panel(lm_model4, plots = "all", type = "standardized"))

})

test_that("glm poisson", {

  # model
  glm_poisson_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)

  # tests for individual plots
  expect_doppelganger(title = "glm poisson - plots = boxplot", fig = resid_panel(glm_poisson_model, plots = "boxplot"))
  expect_doppelganger(title = "glm poisson - plots = cookd", fig = resid_panel(glm_poisson_model, plots = "cookd"))
  expect_doppelganger(title = "glm poisson - plots = index", fig = resid_panel(glm_poisson_model, plots = "index"))
  expect_doppelganger(title = "glm poisson - plots = hist", fig = resid_panel(glm_poisson_model, plots = "hist"))
  expect_doppelganger(title = "glm poisson - plots = lev", fig = resid_panel(glm_poisson_model, plots = "lev"))
  expect_doppelganger(title = "glm poisson - plots = ls", fig = resid_panel(glm_poisson_model, plots = "ls"))
  expect_doppelganger(title = "glm poisson - plots = qq", fig = resid_panel(glm_poisson_model, plots = "qq"))
  expect_doppelganger(title = "glm poisson - plots = resid", fig = resid_panel(glm_poisson_model, plots = "resid"))
  expect_doppelganger(title = "glm poisson - plots = yvp", fig = resid_panel(glm_poisson_model, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "glm poisson - type = pearson", fig = resid_panel(glm_poisson_model, plots = "all", type = "pearson"))
  expect_doppelganger(title = "glm poisson - type = deviance", fig = resid_panel(glm_poisson_model, plots = "all", type = "deviance"))
  #expect_doppelganger(title = "glm poisson - type = response", fig = resid_panel(glm_poisson_model, plots = "all", type = "response"))
  expect_doppelganger(title = "glm poisson - type = stand.deviance", fig = resid_panel(glm_poisson_model, plots = "all", type = "stand.deviance"))
  expect_doppelganger(title = "glm poisson - type = stand.pearson", fig = resid_panel(glm_poisson_model, plots = "all", type = "stand.pearson"))

})

test_that("glm binomial", {

  # model
  glm_binomial_model <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)

  # tests for individual plots
  expect_doppelganger(title = "glm binomial - plots = boxplot", fig = resid_panel(glm_binomial_model, plots = "boxplot"))
  expect_doppelganger(title = "glm binomial - plots = cookd", fig = resid_panel(glm_binomial_model, plots = "cookd"))
  expect_doppelganger(title = "glm binomial - plots = index", fig = resid_panel(glm_binomial_model, plots = "index"))
  expect_doppelganger(title = "glm binomial - plots = hist", fig = resid_panel(glm_binomial_model, plots = "hist"))
  expect_doppelganger(title = "glm binomial - plots = lev", fig = resid_panel(glm_binomial_model, plots = "lev"))
  expect_doppelganger(title = "glm binomial - plots = ls", fig = resid_panel(glm_binomial_model, plots = "ls"))
  expect_doppelganger(title = "glm binomial - plots = qq", fig = resid_panel(glm_binomial_model, plots = "qq"))
  expect_doppelganger(title = "glm binomial - plots = resid", fig = resid_panel(glm_binomial_model, plots = "resid"))
  expect_doppelganger(title = "glm binomial - plots = yvp", fig = resid_panel(glm_binomial_model, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "glm binomial - type = pearson", fig = resid_panel(glm_binomial_model, plots = "all", type = "pearson"))
  expect_doppelganger(title = "glm binomial - type = deviance", fig = resid_panel(glm_binomial_model, plots = "all", type = "deviance"))
  #expect_doppelganger(title = "glm binomial - type = response", fig = resid_panel(glm_binomial_model, plots = "all", type = "response"))
  expect_doppelganger(title = "glm binomial - type = stand.deviance", fig = resid_panel(glm_binomial_model, plots = "all", type = "stand.deviance"))
  expect_doppelganger(title = "glm binomial - type = stand.pearson", fig = resid_panel(glm_binomial_model, plots = "all", type = "stand.pearson"))

})

test_that("lmer", {

  # model
  lmer_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)

  # tests for individual plots
  expect_doppelganger(title = "lmer - plots = boxplot", fig = resid_panel(lmer_model, plots = "boxplot"))
  expect_doppelganger(title = "lmer - plots = index", fig = resid_panel(lmer_model, plots = "index"))
  expect_doppelganger(title = "lmer - plots = hist", fig = resid_panel(lmer_model, plots = "hist"))
  expect_doppelganger(title = "lmer - plots = qq", fig = resid_panel(lmer_model, plots = "qq"))
  expect_doppelganger(title = "lmer - plots = resid", fig = resid_panel(lmer_model, plots = "resid"))
  expect_doppelganger(title = "lmer - plots = yvp", fig = resid_panel(lmer_model, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "lmer - type = pearson", fig = resid_panel(lmer_model, plots = "all", type = "pearson"))
  expect_doppelganger(title = "lmer - type = response", fig = resid_panel(lmer_model, plots = "all", type = "response"))

})

test_that("lmerTest", {

  # model
  library(lmerTest)
  lmerTest_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)

  # tests for individual plots
  expect_doppelganger(title = "lmerTest - plots = boxplot", fig = resid_panel(lmerTest_model, plots = "boxplot"))
  expect_doppelganger(title = "lmerTest - plots = index", fig = resid_panel(lmerTest_model, plots = "index"))
  expect_doppelganger(title = "lmerTest - plots = hist", fig = resid_panel(lmerTest_model, plots = "hist"))
  expect_doppelganger(title = "lmerTest - plots = qq", fig = resid_panel(lmerTest_model, plots = "qq"))
  expect_doppelganger(title = "lmerTest - plots = resid", fig = resid_panel(lmerTest_model, plots = "resid"))
  expect_doppelganger(title = "lmerTest - plots = yvp", fig = resid_panel(lmerTest_model, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "lmerTest - type = pearson", fig = resid_panel(lmerTest_model, plots = "all", type = "pearson"))
  expect_doppelganger(title = "lmerTest - type = response", fig = resid_panel(lmerTest_model, plots = "all", type = "response"))

})

test_that("lme", {

  # model
  lme_model <- lme(weight ~ Time + Diet + Time*Diet, random = ~1|Chick, data = ChickWeight)

  # tests for individual plots
  expect_doppelganger(title = "lme - plots = boxplot", fig = resid_panel(lme_model, plots = "boxplot"))
  expect_doppelganger(title = "lme - plots = index", fig = resid_panel(lme_model, plots = "index"))
  expect_doppelganger(title = "lme - plots = hist", fig = resid_panel(lme_model, plots = "hist"))
  expect_doppelganger(title = "lme - plots = qq", fig = resid_panel(lme_model, plots = "qq"))
  expect_doppelganger(title = "lme - plots = resid", fig = resid_panel(lme_model, plots = "resid"))
  expect_doppelganger(title = "lme - plots = yvp", fig = resid_panel(lme_model, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "lme - type = pearson", fig = resid_panel(lme_model, plots = "all", type = "pearson"))
  expect_doppelganger(title = "lme - type = response", fig = resid_panel(lme_model, plots = "all", type = "response"))

})

test_that("glmer_poisson", {

  # data
  set.seed(20190321)
  example_data1 <- data.frame(y = rpois(54, 3),
                              trt = rep(c("A", "B"), each = 27),
                              subject = rep(1:18, each = 3))

  # model
  glmer_poisson_model <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data1)

  # tests for individual plots
  expect_doppelganger(title = "glmer poisson - plots = boxplot", fig = resid_panel(glmer_poisson_model, plots = "boxplot"))
  expect_doppelganger(title = "glmer poisson - plots = index", fig = resid_panel(glmer_poisson_model, plots = "index"))
  expect_doppelganger(title = "glmer poisson - plots = hist", fig = resid_panel(glmer_poisson_model, plots = "hist"))
  expect_doppelganger(title = "glmer poisson - plots = qq", fig = resid_panel(glmer_poisson_model, plots = "qq"))
  expect_doppelganger(title = "glmer poisson - plots = resid", fig = resid_panel(glmer_poisson_model, plots = "resid"))
  expect_doppelganger(title = "glmer poisson - plots = yvp", fig = resid_panel(glmer_poisson_model, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "glmer poisson - type = pearson", fig = resid_panel(glmer_poisson_model, plots = "all", type = "pearson"))
  expect_doppelganger(title = "glmer poisson - type = deviance", fig = resid_panel(glmer_poisson_model, plots = "all", type = "deviance"))
  expect_doppelganger(title = "glmer poisson - type = response", fig = resid_panel(glmer_poisson_model, plots = "all", type = "response"))

})

test_that("glmer_poisson", {

  # data
  set.seed(20190321)
  example_data2 <- data.frame(success = rpois(54, 5),
                              trt = rep(c("A", "B"), each = 27),
                              subject = rep(1:18, each = 3))
  set.seed(20190321)
  example_data2$total <-  example_data2$success+rpois(54,4)

  # model
  glmer_binomial_model <- glmer(cbind(success, total-success) ~ trt + (1|subject), family = "binomial", data = example_data2)

  # tests for individual plots
  expect_doppelganger(title = "glmer binomial - plots = boxplot", fig = resid_panel(glmer_binomial_model, plots = "boxplot"))
  expect_doppelganger(title = "glmer binomial - plots = index", fig = resid_panel(glmer_binomial_model, plots = "index"))
  expect_doppelganger(title = "glmer binomial - plots = hist", fig = resid_panel(glmer_binomial_model, plots = "hist"))
  expect_doppelganger(title = "glmer binomial - plots = qq", fig = resid_panel(glmer_binomial_model, plots = "qq"))
  expect_doppelganger(title = "glmer binomial - plots = resid", fig = resid_panel(glmer_binomial_model, plots = "resid"))
  expect_doppelganger(title = "glmer binomial - plots = yvp", fig = resid_panel(glmer_binomial_model, plots = "yvp"))

  # tests for residual types
  expect_doppelganger(title = "glmer binomial - type = pearson", fig = resid_panel(glmer_binomial_model, plots = "all", type = "pearson"))
  expect_doppelganger(title = "glmer binomial - type = deviance", fig = resid_panel(glmer_binomial_model, plots = "all", type = "deviance"))
  expect_doppelganger(title = "glmer binomial - type = response", fig = resid_panel(glmer_binomial_model, plots = "all", type = "response"))

})
