context("resid_panel")

# Load libraries
library(lme4)
library(nlme)
library(vdiffr)

test_that("errors", {
  
  # data
  set.seed(20190321)
  example_data1 <- 
    data.frame(
      y = rpois(54, 3),
      trt = rep(c("A", "B"), each = 27),
      subject = rep(1:18, each = 3)
    )
  
  # Fit models
  lm_model1 <- lm(Volume ~ Girth, data = trees)
  glm_poisson_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)
  lmer_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)
  glmer_poisson_model <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data1)
  lme_model <- lme(weight ~ Time + Diet + Time*Diet, random = ~1|Chick, data = ChickWeight)

  # Check model Type error
  expect_error(resid_panel(resid_panel(rnorm(10))), label = "error - check_modeltype")

  # Check residual type error
  # expect_error(resid_panel(lm_model1, type = "deviance"), label = "error - check_residualtype - lm")
  # expect_error(resid_panel(glm_poisson_model, type = "standardized"), label = "error - check_residualtype - glm")
  # expect_error(resid_panel(lmer_model, type = "standardized"), label = "error - check_residualtype - lmer")
  # expect_error(resid_panel(glmer_poisson_model, type = "stand"), label = "error - check_residualtype - glmer")
  # expect_error(resid_panel(lme_model, type = "r"), label = "error - check_residualtype - lme")
  # 
  # # Check if calling for standardized residuals when random effects in model
  # expect_error(resid_panel(lmer_model, plots = "R"), label = "error - check_standardized - lmer")
  # expect_error(resid_panel(lme_model, plots = "ls"), label = "error - check_standardized - lme")
  # expect_error(resid_panel(glmer_model, plots = "lev"), label = "error - check_standardized - glmer")
  # 
  # # Check if calling for cookds D when unavailable for model type
  # expect_error(resid_panel(lmer_model, plots = c("cookd", "resid", "hist")), label = "error - check_cooksd - lmer")
  # expect_error(resid_panel(lme_model, plots = c("cookd")), label = "error - check_cooksd - lme")
  # expect_error(resid_panel(glmer_model, plots = c("boxplot", "cookd")), label = "error - check_cooksd - glmer")

})

# test_that("warnings", {
#   # lm model
#   lm_model1 <- lm(Volume ~ Girth, data = trees)
# 
#   # lm model with constant leverage
#   lm_model2 <- lm(weight ~ group, data = PlantGrowth)
# 
#   # Smoother
#   expect_warning(resid_panel(lm_model1, smoother = "T"), label = "warning - check_smoother")
# 
#   # check_theme
#   expect_warning(resid_panel(lm_model1, theme = "light"), label = "warning - check_theme")
# 
#   # check_title
#   expect_warning(resid_panel(lm_model1, title.opt = "F"), label = "warning - check_title")
# 
#   # check_leverage
#   expect_warning(resid_panel(lm_model2, plots = "R"), label = "warning - check_leverage")
# 
# })

test_that("formatting options", {

  # model
  lm_model1 <- lm(Volume ~ Girth, data = trees)

  expect_doppelganger(
    title = "formatting options - all - nrow = 1",
    fig = resid_panel(
      model = lm_model1,
      plots = c("hist", "qq", "index"),
      bins = 10,
      smoother = TRUE,
      qqline = FALSE,
      qqbands = TRUE,
      scale = 0.8,
      theme = "classic",
      axis.text.size = 6,
      title.text.size = 6,
      nrow = 1
    )
  )

  expect_doppelganger(
    title = "formatting options - all - nrow = 3",
    fig = resid_panel(
      lm_model1,
      plots = c("hist", "qq", "ls"),
      bins = 40,
      smoother = TRUE,
      qqline = FALSE,
      qqbands = TRUE,
      scale = 1,
      theme = "classic",
      axis.text.size = 8,
      title.text.size = 16,
      nrow = 3
    )
  )

})
# 
# test_that("panel and type options", {
# 
#   # model
#   lm_model1 <- lm(Volume ~ Girth, data = trees)
# 
#   # tests for user specified panels
#   expect_doppelganger(
#     title = "panel options - plots = c(boxplot, cookd)", 
#     fig = resid_panel(lm_model1, plots = c("boxplot", "cookd"))
#   )
# 
#   # tests for package prespecified panels
#   expect_doppelganger(
#     title = "panel options - no plots specified", 
#     fig = resid_panel(lm_model1)
#   )
#   expect_doppelganger(
#     title = "panel options - plots = default, pearson", 
#     fig = resid_panel(lm_model1, plots = "default", type = "pearson")
#   )
#   expect_doppelganger(
#     title = "panel options - plots = SAS", 
#     fig = resid_panel(lm_model1, plots = "SAS", type = "standardized")
#   )
#   expect_doppelganger(
#     title = "panel options - plots = R", 
#     fig = resid_panel(lm_model1, plots = "R", type = "response")
#   )
#   expect_doppelganger(
#     title = "panel options - plots = all", 
#     fig = resid_panel(lm_model1, plots = "all", type = "pearson")
#   )
# 
# })
# 
# # test_that("check constantlev plot", {
# # 
# #   # model
# #   lm_model2 <- lm(weight ~ group, data = PlantGrowth)
# # 
# #   # test for warning
# #   expect_warning(resid_panel(lm_model2, plots = "all"))
# # 
# #   # tests for plots
# #   expect_doppelganger(title = "lm with one categorical X - plots = all", fig = resid_panel(lm_model2, plots = "all"))
# # 
# # })
# 
# # test_that("glm poison", {
# # 
# #   # model
# #   glm_poisson_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)
# # 
# #   # test for warning
# #   expect_warning(resid_panel(glm_poisson_model, plots = "all"))
# # 
# #   # tests for plots
# #   expect_doppelganger(title = "glm poisson - plots = all", fig = resid_panel(glm_poisson_model, plots = "all"))
# #   #expect_doppelganger(title = "glm poisson - type = pearson", fig = resid_panel(glm_poisson_model, plots = "all", type = "pearson"))
# #   #expect_doppelganger(title = "glm poisson - type = deviance", fig = resid_panel(glm_poisson_model, plots = "all", type = "deviance"))
# #   #expect_doppelganger(title = "glm poisson - type = response", fig = resid_panel(glm_poisson_model, plots = "all", type = "response"))
# #   #expect_doppelganger(title = "glm poisson - type = stand.deviance", fig = resid_panel(glm_poisson_model, plots = "all", type = "stand.deviance"))
# #   #expect_doppelganger(title = "glm poisson - type = stand.pearson", fig = resid_panel(glm_poisson_model, plots = "all", type = "stand.pearson"))
# # 
# # })
# 
# test_that("glm binomial", {
# 
#   # model
#   glm_binomial_model <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)
# 
#   # tests for plots
#   expect_doppelganger(title = "glm binomial - plots = all", fig = resid_panel(glm_binomial_model, plots = "all"))
#   #expect_doppelganger(title = "glm binomial - type = pearson", fig = resid_panel(glm_binomial_model, plots = "all", type = "pearson"))
#   #expect_doppelganger(title = "glm binomial - type = deviance", fig = resid_panel(glm_binomial_model, plots = "all", type = "deviance"))
#   #expect_doppelganger(title = "glm binomial - type = response", fig = resid_panel(glm_binomial_model, plots = "all", type = "response"))
#   #expect_doppelganger(title = "glm binomial - type = stand.deviance", fig = resid_panel(glm_binomial_model, plots = "all", type = "stand.deviance"))
#   #expect_doppelganger(title = "glm binomial - type = stand.pearson", fig = resid_panel(glm_binomial_model, plots = "all", type = "stand.pearson"))
# 
# })
# 
# test_that("lmer", {
# 
#   # model
#   lmer_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)
# 
#   # tests for plots
#   expect_doppelganger(title = "lmer - plots = all", fig = resid_panel(lmer_model, plots = "all"))
#   #expect_doppelganger(title = "lmer - type = pearson", fig = resid_panel(lmer_model, plots = "all", type = "pearson"))
#   #expect_doppelganger(title = "lmer - type = response", fig = resid_panel(lmer_model, plots = "all", type = "response"))
# 
# })
# 
# test_that("lmerTest", {
# 
#   # model
#   library(lmerTest)
#   lmerTest_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)
# 
#   # tests for plots
#   expect_doppelganger(title = "lmerTest - plots = all", fig = resid_panel(lmerTest_model, plots = "all"))
# 
# })
# 
# test_that("lme", {
# 
#   # model
#   lme_model <- lme(weight ~ Time + Diet + Time*Diet, random = ~1|Chick, data = ChickWeight)
# 
#   # tests for plots
#   expect_doppelganger(title = "lme - plots = all", fig = resid_panel(lme_model, plots = "all"))
#   #expect_doppelganger(title = "lme - type = pearson", fig = resid_panel(lme_model, plots = "all", type = "pearson"))
#   #expect_doppelganger(title = "lme - type = response", fig = resid_panel(lme_model, plots = "all", type = "response"))
# 
# })
# 
# test_that("glmer_poisson", {
# 
#   # data
#   set.seed(20190321)
#   example_data1 <- data.frame(y = rpois(54, 3),
#                               trt = rep(c("A", "B"), each = 27),
#                               subject = rep(1:18, each = 3))
# 
#   # model
#   glmer_poisson_model <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data1)
# 
#   # tests for plots
#   expect_doppelganger(title = "glmer poisson - plots = all", fig = resid_panel(glmer_poisson_model, plots = "all"))
#   #expect_doppelganger(title = "glmer poisson - type = pearson", fig = resid_panel(glmer_poisson_model, plots = "all", type = "pearson"))
#   #expect_doppelganger(title = "glmer poisson - type = deviance", fig = resid_panel(glmer_poisson_model, plots = "all", type = "deviance"))
#   #expect_doppelganger(title = "glmer poisson - type = response", fig = resid_panel(glmer_poisson_model, plots = "all", type = "response"))
# 
# })
# 
# test_that("glmer_binomial", {
# 
#   # data
#   set.seed(20190321)
#   example_data2 <- data.frame(success = rpois(54, 5),
#                               trt = rep(c("A", "B"), each = 27),
#                               subject = rep(1:18, each = 3))
#   set.seed(20190321)
#   example_data2$total <-  example_data2$success+rpois(54,4)
# 
#   # model
#   glmer_binomial_model <- glmer(cbind(success, total-success) ~ trt + (1|subject), family = "binomial", data = example_data2)
# 
#   # tests for plots
#   expect_doppelganger(title = "glmer binomial - plots = all", fig = resid_panel(glmer_binomial_model, plots = "all"))
#   #expect_doppelganger(title = "glmer binomial - type = pearson", fig = resid_panel(glmer_binomial_model, plots = "all", type = "pearson"))
#   #expect_doppelganger(title = "glmer binomial - type = deviance", fig = resid_panel(glmer_binomial_model, plots = "all", type = "deviance"))
#   #expect_doppelganger(title = "glmer binomial - type = response", fig = resid_panel(glmer_binomial_model, plots = "all", type = "response"))
# 
# })
