context("resid_panel")

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

# other models
# glm_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)
#
# library(lme4)
# lmer_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)
#
# library(lmerTest)
# lmer_model2 <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)
#
# example_data1 <- data.frame(y = rpois(54, 3),
#                             trt = rep(c("A", "B"), each = 27),
#                             subject = rep(1:18, each = 3))
# glmer_poisson <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data1)
#
# example_data2 <- data.frame(success = rpois(54, 5),
#                             trt = rep(c("A", "B"), each = 27),
#                             subject = rep(1:18, each = 3))
# example_data2$total <-  example_data2$success+rpois(54,4)
# glmer_binomial <- glmer(cbind(success, total-success) ~ trt + (1|subject), family = "binomial", data = example_data2)

