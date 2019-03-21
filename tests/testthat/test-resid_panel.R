context("resid_panel")

test_that("resid_panel - formatting options", {

  # model
  lm_model1 <- lm(Volume ~ Girth, data = trees)

  # tests for formatting options
  expect_doppelganger(title = "resid_panel - type = pearson", fig = resid_panel(lm_model1, plots = "all", type = "pearson"))
  expect_doppelganger(title = "resid_panel - type = standardized", fig = resid_panel(lm_model1, plots = "all", type = "standardized"))
  expect_doppelganger(title = "resid_panel - bins = 10", fig = resid_panel(lm_model1, plots = "hist", bins = 10))
  expect_doppelganger(title = "resid_panel - smoother = TRUE", fig = resid_panel(lm_model1, plots = "all", smoother = TRUE))
  expect_doppelganger(title = "resid_panel - qqlines = FALSE", fig = resid_panel(lm_model1, plots = "qq", qqline = FALSE))
  expect_doppelganger(title = "resid_panel - qqbands = TRUE", fig = resid_panel(lm_model1, plots = "qq", qqbands = TRUE))
  expect_doppelganger(title = "resid_panel - scale = .8", fig = resid_panel(lm_model1, scale = .8))
  expect_doppelganger(title = "resid_panel - theme = classic", fig = resid_panel(lm_model1, theme = "classic"))
  expect_doppelganger(title = "resid_panel - theme = gray", fig = resid_panel(lm_model1, theme = "gray"))
  expect_doppelganger(title = "resid_panel - theme = grey", fig = resid_panel(lm_model1, theme = "grey"))
  expect_doppelganger(title = "resid_panel - theme = bw", fig = resid_panel(lm_model1, theme = "bw"))
  expect_doppelganger(title = "resid_panel - axis.text.size = 6", fig = resid_panel(lm_model1, plots = "all", axis.text.size = 6))
  expect_doppelganger(title = "resid_panel - title.text.size = 6", fig = resid_panel(lm_model1, plots = "all", title.text.size = 6))
  expect_doppelganger(title = "resid_panel - title.opt = FALSE", fig = resid_panel(lm_model1, plots = "all", title.opt = FALSE))
  expect_doppelganger(title = "resid_panel - nrow = 4", fig = resid_panel(lm_model1, nrow = 4))
  expect_doppelganger(title = "resid_panel - nrow = 2", fig = resid_panel(lm_model1, plots = "all", nrow = 2))
  expect_doppelganger(title = "resid_panel - nrow = 1", fig = resid_panel(lm_model1, plots = c("cookd", "index", "qq"), nrow = 1))

})

test_that("resid_panel - lm with one continuous X", {

  # model
  lm_model1 <- lm(Volume ~ Girth, data = trees)

  # tests for one plot
  expect_doppelganger(title = "lm model1 - boxplot", fig = resid_panel(lm_model1, plots = "boxplot"))
  expect_doppelganger(title = "lm model1 - cookd", fig = resid_panel(lm_model1, plots = "cookd"))
  expect_doppelganger(title = "lm model1 - index", fig = resid_panel(lm_model1, plots = "index"))
  expect_doppelganger(title = "lm model1 - hist", fig = resid_panel(lm_model1, plots = "hist"))
  expect_doppelganger(title = "lm model1 - lev", fig = resid_panel(lm_model1, plots = "lev"))
  expect_doppelganger(title = "lm model1 - ls", fig = resid_panel(lm_model1, plots = "ls"))
  expect_doppelganger(title = "lm model1 - qq", fig = resid_panel(lm_model1, plots = "qq"))
  expect_doppelganger(title = "lm model1 - resid", fig = resid_panel(lm_model1, plots = "resid"))
  expect_doppelganger(title = "lm model1 - yvp", fig = resid_panel(lm_model1, plots = "yvp"))

  # tests for user specified panels
  expect_doppelganger(title = "lm model1 - vector1", fig = resid_panel(lm_model1, plots = c("boxplot", "cookd")))
  expect_doppelganger(title = "lm model1 - vector2", fig = resid_panel(lm_model1, plots = c("qq", "ls", "index")))

  # tests for package prespecified panels
  expect_doppelganger(title = "lm model1 - default panel", fig = resid_panel(lm_model1))
  expect_doppelganger(title = "lm model1 - SAS panel", fig = resid_panel(lm_model1, plots = "SAS"))
  expect_doppelganger(title = "lm model1 - R panel", fig = resid_panel(lm_model1, plots = "R"))
  expect_doppelganger(title = "lm model1 - all panel", fig = resid_panel(lm_model1, plots = "all"))

})

test_that("resid_panel - lm with one categorical X", {

  # model
  lm_model2 <- lm(weight ~ group, data = PlantGrowth)

  # tests for one plot
  expect_doppelganger(title = "lm model2 - boxplot", fig = resid_panel(lm_model2, plots = "boxplot"))
  expect_doppelganger(title = "lm model2 - cookd", fig = resid_panel(lm_model2, plots = "cookd"))
  expect_doppelganger(title = "lm model2 - index", fig = resid_panel(lm_model2, plots = "index"))
  expect_doppelganger(title = "lm model2 - hist", fig = resid_panel(lm_model2, plots = "hist"))
  expect_doppelganger(title = "lm model2 - lev", fig = resid_panel(lm_model2, plots = "lev"))
  expect_doppelganger(title = "lm model2 - ls", fig = resid_panel(lm_model2, plots = "ls"))
  expect_doppelganger(title = "lm model2 - qq", fig = resid_panel(lm_model2, plots = "qq"))
  expect_doppelganger(title = "lm model2 - resid", fig = resid_panel(lm_model2, plots = "resid"))
  expect_doppelganger(title = "lm model2 - yvp", fig = resid_panel(lm_model2, plots = "yvp"))

  # tests for user specified panels
  expect_doppelganger(title = "lm model2 - vector1 - two rows", fig = resid_panel(lm_model2, plots = c("boxplot", "cookd"), nrow = 2))
  expect_doppelganger(title = "lm model2 - vector2 - three rows", fig = resid_panel(lm_model2, plots = c("qq", "ls", "index"), qqbands = TRUE, nrow = 3))

  # tests for package prespecified panels
  expect_doppelganger(title = "lm model2 - default panel", fig = resid_panel(lm_model2))
  expect_doppelganger(title = "lm model2 - SAS panel", fig = resid_panel(lm_model2, plots = "SAS"))
  expect_doppelganger(title = "lm model2 - R panel", fig = resid_panel(lm_model2, plots = "R"))
  expect_doppelganger(title = "lm model2 - all panel", fig = resid_panel(lm_model2, plots = "all"))

})

test_that("resid_panel - lm with multiple continuous X and categorical X", {

  # data
  d <- diamonds[1:50,]
  d <- d[-which(d$cut == "Fair"),]
  d <- d[-which(d$color == "G"),]
  d <- d[-which(d$clarity == "I1"),]
  d <- d[-which(d$clarity == "VVS2"),]
  d <- d[-which(d$clarity == "VVS1"),]

  # model
  lm_model3 <- lm(price ~ carat + cut + color + depth, data = d)

  # tests for one plot
  expect_doppelganger(title = "lm model3 - boxplot", fig = resid_panel(lm_model3, plots = "boxplot"))
  expect_doppelganger(title = "lm model3 - cookd", fig = resid_panel(lm_model3, plots = "cookd"))
  expect_doppelganger(title = "lm model3 - index", fig = resid_panel(lm_model3, plots = "index"))
  expect_doppelganger(title = "lm model3 - hist", fig = resid_panel(lm_model3, plots = "hist"))
  expect_doppelganger(title = "lm model3 - lev", fig = resid_panel(lm_model3, plots = "lev"))
  expect_doppelganger(title = "lm model3 - ls", fig = resid_panel(lm_model3, plots = "ls"))
  expect_doppelganger(title = "lm model3 - qq", fig = resid_panel(lm_model3, plots = "qq"))
  expect_doppelganger(title = "lm model3 - resid", fig = resid_panel(lm_model3, plots = "resid"))
  expect_doppelganger(title = "lm model3 - yvp", fig = resid_panel(lm_model3, plots = "yvp"))

  # tests for package prespecified panels
  expect_doppelganger(title = "lm model3 - default panel", fig = resid_panel(lm_model3))
  expect_doppelganger(title = "lm model3 - SAS panel", fig = resid_panel(lm_model3, plots = "SAS"))
  expect_doppelganger(title = "lm model3 - R panel", fig = resid_panel(lm_model3, plots = "R"))
  expect_doppelganger(title = "lm model3 - all panel", fig = resid_panel(lm_model3, plots = "all"))

})

test_that("resid_panel - lm with multiple categorical X", {

  # data
  d <- diamonds[1:50,]
  d <- d[-which(d$cut == "Fair"),]
  d <- d[-which(d$color == "G"),]
  d <- d[-which(d$clarity == "I1"),]
  d <- d[-which(d$clarity=="VVS2"),]
  d <- d[-which(d$clarity=="VVS1"),]

  # model
  lm_model4 <- lm(price ~ cut + color + clarity, data = d)

  # tests for one plot
  expect_doppelganger(title = "lm model4 - boxplot", fig = resid_panel(lm_model4, plots = "boxplot"))
  expect_doppelganger(title = "lm model4 - cookd", fig = resid_panel(lm_model4, plots = "cookd"))
  expect_doppelganger(title = "lm model4 - index", fig = resid_panel(lm_model4, plots = "index"))
  expect_doppelganger(title = "lm model4 - hist", fig = resid_panel(lm_model4, plots = "hist"))
  expect_doppelganger(title = "lm model4 - lev", fig = resid_panel(lm_model4, plots = "lev"))
  expect_doppelganger(title = "lm model4 - ls", fig = resid_panel(lm_model4, plots = "ls"))
  expect_doppelganger(title = "lm model4 - qq", fig = resid_panel(lm_model4, plots = "qq"))
  expect_doppelganger(title = "lm model4 - resid", fig = resid_panel(lm_model4, plots = "resid"))
  expect_doppelganger(title = "lm model4 - yvp", fig = resid_panel(lm_model4, plots = "yvp"))

  # tests for package prespecified panels
  expect_doppelganger(title = "lm model4 - default panel", fig = resid_panel(lm_model4))
  expect_doppelganger(title = "lm model4 - SAS panel", fig = resid_panel(lm_model4, plots = "SAS"))
  expect_doppelganger(title = "lm model4 - R panel", fig = resid_panel(lm_model4, plots = "R"))
  expect_doppelganger(title = "lm model4 - all panel", fig = resid_panel(lm_model4, plots = "all"))

})
