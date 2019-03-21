context("resid_panel")

# one continuous X
test_that("lm model1", {

  # model
  lm_model1 <- lm(Volume ~ Girth, data = trees)

  # tests
  expect_doppelganger(title = "lm model1 - default panel", fig = resid_panel(lm_model1))
  expect_doppelganger(title = "lm model1 - SAS panel", fig = resid_panel(lm_model1, plots = "SAS"))
  expect_doppelganger(title = "lm model1 - R panel", fig = resid_panel(lm_model1, plots = "R"))

})

# one categorical X
test_that("lm model2", {

  # model
  lm_model2 <- lm(weight ~ group, data = PlantGrowth)

  # tests
  expect_doppelganger(title = "lm model2 - default panel", fig = resid_panel(lm_model2))
  expect_doppelganger(title = "lm model2 - SAS panel", fig = resid_panel(lm_model2, plots = "SAS"))
  expect_doppelganger(title = "lm model2 - R panel", fig = resid_panel(lm_model2, plots = "R"))

})

# multiple continuous X and categorical X
test_that("lm model3", {

  # data
  d <- diamonds[1:50,]
  d <- d[-which(d$cut == "Fair"),]
  d <- d[-which(d$color == "G"),]
  d <- d[-which(d$clarity == "I1"),]
  d <- d[-which(d$clarity == "VVS2"),]
  d <- d[-which(d$clarity == "VVS1"),]

  # model
  lm_model3 <- lm(price ~ carat + cut + color + depth, data = d)

  # tests
  expect_doppelganger(title = "lm model3 - default panel", fig = resid_panel(lm_model3))
  expect_doppelganger(title = "lm model3 - SAS panel", fig = resid_panel(lm_model3, plots = "SAS"))
  expect_doppelganger(title = "lm model3 - R panel", fig = resid_panel(lm_model3, plots = "R"))

})

# multiple categorical X
test_that("lm model3", {

  # data
  d <- diamonds[1:50,]
  d <- d[-which(d$cut == "Fair"),]
  d <- d[-which(d$color == "G"),]
  d <- d[-which(d$clarity == "I1"),]
  d <- d[-which(d$clarity=="VVS2"),]
  d <- d[-which(d$clarity=="VVS1"),]

  # model
  lm_model4 <- lm(price ~ cut + color + clarity, data = d)

  # tests
  expect_doppelganger(title = "lm model4 - default panel", fig = resid_panel(lm_model4))
  expect_doppelganger(title = "lm model4 - SAS panel", fig = resid_panel(lm_model4, plots = "SAS"))
  expect_doppelganger(title = "lm model4 - R panel", fig = resid_panel(lm_model4, plots = "R"))

})

# # All Panel
# resid_panel(lm_model1, plots = "all")
# resid_panel(lm_model2, plots = "all")
# resid_panel(lm_model3, plots = "all")
# resid_panel(lm_model4, plots = "all")
#
# # Vector of plots
# resid_panel(lm_model1, plots = c("boxplot", "cookd"))
# resid_panel(lm_model2, plots = c("boxplot", "cookd"), nrow = 2)
#
# resid_panel(lm_model1, plots = c("qq", "ls", "index"))
# resid_panel(lm_model2, plots = c("qq", "ls", "index"), qqbands = TRUE, nrow = 3)
#
# # Each individual plot
# resid_panel(lm_model1, plots = "boxplot")
# resid_panel(lm_model2, plots = "cookd")
# resid_panel(lm_model4, plots = "cookd")
#
# resid_panel(lm_model1, plots = "hist")
# resid_panel(lm_model1, plots = "index")
# resid_panel(lm_model2, plots = "ls")
# resid_panel(lm_model1, plots = "qq")
#
# resid_panel(lm_model1, plots = "lev")
# resid_panel(lm_model2, plots = "lev")
# # Contour lines do not show (does make sense since no points outside of .5 or 1)
# resid_panel(lm_model4, plots = "lev")
#
# resid_panel(lm_model1, plots = "resid")
#
# resid_panel(lm_model1, plots = "yvp")
#
#
# # Type
# resid_panel(lm_model1, plots = "all", type = "pearson")
# resid_panel(lm_model1, plots = "all", type = "standardized")
#
# # Bins
#
# resid_panel(lm_model1, plots = "all", bins = 20)
# resid_panel(lm_model4, plots = "hist", bins = 10)
#
# # smoother
#
# resid_panel(lm_model1, plots = "all", smoother = TRUE)
# resid_panel(lm_model2, plots = c("resid","index"), smoother = TRUE)
#
# # qqline, qqbands
#
# resid_panel(lm_model1, plots = "all", qqline = FALSE, qqbands = TRUE)
# resid_panel(lm_model2, plots = "qq", qqbands = TRUE)
#
# # Scale
#
# resid_panel(lm_model1, scale = .8)
# resid_panel(lm_model1, plots = "all", scale = .8)
# resid_panel(lm_model2, plots = "qq", scale = .5)
#
# # Theme
#
# resid_panel(lm_model1, theme = "classic")
# resid_panel(lm_model2, theme = "grey")
# resid_panel(lm_model3, plots = "all", theme = "bw")
#
# # axis.tex.size, title.text.size
#
# resid_panel(lm_model1, plots = "all", axis.text.size = 6, title.text.size = 6)
# resid_panel(lm_model2, plots = "SAS", axis.text.size = 18, title.text.size = 18)
#
# # title.opt
#
# resid_panel(lm_model1, plots = c("resid", "cookd", "index"), title.opt = FALSE)
# resid_panel(lm_model2, plots = "R", title.opt = FALSE)
#
# # nrow
#
# resid_panel(lm_model1, nrow = 4)
# resid_panel(lm_model2, plots = "all", nrow = 2)
# resid_panel(lm_model4, plots = c("cookd", "index", "qq"), nrow = 1)
