# context("resid_interact")
#
# test_that("resid_interact - lm with one continuous X", {
#
#   # model
#   lm_model1 <- lm(Volume ~ Girth, data = trees)
#
#   # tests for package prespecified panels
#   expect_doppelganger(title = "interact - lm with one continuous X - default", fig = resid_interact(lm_model1))
#   expect_doppelganger(title = "interact - lm with one continuous X - all", fig = resid_interact(lm_model1, plots = "all"))
#   #expect_doppelganger(title = "interact - lm with one continuous X - R", fig = resid_interact(lm_model1, plots = "R"))
#   expect_doppelganger(title = "interact - lm with one continuous X - SAS", fig = resid_interact(lm_model1, plots = "SAS"))
#
#   # tests for individuals plots
#   expect_doppelganger(title = "interact - lm with one continuous X - boxplot", fig = resid_interact(lm_model1, plots = "boxplot"))
#   expect_doppelganger(title = "interact - lm with one continuous X - cookd", fig = resid_interact(lm_model1, plots = "cookd"))
#   expect_doppelganger(title = "interact - lm with one continuous X - hist", fig = resid_interact(lm_model1, plots = "hist"))
#   expect_doppelganger(title = "interact - lm with one continuous X - index", fig = resid_interact(lm_model1, plots = "index"))
#   expect_doppelganger(title = "interact - lm with one continuous X - lev", fig = resid_interact(lm_model1, plots = "lev"))
#   expect_doppelganger(title = "interact - lm with one continuous X - ls", fig = resid_interact(lm_model1, plots = "ls"))
#   expect_doppelganger(title = "interact - lm with one continuous X - qq", fig = resid_interact(lm_model1, plots = "qq"))
#   expect_doppelganger(title = "interact - lm with one continuous X - resid", fig = resid_interact(lm_model1, plots = "resid"))
#   expect_doppelganger(title = "interact - lm with one continuous X - yvp", fig = resid_interact(lm_model1, plots = "yvp"))
#
# })
#
# test_that("resid_interact - lm with one categorical X", {
#
#   # model
#   lm_model2 <- lm(weight ~ group, data = PlantGrowth)
#
#   # tests for package prespecified panels
#   expect_doppelganger(title = "interact - lm with one categorical X - default", fig = resid_interact(lm_model2))
#   expect_doppelganger(title = "interact - lm with one categorical X - all", fig = resid_interact(lm_model2, plots = "all"))
#   #expect_doppelganger(title = "interact - lm with one categorical X - R", fig = resid_interact(lm_model2, plots = "R"))
#   expect_doppelganger(title = "interact - lm with one categorical X - SAS", fig = resid_interact(lm_model2, plots = "SAS"))
#
#   # tests for individuals plots
#   expect_doppelganger(title = "interact - lm with one categorical X - boxplot", fig = resid_interact(lm_model2, plots = "boxplot"))
#   expect_doppelganger(title = "interact - lm with one categorical X - cookd", fig = resid_interact(lm_model2, plots = "cookd"))
#   expect_doppelganger(title = "interact - lm with one categorical X - hist", fig = resid_interact(lm_model2, plots = "hist"))
#   expect_doppelganger(title = "interact - lm with one categorical X - index", fig = resid_interact(lm_model2, plots = "index"))
#   expect_doppelganger(title = "interact - lm with one categorical X - lev", fig = resid_interact(lm_model2, plots = "lev"))
#   expect_doppelganger(title = "interact - lm with one categorical X - ls", fig = resid_interact(lm_model2, plots = "ls"))
#   expect_doppelganger(title = "interact - lm with one categorical X - qq", fig = resid_interact(lm_model2, plots = "qq"))
#   expect_doppelganger(title = "interact - lm with one categorical X - resid", fig = resid_interact(lm_model2, plots = "resid"))
#   expect_doppelganger(title = "interact - lm with one categorical X - yvp", fig = resid_interact(lm_model2, plots = "yvp"))
#
# })
