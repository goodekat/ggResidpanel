# context("resid_auxpanel")
#
# test_that("resid_auxpanel", {
#
#   # model
#   lm_model1 <- lm(Volume ~ Girth, data = trees)
#
#   # tests for panels
#   expect_doppelganger(title = "auxpanel - default", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1)))
#   expect_doppelganger(title = "auxpanel - SAS", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "SAS"))
#   expect_doppelganger(title = "auxpanel - all", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "all"))
#
#   # tests for formatting options
#   expect_doppelganger(title = "auxpanel - bins = 50", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "hist", bins = 50))
#   expect_doppelganger(title = "auxpanel - smoother = TRUE", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "all", smoother = TRUE))
#   expect_doppelganger(title = "auxpanel - qqline = FALSE, qqbands = TRUE", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "qq", qqband = TRUE, qqline = FALSE))
#   expect_doppelganger(title = "auxpanel - scale = .8", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), scale = .8))
#   expect_doppelganger(title = "auxpanel - theme = classic", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), theme = "classic"))
#   expect_doppelganger(title = "auxpanel - theme = gray", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), theme = "gray"))
#   expect_doppelganger(title = "auxpanel - theme = grey", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), theme = "grey"))
#   expect_doppelganger(title = "auxpanel - theme = bw", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), theme = "bw"))
#   expect_doppelganger(title = "auxpanel - axis.text.size = 16, title.text.size = 20", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "all", axis.text.size = 16, title.text.size = 20))
#   expect_doppelganger(title = "auxpanel - title.opt = FALSE", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), title.opt = FALSE))
#   expect_doppelganger(title = "auxpanel - nrow = 3", fig = resid_auxpanel(resid(lm_model1), fitted(lm_model1), nrow = 3))
#
# })
#
#
# ######auxpanel#########
#
# resid_auxpanel(resid(glmer_model1), fitted(glmer_model1), plots="SAS")
#
# resid_auxpanel(resid(m4), fitted(m4), plots="SAS", title.opt=FALSE)
# ######sboxplot#########
#
# ######histogram#########
#
# resid_auxpanel(resid(lmer_model), fitted(lmer_model), plots="hist")
#
# ######residplot#########
#
# resid_auxpanel(resid(glm_model), fitted(glm_model), plots="resid")
#
# ######qq#########
#
# resid_auxpanel(resid(glmer_model2), fitted(glmer_model2), plots="qq", qqbands = TRUE)
