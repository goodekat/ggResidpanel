

glm_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)

library(lme4)
lmer_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)

library(lmerTest)
lmer_model2 <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)

example_data1 <- data.frame(y = rpois(54, 3),
                            trt = rep(c("A", "B"), each = 27),
                            subject = rep(1:18, each = 3))
glmer_poisson <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data1)

example_data2 <- data.frame(success = rpois(54, 5),
                            trt = rep(c("A", "B"), each = 27),
                            subject = rep(1:18, each = 3))
example_data2$total <-  example_data2$success+rpois(54,4)
glmer_binomial <- glmer(cbind(success, total-success) ~ trt + (1|subject), family = "binomial", data = example_data2)

# Check to see if throwing errors correctly
resid_panel(lmer_model, plots="R")
resid_panel(glmer_model1, plots="R")

#Check errors
resid_panel(lmer_model, plots="cookd")
resid_panel(glmer_model1, plots="cookd")


######auxpanel#########

resid_auxpanel(resid(glmer_model1), fitted(glmer_model1), plots="SAS")

resid_auxpanel(resid(m4), fitted(m4), plots="SAS", title.opt=FALSE)
######sboxplot#########

######histogram#########

resid_auxpanel(resid(lmer_model), fitted(lmer_model), plots="hist")

######residplot#########

resid_auxpanel(resid(glm_model), fitted(glm_model), plots="resid")

######qq#########

resid_auxpanel(resid(glmer_model2), fitted(glmer_model2), plots="qq", qqbands = TRUE)
