######Create one of each model type###########################################
library(ggplot2)
#lm_model1
#lm_model2
#glm_model
#lmer_model
#glmer_model1
#glmer_model2


# Linear mmodel #############################################################

# One continuous X
lm_model1 <- lm(Volume ~ Girth, data = trees)
# One categorical X
lm_model2 <- lm(weight ~ group, data = PlantGrowth)

d <- diamonds[1:50,]
d <- d[-which(d$cut=="Fair"),]
d <- d[-which(d$color=="G"),]
d <- d[-which(d$clarity=="I1"),]
d <- d[-which(d$clarity=="VVS2"),]
d <- d[-which(d$clarity=="VVS1"),]

# Multiple continuous X and categorical X
lm_model3 <- lm(price ~ carat + cut + color + depth, data = d)
# Multiple categorical X
lm_model4 <- lm(price ~ cut + color + clarity, data = d)


# Default panel
resid_panel(lm_model1)
resid_panel(lm_model2)
resid_panel(lm_model3)
resid_panel(lm_model4)

# SAS Panel
resid_panel(lm_model1, plots = "SAS")
resid_panel(lm_model2, plots = "SAS")
resid_panel(lm_model3, plots = "SAS")
resid_panel(lm_model4, plots = "SAS")

# R Panel
resid_panel(lm_model1, plots = "R")
resid_panel(lm_model2, plots = "R")
resid_panel(lm_model3, plots = "R")
resid_panel(lm_model4, plots = "R")


# All Panel
resid_panel(lm_model1, plots = "all")
resid_panel(lm_model2, plots = "all")
resid_panel(lm_model3, plots = "all")
resid_panel(lm_model4, plots = "all")

# Vector of plots
resid_panel(lm_model1, plots = c("boxplot", "cookd"))
resid_panel(lm_model2, plots = c("boxplot", "cookd"), nrow = 2)

resid_panel(lm_model1, plots = c("qq", "ls", "index"))
resid_panel(lm_model2, plots = c("qq", "ls", "index"), qqbands = TRUE, nrow = 3)

# Each individual plot
resid_panel(lm_model1, plots = "boxplot")
resid_panel(lm_model2, plots = "cookd")
resid_panel(lm_model4, plots = "cookd")

resid_panel(lm_model1, plots = "hist")
resid_panel(lm_model1, plots = "index")
resid_panel(lm_model2, plots = "ls")
resid_panel(lm_model1, plots = "qq")

resid_panel(lm_model1, plots = "lev")
resid_panel(lm_model2, plots = "lev")
# Contour lines do not show (does make sense since no points outside of .5 or 1)
resid_panel(lm_model4, plots = "lev")

resid_panel(lm_model1, plots = "resid")

resid_panel(lm_model1, plots = "yvp")


# Type
resid_panel(lm_model1, plots = "all", type = "pearson")
resid_panel(lm_model1, plots = "all", type = "standardized")

# Bins

resid_panel(lm_model1, plots = "all", bins = 20)
resid_panel(lm_model4, plots = "hist", bins = 10)

# smoother

resid_panel(lm_model1, plots = "all", smoother = TRUE)
resid_panel(lm_model2, plots = c("resid","index"), smoother = TRUE)

# qqline, qqbands

resid_panel(lm_model1, plots = "all", qqline = FALSE, qqbands = TRUE)
resid_panel(lm_model2, plots = "qq", qqbands = TRUE)

# Scale

resid_panel(lm_model1, scale = .8)
resid_panel(lm_model1, plots = "all", scale = .8)
resid_panel(lm_model2, plots = "qq", scale = .5)

# Theme

resid_panel(lm_model1, theme = "classic")
resid_panel(lm_model2, theme = "grey")
resid_panel(lm_model3, plots = "all", theme = "bw")

# axis.tex.size, title.text.size

resid_panel(lm_model1, plots = "all", axis.text.size = 6, title.text.size = 6)
resid_panel(lm_model2, plots = "SAS", axis.text.size = 18, title.text.size = 18)

# title.opt

resid_panel(lm_model1, plots = c("resid", "cookd", "index"), title.opt = FALSE)
resid_panel(lm_model2, plots = "R", title.opt = FALSE)

# nrow

resid_panel(lm_model1, nrow = 4)
resid_panel(lm_model2, plots = "all", nrow = 2)
resid_panel(lm_model4, plots = c("cookd", "index", "qq"), nrow = 1)


### Resid_auxpanel


# Panels

resid_auxpanel(resid(lm_model1), fitted(lm_model1))
resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots="SAS")
resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots="all")

# Options

resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "hist", bins = 50)
resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = c("resid", "index"), smoother = TRUE)
resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = "qq", qqband = TRUE, qqline = FALSE)
resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots="SAS", scale = .8)
resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots="all", theme = "grey")
resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots = c("resid", "index"), smoother = TRUE,
               axis.text.size = 16, title.text.size = 20)
resid_auxpanel(resid(lm_model1), fitted(lm_model1), title.opt = FALSE)
### DOES NOT USE THIRD ROW####
resid_auxpanel(resid(lm_model1), fitted(lm_model1), nrow = 3)



resid_auxpanel(resid(lm_model1), fitted(lm_model1), plots="boxplot")

resid_auxpanel(resid(lm_model2), fitted(lm_model2), plots="boxplot", title.opt=FALSE)

resid_interact(lm_model1, plot="boxplot")
resid_interact(lm_model2, plot="boxplot")

resid_interact(lm_model1, plot="cookd")
resid_interact(lm_model2, plot="cookd")

resid_interact(lm_model1, plot="hist")
resid_interact(lm_model2, plot="hist")

resid_interact(lm_model1, plot="lev")
resid_interact(lm_model2, plot="lev")

resid_interact(lm_model1, plot="ls")
resid_interact(lm_model2, plot="ls")

resid_interact(glm_model1, plot="hist")
resid_interact(glm_model2, plot="hist")

resid_interact(lm_model1, plot="resid")
resid_interact(lm_model2, plot="resid")

resid_interact(lm_model1, plot="qq")
resid_interact(lm_model2, plot="qq")

resid_interact(lm_model1, plot="yvp")
resid_interact(lm_model2, plot="yvp")

xxxxxx
#Poisson Model
# counts <- c(18,17,15,20,10,20,25,13,12)
# outcome <- gl(3,1,9)
# treatment <- gl(3,3)
# print(d.AD <- data.frame(treatment, outcome, counts))
# m2 <- glm(counts ~ outcome + treatment, family = poisson())

glm_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)

#Logistic regression: predicting admit/reject: UCBAdmissions

##Mixed linear model
library(lme4)

#m3 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

#another posibility for a lmer: Theoph

lmer_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)
##Mixed generalized linear model (binomial)
library(lmerTest)
lmer_model2 <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)

#Response is count of telephones with random effect for region: WorldPhones
example_data1 <- data.frame(y = rpois(54, 3),
                           trt = rep(c("A", "B"), each = 27),
                           subject = rep(1:18, each = 3))

glmer_model1 <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data1)

example_data2 <- data.frame(success = rpois(54, 5),
                           trt = rep(c("A", "B"), each = 27),
                           subject = rep(1:18, each = 3))


example_data2$total <-  example_data2$success+rpois(54,4)

glmer_model2 <- glmer(cbind(success, total-success) ~ trt + (1|subject), family = "binomial", data = example_data2)

#Logistic regression on survival: turn something into random effect ~ Titanic
m4 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
            data = cbpp, family = binomial)

#Binomial without random effect
m5 <- glm(cbind(incidence, size - incidence) ~ period ,
          data = cbpp, family = binomial)

##Simulated data wiht more than 10 variables
X <- matrix(rnorm(150), nrow=10)
y <- rnorm(10)
data <-data.frame(y,X)
m6 <- lm(y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15,data=data)


###########################################################################

######Default PANEL##########################



#lm_model1
#lm_model2
#glm_model
#lmer_model
#glmer_model1
#glmer_model2

resid_panel(glm_model)
resid_panel(lmer_model)
resid_panel(lmer_model2)

resid_panel(glmer_model1)
resid_panel(glmer_model2)

#OTHER options
resid_panel(lmer_model, plots="SAS", title.opt=FALSE)

resid_panel(glm_model, plots="R", title.opt=FALSE)

#Check to see if throwing errors correctly
resid_panel(lmer_model, plots="R")
resid_panel(glmer_model1, plots="R")

#######ALL#######

resid_panel(glm_model, plots="all")
resid_panel(lmer_model, plots="all")
resid_panel(glmer_model1, plots="all")
resid_panel(glmer_model2, plots="all")


######Boxplot#######

resid_panel(glm_model, plots="boxplot")
resid_panel(lmer_model, plots="boxplot")
resid_panel(glmer_model1, plots="boxplot")
resid_panel(glmer_model2, plots="boxplot")

######cookd#######

resid_panel(glm_model, plots="cookd")

#Check errors
resid_panel(lmer_model, plots="cookd")
resid_panel(glmer_model1, plots="cookd")


######histogram######

resid_panel(glm_model, plots="hist")
resid_panel(lmer_model, plots="hist")
resid_panel(glmer_model1, plots="hist")
resid_panel(glmer_model2, plots="hist")

######leverage######

resid_panel(glm_model, plots="lev")

##check errors
resid_panel(lmer_model, plots="lev")
resid_panel(glmer_model1, plots="lev")

######location-scale######

resid_panel(glm_model, plots="ls")

##check errors
resid_panel(lmer_model, plots="ls")
resid_panel(glmer_model1, plots="ls")


######residual plot######

resid_panel(glm_model, plots="resid")
resid_panel(lmer_model, plots="resid")
resid_panel(glmer_model1, plots="resid")
resid_panel(glmer_model2, plots="resid")


######Q-Q plot######
resid_panel(glm_model, plots="qq")
resid_panel(lmer_model, plots="qq")
resid_panel(glmer_model1, plots="qq")
resid_panel(glmer_model2, plots="qq")

resid_panel(glmer_model1, plots="qq", qqbands=TRUE)

######prediction vs actual######

resid_panel(glm_model, plots="yvp")
resid_panel(lmer_model, plots="yvp")
resid_panel(glmer_model1, plots="yvp")
resid_panel(glmer_model2, plots="yvp")

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


######resid_interact######


###Boxplot#####

resid_interact(glm_model, plot="boxplot")
resid_interact(lmer_model, plot="boxplot", title.opt=FALSE)
resid_interact(glmer_model1, plot="boxplot", title.opt=FALSE)
resid_interact(glmer_model2, plot="boxplot", title.opt=FALSE)

###Boxplot#####
resid_interact(glm_model, plot="cookd")

#Check for error
resid_interact(lmer_model, plot="cookd")
resid_interact(glmer_model1, plot="cookd")



###hist#####

resid_interact(lmer_model, plot="hist")
resid_interact(glmer_model, plot="hist")
resid_interact(glmer_model2, plot="hist")

###leverage#####
resid_interact(glm_model, plot="lev")

#show errors
resid_interact(lmer_model, plot="lev")
resid_interact(glmer_model1, plot="lev")

###location-scale#####
resid_interact(glm_model, plot="ls")

#show errors
resid_interact(lmer_model, plot="ls")
resid_interact(glmer_model1, plot="ls")

###residplot#####
resid_interact(glm_model1, plot="resid")
resid_interact(glm_model2, plot="resid")

resid_interact(lmer_model, plot="resid")
resid_interact(glmer_model1, plot="resid")
resid_interact(glmer_model2, plot="resid")


###qq##### (Still working on)
resid_interact(glm_model, plot="qq")
resid_interact(lmer_model, plot="qq")
resid_interact(glmer_model1, plot="qq")
resid_interact(glmer_model2, plot="qq")

####Actual vs. predicted######
resid_interact(glm_model1, plot="yvp")
resid_interact(glm_model2, plot="yvp")

resid_interact(lmer_model, plot="yvp")
resid_interact(glmer_model1, plot="yvp")
resid_interact(glmer_model2, plot="yvp")

