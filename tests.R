######Create one of each model type###########################################

#Linear mmodel
m_trees <- lm(Volume ~ Girth, data = trees)

#Linear Model experimental design

m_PlantGrowth <- lm(weight ~ group, data = PlantGrowth)


#Poisson Model
# counts <- c(18,17,15,20,10,20,25,13,12)
# outcome <- gl(3,1,9)
# treatment <- gl(3,3)
# print(d.AD <- data.frame(treatment, outcome, counts))
# m2 <- glm(counts ~ outcome + treatment, family = poisson())

m_InsectSprays <- glm(count~spray, data=InsectSprays, family="poisson")

#Logistic regression: predicting admit/reject: UCBAdmissions

##Mixed linear model
library(lme4)

#m3 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

#another posibility for a lmer: Theoph

m_ChickWeight <- lmer(log(weight)~Time+Diet+(1|Chick), data=ChickWeight)

##Mixed generalized linear model (binomial)


#Response is count of telephones with random effect for region: WorldPhones

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

######SAS PANEL##########################


#LM
#Good basic example
resid_panel(m_trees, plots="SAS")
#Good for spotting constant variance issues1
resid_panel(m_PlantGrowth, plots="SAS")

# Warning messages:
#   1: In resid_panel(m_PlantGrowth, plots = "SAS") :
#   By default, bins = 30 in the histogram of residuals. If necessary,
# specify an appropriate number of bins.
# 2: Removed 1 rows containing missing values (geom_bar).

#GLM
resid_panel(m_InsectSprays, plots="SAS")

#LMER
resid_panel(m_ChickWeight, plots="SAS")
resid_panel(m4, plots="SAS")

#OTHER options
resid_panel(m4, plots="SAS", title.opt=FALSE)
resid_panel(m_trees, plots="SAS", type="pearson")
resid_panel(m_trees, plots="SAS", type="standardized")


######SASextend#####

resid_panel(m_trees, plots="SASextend")
resid_panel(m_trees, plots="SASextend", title.opt=FALSE)


##had to remove sas_stats for glm because doesn't work on glm
resid_panel(m_InsectSprays, plots="SASextend")

#Check to see if throwing errors correctly
resid_panel(m_ChickWeight, plots="SASextend")
resid_panel(m4, plots="SASextend")


######R###############################################

resid_panel(m_trees, plots="R")
resid_panel(m_InsectSprays, plots="R")

resid_panel(m_InsectSprays, plots="R", title.opt=FALSE)

#Check to see if throwing errors correctly
resid_panel(m_ChickWeight, plots="R")
resid_panel(m4, plots="R")

#######ALL#######

resid_panel(m_trees, plots="all")

###Error: NOT SHOWING STATS####

#
# geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
#   Warning messages:
#   1: In resid_panel(m_trees, plots = "all") :
#   By default, bins = 30 in the histogram of residuals. If necessary,
# specify an appropriate number of bins.
# 2: In sqrt(0.5 * p * (1 - hh)/hh) : NaNs produced
# 3: In sqrt(0.5 * p * (1 - hh)/hh) : NaNs produced
# 4: In sqrt(1 * p * (1 - hh)/hh) : NaNs produced
# 5: In sqrt(1 * p * (1 - hh)/hh) : NaNs produced
resid_panel(m_InsectSprays, plots="all")

#Check errors
resid_panel(m_ChickWeight, plots="all")
resid_panel(m4, plots="all")


######Boxplot#######

resid_panel(m_trees, plots="boxplot")
resid_panel(m_InsectSprays, plots="boxplot")
resid_panel(m_ChickWeight, plots="boxplot")
resid_panel(m4, plots="boxplot")

######cookd#######

resid_panel(m_trees, plots="cookd")
resid_panel(m_InsectSprays, plots="cookd")

#Check errors
resid_panel(m_ChickWeight, plots="cookd")
resid_panel(m4, plots="cookd")


######histogram######

resid_panel(m_trees, plots="hist")
resid_panel(m_InsectSprays, plots="hist")
resid_panel(m_ChickWeight, plots="hist")
resid_panel(m4, plots="hist")

######leverage######

resid_panel(m_trees, plots="residlev")
resid_panel(m_InsectSprays, plots="residlev")

##check errors
resid_panel(m_ChickWeight, plots="residlev")
resid_panel(m4, plots="residlev")

######location-scale######

resid_panel(m_trees, plots="ls")
resid_panel(m_InsectSprays, plots="ls")

##check errors
resid_panel(m_ChickWeight, plots="ls")
resid_panel(m4, plots="ls")


######residual plot######

resid_panel(m_trees, plots="residplot")
resid_panel(m_InsectSprays, plots="residplot")
resid_panel(m_ChickWeight, plots="residplot")
resid_panel(m4, plots="residplot")

resid_panel(m_ChickWeight, plots="residplot", smoother=TRUE)

######Q-Q plot######

resid_panel(m_trees, plots="qq")
resid_panel(m_InsectSprays, plots="qq")
resid_panel(m_ChickWeight, plots="qq")
resid_panel(m4, plots="qq")

resid_panel(m4, plots="qq", qqbands=TRUE)

######prediction vs actual######

resid_panel(m_trees, plots="respred")
resid_panel(m_InsectSprays, plots="respred")
resid_panel(m_ChickWeight, plots="respred")
resid_panel(m4, plots="respred")

######Spanel#########

resid_auxpanel(resid(m_trees), fitted(m_trees), plots="SAS")
resid_auxpanel(resid(m_InsectSprays), fitted(m_InsectSprays), plots="SAS")
resid_auxpanel(resid(m_ChickWeight), fitted(m_ChickWeight), plots="SAS")
resid_auxpanel(resid(m4), fitted(m4), plots="SAS")

resid_auxpanel(resid(m4), fitted(m4), plots="SAS", title.opt=FALSE)
######sboxplot#########

resid_auxpanel(resid(m_trees), fitted(m_trees), plots="boxplot")
resid_auxpanel(resid(m_InsectSprays), fitted(m_InsectSprays), plots="boxplot")
resid_auxpanel(resid(m_ChickWeight), fitted(m_ChickWeight), plots="boxplot")
resid_auxpanel(resid(m4), fitted(m4), plots="boxplot")

resid_auxpanel(resid(m_trees), fitted(m_trees), plots="boxplot", title.opt=FALSE)

######histogram#########

resid_auxpanel(resid(m_trees), fitted(m_trees), plots="hist")
resid_auxpanel(resid(m_InsectSprays), fitted(m_InsectSprays), plots="hist")
resid_auxpanel(resid(m_ChickWeight), fitted(m_ChickWeight), plots="hist")
resid_auxpanel(resid(m4), fitted(m4), plots="hist")

######residplot#########

resid_auxpanel(resid(m_trees), fitted(m_trees), plots="residplot")
resid_auxpanel(resid(m_InsectSprays), fitted(m_InsectSprays), plots="residplot")
resid_auxpanel(resid(m_ChickWeight), fitted(m_ChickWeight), plots="residplot")
resid_auxpanel(resid(m4), fitted(m4), plots="residplot")

######qq#########

resid_auxpanel(resid(m_trees), fitted(m_trees), plots="qq")
resid_auxpanel(resid(m_InsectSprays), fitted(m_InsectSprays), plots="qq")
resid_auxpanel(resid(m_ChickWeight), fitted(m_ChickWeight), plots="qq")
resid_auxpanel(resid(m4), fitted(m4), plots="qq")


######resid_interact######


###Boxplot#####

resid_interact(m_trees, plot="boxplot")
resid_interact(m_InsectSprays, plot="boxplot")
resid_interact(m_ChickWeight, plot="boxplot")
resid_interact(m4, plot="boxplot", title.opt=FALSE)

###Boxplot#####

resid_interact(m_trees, plot="cookd")
resid_interact(m_InsectSprays, plot="cookd")

##Warning: interactivity not showing cook's d value

#Check for error
resid_interact(m_ChickWeight, plot="cookd")
resid_interact(m4, plot="cookd")



###hist#####

resid_interact(m_trees, plot="hist")
resid_interact(m_InsectSprays, plot="hist")
resid_interact(m_ChickWeight, plot="hist")
resid_interact(m4, plot="hist")

###leverage#####

resid_interact(m_trees, plot="residlev")
resid_interact(m_InsectSprays, plot="residlev")

#show errors
resid_interact(m_ChickWeight, plot="residlev")
resid_interact(m4, plot="residlev")

###location-scale#####

resid_interact(m_trees, plot="ls")
resid_interact(m_InsectSprays, plot="ls")

#show errors
resid_interact(m_ChickWeight, plot="ls")
resid_interact(m4, plot="ls")

###residplot#####

resid_interact(m_trees, plot="residplot")
resid_interact(m_InsectSprays, plot="residplot")
resid_interact(m_ChickWeight, plot="residplot")
resid_interact(m4, plot="residplot")


###qq##### (Still working on)

resid_interact(m_trees, plot="qq")
resid_interact(m_InsectSprays, plot="qq")
resid_interact(m_ChickWeight, plot="qq")
resid_interact(m4, plot="qq")

####Actual vs. predicted######

resid_interact(m_trees, plot="respred")
resid_interact(m_InsectSprays, plot="respred")
resid_interact(m_ChickWeight, plot="respred")
resid_interact(m4, plot="respred")

