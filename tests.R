######Create one of each model type###########################################

#Linear mmodel
m1 <- lm(Volume ~ Girth, data = trees)

#Poisson Model
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
m2 <- glm(counts ~ outcome + treatment, family = poisson())

##Mixed linear model
library(lme4)

m3 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

##Mixed generalized linear model (binomial)
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

######SAS PANEL#######

resid_panel(m1, plots="SAS")
resid_panel(m2, plots="SAS")
resid_panel(m3, plots="SAS")
resid_panel(m4, plots="SAS")


resid_panel(m4, plots="SAS", title=FALSE)
resid_panel(m1, plots="SAS", type="pearson")

######SASextend#####

resid_panel(m1, plots="SASextend")
resid_panel(m1, plots="SASextend", title=FALSE)


##had to remove sas_stats for glm because doesn't work on glm
resid_panel(m2, plots="SASextend")

#Check to see if throwing errors correctly
resid_panel(m3, plots="SASextend")
resid_panel(m4, plots="SASextend")


######R######

resid_panel(m1, plots="R")
resid_panel(m2, plots="R")

resid_panel(m2, plots="R", title=FALSE)

#Check to see if throwing errors correctly
resid_panel(m3, plots="R")
resid_panel(m4, plots="R")

#######ALL#######

resid_panel(m1, plots="all")
resid_panel(m2, plots="all")

#Check errors
resid_panel(m3, plots="all")
resid_panel(m4, plots="all")


######Boxplot#######

resid_panel(m1, plots="boxplot")
resid_panel(m2, plots="boxplot")
resid_panel(m3, plots="boxplot")
resid_panel(m4, plots="boxplot")

######cookd#######

resid_panel(m1, plots="cookd")
resid_panel(m2, plots="cookd")

#Check errors
resid_panel(m3, plots="cookd")
resid_panel(m4, plots="cookd")


######histogram######

resid_panel(m1, plots="hist")
resid_panel(m2, plots="hist")
resid_panel(m3, plots="hist")
resid_panel(m4, plots="hist")

######leverage######

resid_panel(m1, plots="residlev")
resid_panel(m2, plots="residlev")

##check errors
resid_panel(m3, plots="residlev")
resid_panel(m4, plots="residlev")

######location-scale######

resid_panel(m1, plots="ls")
resid_panel(m2, plots="ls")

##check errors
resid_panel(m3, plots="ls")
resid_panel(m4, plots="ls")


######residual plot######

resid_panel(m1, plots="residplot")
resid_panel(m2, plots="residplot")
resid_panel(m3, plots="residplot")
resid_panel(m4, plots="residplot")

######Q-Q plot######

resid_panel(m1, plots="qq")
resid_panel(m2, plots="qq")
resid_panel(m3, plots="qq")
resid_panel(m4, plots="qq")

######prediction vs actual######

resid_panel(m1, plots="respred")
resid_panel(m2, plots="respred")
resid_panel(m3, plots="respred")
resid_panel(m4, plots="respred")

######Spanel#########

resid_spanel(resid(m1), fitted(m1), plots="SAS")
resid_spanel(resid(m2), fitted(m2), plots="SAS")
resid_spanel(resid(m3), fitted(m3), plots="SAS")
resid_spanel(resid(m4), fitted(m4), plots="SAS")

######sboxplot#########

resid_spanel(resid(m1), fitted(m1), plots="boxplot")
resid_spanel(resid(m2), fitted(m2), plots="boxplot")
resid_spanel(resid(m3), fitted(m3), plots="boxplot")
resid_spanel(resid(m4), fitted(m4), plots="boxplot")


######histogram#########

resid_spanel(resid(m1), fitted(m1), plots="hist")
resid_spanel(resid(m2), fitted(m2), plots="hist")
resid_spanel(resid(m3), fitted(m3), plots="hist")
resid_spanel(resid(m4), fitted(m4), plots="hist")

######residplot#########

resid_spanel(resid(m1), fitted(m1), plots="residplot")
resid_spanel(resid(m2), fitted(m2), plots="residplot")
resid_spanel(resid(m3), fitted(m3), plots="residplot")
resid_spanel(resid(m4), fitted(m4), plots="residplot")

######qq#########

resid_spanel(resid(m1), fitted(m1), plots="qq")
resid_spanel(resid(m2), fitted(m2), plots="qq")
resid_spanel(resid(m3), fitted(m3), plots="qq")
resid_spanel(resid(m4), fitted(m4), plots="qq")


######resid_interact######


###Boxplot#####

resid_interact(m1, plots="boxplot")
resid_interact(m2, plots="boxplot")
resid_interact(m3, plots="boxplot")
resid_interact(m4, plots="boxplot")

###Boxplot#####

resid_interact(m1, plots="cookd")
resid_interact(m2, plots="cookd")

#Check for error
resid_interact(m3, plots="cookd")
resid_interact(m4, plots="cookd")



###hist#####

resid_interact(m1, plots="hist")
resid_interact(m2, plots="hist")
resid_interact(m3, plots="hist")
resid_interact(m4, plots="hist")

###leverage#####

resid_interact(m1, plots="residlev")
resid_interact(m2, plots="residlev")

#show errors
resid_interact(m3, plots="residlev")
resid_interact(m4, plots="residlev")

###location-scale#####

resid_interact(m1, plots="ls")
resid_interact(m2, plots="ls")

#show errors
resid_interact(m3, plots="ls")
resid_interact(m4, plots="ls")

###residplot#####

resid_interact(m1, plots="residplot")
resid_interact(m2, plots="residplot")
resid_interact(m3, plots="residplot")
resid_interact(m4, plots="residplot")


###qq##### (Still working on)

resid_interact(m1, plots="qq")
resid_interact(m2, plots="qq")
resid_interact(m3, plots="qq")
resid_interact(m4, plots="qq")

####Actual vs. predicted######

resid_interact(m1, plots="respred")
resid_interact(m2, plots="respred")
resid_interact(m3, plots="respred")
resid_interact(m4, plots="respred")

