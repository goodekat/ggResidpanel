# Example to see how resid_panel works with lmer and glmer models

# Load library
library(lme4)

## lmer ---------------------------------------------------------

# Data for lmer model
d1 <- data.frame(y = rnorm(54, 20, 4),
                 trt = rep(c("A", "B"), each = 27),
                 subject = rep(1:18, each = 3))

# lmer model
model1 <- lmer(y ~ trt + (1|subject), data = d1)

# Residual plot with lmer model
resid_panel(model1, bins = 30)

## glmer --------------------------------------------------------

# Data for glmer model
d2 <- data.frame(y = rpois(54, 30),
                 trt = rep(c("A", "B"), each = 27),
                 subject = rep(1:18, each = 3))

# glmer model
model2 <- glmer(y ~ trt + (1|subject), family = poisson, data = d2)

# Residual plot with glmer model
resid_panel(model2, bins = 30)
