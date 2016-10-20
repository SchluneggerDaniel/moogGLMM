# Generalized Linear Mixed-Effects Model

library(MPDiR)
library(lme4)

load("~/Dropbox/RStudio/MOOG/moogGLMM.Rda")

coef(summary(glm(Resp ~ Stim, binomial(probit), moogGLMM)))

# Intercept only
moog.int.only <- glm(Resp ~ Stim, binomial(probit), moogGLMM)

# Random intercept (for Obs) fixed slope
moog.only <- glmer(Resp ~ Stim + (1 | Obs),moogGLMM, binomial(probit))

# Random intercept (for Obs & Block) fixed slope
moog.glmm1 <- glmer(Resp ~ Stim + (1 | Obs) + (1 | Block), moogGLMM, binomial(probit))

# Random intercept (for Obs & Block) random slope (for Obs)
moog.glmm2 <- glmer(Resp ~ Stim + (Stim | Obs) + (1 | Block), moogGLMM, binomial(probit))

# Random intercept (for Obs & Block) random slope (for Block)
moog.glmm3 <- glmer(Resp ~ Stim + (1 | Obs) + (Stim | Block), moogGLMM, binomial(probit))

# Random intercept and random slope (all random)
moog.glmm4 <- glmer(Resp ~ Stim + (Stim | Obs) + (Stim | Block), moogGLMM, binomial(probit))


anova(moog.glmm1, moog.glmm2, moog.glmm3, moog.glmm4)

summary(moog.glmm1)



# try next page (for orthogonal representation of d' and c???????? no clue)
# Stm <- moogGLMM$Stim
# Stm <- cbind(-1, Stm)
# colnames(Stm) <- c("_Criterion", "_d'")
# 
# glmer(Resp ~ Stm + (1 | Obs) + (1 | Block) - 1, moogGLMM, binomial(probit))













