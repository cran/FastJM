## ----echo=FALSE, results="hide", warning=FALSE--------------------------------
suppressPackageStartupMessages({
  library(FastJM)
})
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", warning=FALSE, message=FALSE, eval=TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("FastJM")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("shanpengli/FastJM")

## -----------------------------------------------------------------------------
require(FastJM)
require(survival)
data(ydata)
data(cdata)
fit <- jmcs(ydata = ydata, cdata = cdata, 
            long.formula = response ~ time + gender + x1 + race, 
            surv.formula = Surv(surv, failure_type) ~ x1 + gender + x2 + race, 
            random =  ~ time| ID)
fit

## -----------------------------------------------------------------------------
ND <- ydata[ydata$ID %in% c(419, 218), ]
ID <- unique(ND$ID)
NDc <- cdata[cdata$ID  %in% ID, ]
survfit <- survfitjmcs(fit, 
                       ynewdata = ND, 
                       cnewdata = NDc, 
                       u = seq(3, 4.8, by = 0.2), 
                       method = "GH",
                       obs.time = "time")
survfit

## -----------------------------------------------------------------------------
res <- DynPredAccjmcs(
  object = fit,
  landmark.time = 3,
  horizon.time = c(3.6, 4, 4.4),
  obs.time = "time",
  method = "GH",
  maxiter = 1000,
  n.cv = 3,
  survinitial = TRUE,
  quantile.width = 0.25,
  metrics = c("AUC", "Cindex", "Brier", "MAE", "MAEQ")
)

summary(res, metric = "Brier")
summary(res, metric = "MAE")
summary(res, metric = "MAEQ")
summary(res, metric = "AUC")
summary(res, metric = "Cindex")

## -----------------------------------------------------------------------------
Concord <- Concordancejmcs(seed = 100, fit, n.cv = 3)
summary(Concord)

## ----eval=TRUE----------------------------------------------------------------
data(mvydata)
data(mvcdata)
library(FastJM)
mvfit <- mvjmcs(ydata = mvydata, cdata = mvcdata,
              long.formula = list(Y1 ~ X11 + X12 + time,
                                  Y2 ~ X11 + X12 + time),
              random = list(~ time | ID,
                            ~ 1 | ID),
              surv.formula = Surv(survtime, cmprsk) ~ X21 + X22, 
              maxiter = 1000, opt = "optim",
              tol = 1e-3, print.para = FALSE)
mvfit

## ----eval=TRUE----------------------------------------------------------------
# Longitudinal fixed effects
fixef(mvfit, process = "Longitudinal")
summary(mvfit, process = "Longitudinal")

# Survival fixed effects
fixef(mvfit, process = "Event")
summary(mvfit, process = "Event")

# Random effects for first few subjects
head(ranef(mvfit))

## ----eval=FALSE---------------------------------------------------------------
# require(dplyr)
# set.seed(08252025)
# sampleID <- sample(mvcdata$ID, 5, replace = FALSE)
# 
# subcdata <- mvcdata %>%
#   dplyr::filter(ID %in% sampleID)
# 
# subydata <- mvydata %>%
#   dplyr::filter(ID %in% sampleID)
# 
# ### Set up a landmark time of 4.75 and make predictions at time u
# survmvfit <- survfitmvjmcs(mvfit, seed = 100, ynewdata = subydata, cnewdata = subcdata,
#                          u = c(7, 8, 9), Last.time = 4.75, obs.time = "time")
# 
# survmvfit
# 

## -----------------------------------------------------------------------------
# Simulate data
  sim <- simmvJMdata(seed = 100, N = 50) # returns list of cdata and ydata for a sample size of 50
  c_data <- sim$mvcdata # survival-side data, one row per ID
  y_data <- sim$mvydata # longitudinal measurements (multiple rows per ID)

## -----------------------------------------------------------------------------
head(y_data)

## -----------------------------------------------------------------------------
head(c_data)

