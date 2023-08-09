## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(jskm)

## -----------------------------------------------------------------------------
#Load dataset
library(survival)
data(colon)
fit <- survfit(Surv(time,status)~rx, data=colon)

#Plot the data
jskm(fit)
jskm(fit, table = T, pval = T, label.nrisk = "No. at risk", size.label.nrisk = 8, 
     xlabs = "Time(Day)", ylabs = "Survival", ystratalabs = c("Obs", "Lev", "Lev + 5FU"), ystrataname = "rx",
     marks = F, timeby = 365, xlims = c(0, 3000), ylims = c(0.25, 1), showpercent = T)

## -----------------------------------------------------------------------------
jskm(fit, ci = T, cumhaz = T,  mark = F, ylab = "Cumulative incidence (%)", surv.scale = "percent", pval =T, pval.size = 6, pval.coord = c(300, 0.7))

## -----------------------------------------------------------------------------
jskm(fit, mark = F,  surv.scale = "percent", pval =T, table = T, cut.landmark = 500, showpercent = T)

## -----------------------------------------------------------------------------
## Make competing risk variable, Not real
colon$status2 <- colon$status
colon$status2[1:400] <- 2
colon$status2 <- factor(colon$status2)
fit2 <- survfit(Surv(time,status2)~rx, data=colon)
jskm(fit2, mark = F, surv.scale = "percent", table = T, status.cmprsk = "1")
jskm(fit2, mark = F, surv.scale = "percent", table = T, status.cmprsk = "1", showpercent = T, cut.landmark = 500)

## -----------------------------------------------------------------------------
library(survey)
data(pbc, package="survival")
pbc$randomized <- with(pbc, !is.na(trt) & trt>0)
biasmodel <- glm(randomized~age*edema,data=pbc)
pbc$randprob <- fitted(biasmodel)

dpbc<-svydesign(id=~1, prob=~randprob, strata=~edema, data=subset(pbc,randomized))

s1 <-svykm(Surv(time,status>0) ~ 1, design = dpbc)
s2 <-svykm(Surv(time,status>0) ~ sex, design = dpbc)

svyjskm(s1)
svyjskm(s2)
svyjskm(s2, cumhaz = T, ylab = "Cumulative incidence(%)", surv.scale = "percent", showpercent = T) 

## -----------------------------------------------------------------------------
s3 <- svykm(Surv(time,status>0) ~ sex, design=dpbc, se = T)
svyjskm(s3)
svyjskm(s3, ci = F, showpercent = T)
svyjskm(s3, ci = F,  surv.scale = "percent", pval = T, table = T, cut.landmark = 1000)

