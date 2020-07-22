## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE,warning=FALSE----------------------------------------------
library(lme4)
library(gANOVA)
library(tidyverse)

## -----------------------------------------------------------------------------
set.seed(42)

df <- expand.grid(pt = paste0("P",str_pad(1:12,width = 2,pad = "0")),
                  A = paste0("A",1:4),
                  r = 1:3,
                  stringsAsFactors = F)%>%
  select(-r)

## -----------------------------------------------------------------------------
# add +(1|pt) random intercept
df<-
  df%>%
  nest(data=-pt)%>%
  mutate(ranef_pt = rnorm(n(), sd = .8))%>%
  unnest(data)

# add +(1|pt:A) random interaction
df<-
  df%>%
  nest(data=-c(pt,A))%>%
  mutate(ranef_ptA = rnorm(n(), sd = 3))%>%
  unnest(data)

# add an error term
df<-
  df%>%
  mutate(err = rnorm(n(),sd = 4))


# create 2 dependant variables
df <- df%>%
  transmute(pt = pt, A= A,
            y_diff = ranef_pt + ranef_ptA + err,
            y_same =  ranef_pt*2 +ranef_ptA + err)

## -----------------------------------------------------------------------------
lmer_same <- lmer(y_same ~ A+ (1|pt) + (1|pt:A), data=df, contrasts = list(A = contr.sum))
summary(lmer_same)

## -----------------------------------------------------------------------------
gANOVA_same <- gANOVA(y_same ~ A + (1|pt|A), data=df, contrasts = list(A = contr.sum))
summary(gANOVA_same)

## -----------------------------------------------------------------------------
lmer_diff <- lmer(y_diff~ A + (1|pt)+(1|pt:A),data=df,contrasts = list(A = contr.sum))
summary(lmer_diff)

## -----------------------------------------------------------------------------
mod_gANOVA <- gANOVA(y_diff~ A+ (1|pt|A),data=df,contrasts = list(A = contr.sum))
summary(mod_gANOVA)

