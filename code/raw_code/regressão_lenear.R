#rm(list = ls())
library(here)
library(pacman)
library(tidyverse)

pacman::p_load(dplyr,ggplot2,car,rstatix,lmtest,ggpubr)

coffee=readRDS('coffee')

mod=lm(Profit ~ Marketing, coffee)

par(mfrow=c(2,2))
plot(mod)


shapiro.test(mod$residuals)
summary(rstandard(mod))
bptest(mod)
