## def.data.assembly.R
## calc statistics of the overall distribution
## see terraclimate.data.R for download and sample of DEF
## process here works, but inefficient....
## major part is subset for specific post fire years
## then join with pc values for plots

rm(list=ls()) ### clear workspace 

library(tidyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

## read in data
ccd_df<-read.table("./data/tab/def.ccd.txt"); conm_df<-read.table("./data/tab/def.conm.txt")
blues_df<-read.table("./data/tab/def.blues.txt");az_df<-read.table("./data/tab/def.az.txt")
## subset years of interest if necessary
## ccd is already ok (2000-2018)
## and sites with year = 2000: hash rock, cerro, outlet, pumpkin 
blues00_df<-filter(as_tibble(blues_df), Fire=="Hash_Rock")
az00_df<-filter(as_tibble(az_df), Burn=="Outlet" | Burn=="Pumpkin")
conm00_df<-filter(conm_df, Burn=="Cerro")
## but these need subsetting
## for 2002 fires:  747 Complex, Roberts Creek, Hayman, Missionary Ridge, Ponil Complex, Rodeo
blues02_df<-filter(as_tibble(blues_df), Fire=="747" | Fire=="Roberts_Creek")
blues02_df<-select(blues02_df, -c(X2000.01.01:X2001.12.01))
conm02_df<-filter(conm_df, Burn=="Mission" | Burn=="Ponil" | Burn=="Hayman")
conm02_df<-select(conm02_df, -c(X2000.01.01:X2001.12.01))
az02_df<-filter(az_df, Burn=="Rodeo")
az02_df<-select(az02_df, -c(X2000.01.01:X2001.12.01))

## 2003: Poplar
az03_df<-filter(az_df, Burn=="Poplar")
az03_df<-select(az03_df, -c(X2000.01.01:X2002.12.01))
## 2005: burnt cabin
blues05_df<-filter(blues_df, Fire=="Burnt_Cabin")
blues05_df<-select(blues05_df, -c(X2000.01.01:X2004.12.01))

## pc's
ccd.pc<-read.table("./data/tab/ccdpcs.rot.txt"); sw1.pc<-read.table("./data/tab/sw1pcs.rot.txt")
sw2.pc<-read.table("./data/tab/sw2pcs.rot.txt"); blu.pc<-read.table("./data/tab/bluespcs.rot.txt")

## make a list for looping...didn't use
#dat_lst<-lst(blues00_df, conm00_df, az00_df, ccd_df, blues02_df, conm02_df, az02_df, az02_df, blues05_df)
dim(blues00_df);dim(conm00_df);dim(az00_df); dim(ccd_df);dim(blues02_df); dim(conm02_df); 
dim(az02_df); dim(az03_df);dim(blues05_df)

## calc 75, 90 quantile, mean, max, variance
## ccd
ccd_datT <- as.data.frame(as.matrix(t(ccd_df[,-c(1:28)])))
ccd_stats<-as_tibble(ccd_datT) %>% gather(xvar, value, 1:60) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))
## blues 2000
blu00_datT<-as.data.frame(as.matrix(t(blues00_df[,-c(1:48)])))
blu00_stats<-as_tibble(blu00_datT) %>% gather(xvar, value, 1:52) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))
## co nm 2000
conm00_datT<-as.data.frame(as.matrix(t(conm00_df[,-c(1:29)])))
conm00_stats<-as_tibble(conm00_datT) %>% gather(xvar, value, 1:50) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))
## az 2000
az00_datT<-as.data.frame(as.matrix(t(az00_df[,-c(1:29)])))
az00_stats<-as_tibble(az00_datT) %>% gather(xvar, value, 1:99) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))

## blues 2002
blues02_datT<-as.data.frame(as.matrix(t(blues02_df[,-c(1:48)]))) 
blues02_stats<-as_tibble(blues02_datT) %>% gather(xvar, value, 1:105) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))
## co, nm 2002
conm02_datT<-as.data.frame(as.matrix(t(conm02_df [,-c(1:29)]))) 
conm02_stats<-as_tibble(conm02_datT) %>% gather(xvar, value, 1:199) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))
## az 2002
az02_datT<-as.data.frame(as.matrix(t(az02_df[,-c(1:29)]))) 
az02_stats<-as_tibble(az02_datT) %>% gather(xvar, value, 1:100) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))
## az 2003
az03_datT<-as.data.frame(as.matrix(t(az03_df[,-c(1:29)]))) 
az03_stats<-as_tibble(az03_datT) %>% gather(xvar, value, 1:50) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))
## blues 2005
blues05_datT<-as.data.frame(as.matrix(t(blues05_df[,-c(1:48)]))) 
blues05_stats<-as_tibble(blues05_datT) %>% gather(xvar, value, 1:30) %>% group_by(xvar) %>% 
  summarise(q90.x=quantile(value, probs=0.9, type=4), q75.x=quantile(value, probs=0.75, type=4),
            max.x=max(value), mean.x=mean(value), var.x=var(value))

## assemble pcs, stats, burn name
# need to rearrange etc
sw_dat1<-select(sw1.pc,c(2:3,5)) # az
sw_dat2<-select(sw2.pc,c(2:3,5)) # co nm
blu_dat1<-select(blu.pc, c(2:3,6))
names(blu_dat1)<-c("MAP1.1", "MAP1.2", "Burn")
blu00_pc<-filter(blu_dat1, Burn=="Hash_Rock")
az00_pc<-filter(sw_dat1, Burn=="Outlet" | Burn=="Pumpkin")
conm00_pc<-filter(sw_dat2, Burn=="Cerro")
blues02_pc<-filter(blu_dat1, Burn=="747" | Burn=="Roberts_Creek")
conm02_pc<-filter(sw_dat2, Burn=="Mission" | Burn=="Ponil" | Burn=="Hayman")
az02_pc<-filter(sw_dat1, Burn=="Rodeo")
az03_pc<-filter(sw_dat1, Burn=="Poplar")
blues05_pc<-filter(blu_dat1, Burn=="Burnt_Cabin")

## join the stats with pc values
ccd_vars<-bind_cols(ccd_stats, ccd.pc[,c(2:3,5)])
blu00_vars<-bind_cols(blu00_stats, blu00_pc)
az00_vars<-bind_cols(az00_stats, az00_pc)
conm00_vars<-bind_cols(conm00_stats, conm00_pc)
blu02_vars<-bind_cols(blues02_stats, blues02_pc)
az02_vars<-bind_cols(az02_stats, az02_pc)
conm02_vars<-bind_cols(conm02_stats, conm02_pc)
az03_vars<-bind_cols(az03_stats, az03_pc)
blu05_vars<-bind_cols(blues05_stats, blues05_pc)

d1<-bind_rows(ccd_vars, blu00_vars, az00_vars,conm00_vars, blu02_vars, az02_vars, conm02_vars,
  az03_vars, blu05_vars)
write.table(d1, "./data/tab/def.stats1.txt")



############### old ########################################
## data, ccd, one col for each plot location
df<-read.table("./data/tab/def.ccd.txt")
#x1<-rep(2000:2018, ea=12)
#x2<-rep(c(1,2,3,4,5,6,7,8,9,10,11,12)/100, length(x1)/12)
#date=x1 + x2
#df$date=date
df$date=seq(as.Date("2000/01/01"), by = "month", length.out = 228)
data<-df[,c(61,1)]
data$group<-rep(1:19, ea=12, length=228)
data2<-data %>%
  group_by(group) %>%
  summarize(yr.avg = mean(CCDIN01X))
data2$year<-2000:2018
data2<-data %>%
  group_by(group) %>%
  summarize(yr.var = var(CCDIN01X))

## what percentage of values are in the 90th percentile?
## one plotid
plot(density(df[,2]))
v=quantile(df[,2], probs=0.9, type=4)
dim(filter(df, CCDIN01X >=v))
## example
#df_test <- data.frame(loc = rep(1:2, each = 4), 
#                 year = rep(1980:1983, times = 2),
#                x1 = rnorm(8), x2 = rnorm(8), x3 = rnorm(8), x4 = rnorm(8), 
#                 x5 = rnorm(8), x6 = rnorm(8), x7 = rnorm(8), x8 = rnorm(8))
#df_test %>% gather(xvar, value, x1:x8) %>% 
#  group_by(loc, year) %>% 
#  summarise(mean.x = quantile(value, probs = 0.50),
#            lower.x = quantile(value, probs = 0.025),
#            upper.x = quantile(value, probs = 0.975))

## using my df
## this gives 90th quantile
df_long<- df %>% gather(xvar, value, CCDIN01X:CCDOUT963)
q90<-df %>% gather(xvar, value, CCDIN01X:CCDOUT963) %>% group_by(xvar) %>% 
  summarise(upper.x=quantile(value, probs=0.9, type=4))




p1<-length(df[df$CCDIN01X>=148,1])/228
p2<-length(df[df$CCDIN02X>=144,2])/228
## use apply with a function by col
dat<-as.matrix(df[,1:60])
fun=function(x) quantile(x, probs=0.9, type=4)
t1=apply(dat, MARGIN=2, FUN=fun)
fun-function(x) length(x>=quantile(x, probs=0.9, type=4))
t2=apply(dat, MARGIN=2, FUN=fun)
length(dat[dat>=t1])
