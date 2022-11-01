#Community matters August 8, 2022
#Ctrl 1 and 2 toggle from source to console.
#Once I do some initial exploratory analyses I’d like to take a looks at which species are dominant or contributing in major ways to each cluster or dominant type.

#picking up from "community.txt" and "lab book.txt"
#using fulldata

#Sandra sent me updated file that includes field measured distance to nearest PIPO tree. NA’s for SW and CC that are equivalent to the 600 (not within measurable distance) coding for Blues.
#fulldata.import = read.delim("I:\\Box Sync\\meg\\projects\\refugia\\extensive dataset analyses\\data 13 fires\\for r analyses\\from sandra\\allvars_plus_treedist.csv", sep=",", header=T)

#updateR
install.packages("installr")
library(installr)
updateR()

#need to replace NAs with 600s
hist(fulldata.import$pinpon.dist)

fulldata = fulldata.import

install.packages("tidyverse")
library(tidyverse)

##need to use tidyr to do replaces -- learn more
pinpon.dist.600 = fulldata$pinpon.dist %>% replace_na(600)
fulldata.1 = cbind(fulldata, pinpon.dist.600)
head(fulldata.1)
fulldata = fulldata.1
head(fulldata)
rm(fulldata.1)


#aiming to use hierarchical gam structures, based on Pederson et al (2019)
#random effects to integrate community types and efficient estimates

#load gam library*****
library(mgcv)

#load gratis package from Gavin Simpson
install.packages("gratia")
library(gratia)

#working through Pederson examples using our data
head(fulldata)

hist(fulldata$pinpon_tally, xlim=c(0,100),nclass=15) 
#lots of zeroes for the plot-only tallies. we'll use family = nb() since this will include Poisson and NB
#Let's see how it goes
#But I think this is what we want rather than the variable radius, when all is said and done

#initially with a random effect for community type (clust.7)
#I need to make it a factor so it can be a random effect
is.factor(fulldata$clust.7)
fulldata[,'clust.7'] = as.factor(fulldata[,'clust.7'])
is.factor(fulldata$clust.7)
is.ordered(fulldata$clust.7) #FALSE


#cm1 - using initial fixed effect as q90.x (quantiles of climatic wader deficits for post-fire years)
#and allowing random effect for each community type
#default k for tp smoother on q90.x, and randome effect smoother "re" with k value equal to
#number of types, so k=7)

cm1 = gam(pinpon_tally~s(q90.x, k=10, bs="tp")+ s(clust.7,k=7,bs="re"), data=fulldata, method="REML", family=nb())

draw(cm1) #concave shape for q90.x global

#gratia draw() for community types

cm1_pred <- predict(cm1, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                 cm1_fit = cm1_pred$fit, 
                 cm1_se = cm1_pred$se.fit)

ggplot(data=fulldata.1, aes(x=q90.x, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm1_fit-2*cm1_se),
                  ymax=exp(cm1_fit+2*cm1_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm1_fit))) +
  geom_point() +
  labs(x='q90.x', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#check summary of model to see if q90.x is significant
#interesting shape for type 3 and 5 but otherwise fairly flat.
summary(cm1)

#now let's try it with max.x, then we'll loop back for other
#full family comparison of models.
cm1 = gam(pinpon_tally~s(max.x, k=10, bs="tp")+ s(clust.7,k=7,bs="re"), data=fulldata, method="REML", family=nb())

draw(cm1) #concave shape for max.x global, this is decreasing monotonic
#whereas q90.x was concave... Also not significant and far less explantory. interesting.
#seems that q90.x is better so far in these simple models
summary(cm1)
#deviance exaplained is 6.91%

#gratia draw() for community types

cm1_pred <- predict(cm1, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm1_fit = cm1_pred$fit, 
                        cm1_se = cm1_pred$se.fit)

ggplot(data=fulldata.1, aes(x=q90.x, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm1_fit-2*cm1_se),
                  ymax=exp(cm1_fit+2*cm1_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm1_fit))) +
  geom_point() +
  labs(x='max.x', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#some interesting/good fits/ but looks like needs to capture different shapes as well.
#pretty flat across the board.

#we'll need to come back and do AIC and model comparison, but let's continue exploring
#general model functionality -- yay

#but quick left turn, let's take a step back and look at the data
plt_labs = labs(y='PIPO tally', x= 'q90.x CWD post-fire',colour = 'Community')

ggplot(fulldata, aes(x=q90.x, y=pinpon_tally, group=clust.7, colour=clust.7))+
  geom_point()+
  facet_wrap(~clust.7,ncol=7)+
  ylim(0,100)+
  plt_labs


#let's try the tensor product for an interaction, based on topo-position. And add in distance to mama.
cm2 = gam(pinpon_tally~te(q90.x,topo.position, k=c(10,10), bs=c("tp","tp"))+ s(clust.7,k=7,bs="re") + s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm2)

draw(cm2)

#the graphics show fewer PIPO as pinpon distance increases, as would be expected -- lovely!
#then the tensor product shows for higher deficit (e.g., 150) we have more PIPO at lower topographic position, and fewer PIPO at higher position
#whereas at lower deficit (e.g., 110) we have more PIPO at higher topographic position and fewer PIPO at lower position.
#deviance explained = 39.6% (includes pinpon.dist.600)
cm2_pred <- predict(cm2, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm2_fit = cm2_pred$fit, 
                        cm2_se = cm2_pred$se.fit)

ggplot(data=fulldata.1, aes(x=q90.x, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm2_fit-2*cm2_se),
                  ymax=exp(cm2_fit+2*cm2_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm2_fit))) +
  geom_point() +
  labs(x='q90.x', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#topo.position has a global effect of decreasing PIPO with higher topo.position

#this ends up with a very overfitted look to the q90.x relationship. no longer smooth. B/c of interaction?
#trying with pinpon.dist.600
cm2.a = gam(pinpon_tally~s(topo.position, k=10, bs="tp")+ s(clust.7,k=7,bs="re") + s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm2.a)
draw(cm2.a)

#overall, let's move along to "single common smoother plus group-level smoothers (GS models)
#akin to having varying intercepts and varying slopes, same functional response but variability.
#removed bs="tp" from first argument, and added m=s as an argument even though I don't know what it means or does
cm3 = gam(pinpon_tally~s(q90.x, k=10, m=2)+ s(q90.x, clust.7,k=7, bs="fs", m=2), data=fulldata, method="REML", family=nb())
summary(cm3)
#deviance explained 16.5%, significant smooth terms.

draw(cm3) #effects have different slopes in plot.
#working with additional plot from supplementary

cm3_pred <- predict(cm3, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm3_fit = cm3_pred$fit, 
                        cm3_se = cm3_pred$se.fit)

ggplot(data=fulldata.1, aes(x=q90.x, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm3_fit-2*cm3_se),
                  ymax=exp(cm3_fit+2*cm3_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm3_fit))) +
  geom_point() +
  labs(x='q90.x', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#now trying the GI type model that includes random effect for wiggliness
cm4 = gam(pinpon_tally~s(q90.x, k=10, m=2, bs="tp")+ s(q90.x, by=clust.7, k=7, bs="tp", m=1) +s(clust.7, bs="re", k=7), data=fulldata, method="REML", family=nb())
summary(cm4)

#deviance explained 20.5%, good evidence for different shapes for different groups.

draw(cm4) #shows slightly different shapes. 

gam.check(cm4)
qq_plot(cm4)
k.check(cm4)
#now fitting without the global smooth, so individual shapes with no common "root'
#need to make sure use as.factor()
cm5 = gam(pinpon_tally~s(q90.x,clust.7, k=7, m=2, bs="fs"), data=fulldata, method="REML", family=nb())
summary(cm5)
draw(cm5)

#plot shows very different shapes. deviance explained 17.8% (slightly lower than above, so global is helpful?)
cm5_pred <- predict(cm5, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm5_fit = cm5_pred$fit, 
                        cm5_se = cm5_pred$se.fit)

ggplot(data=fulldata.1, aes(x=q90.x, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm5_fit-2*cm5_se),
                  ymax=exp(cm5_fit+2*cm5_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm5_fit))) +
  geom_point() +
  labs(x='q90.x', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#compare using AIC
#remember that cm2 has interaction with topography and pinpon.dist.600 -- not not appleas to apples.
AIC_table <- AIC(cm1,cm2,cm3,cm4,cm5)
#best model is the one with other covariates included, but otherwise cm4 seems best?

#zero-inflated models required?

#now looping back to fill out the model now that I understand things a bit more

cm6.1 = gam(pinpon_tally~te(q90.x,topo.position, k=c(10,10), bs=c("tp","tp"))+ s(cluster.7,k=7,bs="re") + s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm6.1) #39.6%
draw(cm6.1)

#interaction with heatload rather than topo.position
cm6.2 = gam(pinpon_tally~te(q90.x,heat.load, k=c(10,10), bs=c("tp","tp"))+ s(clust.7,k=7,bs="re") + s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm6.2) #38.3% -- heatload is quite interpretable. At highest q90.x we see more PIPO at lower heat load positions and fewer PIPO at high leat load.
draw(cm6.2)

cm6.2_pred <- predict(cm6.2, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm6.2_fit = cm6.2_pred$fit, 
                        cm6.2_se = cm6.2_pred$se.fit)

ggplot(data=fulldata.1, aes(x=q90.x, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm6.2_fit-2*cm6.2_se),
                  ymax=exp(cm6.2_fit+2*cm6.2_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm6.2_fit))) +
  geom_point() +
  labs(x='q90.x', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#just seeing if visualizing the other way helps out
cm6.2.a = gam(pinpon_tally~te(heat.load,q90.x, k=c(10,10), bs=c("tp","tp"))+ s(clust.7,k=7,bs="re") + s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm6.2.a)
draw(cm6.2.a)

#now adding extra exaplantories to the cm4 (GI) model (interactions with heat load and distance to seed source)
#I think this has some redundancies, but that's okay. The goal is to think about whether community matters.
cm6.3 = gam(pinpon_tally~te(heat.load,q90.x, k=c(10,10), bs=c("tp","tp"))+s(pinpon.dist.600)+ s(q90.x, by=clust.7, k=7, bs="tp", m=1) +s(clust.7, bs="re", k=7), data=fulldata, method="REML", family=nb())
summary(cm6.3) #41.5%

#building up a model and comparing for a global smoother -- and as part of this exploring which explanatory
#is best for the stresss gradient

cm7.a = gam(pinpon_tally~ s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.a) #22.2%
draw(cm7.a)

cm7.b = gam(pinpon_tally~te(CMI, heat.load, k=c(10,10), bs=c("tp","tp"))+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.b) #41.8%
draw(cm7.b)

cm7.c = gam(pinpon_tally~s(CMI)+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.c) #30.2%, positive/asymptotic relationship with CMI, better than cm7.d by AIC as well
draw(cm7.c)

cm7.c.a = gam(pinpon_tally~s(heat.load)+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.c.a) #23%, negative relationship (linear) with heatload, cm7.c better with AIC
draw(cm7.c.a)

cm7.d = gam(pinpon_tally~s(CMD)+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.d) #25.1, negative/asymptotic relationship with CMD, but not as good as CMI
draw(cm7.d)

AIC(cm7.d,cm7.c)#equivalent df, but cm7.c is lower AIC (2429 vs 2396**)

cm7.e = gam(pinpon_tally~s(SHM)+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.e) #32%, positive/asymptotic relationship with SHM, and lower AIC than CMI
draw(cm7.e)

cm7.f = gam(pinpon_tally~s(AHM)+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.f) #30.5%, somewhat curvy, not as strong
draw(cm7.f)

summary(cm7.b) #41.8%, AIC is 2363 versus cm7.c is 2396 so added complexity of interaction is warranted
AIC(cm7.b,cm7.c) #significant, but interaction visual isn't clearly interpretable... hmmm think more
draw(cm7.b)

cm7.e.a = gam(pinpon_tally~te(SHM, heat.load, k=c(10,10), bs=c("tp","tp"))+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.e.a) #36.2, so lower than CMI:heat.load even though SHM on own is better than CMI
draw(cm7.e.a)

cm7.g = gam(pinpon_tally~s(q90.x)+s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm7.g) #26.4%, somewhat curvy, not as strong as CMI, same general shape
draw(cm7.g)

#stop here and explore different cluster.7 relationships
#here including random effect for each cluster
#the question is, do any of these relationships differ by cluster
#so try s(CMI), s(90.x), s(heat.load)

#model G for CMI
cm7.hG = gam(pinpon_tally~s(CMI, bs="tp") + s(clust.7,k=7,bs="re")+s(pinpon.dist.600, bs="tp") , data=fulldata, method="REML", family=nb())
summary(cm7.hG) #42.4%, positive/asymptotic relationship with CMI, better than cm7.d by AIC as well
draw(cm7.hG)

AIC(cm7.hG,cm7.c) #with cluster (2319) and without (2396): with cluster is improved

cm_pred <- predict(cm7.hG, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm_fit = cm_pred$fit, 
                        cm_se = cm_pred$se.fit)

ggplot(data=fulldata.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point() +
  labs(x='CMI', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#model GS for CMI asks if different intercept and smoothers (slopes) is a better fit with CMI
#We can ask this for CMI and 
cm7.hGS = gam(pinpon_tally~s(CMI, bs="tp",m=2) + s(CMI,clust.7,bs="fs",m=2) +s(pinpon.dist.600, bs="tp") , data=fulldata, method="REML", family=nb())
#
summary(cm7.hGS) #44.8%, 
AIC(cm7.hG,cm7.hGS) #AIC shows 2319 and 2318 so essentially no support for one over the other
draw(cm7.hGS) #but the draw shows two clusters with very different shape from others.
#suggesting that global function might be inappropriate? Curious to see how this plays out for other explanatories

cm_pred <- predict(cm7.hGS, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm_fit = cm_pred$fit, 
                        cm_se = cm_pred$se.fit)

ggplot(data=fulldata.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point() +
  labs(x='CMI', y='PIPO tally') +
  ylim(0,100) #to focus in on core of data, excludes PIPO tally at 400

#this plot shows different shapes, but generally same hump at -50 to 0 of CMI range, regardless of cluster. But
#different range of values for some community types. And overall much higher values of PIPO
#for 1,5,7

##model GI (single smoother plus group-level smoothers with differeing wiggliness)
#here we're still assuming there's a common global smooth. we'll test that next

cm7.hGI = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) , data=fulldata, method="REML", family=nb())
#
summary(cm7.hGI) # 47.3%
AIC(cm7.hG,cm7.hGS,cm7.hGI) #AIC shows 2319,2318,2307 so GI has more support
draw(cm7.hGI) #draw shows very different shapes for different clusters. But 2,5 have no shape to them (rectangluar error bars)

gam.check(cm7.hGI)
qq_plot(cm7.hGI)
k.check(cm7.hGI)

cm_pred <- predict(cm7.hGI, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm_fit = cm_pred$fit, 
                        cm_se = cm_pred$se.fit)

ggplot(data=fulldata.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point() +
  labs(x='CMI', y='PIPO tally') +
  ylim(0,100) #to focus 


##model S (no global smoother, shared smoothers) -- each group can be differently shaped without restriction, assumes same smoothness but individual shapes unrelated no shared form
#when few data points in each grouping level, estimates from model @ will be much more variables than from model GS since
#no way to share information on function shape between grouping levels wihtout a global smoother
cm7.hS = gam(pinpon_tally~ s(CMI,clust.7,bs="fs",m=2) +s(pinpon.dist.600, bs="tp") , data=fulldata, method="REML", family=nb())
#
summary(cm7.hS) # 46.6%
AIC(cm7.hG,cm7.hGS,cm7.hGI,cm7.hS) #AIC shows 2319,2318,2307 and 2311. so not as good as GS (meaning support for global smoother)
draw(cm7.hS) #draws


cm_pred <- predict(cm7.hS, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm_fit = cm_pred$fit, 
                        cm_se = cm_pred$se.fit)

ggplot(data=fulldata.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point() +
  labs(x='CMI', y='PIPO tally') +
  ylim(0,100) #to focus 

###type 4 is the only one that has observations above 45/50 CMI -- it is what drives the uptick at the tail

#model I (no global smoother and each can have its own wiggliness)
cm7.hI = gam(pinpon_tally~ s(CMI, by=clust.7, bs="tp", m=2) + s(clust.7,bs="re",k=7) +s(pinpon.dist.600, bs="tp")  , data=fulldata, method="REML", family=nb())
#
summary(cm7.hI) # 47.5%
AIC(cm7.hG,cm7.hGS,cm7.hGI,cm7.hS, cm7.hI) #AIC shows 2319,2318,2307, 2311, 2309. So GI, S and I all have equivalent support 
draw(cm7.hI) #draws

cm_pred <- predict(cm7.hI, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                        cm_fit = cm_pred$fit, 
                        cm_se = cm_pred$se.fit)

ggplot(data=fulldata.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point() +
  labs(x='CMI', y='PIPO tally') +
  ylim(0,100) #to focus 

#plot comparison of model cm7.hGI and cm7.hI
libary(tidyr)
library(dplyr)
library(ggplot2)

cm7hGI_fit <- predict(cm7.hGI, 
                        fulldata, 
                        se.fit = TRUE)
cm7hI_fit <- predict(cm7.hI, 
                        fulldata, 
                        se.fit = TRUE)

fulldata.1$modGI_fit <- as.numeric(cm7hGI_fit$fit)
fulldata.1$modI_fit <- as.numeric(cm7hI_fit$fit)

plants_plot_data <- gather(fulldata.1, model, fit, modGI_fit, modI_fit)
plants_plot_data.1 <- mutate(plants_plot_data, se= c(as.numeric(cm7hGI_fit$se.fit),
                                             as.numeric(cm7hI_fit$se.fit)),
                        upper = exp(fit + (2 * se)),
                        lower = exp(fit - (2 * se)),
                        fit   = exp(fit))
#looks good to here 09/06/2022, plants_plot_data.1 has all information in it

#Plot the model output, with means plus standard deviations for each model.
plants_plot_model_labels = paste("Model", c("GI","I"))
plants_plot_model_labels = factor(plants_plot_model_labels, 
                               levels = plants_plot_model_labels)

plants_plot <- ggplot(plants_plot_data.1) +
  facet_wrap(~clust.7, nrow = 7)+#best with nrow=7 for comparisons
    coord_cartesian(ylim=c(0,75))+ 
 # ,scales = "free_y"
  geom_ribbon(aes(x=CMI,
                  ymin = lower,
                  ymax = upper,
                  fill = model),
              alpha=0.2)+
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,50,100,800))),size=1.0)+
    scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,50]" = "blue", "(50,100]"= "yellow", "(100,800]"= "red"),
                       labels=c("<=50", "50-100","100+"))+
    geom_line(aes(x = CMI, y = fit, color = model))+
  labs(y = "PIPO tally", 
       x = "CMI") +
    #scale_fill_brewer(name = "", palette = "Dark2",
                    #labels = plants_plot_model_labels) +
  #scale_colour_brewer(name = "",
                     # palette = "Dark2", labels = plants_plot_model_labels)+
  theme(legend.position = "top")

plants_plot

#without the global trend, community type 3 seems poorly fit (e.g., Model I shows it ramping up at low values because of outlier in mid-range of data?), similarly for type 2.
#overall, Model GI (with the global) looks like best fit 
#this from visualizing model without pinpon600 distance.
#now compare against models with dist.

#Now let's focus on the GI model, but constrain it to pinpon.dist.600<100 and see how results are focused
#without needing to use the distance variable in the model.
#also compare against CMI, heat.load, q90.x, SHM by cluster.7

fulldata.close = fulldata[fulldata$pinpon.dist.600<=100,] #goes from n=539 to n=400 points

#CMI
cm8.hGI = gam(pinpon_tally~s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.close, method="REML", family=nb())
#
summary(cm8.hGI) # 36.8%
draw(cm8.hGI) #

cm_pred <- predict(cm8.hGI, se.fit=TRUE)
fulldata.close.1 <- transform(fulldata.close, 
                        cm_fit = cm_pred$fit, 
                        cm_se = cm_pred$se.fit)

ggplot(data=fulldata.close.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7, nrow=7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,25,75,100))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,25]" = "blue", "(25,75]"= "yellow", "(75,100]"= "red"),
                     labels=c("<=25", "25-75","75-100"))+
  
  #geom_point() +
  labs(x='CMI', y='PIPO tally') +
  ylim(0,100) +#to focus 

  theme(legend.position = "top")
 
#q90.x
cm9.hGI = gam(pinpon_tally~s(q90.x, bs="tp",m=2) + s(q90.x,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.close, method="REML", family=nb())
#but G term of q90.x and then random term by CMI is 40.8 -- really good model.
summary(cm9.hGI) # 20.3%
draw(cm9.hGI) #

cm_pred <- predict(cm9.hGI, se.fit=TRUE)
fulldata.close.1 <- transform(fulldata.close, 
                              cm_fit = cm_pred$fit, 
                              cm_se = cm_pred$se.fit)

ggplot(data=fulldata.close.1, aes(x=q90.x, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7, nrow=7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point(aes(x = q90.x, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,25,75,100))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,25]" = "blue", "(25,75]"= "yellow", "(75,100]"= "red"),
                     labels=c("<=25", "25-75","75-100"))+
  
  #geom_point() +
  labs(x='q90.x', y='PIPO tally') +
  ylim(0,100) +#to focus 
  
  theme(legend.position = "top")

#heat.load
cm10.hGI = gam(pinpon_tally~s(heat.load, bs="tp",m=2) + s(heat.load,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.close, method="REML", family=nb())
summary(cm10.hGI) # 19.9%
draw(cm10.hGI) #

cm_pred <- predict(cm10.hGI, se.fit=TRUE)
fulldata.close.1 <- transform(fulldata.close, 
                              cm_fit = cm_pred$fit, 
                              cm_se = cm_pred$se.fit)

ggplot(data=fulldata.close.1, aes(x=heat.load, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7, nrow=7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point(aes(x = heat.load, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,25,75,100))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,25]" = "blue", "(25,75]"= "yellow", "(75,100]"= "red"),
                     labels=c("<=25", "25-75","75-100"))+
  
  #geom_point() +
  labs(x='heat.load', y='PIPO tally') +
  ylim(0,100) +#to focus 
  
  theme(legend.position = "top")

#interactions again
#take into account pinpondist, then ask about heat.load and CMI according to clust.7:
cm11 = gam(pinpon_tally~te(CMI,heat.load, k=c(10,10), bs=c("tp","tp"))+ s(clust.7,k=7,bs="re") + s(pinpon.dist.600), data=fulldata, method="REML", family=nb())
summary(cm11) #46.8
draw(cm11)
#interaction plot doesn't show very meaningful different for heat.load vs CMI

#more variables in the model, but no interactions, fulldata
cm12.hGI = gam(pinpon_tally~s(pinpon.dist.600) + s(CMI) + s(heat.load, bs="tp",m=2) + s(heat.load,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata, method="REML", family=nb())
summary(cm12.hGI) # 47%, heat.load not important in terms of p-values
draw(cm12.hGI) #

#now change back to full model with pinpon.dist.600 (cm7.hGI)
cm13.hGI = gam(pinpon_tally~s(pinpon.dist.600) + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata, method="REML", family=nb())
summary(cm13.hGI) # 47.3%
draw(cm13.hGI) #
#heat.load not important once CMI is in the model

cm_pred <- predict(cm10.hGI, se.fit=TRUE)
fulldata.close.1 <- transform(fulldata.close, 
                              cm_fit = cm_pred$fit, 
                              cm_se = cm_pred$se.fit)

ggplot(data=fulldata.close.1, aes(x=heat.load, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7, nrow=7) +
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point(aes(x = heat.load, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,25,75,100))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,25]" = "blue", "(25,75]"= "yellow", "(75,100]"= "red"),
                     labels=c("<=25", "25-75","75-100"))+
  
  #geom_point() +
  labs(x='heat.load', y='PIPO tally') +
  ylim(0,100) +#to focus 
  
  theme(legend.position = "top")

#CMI
cm7.hGI = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) , data=fulldata, method="REML", family=nb())
#
summary(cm7.hGI) # 47.3%
draw(cm7.hGI) #

cm_pred <- predict(cm7.hGI, se.fit=TRUE)
fulldata.1 <- transform(fulldata, 
                              cm_fit = cm_pred$fit, 
                              cm_se = cm_pred$se.fit)

ggplot(data=fulldata.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7, nrow=7) +
  coord_cartesian(ylim=c(0,100))+
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,50,150,800))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,50]" = "blue", "(50,150]"= "yellow", "(150,800]"= "red"),
                     labels=c("<=50", "50-150","150+"))+
  
  #geom_point() +
  labs(x='CMI', y='PIPO tally') +
  #ylim(0,100) +#to focus 
  
  theme(legend.position = "top")

#are the high CMI values associated with one particular fire? Yes, Burnt Cabin. And Burnt Cabin is only type 4.

head(fulldata)
boxplot(fulldata$CMI~fulldata$burn)
table(fulldata$clust.7,fulldata$burn)

#remove Burnt Cabin?
#CMI without Burnt Cabin. now range is -74 to 36
fulldata.noBC = subset(fulldata, burn!="Burnt_Cabin")

cm7.hGI.noBC = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) , data=fulldata.noBC, method="REML", family=nb())
#
summary(cm7.hGI.noBC) # 48.4%
draw(cm7.hGI.noBC) #
#brings CMI relationship to hump-shaped. Maybe re-fit all earlier tests?
#shapes now look a bit more different from one another...
#hmm....

cm_pred <- predict(cm7.hGI.noBC, se.fit=TRUE)
fulldata.1 <- transform(fulldata.noBC, 
                        cm_fit = cm_pred$fit, 
                        cm_se = cm_pred$se.fit)

ggplot(data=fulldata.1, aes(x=CMI, y=pinpon_tally, group=clust.7)) +
  facet_wrap(~clust.7, nrow=7) +
  coord_cartesian(ylim=c(0,100))+
  geom_ribbon(aes(ymin=exp(cm_fit-2*cm_se),
                  ymax=exp(cm_fit+2*cm_se)), alpha=0.25) +
  geom_line(aes(y=exp(cm_fit))) +
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,50,150,800))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,50]" = "blue", "(50,150]"= "yellow", "(150,800]"= "red"),
                     labels=c("<=50", "50-150","150+"))+
  
  #geom_point() +
  labs(x='CMI', y='PIPO tally') +
  #ylim(0,100) +#to focus 
  
  theme(legend.position = "top")

#now using other plotting code, to see if it provides smoother fits/intervals


cm7hGI_fit.noBC <- predict(cm7.hGI.noBC, 
                      fulldata.noBC, 
                      se.fit = TRUE)
#cm7hI_fit <- predict(cm7.hI, 
 #                    fulldata, 
  #                   se.fit = TRUE)

fulldata.noBC$modGI_fit_noBC <- as.numeric(cm7hGI_fit.noBC$fit)
#fulldata.1$modI_fit <- as.numeric(cm7hI_fit$fit)

plants_plot_data <- gather(fulldata.noBC, model, fit, modGI_fit_noBC)
plants_plot_data.1 <- mutate(plants_plot_data, se= as.numeric(cm7hGI_fit.noBC$se.fit),
                             upper = exp(fit + (2 * se)),
                             lower = exp(fit - (2 * se)),
                             fit   = exp(fit))

#Plot the model output, with means plus standard deviations for each model.
#plants_plot_model_labels = paste("Model", c("GI","I"))
#plants_plot_model_labels = factor(plants_plot_model_labels, 
#                                  levels = plants_plot_model_labels)

plants_plot <- ggplot(plants_plot_data.1) +
  facet_wrap(~clust.7, nrow = 7)+#best with nrow=7 for comparisons
  coord_cartesian(ylim=c(0,75))+ 
  # ,scales = "free_y"
  geom_ribbon(aes(x=CMI,
                  ymin = lower,
                  ymax = upper,
                  fill = model),
              alpha=0.2)+
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,50,100,800))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,50]" = "blue", "(50,100]"= "yellow", "(100,800]"= "red"),
                     labels=c("<=50", "50-100","100+"))+
  geom_line(aes(x = CMI, y = fit, color = model))+
  labs(y = "PIPO tally", 
       x = "CMI") +
  #scale_fill_brewer(name = "", palette = "Dark2",
  #labels = plants_plot_model_labels) +
  #scale_colour_brewer(name = "",
  # palette = "Dark2", labels = plants_plot_model_labels)+
  theme(legend.position = "top")

plants_plot

###September 13th revisiting relationship using the noBC dataset.

boxplot(fulldata$q90.x~fulldata$burn)

cm1 = gam(pinpon_tally~s(q90.x, k=10, bs="tp")+ s(clust.7,k=7,bs="re"), data=fulldata.noBC, method="REML", family=nb())
summary(cm1)
draw(cm1) #concave shape for q90.x global
#same shape with and without BC for heat.load and q90.x

#CMI is still the best predictor, but let's check on the alternative models again
cm7.hGI.noBC = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) , data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC) #48.4%
draw(cm7.hGI.noBC)

cm7.c = gam(pinpon_tally~s(CMI)+s(pinpon.dist.600), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.c) #30.7%, humpshaped relationship with CMI
draw(cm7.c)

#G model
cm7.hG.noBC = gam(pinpon_tally~s(CMI, bs="tp") + s(clust.7,k=7,bs="re")+s(pinpon.dist.600, bs="tp") , data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hG.noBC) #43.1%, positive/asymptotic relationship with CMI, better than cm7.d by AIC as well
draw(cm7.hG.noBC)

#plot G model

cm7hG_fit.noBC <- predict(cm7.hG.noBC, 
                           fulldata.noBC, 
                           se.fit = TRUE)

fulldata.noBC$modG_fit_noBC <- as.numeric(cm7hG_fit.noBC$fit)
#fulldata.1$modI_fit <- as.numeric(cm7hI_fit$fit)

plants_plot_data <- gather(fulldata.noBC, model, fit, modG_fit_noBC)
plants_plot_data.1 <- mutate(plants_plot_data, se= as.numeric(cm7hG_fit.noBC$se.fit),
                             upper = exp(fit + (2 * se)),
                             lower = exp(fit - (2 * se)),
                             fit   = exp(fit))

#Plot the model output, with means plus standard deviations for each model.
#plants_plot_model_labels = paste("Model", c("GI","I"))
#plants_plot_model_labels = factor(plants_plot_model_labels, 
#                                  levels = plants_plot_model_labels)

plants_plot <- ggplot(plants_plot_data.1) +
  facet_wrap(~clust.7, nrow = 7)+#best with nrow=7 for comparisons
  coord_cartesian(ylim=c(0,75))+ 
  # ,scales = "free_y"
  geom_ribbon(aes(x=CMI,
                  ymin = lower,
                  ymax = upper,
                  fill = model),
              alpha=0.2)+
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(pinpon.dist.600,c(-Inf,50,100,800))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,50]" = "blue", "(50,100]"= "yellow", "(100,800]"= "red"),
                     labels=c("<=50", "50-100","100+"))+
  geom_line(aes(x = CMI, y = fit, color = model))+
  labs(y = "PIPO tally", 
       x = "CMI") +
  #scale_fill_brewer(name = "", palette = "Dark2",
  #labels = plants_plot_model_labels) +
  #scale_colour_brewer(name = "",
  # palette = "Dark2", labels = plants_plot_model_labels)+
  theme(legend.position = "top")

plants_plot

#model GS for CMI asks if different intercept and smoothers (slopes) is a better fit with CMI
#We can ask this for CMI and 
cm7.hGS.noBC = gam(pinpon_tally~s(CMI, bs="tp",m=2) + s(CMI,clust.7,bs="fs",m=2) +s(pinpon.dist.600, bs="tp") , data=fulldata.noBC, method="REML", family=nb())
#
summary(cm7.hGS.noBC) #45.6.8%, 
draw(cm7.hGS.noBC) #the draw shows two clusters with very different shape from others.

#hGI (above)


##model S (no global smoother, shared smoothers) -- each group can be differently shaped without restriction, assumes same smoothness but individual shapes unrelated no shared form
#when few data points in each grouping level, estimates from model @ will be much more variables than from model GS since
#no way to share information on function shape between grouping levels wihtout a global smoother
cm7.hS.noBC = gam(pinpon_tally~ s(CMI,clust.7,bs="fs",m=2) +s(pinpon.dist.600, bs="tp") , data=fulldata.noBC, method="REML", family=nb())
#
summary(cm7.hS.noBC) # 47.8%
AIC(cm7.hG.noBC,cm7.hGS.noBC,cm7.hGI.noBC,cm7.hS.noBC) #AIC shows hGI is still best 
draw(cm7.hS.noBC) #draws

#CMI is still the best predictor, let's include some randome effects for q90.x and heat.load
#then see if we can develop prediction plot using mean pinpon.dist.600

cm7.hGI.noBC = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) , data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC) #48.4%
draw(cm7.hGI.noBC, residuals=T,scales=c("fixed")) #good plot

cm7.hGI.noBC.a = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) +s(q90.x), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.a) #51.3%, so improved with q90.x added
draw(cm7.hGI.noBC.a, residuals=T,scales=c("fixed")) #good plot

AIC(cm7.hGI.noBC, cm7.hGI.noBC.a) #and reduces AIC, so q90.x improves the model.

cm7.hGI.noBC.b = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) +s(heat.load), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.b) #49.1%, no real trend in heat.load data.
draw(cm7.hGI.noBC.b, residuals=T,scales=c("fixed")) #good plot

cm7.hGI.noBC.c = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) +s(q90.x,by=clust.7,bs="tp",m=1), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.c) #53.3%, so slightly improved with q90.x cluster effect added
draw(cm7.hGI.noBC.c, residuals=T,scales=c("fixed")) #good plot

AIC(cm7.hGI.noBC.a, cm7.hGI.noBC.c) #.a has lowest AIC so no evidence to support inclusion of random on q90.x

##so I think the "best fit" model in our group is the cm7.hGI.noBC.a with s(q90.x)

cm7.hGI.noBC.d = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.d) #49%, with mean.x added
draw(cm7.hGI.noBC.d, residuals=T,scales=c("fixed")) #good plot

#q75.x is interpretable linear negative, mildly significant
#whereas q90.x in uninterpreatble curvy, more significant and lower AIC
#BUT I think q75.x makes more sense ecologically. AIC 2222.134
#and preserves shapes in CMI effects

#max.x has same nice linear decrease, and is more significant than q97.5, but AIC still doesn't beat q90.x curves
#SO max.x is now my current favorite 2220.957

#mean.x even moreso. AIC 2219.392. And has lowest AIC of the non-curvy percentile relationships.

AIC(cm7.hGI.noBC.d)

#no global
cm7.hS.noBC = gam(pinpon_tally~ s(CMI,clust.7,bs="fs",m=2) +s(pinpon.dist.600, bs="tp") + s(mean.x) , data=fulldata.noBC, method="REML", family=nb())
#
summary(cm7.hS.noBC) # 48.2%
AIC(cm7.hS.noBC) #2227.367
draw(cm7.hS.noBC)

#model I (no global smoother and each can have its own wiggliness)
cm7.hI.noBC = gam(pinpon_tally~ s(CMI, by=clust.7, bs="tp", m=2) + s(clust.7,bs="re",k=7) +s(pinpon.dist.600, bs="tp") + s(mean.x) +s(elevation) , data=fulldata, method="REML", family=nb())
#
summary(cm7.hI.noBC) # 48.2%
draw(cm7.hI.noBC, residuals=T)
AIC(cm7.hI.noBC) #2304

#trying out some other predictors, terrain, PC1/PC2 to confirm.

cm7.hGI.noBC.x = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(PC1, bs="tp",m=2) + s(PC1,by=clust.7,bs="tp",m=1)+s(PC1,bs="re", k=7) + s(topo.wetness) + s(mean.x), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.x)
draw(cm7.hGI.noBC.x, residuals=T,scales=c("fixed"))
AIC(cm7.hGI.noBC.x, cm7.hGI.noBC.d)

cm7.hGI.noBC.x = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, k=10, bs="tp")+ s(clust.7,k=7,bs="re") , data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.x)
draw(cm7.hGI.noBC.x, residuals=T,scales=c("fixed"))
AIC(cm7.hGI.noBC.x) #2279

##so I think the "best fit" model in our group is the cm7.hGI.noBC.a with s(mean.x) and s(elevation)

cm7.hGI.noBC.d = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.d) #50.1%, with elevation added
draw(cm7.hGI.noBC.d, residuals=T,scales=c("fixed")) #good plot

#next step is to do prediction plots to look at overall PIPO tallies (using mean values, or 100m)
#generate dataframe with mean (or median?) values for elevation, mean.x, nad pinpon.dist.600

boxplot(fulldata.noBC$elevation)
summary(fulldata.noBC$elevation) #median = 2322.1, mean = 2231.3

boxplot(fulldata.noBC$mean.x)
summary(fulldata.noBC$mean.x) #median = 50.91, mean = 53.11

boxplot(fulldata.noBC$pinpon.dist.600)
summary(fulldata.noBC$pinpon.dist.600) #median = 39.40, mean = 125.28

#select median values for all

fulldata.noBC.fixed = fulldata.noBC

fulldata.noBC.fixed$pinpon.dist.600 = rep(39.4, length(fulldata.noBC.fixed$pinpon.dist.600))
fulldata.noBC.fixed$mean.x = rep(50.91, length(fulldata.noBC.fixed$mean.x))
fulldata.noBC.fixed$elevation = rep(2322.1, length(fulldata.noBC.fixed$elevation))


bar = c("1 - Bit of everything, but higher perennials", "2 - Annual forbs and grasses","3 - Evergreen shrubs", "4 - Deciduous shrubs","5 - Evergreen trees","6 - Deciduous trees","7 - Bit of everything, more woody species") 
names(bar) = seq(1,7,1)

#need plyr for this one, then re-load dplyr
fulldata.noBC.fixed$clust.7.names<- as.character(revalue(fulldata.noBC.fixed$clust.7, replace = bar))



#plot GI.d model holding values fixed

cm7hGI_fit.noBC.d <- predict(cm7.hGI.noBC.d, 
                          fulldata.noBC.fixed, 
                          se.fit = TRUE)

fulldata.noBC.fixed$modGI_fit_noBC.d <- as.numeric(cm7hGI_fit.noBC.d$fit)

plants_plot_data <- gather(fulldata.noBC.fixed, model, fit, modGI_fit_noBC.d)
plants_plot_data.1 <- mutate(plants_plot_data, se= as.numeric(cm7hGI_fit.noBC.d$se.fit),
                             upper = exp(fit + (2 * se)),
                             lower = exp(fit - (2 * se)),
                             fit   = exp(fit))

#Plot the model output, with means plus standard deviations for each model.
#plants_plot_model_labels = paste("Model", c("GI","I"))
#plants_plot_model_labels = factor(plants_plot_model_labels, 
#                                  levels = plants_plot_model_labels)


plants_plot <- ggplot(plants_plot_data.1) +
  facet_wrap(~clust.7.names, nrow = 7 )+#best with nrow=7 for comparisons, names???
  coord_cartesian(ylim=c(0,75))+ 
  # ,scales = "free_y"
  geom_ribbon(aes(x=CMI,
                  ymin = lower,
                  ymax = upper,
                  fill = model),
              alpha=0.2)+
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(fulldata.noBC$pinpon.dist.600,c(-Inf,50,100,800))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,50]" = "blue", "(50,100]"= "yellow", "(100,800]"= "red"),
                     labels=c("<=50", "50-100","100+"))+
  geom_line(aes(x = CMI, y = fit, color = model))+
  labs(y = "PIPO tally", 
       x = "CMI") +
  #scale_fill_brewer(name = "", palette = "Dark2",
  #labels = plants_plot_model_labels) +
  #scale_colour_brewer(name = "",
  # palette = "Dark2", labels = plants_plot_model_labels)+
  theme(legend.position = "top")

plants_plot

getwd()

save.image()

library(mgcv)
library(gratia)

#feedback from Sandra, suggesting "unorder" cluster groups, use select = TRUE for model selection

#dat <- transform(dat, covertypeUO=factor(covertype, ordered=FALSE))
fulldata.noBC = transform(fulldata.noBC, clust.7.UO=factor(clust.7, ordered=FALSE))
#no change in results, checked is.ordered(fulldata.noBC$clust.7) and returns FALSE
cm7.hGI.noBC.d = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7.UO,bs="tp",m=1)+s(clust.7.UO,bs="re", k=7) + s(mean.x) + s(elevation), data=fulldata.noBC, method="REML", family=nb())

#so return to original for simplicity
cm7.hGI.noBC.d = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation), data=fulldata.noBC, method="REML", family=nb())

#test out model selection with select = TRUE
cm7.hGI.noBC.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation), data=fulldata.noBC, method="REML", family=nb(), select=TRUE)
summary(cm7.hGI.noBC.d.select)
draw(cm7.hGI.noBC.d.select, residuals=T,scales=c("fixed"))
#seems to have the same look in plots/shapes, though slightly different numbers?

#try with adding remaining topo variables (elevation, heat.load, local.slope, relative.position) and additional q90.x variable
cm7.hGI.noBC.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation) + s(heat.load)+s(relative.position)+s(local.slope)+s(q90.x), data=fulldata.noBC, method="REML", family=nb(), select=TRUE)
summary(cm7.hGI.noBC.d.select)
draw(cm7.hGI.noBC.d.select, residuals=T,scales=c("fixed"))
#elevation sign again, heat.load/relative.position no, local.slope yes, q90.x yes but again more curvilinear than the interpretable mean.x

#add local.aspect, topo.wetness, topo.position, focaldist

cm7.hGI.noBC.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation) + s(heat.load)+s(relative.position)+s(local.slope)+s(topo.wetness)+s(local.aspect)+s(topo.position)+s(focaldist), data=fulldata.noBC, method="REML", family=nb(), select=TRUE)
#52.1% suggests focaldist, relative.position, local.slope, mean.x may be additionally useful to base CMI, elevation, and pinpondist.600
#test using AIC

AIC(cm7.hGI.noBC.d, cm7.hGI.noBC.d.select)#2210 vs 2193 (delta 17) so select is better model, and 52.1% vs. 50.1%

#now scatter plot matrix
install.packages("gpairs")
library(gpairs)

gpairs(fulldata.noBC[,c(2,5,7:16,19,20,43,55,60)])
#shows that mean.x/q90.x are independent of CMI/SHM, and some variability between CMI and SHM.
#SHM closely correlated to elevation, cor() is -0.82

#now retest mean.x/q90.x vs CMI, vs SHM in core model

#CMI core model
cm7.hGI.noBC.try1 = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.try1) #48.4%
draw(cm7.hGI.noBC.try1, residuals=T,scales=c("fixed"))

#q90.x model
cm7.hGI.noBC.try2 = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(q90.x, bs="tp",m=2) + s(q90.x,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.try2) #39.8%
draw(cm7.hGI.noBC.try2, residuals=T,scales=c("fixed"))
#q90.x not as good as CMI

#mean.x model
cm7.hGI.noBC.try3 = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(mean.x, bs="tp",m=2) + s(mean.x,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.try3) #36%
draw(cm7.hGI.noBC.try3, residuals=T,scales=c("fixed"))
AIC(cm7.hGI.noBC.try1, cm7.hGI.noBC.try3)#2224 for try1 vs 2299 for try3
#mean.x not as good as CMI, also supported with AIC

#CMD model
cm7.hGI.noBC.try4 = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMD, bs="tp",m=2) + s(CMD,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.try4)#44.5%
draw(cm7.hGI.noBC.try4, residuals=T,scales=c("fixed"))
AIC(cm7.hGI.noBC.try1, cm7.hGI.noBC.try4) #2224 for try1 vs 2263 for try4

#SHM model
cm7.hGI.noBC.try5 = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(SHM, bs="tp",m=2) + s(SHM,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7), data=fulldata.noBC, method="REML", family=nb())
summary(cm7.hGI.noBC.try5)#40.3%
draw(cm7.hGI.noBC.try5, residuals=T,scales=c("fixed"))
AIC(cm7.hGI.noBC.try1, cm7.hGI.noBC.try5)#try1 2224 vs try5 2263

#confirmed, CMI is the best core model

cm7.hGI.noBC.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation) + s(relative.position)+s(local.slope)+s(focaldist), data=fulldata.noBC, method="REML", family=nb(), select=TRUE)
summary(cm7.hGI.noBC.d.select)
draw(cm7.hGI.noBC.d.select, residuals=T,scales=c("fixed"))

#plot GI.d model holding values fixed
library(tidyr)
library(dplyr)
library(ggplot2)

#need to include additionals: relative.position, local.slope, focaldist

boxplot(fulldata.noBC$relative.position)
summary(fulldata.noBC$relative.position) #median = 0.525, mean = 0.529

boxplot(fulldata.noBC$local.slope)
summary(fulldata.noBC$local.slope) #median = 0.211, mean = 0.222

boxplot(fulldata.noBC$focaldist)
summary(fulldata.noBC$focaldist) #median = 0.345, mean = 0.357

#select median values for all

# generated earlier: fulldata.noBC.fixed = fulldata.noBC

fulldata.noBC.fixed$relative.position = rep(0.525, length(fulldata.noBC.fixed$relative.position))
fulldata.noBC.fixed$local.slope = rep(0.211, length(fulldata.noBC.fixed$local.slope))
fulldata.noBC.fixed$focaldist = rep(0.345, length(fulldata.noBC.fixed$focaldist))


#bar = c("1 - Bit of everything, but higher perennials", "2 - Annual forbs and grasses","3 - Evergreen shrubs", "4 - Deciduous shrubs","5 - Evergreen trees","6 - Deciduous trees","7 - Bit of everything, more woody species") 
#names(bar) = seq(1,7,1)

#need plyr for this one, then re-load dplyr
#fulldata.noBC.fixed$clust.7.names<- as.character(revalue(fulldata.noBC.fixed$clust.7, replace = bar))

#########################

cm7hGI_fit.noBC.d.select <- predict(cm7.hGI.noBC.d.select, 
                             fulldata.noBC.fixed, 
                             se.fit = TRUE)

fulldata.noBC.fixed$modGI_fit_noBC.d.select <- as.numeric(cm7hGI_fit.noBC.d.select$fit)

plants_plot_data <- gather(fulldata.noBC.fixed, model, fit, modGI_fit_noBC.d.select)
plants_plot_data.1 <- mutate(plants_plot_data, se= as.numeric(cm7hGI_fit.noBC.d.select$se.fit),
                             upper = exp(fit + (2 * se)),
                             lower = exp(fit - (2 * se)),
                             fit   = exp(fit))

#Plot the model output, with means plus standard deviations for each model.
#plants_plot_model_labels = paste("Model", c("GI","I"))
#plants_plot_model_labels = factor(plants_plot_model_labels, 
#                                  levels = plants_plot_model_labels)


plants_plot <- ggplot(plants_plot_data.1) +
  facet_wrap(~clust.7.names, nrow = 7 )+#best with nrow=7 for comparisons, names???
  coord_cartesian(ylim=c(0,75))+ 
  # ,scales = "free_y"
  geom_ribbon(aes(x=CMI,
                  ymin = lower,
                  ymax = upper,
                  fill = model),
              alpha=0.2)+
  geom_point(aes(x = CMI, y = pinpon_tally, color= cut(fulldata.noBC$pinpon.dist.600,c(-Inf,50,100,800))),size=1.0)+
  scale_color_manual(name="pinpon.dist.600", values=c("(-Inf,50]" = "blue", "(50,100]"= "yellow", "(100,800]"= "red"),
                     labels=c("<=50", "50-100","100+"))+
  geom_line(aes(x = CMI, y = fit, color = model))+
  labs(y = "PIPO tally", 
       x = "CMI") +
  #scale_fill_brewer(name = "", palette = "Dark2",
  #labels = plants_plot_model_labels) +
  #scale_colour_brewer(name = "",
  # palette = "Dark2", labels = plants_plot_model_labels)+
  theme(legend.position = "top")

plants_plot

#October 18, 2022
#Sandra has generated climatology for CWD using the same data as q90.x variables, so more consistent data use for water balance metrics.
#So now want to re-run models swapping out CMI for this new data field.

cwd.import = read.delim("I:\\Box Sync\\meg\\projects\\refugia\\extensive dataset analyses\\data 13 fires\\for r analyses\\from sandra\\allvars_plus_Dnormals.csv", sep=",", header=T)

names(cwd.import)
names(fulldata.noBC)

#merge new CWD variables to fulldata.noBC, Dnormals1981_2010 and Dnormals1991_2020
#merge using plot.id.x

length(cwd.import$plot.id.x)
length(fulldata.noBC$plot.id.x) #with 21 points from Burnt Cabin removed

plot(cwd.import$CMI, cwd.import$Dnormals1981_2010) #fairly similar but also some differences
plot(cwd.import$Dnormals1981_2010, cwd.import$Dnormals1991_2020) #almost linear

#fulldata is dataset where NA's in pinpon.dist were replaced with 600s, so use that one
fulldata$plot.id.x
cwd.import$plot.id.x

fulldata$Dnormals1981_2010 = cwd.import$Dnormals1981_2010
fulldata$Dnormals1991_2020 = cwd.import$Dnormals1991_2020

#try updated model on full dataset as well, just to see if BC data aren't as much of an outlier

cm7.hGI.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(Dnormals1981_2010, bs="tp",m=2) + s(Dnormals1981_2010,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation) + s(relative.position)+s(local.slope)+s(focaldist), data=fulldata, method="REML", family=nb(), select=TRUE)
summary(cm7.hGI.d.select)
draw(cm7.hGI.d.select, residuals=T,scales=c("fixed"))

#interesting, the Dnormals term isn't significant in this model, down to 43.7% deviance explained, aic = 2337.1
#try with 1991_2020, and against CMI term for whole dataset:

cm7.hGI.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(Dnormals1991_2020, bs="tp",m=2) + s(Dnormals1991_2020,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation) + s(relative.position)+s(local.slope)+s(focaldist), data=fulldata, method="REML", family=nb(), select=TRUE)
summary(cm7.hGI.d.select)
draw(cm7.hGI.d.select, residuals=T,scales=c("fixed"))

#same for 1991_2020. the Dnormals isn't significant on its own and deviance explained drops to 43.1%

cm7.hGI.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation) + s(relative.position)+s(local.slope)+s(focaldist), data=fulldata, method="REML", family=nb(), select=TRUE)
summary(cm7.hGI.d.select)
draw(cm7.hGI.d.select, residuals=T,scales=c("fixed"))
AIC(cm7.hGI.d.select)

#with CMI we have deviance explained at 50.9%, aic = 2276.9 (delta AIC of 60, improvement)

#back to best models with CMI and nobC

cm7.hGI.noBC.d.select = gam(pinpon_tally~s(pinpon.dist.600, bs="tp") + s(CMI, bs="tp",m=2) + s(CMI,by=clust.7,bs="tp",m=1)+s(clust.7,bs="re", k=7) + s(mean.x) + s(elevation) + s(relative.position)+s(local.slope)+s(focaldist), data=fulldata.noBC, method="REML", family=nb(), select=TRUE)
summary(cm7.hGI.noBC.d.select)
draw(cm7.hGI.noBC.d.select, residuals=T,scales=c("fixed"))


getwd()

save.image()
