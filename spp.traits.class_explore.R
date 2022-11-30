## spp.traits.class_explore.R
## first use the vegan wrappers to explore
## then try the mclust fnc to see how the groups fall out
## 7 nov 2022 revisted 29 nov 2022
## note from meg
## Here’s my updated community functional groups file that now includes the 
## information on nitrogen fixing forbs and shrubs.
## So there are now 11 functional groups that each include the sum of plant 
## cover for that type at a give plot, an then the sum cover (cover.total):
## PLOT_ID
## forb.a
## forb.p1
## forb.p.n
## gram.a
## gram.p
## shru.d1
## shru.e1
##  shru.e.n
## shru.d.n
## tree.e
## tree.d
##  cover.total

##I tried to do some clustering with these new data using your vegan/hclust code, 
##and ended not having much luck cracking the big group(s). Maybe you can give 
## it another round?

library(tidyr)
library(readr)
library(dplyr)
library(cluster)
library(mclust)
library(vegan)
library(ggplot2)
library(ggpubr)

## traits class data
## type values are sum of cover estimates
z<-read_csv("./data/community_functional_groups_11182022.csv") #543
z2<-z %>% select(forb.a:tree.d) ## cols w traits dta
## follow vegan tutorial
#par(mfrow=c(1,1))
## tried several vegdist methods and found that chord is useful in distributing 
## the samples across groups whereas other methods tend to put a large proportion
## of the samples in a single group
## Chord distances ("chord") are Euclidean distance of a matrix where rows are 
## standardized to unit norm (their sums of squares are 1) using decostand. 
## Geometrically this standardization moves row points to a surface of 
## multidimensional unit sphere, and distances are the chords across the hypersphere.
## 
## the complete cluster method did the best job in combo with chord dist judging
## by the same criteria
hcc <-z2 %>%
   vegdist(method="chord") %>% hclust(method = "complete")
plot(hcc, hang=-1) 
cl1 <- cutree(hcc, 6)
table(cl1)

hca <-z2 %>% 
   vegdist(method="chord") %>% hclust(method = "aver") 
plot(hca, hang=-1)
cl2 <- cutree(hca, 6)
table(cl2)

hcs <-z2 %>%
   vegdist(method="chord") %>% hclust(method = "single")
plot(hcs, hang=-1)
cl3 <- cutree(hcs, 6)
table(cl3)

## next want to see what 6 classes for cl1 look like...
## chord distance, complete method
#cl1
#  1   2   3   4   5   6 
# 154 140  83  88  44  33 
z2$chordcomplete<-as.factor(cl1)
by_class <- z2 %>%
  group_by(chordcomplete)
class.stats<-by_class %>%
  summarise_all(list(min, mean, max))
write_csv(class.stats, "./data/chordcomplete_stats.csv")

## plot the stats by class
z2p<- z2 %>% pivot_longer(cols=forb.a:tree.d)
p<-ggboxplot(z2p, x="name", y="value", color="name",facet.by="chordcomplete")
ggpar(p, ylab=FALSE, 
main="Six classes based on hierarchical clustering with chord distance", 
ggtheme=theme_grey(), x.tickslab=FALSE, legend="none", rotate=TRUE)
ggsave("./plots/chordcompleteclassesbox2.png")

###################################### mclust
set.seed(610)
bic<-z %>% select(forb.a:tree.d) %>% 
  mclustBIC() # parameterize mixture models. 
              # default G=1:9
              # modelNames default is taken from data values
             
plot(bic) 
summary(bic)
#Best BIC values:
#            EEE,1    EEV,1    EVE,1
# BIC      57982.24 57982.24 57982.24
# BIC diff     0.00     0.00     0.00
mod1 <- z %>% select(forb.a:tree.d) %>% 
  Mclust(x = bic)
sum1<-summary(mod1, parameters = TRUE) # need this due to num vars
sum1$mean
sum1$printClassification # TRUE
sum1$classification  ## add these classes to the original data

sum1$printParameters # TRUE
sum1$pro # proportion of samples in each class
#         1          2          3          4          5          6          7          8 
#0.04972376 0.17011926 0.04545216 0.16679741 0.20575444 0.03314833 0.22819240 0.07871299 
 #        9 
#0.02209926 
## too many vars for single plots...
plot(mod1, what="classification")

## put the classes together with the data and summarize
z2$mclust9class<-as.factor(sum1$classification)
## count per class
z2 %>% group_by(mclust9class) %>% count() %>% arrange(-n)

by_class <- z2 %>%
  group_by(mclust9class)
class.stats<-by_class %>%
  summarise_all(list(min, mean, max))
write_csv(class.stats, "./data/mclust9_stats.csv")

## plot the stats by class
z2p<- z2 %>% pivot_longer(cols=forb.a:tree.d)
p<-ggboxplot(z2p, x="name", y="value", color="name",facet.by="mclust9class")
ggpar(p, ylab=FALSE, 
main="Nine classes based on mclust (BIC)", 
ggtheme=theme_grey(), x.tickslab=FALSE, legend="none", rotate=TRUE)
ggsave("./plots/mclust9box.png")



################################## still working on this section....
################################# after we figure out joining datasets...


## mds
mds1<-metaMDS(z2, k=3, try=20, trymax=100) # had to incr k to reach solution
plot(mds1)
ordiplot(mds1,type="n") #Ordination plot function especially for congested plots
orditorp(mds1,display="species",col="red",air=0.01) #The function adds text or points to ordination plots
#orditorp(mds1,display="sites",cex=1.25,air=0.01) too messy

# plot a stress gradient
dat<-read_csv("./data/tab/allvars_plus_Dnormals.csv") # 539 rows (z has 542)
dat$PLOT_ID<-dat$plot.id.x
j1<-inner_join(z, dat, by="PLOT_ID") # 427
z3<-j1 %>% select(forb.a:forb.p1)
mds1<-metaMDS(z3, k=3, try=20, trymax=100) # had to incr k to reach solution
# Use the function ordisurf to plot contour lines
with(j1, ordisurf(mds1,heat.load,main="",col="forestgreen") )
# Finally, display species on plot
orditorp(mds1,display="species",col="red",air=0.1, cex=1)



################ scratch
data(dune, dune.taxon)
taxontree <- hclust(taxa2dist(dune.taxon))
plotree <- hclust(vegdist(dune), "average")
## Automatic reordering of clusters
tabasco(dune, plotree, sp.ind = taxontree) # compares two resutls
