## plot.def.stats.R
## see def.data.assembly.R for previous steps
## plotting statistics of the overall distribution of def values at each sample point thru time

rm(list=ls()) ### clear workspace 

library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(wesanderson)

pal1=wes_palette("Zissou1", 100, type="continuous")
pal2=wes_palette("Rushmore1", 100, type = "continuous")
pal3=wes_palette("FantasticFox1", 100, type = "continuous")
pal4=wes_palette("Moonrise2", 100, type = "continuous")
pal5=wes_palette("Moonrise1", 100, type = "continuous")
pal6=wes_palette("GrandBudapest1")
pal7=wes_palette("Cavalcanti1", 100, type="continuous")
pal8=wes_palette("Darjeeling2", 20, type="continuous")
pal9=wes_palette("Moonrise3", 100, type = "continuous")
pal13 = c("#E41A1C", "#705C83", "#3E8E93", "#4DAF4A", "#7E6E85", "#BA5E6C",
        "#FF7F00", "#FFD421", "#E1C62F", "#A65628", "#DB728C", "#D789B2", "#999999")
pal20=c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
        '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', 
        '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')

## read in data
d1<-read.table("./data/tab/def.stats1.txt")

## box plot
g1<-ggboxplot(d1, x="Burn", y="q90.x", color="black", fill="Burn", palette=pal13, ggtheme=theme_gray())
ggpar(g1, x.text.angle=45, title="Def: 90th quantile (post-fire years)")
ggsave("./plots/def.q90box.png")

g2<-ggboxplot(d1, x="Burn", y="q75.x", color="black", fill="Burn", palette=pal13, ggtheme=theme_gray())
ggpar(g2, x.text.angle=45, title="Def: 75th quantile (post-fire years)")
ggsave("./plots/def.q75box.png")

g3<-ggboxplot(d1, x="Burn", y="var.x", color="black", fill="Burn", palette=pal13, ggtheme=theme_gray())
ggpar(g3, x.text.angle=45,title="Def: variance (post-fire years)")
ggsave("./plots/def.varbox.png")

g4<-ggboxplot(d1, x="Burn", y="mean.x", color="black", fill="Burn", palette=pal13, ggtheme=theme_gray())
ggpar(g4, x.text.angle=45, title="Def: mean (post-fire years)")
ggsave("./plots/def.meanbox.png")

g5<-ggboxplot(d1, x="Burn", y="max.x", color="black", fill="Burn", palette=pal13, ggtheme=theme_gray())
ggpar(g5, x.text.angle=45, title="Def: max (post-fire years)")
ggsave("./plots/def.maxbox.png")

## hex plot
p1<-ggplot(d1, aes(MAP1.1, MAP1.2, z=q90.x))
p2<-ggplot(d1, aes(MAP1.1, MAP1.2, z=q75.x))
p3<-ggplot(d1, aes(MAP1.1, MAP1.2, z=max.x))
p4<-ggplot(d1, aes(MAP1.1, MAP1.2, z=mean.x))
p5<-ggplot(d1, aes(MAP1.1, MAP1.2, z=var.x))

p1 + stat_summary_hex(bins=30) + 
  scale_fill_gradientn(colors=pal1) + #facet_wrap(vars(Burn)) + 
  #theme(strip.background = element_blank(), strip.placement = "outside",  strip.text = element_text(size = 6)) +  
  labs(title = "Def: 90th quantile", x = "pc1", y = "pc2",fill="q90.x") 
ggsave("./plots/def.q90.png")
p2 + stat_summary_hex(bins=30) + 
  scale_fill_gradientn(colors=pal1) + #facet_wrap(vars(Burn)) + 
  #theme(strip.background = element_blank(), strip.placement = "outside",  strip.text = element_text(size = 6)) +  
  labs(title = "Def: 75th quantile", x = "pc1", y = "pc2",fill="q75.x") 
ggsave("./plots/def.q75.png")
p3 + stat_summary_hex(bins=30) + 
  scale_fill_gradientn(colors=pal1) + #facet_wrap(vars(Burn)) + 
  #theme(strip.background = element_blank(), strip.placement = "outside",  strip.text = element_text(size = 6)) +  
  labs(title = "Def: Maximum value", x = "pc1", y = "pc2",fill="max.x") 
ggsave("./plots/def.max.png")
p4 + stat_summary_hex(bins=30) + 
  scale_fill_gradientn(colors=pal1) + #facet_wrap(vars(Burn)) + 
  #theme(strip.background = element_blank(), strip.placement = "outside",  strip.text = element_text(size = 6)) +  
  labs(title = "Def: mean value", x = "pc1", y = "pc2",fill="mean.x") 
ggsave("./plots/def.mean.png")
p5 + stat_summary_hex(bins=30) + 
  scale_fill_gradientn(colors=pal1) + #facet_wrap(vars(Burn)) + 
  #theme(strip.background = element_blank(), strip.placement = "outside",  strip.text = element_text(size = 6)) +  
  labs(title = "Def: variance", x = "pc1", y = "pc2",fill="var.x") 
ggsave("./plots/def.var.png")
