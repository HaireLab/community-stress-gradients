## csplots.pcs.R

library(ggplot2)
library(ggpubr)

pal = c("#E41A1C", "#705C83", "#3E8E93", "#4DAF4A", "#7E6E85", "#BA5E6C",
        "#FF7F00", "#FFD421", "#E1C62F", "#A65628", "#DB728C", "#D789B2", "#999999")
pal20=c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
        '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', 
        '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')

d1<-read.table("./data/tab/sample.pcs.txt")
## just need burn name and pc values
d2<-d1[,c(4,5,11)]
names(d2)<-c("PC1", "PC2", "Fire.name")

ggscatter(d2, x="PC1", y="PC2", color="Fire.name",palette=pal20, legend.title="")
ggsave("./plots/pcscatter1.png", width=11, height=8)

ggscatter(d2, x="PC1", y="PC2", color="Fire.name",palette=pal20, ellipse=TRUE, legend.title="")
ggsave("./plots/pcscatter2.png", width=11, height=8)

ggscatter(d2, x="PC1", y="PC2", color="Fire.name",palette=pal20, ellipse=TRUE, 
          ellipse.border.remove=TRUE, legend.title="")
ggsave("./plots/pcscatter3.png", width=11, height=8)
