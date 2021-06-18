## trees.and.spei.plots_2002.R
## for stress grad ms background and 
## usfs final report, june 2021
## 2002 burn sites 
#747 Complex
#Roberts Creek
#Hayman 
#Missionary Ridge 
#Ponil Complex 
#Rodeo 

library(sf)
library(dplyr)
library(raster)
library(ggplot2)
library(ggpubr)
library(patchwork)

## read data and get ready to plot
## spei ts
v1=scan("hayman.spei.txt"); df_hay=tibble::as_tibble(v1); df_hay$date=seq(as.Date("2002/1/1"), by = "month", length.out = 192)
v1=scan("b747.spei.txt"); df_747=tibble::as_tibble(v1);df_747$date=seq(as.Date("2002/1/1"), by = "month", length.out = 192)
v1=scan("rob.spei.txt"); df_rob=tibble::as_tibble(v1);df_rob$date=seq(as.Date("2002/1/1"), by = "month", length.out = 192)
v1=scan("mis.spei.txt"); df_mis=tibble::as_tibble(v1);df_mis$date=seq(as.Date("2002/1/1"), by = "month", length.out = 192)
v1=scan("pon.spei.txt"); df_pon=tibble::as_tibble(v1);df_pon$date=seq(as.Date("2002/1/1"), by = "month", length.out = 192)
v1=scan("rodeo.spei.txt"); df_rod=tibble::as_tibble(v1);df_rod$date=seq(as.Date("2002/1/1"), by = "month", length.out = 192)
## delete obs for 2017 to match tree age range
df_hay<-dplyr::filter(df_hay, date < "2017-01-01")
df_747<-dplyr::filter(df_747, date < "2017-01-01")
df_rob<-dplyr::filter(df_rob, date < "2017-01-01")
df_mis<-dplyr::filter(df_mis, date < "2017-01-01")
df_pon<-dplyr::filter(df_pon, date < "2017-01-01")
df_rod<-dplyr::filter(df_rod, date < "2017-01-01")

## all trees
trees<-read_csv("./data/tab/plot.age.merge.all2.csv")
unique(trees$Burn)
## [1] "747"              "Burnt_Cabin"      "Cerro_Grande"     "Hayman"           "Hash_Rock"       
## [6] "Missionary_Ridge" "Outlet"           "Ponil"            "Poplar"           "Pumpkin"         
## [11] "Roberts_Creek"    "Rodeo_Chediski" 

######################################## hayman ##################################
## values different for ea site...output datatype options
haytrees<- filter(trees, Burn=="Hayman")
unique(haytrees$internodes) # "4"  "3"  "5"  "2"  "8"  "9"  "7"  "11" "6"  "14" "1"  "10"
haytrees$internodes.f<-factor(haytrees$internodes, levels=c("1" ,"2",   "3",   "4",   "5",
  "6",   "7",   "8",   "9", "10",  "11", "14"))
haytrees$internodes.n<-as.numeric(haytrees$internodes)
## convert to years. shift over one yr so tree count is mapped at the years end
haytrees$year<-recode(haytrees$internodes.f, '1'="2017",'2'="2016",'3'="2015",'4'="2014",
                     '5'="2013", '6'="2012", '7'="2011", '8'="2010", '9'="2009",
                     '10'="2008", '11'="2007",'14'="2004")

# convert factor to number so I have both
haytrees$year.num<-as.numeric(levels(haytrees$year))[haytrees$year]
## calc num trees by year
haycount<-haytrees %>% group_by(year.num) %>% summarise(n=n())

## plot SPEI
p <- ggplot(df_hay, aes(x=date, y=value)) + geom_area() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  
    xlab("Year")+ ylab("SPEI") + theme_gray()+ ggtitle("Hayman") +
   stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") 
p

## put the tree plots and hli plots on same x scale as spei
## plot num trees
p1<-ggscatter(haycount, x="year.num", y="n", shape=23, size=4, color="forestgreen", fill="forestgreen") +
 scale_x_continuous(limits = c(2002, 2017),breaks=seq(2002,2017,1)) + 
    xlab("Year") +  ylab("Tree count") + theme_gray()
p1

## plot heat load index
yrv<-c("2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011",
  "2012","2013","2014","2015","2016","2017") # ended up doing this, couldn't fig out how to reverse and specify labels
p2 <- ggplot(haytrees, aes(x = year, y = hli)) + scale_x_discrete(limits=yrv) + #scale_x_discrete(limits=rev) +
    geom_boxplot(fill = "grey92") + xlab("Year")+ ylab("Heat Load Index") +
    geom_point(size = 2,alpha = .3, position = position_jitter(seed = 1, width = .2))

p2
p/p1/p2
ggsave("./plots/hayman3plots.png", height=9, width=9)

############################### 747 ###########################################
## values different for ea site...output datatype options
b747trees<- filter(trees, Burn=="747")
unique(b747trees$internodes) #"7"  "11" "10" "3"  "1"  "13" "2"  "5"  "8"  "9"  "4"  "6"  "12"
b747trees$internodes.f<-factor(b747trees$internodes, levels=c("1","2",   "3",   "4",   "5",
  "6",   "7",   "8",   "9", "10",  "11", "12", "13"))
b747trees$internodes.n<-as.numeric(b747trees$internodes.f)
## convert to years...shift over one
b747trees$year<-recode(b747trees$internodes.f, '1'="2017",'2'="2016",'3'="2015",'4'="2014",
                     '5'="2013", '6'="2012", '7'="2011", '8'="2010", '9'="2009",
                     '10'="2008", '11'="2007",'12'="2006", '13'="2005")

# convert factor to number so I have both
b747trees$year.num<-as.numeric(levels(b747trees$year))[b747trees$year]
## calc num trees by year
b747count<-b747trees %>% group_by(year.num) %>% summarise(n=n())

## plot SPEI
p <- ggplot(df_747, aes(x=date, y=value)) + geom_area() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  
    xlab("Year")+ ylab("SPEI") + theme_gray()+ ggtitle("747 Complex") +
   stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") 
p

## put the tree plots and hli plots on same x scale as spei
## plot num trees
p1<-ggscatter(b747count, x="year.num", y="n", shape=23, size=4, color="forestgreen", fill="forestgreen") +
 scale_x_continuous(limits = c(2002, 2017),breaks=seq(2002,2017,1)) + 
    xlab("Year") +  ylab("Tree count") + theme_gray()
p1

## plot heat load index
yrv<-c("2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011",
  "2012","2013","2014","2015","2016","2017") # ended up doing this, couldn't fig out how to reverse and specify labels
p2 <- ggplot(b747trees, aes(x = year, y = hli)) + scale_x_discrete(limits=yrv) + #scale_x_discrete(limits=rev) +
    geom_boxplot(fill = "grey92") + xlab("Year")+ ylab("Heat Load Index") +
    geom_point(size = 2,alpha = .3, position = position_jitter(seed = 1, width = .2))

p2
p/p1/p2
ggsave("./plots/b747_3plots.png", height=9, width=9)

################ roberts creek ####################################
## values different for ea site...output datatype options
robtrees<- filter(trees, Burn=="Roberts_Creek")
unique(robtrees$internodes) # "5"   "4"   "8"   "13"  "9"   "3"   "14"  "1"   "7"   "6"   "12"  "2"   "10"  "11"  ">15"
robtrees$internodes.f<-factor(robtrees$internodes, levels=c("1","2",   "3",   "4",   "5",
  "6",   "7",   "8",   "9", "10",  "11", "12", "13", "14", ">15"))
robtrees$internodes.f2<-recode(robtrees$internodes.f, ">15" = "16")
robtrees$internodes.n<-as.numeric(robtrees$internodes.f2)
## convert to years...shift over one
robtrees$year<-recode(robtrees$internodes.f2, '1'="2017",'2'="2016",'3'="2015",'4'="2014",
                     '5'="2013", '6'="2012", '7'="2011", '8'="2010", '9'="2009",
                     '10'="2008", '11'="2007",'12'="2006", '13'="2005",'14'="2004", '16'="2002")

# convert factor to number so I have both
robtrees$year.num<-as.numeric(levels(robtrees$year))[robtrees$year]
## calc num trees by year
robcount<-robtrees %>% group_by(year.num) %>% summarise(n=n())

## plot SPEI
p <- ggplot(df_rob, aes(x=date, y=value)) + geom_area() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  
    xlab("Year")+ ylab("SPEI") + theme_gray()+ ggtitle("Roberts Creek") +
   stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") 
p

## put the tree plots and hli plots on same x scale as spei
## plot num trees
p1<-ggscatter(robcount, x="year.num", y="n", shape=23, size=4, color="forestgreen", fill="forestgreen") +
 scale_x_continuous(limits = c(2002, 2017),breaks=seq(2002,2017,1)) + 
    xlab("Year") +  ylab("Tree count") + theme_gray()
p1

## plot heat load index
yrv<-c("2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011",
  "2012","2013","2014","2015","2016","2017") # ended up doing this, couldn't fig out how to reverse and specify labels
p2 <- ggplot(robtrees, aes(x = year, y = hli)) + scale_x_discrete(limits=yrv) + #scale_x_discrete(limits=rev) +
    geom_boxplot(fill = "grey92") + xlab("Year")+ ylab("Heat Load Index") +
    geom_point(size = 2,alpha = .3, position = position_jitter(seed = 1, width = .2))

p2
p/p1/p2
ggsave("./plots/rob_3plots.png", height=9, width=9)

############################### miss ridge ####################################
## values different for ea site...output datatype options
mistrees<- filter(trees, Burn=="Missionary_Ridge") 
unique(mistrees$internodes)#"2"   "N/A" "5"   "8"   "6"   "4"   "3"   "1"   "7"  
mistrees<-filter(mistrees, internodes!="N/A") 
mistrees$internodes.f<-factor(mistrees$internodes, levels=c("1", "2",   "3",   "4",   "5",
  "6",   "7",   "8"))
mistrees$internodes.n<-as.numeric(mistrees$internodes)
## convert to years...shift over one
mistrees$year<-recode(mistrees$internodes.f, '1'="2017",'2'="2016",'3'="2015",'4'="2014",
                     '5'="2013", '6'="2012", '7'="2011", '8'="2010")

# convert factor to number so I have both
mistrees$year.num<-as.numeric(levels(mistrees$year))[mistrees$year]
## calc num trees by year
miscount<-mistrees %>% group_by(year.num) %>% summarise(n=n())

## plot SPEI
p <- ggplot(df_mis, aes(x=date, y=value)) + geom_area() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  
    xlab("Year")+ ylab("SPEI") + theme_gray()+ ggtitle("Missionary Ridge") +
   stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") 
p

## put the tree plots and hli plots on same x scale as spei
## plot num trees
p1<-ggscatter(miscount, x="year.num", y="n", shape=23, size=4, color="forestgreen", fill="forestgreen") +
 scale_x_continuous(limits = c(2002, 2017),breaks=seq(2002,2017,1)) + 
    xlab("Year") +  ylab("Tree count") + theme_gray()
p1

## plot heat load index
yrv<-c("2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011",
  "2012","2013","2014","2015","2016","2017") # ended up doing this, couldn't fig out how to reverse and specify labels
p2 <- ggplot(mistrees, aes(x = year, y = hli)) + scale_x_discrete(limits=yrv) + #scale_x_discrete(limits=rev) +
    geom_boxplot(fill = "grey92") + xlab("Year")+ ylab("Heat Load Index") +
    geom_point(size = 2,alpha = .3, position = position_jitter(seed = 1, width = .2))

p2
p/p1/p2
ggsave("./plots/mis_3plots.png", height=9, width=9)

############################### ponil ####################################
## values different for ea site...output datatype options
pontrees<- filter(trees, Burn=="Ponil")
unique(pontrees$internodes)#"N/A" "2"   "5"   "4"   "3"   "1"   "6"  
pontrees<-filter(pontrees, internodes!="N/A") 
pontrees$internodes.f<-factor(pontrees$internodes, levels=c("1","2",   "3",   "4",   "5",
  "6"))
pontrees$internodes.n<-as.numeric(pontrees$internodes)
## convert to years...shift over one
pontrees$year<-recode(pontrees$internodes.f, '1'="2017",'2'="2016",'3'="2015",'4'="2014",
                     '5'="2013", '6'="2012")

# convert factor to number so I have both
pontrees$year.num<-as.numeric(levels(pontrees$year))[pontrees$year]
## calc num trees by year
poncount<-pontrees %>% group_by(year.num) %>% summarise(n=n())

## plot SPEI
p <- ggplot(df_pon, aes(x=date, y=value)) + geom_area() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  
    xlab("Year")+ ylab("SPEI") + theme_gray()+ ggtitle("Ponil Complex") +
   stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") 
p

## put the tree plots and hli plots on same x scale as spei
## plot num trees
p1<-ggscatter(poncount, x="year.num", y="n", shape=23, size=4, color="forestgreen", fill="forestgreen") +
 scale_x_continuous(limits = c(2002, 2017),breaks=seq(2002,2017,1)) + 
    xlab("Year") +  ylab("Tree count") + theme_gray()
p1

## plot heat load index
yrv<-c("2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011",
  "2012","2013","2014","2015","2016","2017") # ended up doing this, couldn't fig out how to reverse and specify labels
p2 <- ggplot(pontrees, aes(x = year, y = hli)) + scale_x_discrete(limits=yrv) + #scale_x_discrete(limits=rev) +
    geom_boxplot(fill = "grey92") + xlab("Year")+ ylab("Heat Load Index") +
    geom_point(size = 2,alpha = .3, position = position_jitter(seed = 1, width = .2))

p2
p/p1/p2
ggsave("./plots/pon_3plots.png", height=9, width=9)

############################### rodeo ####################################
## values different for ea site...output datatype options
rodtrees<- filter(trees, Burn=="Rodeo_Chediski")
unique(rodtrees$internodes) #"7"   "12"  "5"   "8"   "6"   "10"  "3"   "13"  "11"  "N/A" ">15" "9"   "2"   "4"   "15"  "14" 
#"<15" ">10"
rodtrees<-filter(rodtrees, internodes!="N/A") 
rodtrees$internodes.f<-factor(rodtrees$internodes, levels=c("2",   "3",   "4",   "5",
  "6",   "7",   "8",   "9", "10",  "11", "12", "13", "14", "15", ">15", "<15", ">10"))
rodtrees$internodes.f2<-recode(rodtrees$internodes.f, ">15" = "16", "<15"="14", ">10"="11")
rodtrees$internodes.n<-as.numeric(rodtrees$internodes.f2)
## convert to years...shift over one
rodtrees$year<-recode(rodtrees$internodes.f2, '2'="2016",'3'="2015",'4'="2014",
                     '5'="2013", '6'="2012", '7'="2011", '8'="2010", '9'="2009",
                     '10'="2008", '11'="2007",'12'="2006", '13'="2005", '14'="2004", '15'="2003", '16'="2002")

# convert factor to number so I have both
rodtrees$year.num<-as.numeric(levels(rodtrees$year))[rodtrees$year]
## calc num trees by year
rodcount<-rodtrees %>% group_by(year.num) %>% summarise(n=n())

## plot SPEI
p <- ggplot(df_rod, aes(x=date, y=value)) + geom_area() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  
    xlab("Year")+ ylab("SPEI") + theme_gray()+ ggtitle("Rodeo") +
   stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") 
p

## put the tree plots and hli plots on same x scale as spei
## plot num trees
p1<-ggscatter(rodcount, x="year.num", y="n", shape=23, size=4, color="forestgreen", fill="forestgreen") +
 scale_x_continuous(limits = c(2002, 2017),breaks=seq(2002,2017,1)) + 
    xlab("Year") +  ylab("Tree count") + theme_gray()
p1

## plot heat load index
yrv<-c("2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011",
  "2012","2013","2014","2015","2016","2017") # ended up doing this, couldn't fig out how to reverse and specify labels
p2 <- ggplot(rodtrees, aes(x = year, y = hli)) + scale_x_discrete(limits=yrv) + #scale_x_discrete(limits=rev) +
    geom_boxplot(fill = "grey92") + xlab("Year")+ ylab("Heat Load Index") +
    geom_point(size = 2,alpha = .3, position = position_jitter(seed = 1, width = .2))

p2
p/p1/p2
ggsave("./plots/rod_3plots.png", height=9, width=9)

