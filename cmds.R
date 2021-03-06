setwd("G:\\Economics of security")
iot = read.csv("iot_preprocessed.csv",header = T,strip.white = T)
iot$scandate <- as.Date(iot$scandate)
iot$maker <- tolower(iot$maker)
attach(attacks)
attacks <- iot %>% group_by(scandate) %>% mutate(count = n_distinct(host)) %>% select(scandate,count)

View(subset(vendorChange, !duplicated(vendorChange$maker)))
vendorChange <- iot %>% group_by(maker,scandate) %>% mutate(countn = n()) %>% select(maker,scandate,countn)  %>% distinct(maker,scandate,countn)

popularVendors <- iot %>% group_by(maker) %>% mutate(count = n()) %>% filter(count>200) %>% select(maker)
popularVendors <- subset(popularVendors, !duplicated(popularVendors))

vendorChange <- vendorChange[1:3]

plotDat <- subset(vendorChange, vendorChange$maker %in% vendors$maker)

plotDat <- subset(plotDat, !plotDat$maker=="")

View(subset(vendorChange, vendorChange$maker %in% vendors$maker))

ggplot(plotDat, aes(x = scandate, y = count, colour = maker)) + geom_point() + geom_line()

plotDat <- subset(vendorChange, vendorChange$maker %in% c("zyxel","tp-link", "d-link"))


vendorService <- iot %>% group_by(service, maker) %>% mutate(countn = n()) %>% select(service,maker,countn)  %>% distinct(service, maker,countn)
vendorPlotDat <- subset(vendorService, vendorService$maker %in% vendors$maker)
vendorPlotDat <- subset(vendorPlotDat, !vendorPlotDat$maker=="")


library(ggplot2)

plot(x = scandate, y = count, main="Botnet size", xlab="date ", ylab="infected hosts", pch=19)

View(subset(vendorChange, !duplicated(vendorChange$maker)))

honeypot <- readLines("cmds_sequence_2016-07-01.csv")
honeypot <- gsub(pattern = "$$",replacement = "€",x = honeypot,fixed = T)
flieconn <- file("hp_preprocessed.csv")
writeLines(honeypot,flieconn)
honeypot <- read.table("hp_preprocessed.csv",sep = "€",strip.white = T)

honeypots <- read.csv("hp_preprocessed.csv",sep="€",strip.white = T,header = F)
library(plyr)
shells <- count(honeypots$V7)
honeypots$V1 <- as.POSIXct(honeypots$V1,format="%Y-%m-%d %H:%M:%OS")
summary(shells)
head(subset(shells,shells$freq==1),n = 100)
install.packages("dplyr")
library(dplyr)
honeypots %>% group_by(honeypots$"V6") %>% filter(n()>2)
sample_frac() 
ff <- table(honeypots$V6)
ff <- as.data.frame(ff)
head(ff$Freq==1,20)
head(subset(ff,ff$Freq==1,"Var1"),20)

write(vendors[2:nrow(vendors),],file = "G:\\Economics of security\\vendors")

vendmeg <- merge(vendPop,cts[which(cts$x %in% vendPop$x),],"x", all=TRUE)

f2 <- file("G:\\Economics of security\\vendor_data.csv")

write.csv(as.data.frame(vendmeg),file = f2)
options("scipen"=999, "digits"=1)
vendPop$breachProbability <- ((vendPop$freq/vendPop$count) * 100)

boxplot(round(vendPop$breachProbability,0),horizontal = TRUE, axes = FALSE, staplewex = 1)

text(x=fivenum(round(vendPop$breachProbability,0)), labels =fivenum(round(vendPop$breachProbability,0)), y=1.25)

cta <- count(iot$maker)

cts <- cta[with(cta,order(-freq)),]

ctsbanner <- count(iot$banner)

ctsbanner <- ctsbanner[with(ctsbanner,order(-freq)),]

vendPop$vendor <- as.character(vendPop$vendor)

DahuaD <- c("Dahua",781227,311294,40)

vendPop <- rbind(vendPop,DahuaD)

write.table(as.data.frame(vendPop),"G:\\Economics of security\\vendors_breach_probability.csv", sep = "\t")

vendPop <- read.csv("G:\\Economics of security\\vendors_breach_probability.csv",sep = "\t")
