library(tidyverse)
byearL <- 1800
eyearL <- 1917
newspapers <- read.csv("input/raw/finnish-newspapers-data/processed/newspapers-utf8.csv", na.strings=c(""))
newspapers <- newspapers %>% mutate(size = replace(KOKO, nchar(as.character(KOKO))>2, NA))
newspapers <- newspapers <- newspapers %>% mutate(byear=as.numeric(gsub("\\d\\d\\.","",ILM_ALPVM)))
newspapers <- newspapers <- newspapers %>% mutate(eyear=coalesce(as.numeric(gsub("\\d\\d\\.","",ILM_LOPVM)),9999))
newspapers2 <- newspapers %>% filter(AINYLEISMAARE=="SAN", JULKAISUMAA=="FI",byear<=eyearL,byear>=byearL)
years <- data.frame(c(byearL:eyearL))
names(years) = c("year")
library(sqldf)
npsizebyyear <- sqldf("select year,size from years left join newspapers2 on years.year between newspapers2.byear and newspapers2.eyear")
npsizesbyyear <- npsizebyyear %>% group_by(year,size) %>% summarise(count=n()) %>% mutate(percentage=100*count/sum(count))
library(ggplot2)
ggplot(npsizesbyyear,aes(x=year,y=percentage,group=size,fill=size)) + geom_bar(stat="identity")

npsizes <- read.csv("input/raw/finnish-newspapers-data/processed/npsizes.csv")  %>% left_join(read.csv("input/raw/finnish-newspapers-data/processed/npissues.csv"))
npsizes <- npsizes %>% mutate(year = as.numeric(substr(date,1,4)), area = width*height/100)

npsizemareabyyear <- npsizes %>% filter(year<=eyearL,year>=byearL) %>% group_by(ISSN,year) %>% summarise(aarea=mean(area))

# papersizes <- data.frame("type" = c("A0","A1","A2","A3","A4","A5","A6","A7"), "area" = c(814*1189,594*841,420*594,297*420,210*297,148*210,105*148,74*105))
# papersizes$type <- factor(papersizes$type, levels = c("A7","A6","A5","A4","A3","A2","A1","A0"))
papersizes <- data.frame("type" = c("A0","A1","A2","A3","A4","A5"), "area" = c(814*1189,594*841,420*594,297*420,210*297,148*210))
papersizes$type <- factor(papersizes$type, levels = c("A5","A4","A3","A2","A1","A0"))
papersizes <- papersizes %>% mutate(uboundary = coalesce((area + lag(area))/2,9999999),lboundary=coalesce((area + lead(area))/2,0))

sizecategorybyyear <- sqldf("select type,ISSN,year from papersizes left join npsizemareabyyear on aarea between lboundary and uboundary")

cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#56B4E9","#F0E442")
plot <- ggplot(sizecategorybyyear %>% filter(ISSN %in% newspapers2$ISSN) %>% group_by(year,type) %>% summarise(count=n()) %>% mutate(percentage=100*count/sum(count)),aes(x=year,y=count,group=type,fill=type)) + geom_bar(width=1,stat='identity',color="black") + scale_fill_manual(values=cbPalette) + labs(x="Year",y="Number of newspapers",fill="Paper size:") + theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
ggsave("output/figures/fig7_newspapers_by_page_size.png", plot, width = 7, height = 4, dpi=300)