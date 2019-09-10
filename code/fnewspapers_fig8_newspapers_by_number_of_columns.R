library(tidyverse)
byearL <- 1800
eyearL <- 1917
newspapers <- read.csv("input/raw/finnish-newspapers-data/processed/newspapers-utf8.csv", na.strings=c(""))
newspapers <- newspapers %>% mutate(size = replace(KOKO, nchar(as.character(KOKO))>2, NA))
newspapers <- newspapers <- newspapers %>% mutate(byear=as.numeric(gsub("\\d\\d\\.","",ILM_ALPVM)))
newspapers <- newspapers <- newspapers %>% mutate(eyear=coalesce(as.numeric(gsub("\\d\\d\\.","",ILM_LOPVM)),9999))
newspapers2 <- droplevels(newspapers %>% filter(AINYLEISMAARE=="SAN", JULKAISUMAA=="FI", KIELI=="fin" | KIELI=="swe",byear<=eyearL,byear>=byearL))
years <- data.frame(c(byearL:eyearL))
names(years) = c("year")

npcolumns <- read.csv("input/raw/finnish-newspapers-data/processed/npcolumns.csv") %>% left_join(read.csv("input/raw/finnish-newspapers-data/processed/npissues.csv"))

npcolumns <- npcolumns %>% mutate(year = as.numeric(substr(date,1,4)))

npcolumnsbyyear <- npcolumns %>% filter(year<=eyearL,year>=byearL) %>% inner_join(newspapers2 %>% select(ISSN)) %>% group_by(year,ISSN) %>% summarise(mcolumns = floor(median(wmediancols))) %>% group_by(year,mcolumns) %>% summarise(count=n())
library(sqldf)

library(ggplot2)
cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#56B4E9","#F0E442")

#plot <- ggplot(newspapersByYearAndLanguage %>% group_by(year,KIELI) %>% summarise(count=n()),aes(x=year,y=count,fill=KIELI)) + geom_bar(stat="identity",color="black") +theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + scale_fill_manual(labels=c("Finnish","Swedish"),values=c(finnishPal,swedishPal)) + labs(x="Year",y="Newspapers",fill="Language:") + theme(legend.position="bottom")
plot <- ggplot(npcolumnsbyyear  %>% filter(mcolumns<9),aes(x=year,y=count,fill=as.character(mcolumns))) + geom_bar(stat="identity",color="black") + theme_grey() + scale_fill_manual(values=cbPalette) + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + labs(x="Year",y="Newspapers",fill="Number of columns:") + theme(legend.position="bottom")  + guides(fill = guide_legend(nrow = 1))
ggsave("output/figures/fig8_newspapers_by_number_of_columns.png", plot, width = 7, height = 4, dpi=300)