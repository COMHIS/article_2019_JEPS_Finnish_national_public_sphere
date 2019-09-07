library(tidyverse)
library(scales)
byearL <- 1800
eyearL <- 1917
newspapers <- read.csv("input/raw/newspapers-utf8.csv", na.strings=c(""))
newspapers <- newspapers %>% mutate(size = replace(KOKO, nchar(as.character(KOKO))>2, NA))
newspapers <- newspapers <- newspapers %>% mutate(byear=as.numeric(gsub("\\d\\d\\.","",ILM_ALPVM)))
newspapers <- newspapers <- newspapers %>% mutate(eyear=coalesce(as.numeric(gsub("\\d\\d\\.","",ILM_LOPVM)),9999))
fonewspapers <- newspapers %>% filter(AINYLEISMAARE=="SAN", JULKAISUMAA=="FI", KIELI=="fin" | KIELI=="swe",byear<=eyearL,byear>=byearL)

years <- data.frame(c(byearL:eyearL))
names(years) = c("year")
library(sqldf)
newspapersByYearAndLanguage = sqldf("select year,KIELI from years left join fonewspapers on years.year between fonewspapers.byear and fonewspapers.eyear")

library(ggplot2)
cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#56B4E9","#F0E442")
swedishPal <- cbPalette[8]
finnishPal <- cbPalette[7]
bothPal <- cbPalette[6]

#plot <- ggplot(newspapersByYearAndLanguage %>% group_by(year,KIELI) %>% summarise(count=n()),aes(x=year,y=count,fill=KIELI)) + geom_bar(stat="identity",color="black") +theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + scale_fill_manual(labels=c("Finnish","Swedish"),values=c(finnishPal,swedishPal)) + labs(x="Year",y="Newspapers",fill="Language:") + theme(legend.position="bottom")
plot <- ggplot(newspapersByYearAndLanguage %>% group_by(year,KIELI) %>% summarise(count=n()),aes(x=year,y=count,color=KIELI)) + geom_line(size=1.5) +theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + scale_color_manual(labels=c("Finnish","Swedish"),values=c(finnishPal,swedishPal)) + labs(x="Year",y="Newspapers",color="Language:") + theme(legend.position="bottom") + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))
ggsave("~/Downloads/number-of-newspapers.png", plot, width = 7, height = 4, dpi=300)
npplaces <- read.csv("input/raw/publication_locations-utf8.csv")
npplaces <- npplaces %>% mutate(byear = as.numeric(substr(ALPVM,7,11))) %>% mutate(byear = ifelse(byear == 1, NA, byear))
npplaces <- npplaces %>% mutate(eyear = as.numeric(substr(LOPVM,7,11)))
fnpplaces <- npplaces %>% inner_join (fonewspapers %>% select(ISSN,KIELI)) %>% left_join(fonewspapers %>% mutate(byear2 = byear,eyear2 = eyear) %>% select(ISSN,byear2,eyear2)) %>% mutate(byear=coalesce(byear,byear2),eyear=coalesce(eyear,eyear2)) %>% select(ISSN,KAUPUNKI_NORM,KIELI,byear,eyear)

npsbyyear <- sqldf("select year,KAUPUNKI_NORM,KIELI from years left join fnpplaces on years.year between fnpplaces.byear and fnpplaces.eyear")
npslbyyear <- npsbyyear %>% group_by(year,KAUPUNKI_NORM,KIELI) %>% summarise(count=n()) %>% mutate(percentage=100*count/sum(count))

has_finnish <- npslbyyear %>% inner_join(npslbyyear %>% filter(KIELI=="fin")) %>% select(year,KAUPUNKI_NORM)
has_swedish <- npslbyyear %>% inner_join(npslbyyear %>% filter(KIELI=="swe")) %>% select(year,KAUPUNKI_NORM)
has_both <- has_finnish %>% inner_join(has_swedish) %>% mutate(lang="fin+swe")
has_only_finnish <- has_finnish %>% anti_join(has_both) %>% mutate(lang="fin")
has_only_swedish <- has_swedish %>% anti_join(has_both) %>% mutate(lang="swe")

npltypes <- has_only_finnish %>% full_join(has_only_swedish) %>% full_join(has_both)
npltypesbyyear <- npltypes %>% group_by(year,lang) %>% summarise(count = n()) %>% mutate(percentage=100*count/sum(count)) %>% complete(year,lang, fill = list(count = 0, percentage = 0))
plot <- ggplot(npltypesbyyear,aes(x=year,y=count,group=lang,fill=lang)) + geom_bar(stat="identity",color="black") +theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + scale_fill_manual(labels=c("Only Finnish","Finnish and Swedish","Only Swedish"),values=c(finnishPal,bothPal,swedishPal)) + labs(x="Year",y="Number of towns",fill="Newspaper languages in town:") + theme(legend.position="bottom")
ggsave("output/figures/fig9_newspaper_languages_by_town.png", plot, width = 7, height = 4, dpi=300)

#library(cowplot)
#theme_set(theme_grey())
#p1 <- ggplot(npltypesbyyear,aes(x=year,y=count,group=lang,fill=lang)) + geom_bar(stat="identity") + scale_x_continuous(breaks= seq(1800,1920,by=10)) + scale_fill_manual(labels=c("Only Finnish","Finnish and Swedish","Only Swedish"),values=c("#0000FF","#999999","#FFFF00")) + labs(x="",y="Number of Cities",fill="Newspaper Languages in City:")
#p2 <- p2 <- ggplot(npltypesbyyear,aes(x=year,y=percentage,group=lang,fill=lang)) + geom_bar(stat="identity") + scale_x_continuous(breaks= seq(1800,1920,by=10),position="top") + scale_fill_manual(labels=c("Only Finnish","Finnish and Swedish","Only Swedish"),values=c("#0000FF","#999999","#FFFF00")) + labs(x="Year",y="Percentage of Cities",fill="Newspaper Languages in City:")
#legend <- get_legend(p1 + theme(legend.position="bottom"))
#plot_grid(p1 + theme(legend.position="none",axis.title.x = element_blank()),p2 + theme(legend.position="none",axis.title.x = element_blank(),axis.text.x = element_blank()),legend,align="v",ncol=1,axis='l',rel_heights = c(1,1,.2))
