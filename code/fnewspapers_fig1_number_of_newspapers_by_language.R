library(tidyverse)
library(scales)
byearL <- 1800
eyearL <- 1917
newspapers <- read.csv("input/raw/finnish-newspapers-data/processed/newspapers-utf8.csv", na.strings=c(""))
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
plot <- ggplot(newspapersByYearAndLanguage %>% group_by(year,KIELI) %>% summarise(count=n()),aes(x=year,y=count,color=KIELI)) + geom_line(size=1.5) +theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + scale_color_manual(labels=c("Finnish","Swedish"),values=c(finnishPal,swedishPal)) + labs(x="Year",y="Newspapers",color="Language:") + theme(legend.position="bottom")
ggsave("output/figures/fig1_number_of_newspapers_by_language.png", plot, width = 7, height = 4, dpi=300)
