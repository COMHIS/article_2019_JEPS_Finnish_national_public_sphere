library(tidyverse)
byearL <- 1771
eyearL <- 1917
newspapers <- read.csv("input/raw/finnish-newspapers-data/processed/newspapers-utf8.csv", na.strings=c(""))
newspapers <- newspapers <- newspapers %>% mutate(byear=as.numeric(gsub("\\d\\d\\.","",ILM_ALPVM)))
newspapers <- newspapers <- newspapers %>% mutate(eyear=coalesce(as.numeric(gsub("\\d\\d\\.","",ILM_LOPVM)),9999))
newspapers2 <- droplevels(newspapers %>% filter(AINYLEISMAARE=="SAN", JULKAISUMAA=="FI", KIELI=="fin" | KIELI=="swe",byear<=eyearL,byear>=byearL))
years <- data.frame(c(byearL:eyearL))
names(years) = c("year")
npwordschars <- read.csv("input/raw/finnish-newspapers-data/processed/npwordschars.csv") %>% left_join(read_csv("input/raw/finnish-newspapers-data/processed/npissues.csv"))
npwordschars <- npwordschars %>% mutate(year = as.numeric(substr(date,1,4)))

npwordscharsbyyear <- npwordschars %>% filter(year<=eyearL,year>=byearL) %>% inner_join(newspapers2 %>% select(ISSN,KIELI)) %>% group_by(year,KIELI) %>% summarise(swords=sum(words),schars=sum(chars)) %>% complete(year,KIELI, fill = list(schars = NA, swords = NA))

cbPalette <- c("#999999", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#56B4E9","#F0E442")
swedishPal <- cbPalette[8]
finnishPal <- cbPalette[7]
bothPal <- cbPalette[1]
plot <- ggplot(npwordscharsbyyear,aes(x=year,y=swords,color=KIELI)) + geom_line(size=1.5) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) + scale_color_manual(labels=c("Finnish","Swedish"),values=c(finnishPal,swedishPal)) + labs(x="Year",y="Number of words (log scale)",color="Language:") + theme_gray() + scale_x_continuous(breaks= seq(1700,1920,by=10)) + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
ggsave("output/figures/fig2_words_by_lang.png", plot, width = 7, height = 4, dpi=300)
#ggplot(npwordscharsbyyear,aes(x=year,y=schars,color=KIELI)) + geom_line(size=1.5) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) + scale_color_manual(labels=c("Finnish","Swedish"),values=c(finnishPal,swedishPal)) + labs(x="Year",y="Number of characters (log scale)",color="Language:") + theme_gray() + scale_x_continuous(breaks= seq(1700,1920,by=10)) + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
