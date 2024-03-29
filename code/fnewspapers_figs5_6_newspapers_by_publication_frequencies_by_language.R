library(tidyverse)
byearL <- 1800
eyearL <- 1917
newspapers <- read.csv("input/raw/finnish-newspapers-data/processed/newspapers-utf8.csv", na.strings=c(""))
newspapers <- newspapers <- newspapers %>% mutate(byear=as.numeric(gsub("\\d\\d\\.","",ILM_ALPVM)))
newspapers <- newspapers <- newspapers %>% mutate(eyear=coalesce(as.numeric(gsub("\\d\\d\\.","",ILM_LOPVM)),9999))
newspapers2 <- droplevels(newspapers %>% filter(AINYLEISMAARE=="SAN", JULKAISUMAA=="FI", KIELI=="fin" | KIELI=="swe",byear<=eyearL,byear>=byearL))
years <- data.frame(c(byearL:eyearL))
names(years) = c("year")
npissues <- read.csv("input/raw/finnish-newspapers-data/processed/npissues.csv")

npdates <- npissues %>% mutate(year =as.numeric(substr(date,1,4))) %>% mutate(date = as.Date(date))

npdatesfreqs <- npdates %>% filter(year>=byearL,year<=eyearL) %>% group_by(ISSN,year) %>% arrange(date) %>% summarize(avg = as.numeric(mean(diff(date))))
freqs = data.frame(freq1 = c(2,3,4,5,6,7))
freqs <- freqs %>% mutate(freq2=7/freq1,freq=as.character(freq1)) %>% mutate(uboundary = coalesce((freq2 + lag(freq2))/2,9999999),lboundary=coalesce((freq2 + lead(freq2))/2,0))
# freqs$freq <- factor(freqs$freq, levels = c("10","9","8","7","6","5","4","3","2"))
freqcategorybyyear <- sqldf("select year,freq,ISSN from freqs left join npdatesfreqs on avg between lboundary and uboundary")

cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#56B4E9","#F0E442","#333333")
fnewspapers <- newspapers2 %>% filter(KIELI=='fin')
snewspapers <- newspapers2 %>% filter(KIELI=='swe')
plot <- ggplot(freqcategorybyyear %>% filter(ISSN %in% snewspapers$ISSN) %>% group_by(year,freq) %>% summarise(count=n()) %>% mutate(percentage=100*count/sum(count)),aes(x=year,y=count,fill=freq)) + geom_bar(width=1,stat='identity',color="black") + scale_fill_manual(values=cbPalette) + labs(x="Year",y="Number of newspapers",fill="Appears times per week:") + theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
ggsave("output/figures/fig5_swe_freq.png", plot, width = 7, height = 4, dpi=300)
plot <- ggplot(freqcategorybyyear %>% filter(ISSN %in% fnewspapers$ISSN) %>% group_by(year,freq) %>% summarise(count=n()) %>% mutate(percentage=100*count/sum(count)),aes(x=year,y=count,fill=freq)) + geom_bar(width=1,stat='identity',color="black") + scale_fill_manual(values=cbPalette) + labs(x="Year",y="Number of newspapers",fill="Appears times per week:") + theme_gray() + scale_y_continuous(breaks=seq(0,1000,by=10)) + scale_x_continuous(breaks= seq(1800,1920,by=10)) + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
ggsave("output/figures/fig6_fin_freq.png", plot, width = 7, height = 4, dpi=300)
