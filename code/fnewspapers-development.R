library(tidyverse)
library(ggplot2)
library(sqldf)
library(viridis)
library(cowplot)
library(plotly)
theme_set(theme_gray())
newspapers <- read.csv("/Users/jiemakel/tyo/data-public/finnish-newspapers/unified/newspapers-utf8.csv", na.strings=c(""), stringsAsFactors = FALSE)
newspapers <- newspapers %>% left_join(read.csv("/Users/jiemakel/tyo/data-public/finnish-newspapers/unified/publication_locations-utf8.csv", na.strings=c(""), stringsAsFactors = FALSE),by=c("ISSN"))
newspapers <- newspapers %>% mutate(size = replace(KOKO, nchar(as.character(KOKO))>2, NA))
newspapers <- newspapers %>% mutate(byear=as.numeric(gsub("\\d\\d\\.","",ILM_ALPVM)))
newspapers <- newspapers %>% mutate(eyear=coalesce(as.numeric(gsub("\\d\\d\\.","",ILM_LOPVM)),9999))
npsizes <- read.csv("/Users/jiemakel/Downloads/npsizes.csv", stringsAsFactors = FALSE)
npsizes <- npsizes %>% mutate(year = as.numeric(gsub("\\d\\d\\.","",date)), area = width*height, date = as.Date(date,"%d.%m.%Y"))

papersizes <- data.frame("type" = c("A0","A1","A2","A3","A4","A5"), "area" = c(814*1189,594*841,420*594,297*420,210*297,148*210))
papersizes$type <- factor(papersizes$type, levels = c("A5","A4","A3","A2","A1","A0"))
papersizes <- papersizes %>% mutate(uboundary = coalesce((area + lag(area))/2,9999999999),lboundary=coalesce((area + lead(area))/2,0))

npsizes <- sqldf("select ISSN,issueId,date,page,width,height,year,npsizes.area,type from npsizes left join papersizes on npsizes.area between lboundary and uboundary")

npcolumns <- read.csv("/Users/jiemakel/Downloads/npcolumns.csv", stringsAsFactors = FALSE)
npcolumns <- npcolumns %>% mutate(year = as.numeric(substr(date,1,4)),date = as.Date(date,"%Y-%m-%d"))

npwordschars <- read.csv("/Users/jiemakel/Downloads/npwordschars.csv", stringsAsFactors = FALSE)
npwordschars <- npwordschars %>% mutate(year = as.numeric(substr(date,1,4)),date = as.Date(date,"%Y-%m-%d"))

nppagedata <- npsizes %>% inner_join(npcolumns,by=c("ISSN","issueId","date","page","year")) %>% inner_join(npwordschars,by=c("ISSN","issueId","date","page","year"))
nppagedata <- nppagedata %>% arrange(ISSN,date,page)
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
npissuedata <- nppagedata %>% group_by(ISSN,year,date,issueId) %>% summarise(pages=n(),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% group_by(ISSN) %>% arrange(ISSN,year,date,issueId) %>% mutate(datesbetween=date-lag(date)) %>% ungroup()

plotdata <- function(fnpissuedata,fnppagedata,proportionFilter = 0.01,by = "paper", type="cow") {
  #p1 <- ggplot(fnpissuedata %>% filter(datesbetween<=30),aes(x=date,y=datesbetween)) + geom_bin2d() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL)
  #p2 <- ggplot(fnpissuedata %>% filter(pages<=20),aes(x=date,y=pages)) + geom_bin2d() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL)
  #p3 <- ggplot(fnppagedata %>% filter(wmediancols<=16),aes(x=date,y=wmediancols)) + geom_bin2d() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL)
  #p4 <- ggplot(fnppagedata,aes(x=date,y=area/1000000)) + geom_bin2d() + scale_fill_viridis() + theme(legend.position = "none")
  #p5 <- ggplot(fnppagedata,aes(x=date,y=words)) + geom_bin2d() + scale_fill_viridis() + theme(legend.position = "none")
  
  #plot_grid(p1 + theme(axis.title.x = element_blank(),axis.text.x = element_blank()),p2 + theme(axis.title.x = element_blank(),axis.text.x = element_blank()),p3 + theme(axis.title.x = element_blank(),axis.text.x = element_blank()),p4 + theme(axis.title.x = element_blank(),axis.text.x = element_blank()),p5,ncol=1,axis='l',rel_heights = c(.75,1,.75,1,1))
  if (length(unique(fnpissuedata$ISSN))==1 && by =="page") {
    p1 <- ggplot(fnpissuedata %>% filter(datesbetween<=20) %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN")) %>% group_by(year,datesbetween) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) %>% filter(proportion>=proportionFilter),aes(x=year,y=datesbetween,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Days between issues",fill="Proportion") + scale_x_continuous(position="top",breaks=seq(0,2000,by=10),sec.axis = dup_axis(name=NULL))
  } else {
    p1 <- ggplot(fnpissuedata %>% group_by(ISSN,year) %>% summarise(datesbetween=Mode(datesbetween)) %>% filter(datesbetween<=20) %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN")) %>% group_by(year,datesbetween) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) %>% filter(proportion>=proportionFilter),aes(x=year,y=datesbetween,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Days between issues",fill="Proportion") + scale_x_continuous(position="top",breaks=seq(0,2000,by=10),sec.axis = dup_axis(name=NULL))
  }
  if (by == "paper") {
    fnpissuedata <- fnpissuedata %>% group_by(ISSN,year) %>% summarise(datesbetween=Mode(datesbetween),pages=Mode(pages)) %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN"))
    fnppagedata <- fnppagedata %>% group_by(ISSN,year) %>% summarise(wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN"))
  } else if (by == "issue") {
    fnpissuedata <- fnpissuedata %>% group_by(ISSN,year,issueId) %>% summarise(datesbetween=Mode(datesbetween),pages=Mode(pages)) %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN"))
    fnppagedata <- fnppagedata %>% group_by(ISSN,year,issueId) %>% summarise(wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN"))
  } else {
    fnpissuedata <- fnpissuedata %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN"))
    fnppagedata <- fnppagedata %>% left_join(newspapers %>% select(ISSN,PAANIMEKE),by=c("ISSN"))
  }
  p2 <- ggplot(fnpissuedata %>% filter(pages<=20) %>% group_by(year,pages) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) %>% filter(proportion>=proportionFilter),aes(x=year,y=pages,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Pages",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL))
  p3 <- ggplot(fnppagedata %>% filter(wmodecols<=16) %>% group_by(year,wmodecols) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) %>% filter(proportion>=proportionFilter),aes(x=year,y=wmodecols,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Columns",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL))
  p4 <- ggplot(fnppagedata %>% filter(!is.na(type)) %>% group_by(year,type) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) %>% filter(proportion>=proportionFilter),aes(x=year,y=type,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Year",y="Approx. page size",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL))
  p5 <- ggplot(fnppagedata %>% mutate(group = round(words/500)) %>% group_by(year,group) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) %>% filter(proportion>=proportionFilter),aes(x=year,y=group,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Words/page (std A4s)",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL))
  p6 <- ggplot(fnppagedata %>% mutate(group = round(area/chars/5)*5) %>% filter(group<=50) %>% group_by(year,group) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) %>% filter(proportion>=proportionFilter),aes(x=year,y=group,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "bottom")  + labs(x="Year",y='sqmm/letter',fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + scale_y_continuous(breaks=seq(0,2000,by=5),minor_breaks=NULL)
  if (type == "cow") {
    plot <- plot_grid(p1,p2 + theme(axis.title.x = element_blank()),p3 + theme(axis.title.x = element_blank()),p4 + theme(axis.title.x = element_blank()),p5 + theme(axis.title.x = element_blank()), p6,ncol=1,align='v',axis='l',rel_heights = c(1,1,.75,.5,1,1)/5.25)
  } else {
    plot <- subplot(p1,p2 + theme(axis.title.x = element_blank()),p3 + theme(axis.title.x = element_blank()),p4 + theme(axis.title.x = element_blank()),p5 + theme(axis.title.x = element_blank()), p6,nrows=6,shareX = TRUE,heights = c(1,1,.75,.5,1,1)/5.25)
  }
  return(plot)
}

ISSNFilter <- (newspapers %>% filter(AINYLEISMAARE=="SAN",JULKAISUMAA=="FI"))$ISSN
fnpissuedata <- npissuedata %>% filter(ISSN %in% ISSNFilter,year<=1917)
fnppagedata <- nppagedata %>% filter(ISSN %in% ISSNFilter,year<=1917) 
plot <- plotdata(fnpissuedata,fnppagedata,by="paper")
ggsave("~/Downloads/newspaper-development.png", plot, units="cm", width = 21, height = 29.7, dpi=300)
plot <- plotdata(fnpissuedata,fnppagedata,by="paper",type="plotly")
api_create(plot,"newspaper-development","overwrite")
ISSNFilter <- (newspapers %>% filter(KIELI=="fin",AINYLEISMAARE=="SAN",JULKAISUMAA=="FI"))$ISSN
fnpissuedata <- npissuedata %>% filter(ISSN %in% ISSNFilter,year<=1917,year>=1800)
fnppagedata <- nppagedata %>% filter(ISSN %in% ISSNFilter,year<=1917,year>=1800) 
plot <- plotdata(fnpissuedata,fnppagedata,by="paper")
ggsave("~/Downloads/newspaper-development-fi.png", plot, units="cm", width = 21, height = 29.7, dpi=300)
plot <- plotdata(fnpissuedata,fnppagedata,by="paper",type="plotly")
api_create(plot,"newspaper-development-fi","overwrite")
ISSNFilter <- (newspapers %>% filter(KIELI=="swe",AINYLEISMAARE=="SAN",JULKAISUMAA=="FI"))$ISSN
fnpissuedata <- npissuedata %>% filter(ISSN %in% ISSNFilter,year<=1917)
fnppagedata <- nppagedata %>% filter(ISSN %in% ISSNFilter,year<=1917) 
plot <- plotdata(fnpissuedata,fnppagedata,by="paper")
ggsave("~/Downloads/newspaper-development-sv.png", plot, units="cm", width = 21, height = 29.7, dpi=300)
plot <- plotdata(fnpissuedata,fnppagedata,by="paper",type="plotly")
api_create(plot,"newspaper-development-sv","overwrite")
ISSNFilter <- (newspapers %>% filter(AINYLEISMAARE=="SAN",JULKAISUMAA=="FI",KAUPUNKI_NORM=="Helsinki"))$ISSN
fnpissuedata <- npissuedata %>% filter(ISSN %in% ISSNFilter,year<=1917)
fnppagedata <- nppagedata %>% filter(ISSN %in% ISSNFilter,year<=1917) 
plot <- plotdata(fnpissuedata,fnppagedata,by="paper")
ggsave("~/Downloads/newspaper-development-hel.png", plot, units="cm", width = 21, height = 29.7, dpi=300)
plot <- plotdata(fnpissuedata,fnppagedata,by="paper",type="plotly")
api_create(plot,"newspaper-development-hel","overwrite")
ISSNFilter <- (newspapers %>% filter(AINYLEISMAARE=="SAN",JULKAISUMAA=="FI",KAUPUNKI_NORM=="Turku"))$ISSN
fnpissuedata <- npissuedata %>% filter(ISSN %in% ISSNFilter,year<=1917)
fnppagedata <- nppagedata %>% filter(ISSN %in% ISSNFilter,year<=1917) 
plot <- plotdata(fnpissuedata,fnppagedata,by="paper")
ggsave("~/Downloads/newspaper-development-tur.png", plot, units="cm", width = 21, height = 29.7, dpi=300)
plot <- plotdata(fnpissuedata,fnppagedata,by="paper",type="plotly")
api_create(plot,"newspaper-development-tur","overwrite")
ISSNFilter <- c("1457-4314")
fnpissuedata <- npissuedata %>% filter(ISSN %in% ISSNFilter,year<=1917)
fnppagedata <- nppagedata %>% filter(ISSN %in% ISSNFilter,year<=1917) 
plot <- plotdata(fnpissuedata,fnppagedata,by="page")
ggsave("~/Downloads/newspaper-development-allmanna.png", plot, units="cm", width = 21, height = 29.7, dpi=300)
plot <- plotdata(fnpissuedata,fnppagedata,by="paper",type="plotly")
api_create(plot,"newspaper-development-allmanna","overwrite")

View(npissuedata %>% group_by(pages,type,wmodecols) %>% summarise(count=n()) %>% arrange(-count))
