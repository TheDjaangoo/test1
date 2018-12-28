getwd()
setwd("C:/Users/rafal/Desktop/Projekt_1")     

rm(list=ls(all=TRUE))


air_transport<-read.table(file = 'ttr00012.tsv.gz', sep = '\t', header = TRUE,stringsAsFactors=FALSE)
air_transport<-air_transport[c(5,6,9,29,31),]
air_transport<-air_transport[,c(1,10,11,12,13)]

rownames(air_transport)<-1:5

air_transport$unit.tra_meas.tra_cov.schedule.geo.time<-c("Cypr","Czechy","Estonia","Polska","Rumunia")


str(air_transport)
num<-c(2:5)
for (i in num){
  air_transport[,i]<-as.numeric(air_transport[,i])
  air_transport[,i]<-air_transport[,i]/1000000
}
air_transport<-arrange(air_transport,desc(X2014) )
barplot(as.matrix(air_transport[,c(2,3,4,5)]),beside=T,col=c("red","green","navy","black","yellow"),xlab="Rok",ylab="Pasazerowie w milionach",main='Transport lotniczy w wybranych państwach w latach 2014-2017',cex.main=1)
legend("topleft",
       air_transport$unit.tra_meas.tra_cov.schedule.geo.time,
       fill = c("red","green","navy","black","yellow")
)
  


#zabicia na drogach
zab_drog<-read.table(file = 'sdg_11_40.tsv.gz', sep = '\t', header = TRUE,stringsAsFactors=FALSE)
dim(zab_drog)
zab_drog<-zab_drog[,c(1,14,15,16,17)] 
zab_drog<-zab_drog[c(4,5,8,23,25),]
zab_drog$unit.geo.time<-c("Cypr","Czechy","Estonia","Polska","Rumunia")
rownames(zab_drog)<-1:5

num<-c(2:5)
for (i in num){
  zab_drog[,i]<-as.numeric(zab_drog[,i])
  
}
zab_drog<-arrange(zab_drog, desc(X2014) )
barplot(as.matrix(zab_drog[,c(2,3,4,5)]),beside=T,col=c("red","green","navy","grey","yellow"),xlab="Rok",ylab="Liczba smiertelnych wypadkow drogowych ",main='Liczba smiertelnych wypadkow drogowych w latach 2013-2016',cex.main=1,cex.lab=1.5 ,ylim=c(0,3700)) 
legend("topright",xpd=T,inset = c(0, 
                                0), bty = "n",
       zab_drog$unit.geo.time,
       fill = c("red","green","navy","grey","yellow")
      
)

#energia odnawialna
install.packages("xlsReadWrite")
eng<-read.table(file = 't2020_31.tsv.gz', sep = '\t', header = TRUE,stringsAsFactors=FALSE)
dim(eng)
eng<-eng[,c(1,8,12,13,14,15)]
eng<-eng[c(5,6,9,29,31),]

eng$unit.indic_eu.geo.time<-c("Cypr","Czechy","Estonia","Polska","Rumunia")
colnames(eng)[6]<-"Cel"
colnames(eng)[1]<-"Panstwo"
rownames(zab_drog)<-1:5
View(eng)
write.csv(eng,file="table.csv")


eng_ga<-gather(eng,key=lata,values=X2016:Cel)
eng_ga$value<-as.numeric(eng_ga$value)
arrange(eng_ga,desc(value) )

colnames(eng_ga)[1]<-"Panstwo"

str(eng_ga)
ggplot(eng_ga, aes(x=lata,y=value/100,group=Panstwo,col=Panstwo))+
geom_line()+geom_point()+
  labs(title="Udział procentowy energii odnawialnej w konsumpcji energii brutto",x="Cel na rok 2020 oraz udział w roku 2016" ,y="Udział procentowy energii odnawialnej")+
  theme_bw() +
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(size=20),axis.title.x = element_text(size=15), legend.text=element_text(size=15))
  

#gdp

gdp<-read.table(file = 'tec00115.tsv.gz', sep = '\t', header = TRUE,stringsAsFactors=FALSE)
gdp<-gdp[c(7,8,13,32,34),]
gdp<-gdp[c(1,13)]
str(gdp)
gdp<-separate(gdp,X2017,into=c("X2017","Warning"),sep=" ")
colnames(gdp)[1]<-"Panstwo"
gdp$Panstwo<-c("Cypr","Czechy","Estonia","Polska","Rumunia")
rownames(gdp)<-1:5
gdp$X2017<-as.numeric(gdp$X2017)
ggplot(gdp,aes(x=Panstwo  ,y=X2017,fill=Panstwo))+
  geom_bar(stat="identity")+scale_y_continuous(name="Wzrost % PKB w roku 2017", breaks=seq(0,8,by=0.5) )+
  labs(title="Wzrost procentowy PKB w 2017 roku w wybranych państwach")+
  theme(plot.title = element_text(hjust = 0.5))


#inflacja




inf<-read.table(file = 'tec00118.tsv.gz', sep = '\t', header = TRUE,stringsAsFactors=FALSE)
inf<-inf[c(5,6,9,29,31),]
str(inf)
dim(inf)

num<-c(2:13)
for (i in num){
  inf[,i]<-as.numeric(inf[,i])
  
}
colnames(inf)[1]<-"Panstwo"
inf$Panstwo<-c("Cypr","Czechy","Estonia","Polska","Rumunia")
rownames(inf)<-1:5

inf<-gather(inf,key=Rok,values=X2006:X2017)

ggplot(inf,aes(x=Rok,y=value/100,group=Panstwo,col=Panstwo))+
  geom_point()+geom_line()+ scale_y_continuous(name="Roczna inflacja ",labels=scales::percent ) +labs(title="Roczna procentowa inflacja w państwach w latach 2006-2017")+
  theme(axis.text.x = element_text(size=10),axis.title.x = element_text(size=15), legend.text=element_text(size=15))

tabelka<-group_by(inf,Panstwo)
tabelka1<-summarise(tabelka,Odchylenie_inflacji=sd(value,na.rm=T))

write.csv(tabelka1,file="table1.csv")





#Bezpieczenstwo

inf<-read.table(file = 'crim_off_cat_1.tsv', sep = '\t', header = TRUE,stringsAsFactors=FALSE)

inf<-inf[c(3,6,13,21,23),]

inf<-inf[,c(1,5,7)]
str(inf)
num<-c(2:3)
for (i in num){
  inf[,i]<-as.numeric(inf[,i])
  
}



nazw_Panstw<-c("Czech Republic", "Cyprus","Estonia","Poland","Romania")
inf$GEO.TIME.ICCS.UNIT<-nazw_Panstw


library(rworldmap)

n <- joinCountryData2Map(inf, joinCode="NAME", nameJoinColumn="GEO.TIME.ICCS.UNIT")




mapCountryData(n, nameColumnToPlot="Rape.Per.hundred.thousand.inhabitants", mapTitle="Liczba gwałtów na 100 tys. mieszkańców",
               xlim=c(-10, 40), ylim=c(35, 60),
               addLegend=TRUE,
               oceanCol="lightblue", missingCountryCol="black")




mapCountryData(n, nameColumnToPlot = "Sexual.assault.Per.hundred.thousand.inhabitants",xlim=c(-10, 40), ylim=c(35, 60),  
               oceanCol="lightblue",missingCountryCol="black",borderCol="white",colourPalette="heat",mapTitle=" Liczba napaści na 100 tys. mieszkańców",addLegend=TRUE)

