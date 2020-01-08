date()
TPCPfiles<-as.matrix.data.frame(read.csv("/stations50000_2.csv"))
date()

Stations<-as.matrix(unique(TPCPfiles[,2]))
stations2<-stationbygrep

iseq<-seq(1,nrow(TPCPfiles),10000)
listallstations<-TPCPfiles[,2]

##START!
date()
stationbygrep<-array()
for (i in 1:length(stations2)){
  station<-toString(stations2[i])
  station<-tail(Stations[grep(station,Stations)],1)
  uu<-1
  while (uu < length(iseq)){
    
    pr<-grep(station,listallstations[iseq[uu]:iseq[(uu+1)]])
    if (length(pr)!=0){break}
    uu<-uu+1
  }
  greplocation<-(iseq[(uu)]+pr)-1
  if (length(greplocation)>=100){
    stationbygrep<-c(stationbygrep,station)
  }
}
date()
write.csv(stationbygrep,"/stationnamesonly1-50k_50percent.csv")
date()


stationbygrep<-as.matrix(read.csv("/stationnamesonly1-50k_50percent.csv"))[,2]
###### once the stations are chosen, parse the precipfiles


date()

Years<-c(1998:2015)
Months<-c(1:12)
Months[1:9]<-paste0("0",Months[1:9])
Months<-paste0(Months,"01")
Date1<-array(c("StationNo","StationName","Lat","Lon","Elevation"))
for (p in 1:length(Years)){
  Yearbyyear<-paste0(Years[p],Months)
  Date1<-c(Date1,Yearbyyear)
}
date()
Date3<-Date1
Date50<-Date1
Date60<-Date1
Date70<-Date1
Date80<-Date1
Date90<-Date1
Date100<-Date1

date()
for (i in 2:length(stationbygrep)){
  cstation<-stationbygrep[i]
  greplocation<-grep(cstation,TPCPfiles[,2])
  Precip<-as.matrix(TPCPfiles[greplocation,8])
  Date2<-(TPCPfiles[greplocation,7])
  Date2<-paste0(substr(Date2,1,4),substr(Date2,6,7),"01")
  checkprecip<-array("Precipitation")
  for (jj in 6:length(Date1)){
    CurrentPrecip<-Precip[grep(Date1[jj],Date2)]
    if (length(CurrentPrecip)==0 || CurrentPrecip==-9999 || is.na(CurrentPrecip)){CurrentPrecip<-"No Report"}
    checkprecip<-c(checkprecip,CurrentPrecip)
  }
  if(sum((checkprecip)=="No Report")<1){
    
    #StationName<-toString(StationNames[i]) # there are not the same number of unique station names
    StationName<-cstation #so stations will just have to be listed twice
    Lat<-unique(as.numeric((TPCPfiles[greplocation,4])))
    Lon<-unique(as.numeric((TPCPfiles[greplocation,5])))
    Elevate<-as.numeric(head(TPCPfiles[greplocation,6],1))
    precipitationstations<-t(as.matrix(c(cstation,StationName,Lat,Lon,Elevate,checkprecip[2:217])))
    if(sum((checkprecip)=="No Report")<103){
      Date50<-rbind(Date50,precipitationstations)
    }
    if(sum((checkprecip)=="No Report")<82){
      Date60<-rbind(Date60,precipitationstations)
    }
    if(sum((checkprecip)=="No Report")<62){
      Date70<-rbind(Date70,precipitationstations)
    }
    if(sum((checkprecip)=="No Report")<41){
      Date80<-rbind(Date80,precipitationstations)
    }
    if(sum((checkprecip)=="No Report")<21){
      Date90<-rbind(Date90,precipitationstations)
    }
    Date100<-rbind(Date100,precipitationstations)
  }
}
date()
write.csv(Date100,"/ParsedStations_100percent.csv")
write.csv(Date90,"/ParsedStations_90percent.csv")
write.csv(Date80,"/ParsedStations_80percent.csv")
write.csv(Date70,"/ParsedStations_70percent.csv")
write.csv(Date60,"/ParsedStations_60percent.csv")
write.csv(Date50,"/ParsedStations_50percent.csv")

date()
