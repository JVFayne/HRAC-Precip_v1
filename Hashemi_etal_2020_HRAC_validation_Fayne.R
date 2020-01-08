#Relative Bias Figure (Figure 3) 

#####################
#relative bias
####################
extractbuoy3<-as.matrix.data.frame(read.csv("/originaltrmmat50percent.csv"))[,2:208] #select corrected or un-corrected (original)
windowsFonts(A = windowsFont("Times New Roman"))
#for (yy in 1:12){ #months: Select which month to plot biases, December has the largest biases so we want to show how they are improved
yy=12 ## yy is set to 12 for december

svg(filename = paste0("/CorrectedALLPercentCoverage_December.svg"),
    width = 7.5, height = 10.5,  pointsize = 10,
    bg = "white", family = "serif")

layout(matrix(c(1:12), nrow = 4, byrow = T));par(mfrow = c(4,3),pin = c(2,1), oma = c(0,0,0,0)); 
#this creates a plot showing the before and after for the 50-100 percent station coverage areas

for (ty in seq(50,100,10)){ #for each of the station coverage percentages (50 to 100 by 10)
  
  buoyvalues<-as.matrix(read.csv(paste0("/ParsedStations_",ty,"percent.csv")))
  elev<-array()
  biases<-array()
  for (tt in 2:nrow(buoyvalues)){
    buoyname<-buoyvalues[(tt),2]
    compareloc<-grep(buoyname,extractbuoy3[,1])
    buoyele<-as.numeric(buoyvalues[(tt),6])
    CTRMMcell<-as.numeric(extractbuoy3[compareloc,2:207])*24*30 #mm/hr to mm/month
    Gaugevalue<-as.numeric(buoyvalues[(tt),7:212]) #mm/month
    rbias<-2*((CTRMMcell-Gaugevalue)/(15+(CTRMMcell+Gaugevalue)))
    
    monthy<-t(as.numeric(rbias[seq(yy,204,12)]))
    monthy.elevation<-t(rep(buoyele,length(monthy)))
    
    elev<-cbind(elev, monthy.elevation)
    biases<-cbind(biases,monthy)
  }
 
  biase3<-biases[is.na(biases)==F]
  ele3<-elev[is.na(biases)==F]
  ########## blue contour cloud with points
  
  xlab1<-""
  ylab1<-""
  
  if (ty==50 |ty==80) {ylab1="Relative Bias"} 
  
  par(mar=c(4,4,1,2)+0.1)
  if (ty==50|ty==60|ty==70) {par(mar=c(4,4,4,2)+0.1)}
  
  smoothScatter(ele3, biase3,nrpoints=5000,xlab=xlab1, ylab=ylab1,ylim=c(-2,2),xlim=c(1,3500),pch=".",cex=0.001)  
  lines(lowess(ele3,biase3,f = 1/6),col='red')
  abline(h=0, v=1500, lwd=1,col="black")
  rect(1000, 1.5, 3500, 2, density = NULL, angle = 45,
       col = "white", border = 'white', lty = 1, lwd = 1)
  text(2250,1.7, paste0(ty,"% Coverage"),cex=1.5,font=6)
}   

mtext("a)", side = 3, line = -2, outer = TRUE) #for the figure
mtext("b)", side = 3, line = -49, outer = TRUE)

extractbuoy3<-as.matrix.data.frame(read.csv("/Correctedtrmmat50percent.csv"))[,2:208] #select corrected or un-corrected (original)
for (ty in seq(50,100,10)){ #for each of the 
  
  buoyvalues<-as.matrix(read.csv(paste0("/ParsedStations_",ty,"percent.csv")))
  elev<-array()
  biases<-array()
  for (tt in 2:nrow(buoyvalues)){
    buoyname<-buoyvalues[(tt),2]
    compareloc<-grep(buoyname,extractbuoy3[,1])
    buoyele<-as.numeric(buoyvalues[(tt),6])
    CTRMMcell<-as.numeric(extractbuoy3[compareloc,2:207])*24*30 #mm/hr to mm/month
    Gaugevalue<-as.numeric(buoyvalues[(tt),7:212]) #mm/month
    rbias<-2*((CTRMMcell-Gaugevalue)/(15+(CTRMMcell+Gaugevalue)))
    
    monthy<-t(as.numeric(rbias[seq(yy,204,12)]))
    monthy.elevation<-t(rep(buoyele,length(monthy)))
    
    elev<-cbind(elev, monthy.elevation)
    biases<-cbind(biases,monthy)
  }
  
  biase3<-biases[is.na(biases)==F]
  ele3<-elev[is.na(biases)==F]
  ########## blue contour cloud with points
  
  xlab1<-""
  ylab1<-""
  
  if (ty==80|ty==90|ty==100) {xlab1="Elevation (m.amsl)"} #for the second grouping only
  if (ty==50 |ty==80) {ylab1="Relative Bias"} 
  
  par(mar=c(4,4,1,2)+0.1)
  if (ty==50|ty==60|ty==70) {par(mar=c(4,4,4,2)+0.1)}
 
  smoothScatter(ele3, biase3,nrpoints=7500,xlab=xlab1, ylab=ylab1,ylim=c(-2,2),xlim=c(1,3500),pch=".",cex=0.01)  
  lines(lowess(ele3,biase3,f = 1/6),col='red')
  abline(h=0, v=1500, lwd=1,col="black")
  rect(1000, 1.5, 3500, 2, density = NULL, angle = 45,
       col = "white", border = 'white', lty = 1, lwd = 1)
  text(2250,1.7, paste0(ty,"% Coverage"),cex=1.5,font=6)
}  
dev.off()