# Original script used to correct the TRMM TMPA data to HRAC-Precip_v1 using coefficients from the Hashemi et al 2017 : 
#https://journals.ametsoc.org/doi/full/10.1175/JHM-D-17-0025.1


# read TRMM file
# read DEM

a<-c(0.0007316,0.0008422,0.0006415,0.0004654,0.0002078,0.0001372,0.0002642,0.0002458,0.0002344,0.0003299,0.0006928,0.0007785)
b<-c(-1.3202,-1.4827,-1.0475,-0.7371,-0.2777,-0.2639,-0.4941,-0.4266,-0.3592,-0.4655,-1.1823,-1.3022)
ref<-raster("/TRMM20130101clipped_resampled.tif")
DEM<-(raster("/GTOPO30/GTOPO30/DEM_Converted/UScontig2_DEM_1km.tif"))
DEM[is.na(DEM)]<- -9999
DEMh<-DEM
DEMh[DEMh<=1500]<-0
DEMh[DEMh>1500]<-1
states<-shapefile("/contigstates.shp")
TRMMs<-list.files("/TRMMTIF",pattern=".tif")
setwd("/TRMMTIF")
TRMM<-brick(lapply(TRMMs,raster))
TRMM2<-crop(TRMM,ref)
date()
TRMM2<-resample(TRMM2,ref,method="ngb")
date()

date()
for (i in 1:length(TRMMs)){
  month<-as.numeric(substr(TRMMs[i],9,10))
  coa<-a[month]
  cob<-b[month]
  
  precipfile<-as.matrix(TRMM2[[i]])
  
  precipfile[DEM>1500]<-precipfile[DEM>1500]*(coa*DEM[DEM>1500]+cob+1)
  precipfile[precipfile<0]<-NA
  precipfile[DEM== -9999]<-NA
  TRMM3<-raster(precipfile,xmn=xmin(ref),xmx=xmax(ref),ymn=ymin(ref),ymx=ymax(ref),crs=crs(ref),template=NULL)
  writeRaster(TRMM3,paste0("/Corrected3/",substr(TRMMs[i],1,12),"corrected.tif"),
              format="GTiff",datatype="FLT4S",overwrite=T)
  
}
date()
