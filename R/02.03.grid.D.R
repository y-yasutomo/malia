#02.03 grid.D
#'Calculate densities in each grid
#'@description Calculate densities in each grid based on leg.D result object
#'
#'@param leg.D.res result of densities in each leg
#'@param divide
#'@param ignore
#'
#'@author Hiroaki Murata
#'@export
#'
#'@examples
#'
#'
grid.D<-function(leg.D.res,divide=0.5,ignore=mean(leg.D.res$Leg.Length)*0.3){

  lat.range<-range(leg.D.res$Lat.Start,leg.D.res$Lat.End)
  lon.range<-range(leg.D.res$Lon.Start,leg.D.res$Lon.End)
  lat.range<-c(floor(lat.range[1]),ceiling(lat.range[2]))
  lon.range<-c(floor(lon.range[1]),ceiling(lon.range[2]))
  lats<-seq(lat.range[1],lat.range[2],divide)
  lons<-seq(lon.range[1],lon.range[2],divide)
  lat.min<-lats[1:(length(lats)-1)]
  lat.max<-lats[2:length(lats)]
  lon.min<-lons[1:(length(lons)-1)]
  lon.max<-lons[2:length(lons)]
  lat.mins<-rep(lat.min,length(lon.min))
  lat.maxs<-rep(lat.max,length(lon.max))
  lon.mins<-rep(lon.min,each=length(lat.min))
  lon.maxs<-rep(lon.max,each=length(lat.max))
  grids<-data.frame(lat.min=lat.mins,lat.max=lat.maxs,lon.min=lon.mins,lon.max=lon.maxs)


  grid.D.res<-NULL
  for(i in 1:nrow(leg.D.res)){
    #i<-1
    add.data<-NULL
    add.data<-eff.grid(leg.D.res$Lon.Start[i],leg.D.res$Lat.Start[i],leg.D.res$Lon.End[i],leg.D.res$Lat.End[i],lons,lats)
    names(add.data)<-c("lon.m","lat.m","leg.length")
    tmp.den<-unlist(leg.D.res[i,"Density"])
    tmp.den<-as.data.frame(matrix(rep(as.vector(tmp.den),nrow(add.data)),ncol=1,byrow=T))
    names(tmp.den)<-"Density"
    add.data<-cbind(add.data,tmp.den)
    #add.data$density<-eval(parse(text=paste("leg.D.res$",all.types[i1],sep="")))[i]
    grid.D.res<-rbind(grid.D.res,add.data)
  }
  grid.D.res<-grid.D.res[grid.D.res$leg.length>ignore,]
  coord.data<-data.frame(lat.min=lat.mins,lat.max=lat.maxs,lon.min=lon.mins,lon.max=lon.maxs)
  grid.den<-leg.leng<-numeric(nrow(coord.data))
  for(i in 1:nrow(coord.data)){
    use.Data<-NULL
    use.Data<-grid.D.res[coord.data$lat.min[i]<=grid.D.res$lat&grid.D.res$lat<coord.data$lat.max[i]&coord.data$lon.min[i]<=grid.D.res$lon&grid.D.res$lon<coord.data$lon.max[i],]
    Subs<-sum(use.Data$Density*use.Data$leg.length)/sum(use.Data$leg.length)
    #Subs<-eval(parse(text=paste("sum(use.Data$Density*use.Data$leg.length)/sum(use.Data$leg.length)",sep="")))
    grid.den[i]<-Subs
    leg.leng[i]<-sum(use.Data$leg.length)
  }#for(i)
  grid.D.res2<-cbind(coord.data,Density=grid.den,Total.length=leg.leng)
  grid.D.res2
}#grid.D

