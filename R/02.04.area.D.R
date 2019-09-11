crosspointer<-function(xs,ys,xe,ye,xbs,ybs,xbe,ybe){
  #Calculate slope and intercept
  beta<-(ys-ye)/(xs-xe)
  beta[beta==Inf|beta==-Inf]<-0
  alpha<-ys-xs*beta
  b<-(ybs-ybe)/(xbs-xbe)
  b[b==Inf|b==-Inf]<-0
  a<-ybs-xbs*b
  #xp<-(a-alpha)/(beta-b)
  #yp<-alpha+beta*xp
  xp<-((a-alpha)/(beta-b))*(!((xbs-xbe)==0))+xbs*((xbs-xbe)==0)
  yp<-alpha+beta*xp
  result<-data.frame(x=xp,y=yp,no=1:length(xp))
  result<-result[min(xs,xe)<=xp&xp<=max(xs,xe)&min(ys,ye)<=yp&yp<=max(ys,ye),]
  result
}


#02.04 area.D
#'Calculate densities in each area
#'@description Calculate densities in each area based on leg.D result object
#'
#'@param leg.D.res result of densities in each leg
#'
#'@author Hiroaki Murata
#'@export
#'
#'@examples
#'
#'
area.D<-function(leg.D.res){
  if(F){
    Xbs<-c(131.6901,137.346183,140.197526,140.912595,141.185086,140.850333,136.013426,131.160302,130.96188,131.885671,134.750218)
    Ybs<-c(34.6814888,37.5161048,41.3973511,41.5464391,41.8152797,35.7440398,33.7267461,31.4598981,33.9597348,32.7460618,33.8347259)
    Xbe<-c(131.6901,137.346183,135,140.720999,145,145,136.013426,131.160302,130.955678,132.655354,135.057631)
    Ybe<-c(40,45,41.3973511,41.744508,41.8152797,35.7440398,30,25,33.9633209,32.9251463,33.8798075)
    area.number<-1:9
    area.border<-data.frame(border=c(1,2,3,4,5,6,7,8,9,10,11),
                            area1=c(5,6,7,1,1,1,2,3,4,3,3),
                            area2=c(6,7,8,7,9,2,3,5,5,4,4))
  }#IF
  #area.bobj<-read.csv("area.border.csv")
  Xbs<-area.bobj$Xbs
  Xbe<-area.bobj$Xbe
  Ybs<-area.bobj$Ybs
  Ybe<-area.bobj$Ybe
  area.number<-1:17
  area.border<-area.bobj[,c("border","area1","area2")]
  lat<-seq(50,10,-1/20)
  lon<-seq(120,180,1/20)
  lat.l<-lat-1/40
  lat.u<-lat+1/40
  lon.l<-lon-1/40
  lon.u<-lon+1/40
  #sea.area<-read.csv("sea.area.csv")
  rownames(sea.area)<-sea.area[,1]
  sea.area<-sea.area[,-1]

  if(F){
    leg.lengths<-NULL
    for(i0 in 1:length(Data.names)){
      den.res<-read.xlsx("Density estimate.xlsx",sheetName=Data.names[i0])
      leg.lengths<-c(leg.lengths,den.res$leg.length)
    }
    leg.lengths<-leg.lengths[leg.lengths<40]
    mean.length<-mean(leg.lengths)
    border.length<-mean.length*0.3
  }
  border.length<-0


  adleg<-NULL
  for(i1 in 1:nrow(leg.D.res)){
    #i1<-1
    tmp<-leg.D.res[i1,]
    y.s<-which(lat.l<tmp$Lat.Start&tmp$Lat.Start<=lat.u)
    x.s<-which(lon.l<tmp$Lon.Start&tmp$Lon.Start<=lon.u)
    y.e<-which(lat.l<tmp$Lat.End&tmp$Lat.End<=lat.u)
    x.e<-which(lon.l<tmp$Lon.End&tmp$Lon.End<=lon.u)
    label.s<-sea.area[y.s,x.s]
    label.e<-sea.area[y.e,x.e]
    if(label.s==label.e){
      tmp$area<-label.s
      adleg<-rbind(adleg,tmp)
    }else{
      Xs<-tmp$Lon.Start
      Ys<-tmp$Lat.Start
      Xe<-tmp$Lon.End
      Ye<-tmp$Lat.End
      tmp.c.res<-crosspointer(Xs,Ys,Xe,Ye,Xbs,Ybs,Xbe,Ybe)
      #arrows(Xbs,Ybs,Xbe,Ybe,length=0)
      #arrows(Xs,Ys,Xe,Ye,length=0,col="red")
      if(length(tmp.c.res$x)==0){
        if(label.s==99|label.e==99){
          tmp.label<-c(label.s,label.e)
          tmp$area<-tmp.label[tmp.label!=99]
          adleg<-rbind(adleg,tmp)
        }else{
          tmp.cp<-eff.grid(Xs,Ys,Xe,Ye,lat,lon)
          tmp.area<-NULL
          for(i2 in 1:nrow(tmp.cp)){
            y<-which(lat.l<tmp.cp$lat[i2]&tmp.cp$lat[i2]<=lat.u)
            x<-which(lon.l<tmp.cp$lon[i2]&tmp.cp$lon[i2]<=lon.u)
            tmp.area<-c(tmp.area,sea.area[y,x])
          }
          tmp$area<-as.numeric(names(sort(table(c(tmp.area,label.s,label.e)),decreasing=T)[1]))
          adleg<-rbind(adleg,tmp)
        }#else#if(label.s==99|label.e==99)
      }else{
        if(label.s%in%c(77,99)|label.e%in%c(77,99)){
          tmp.label<-c(label.s,label.e)
          if(sum(tmp.label%in%area.bobj[area.border$border%in%tmp.c.res$no,c("area1","area2")])==0)next
          is.77<-tmp.label==77
          tmp1<-tmp2<-tmp
          tmp1$Lon.End<-tmp.c.res$x
          tmp1$Lat.End<-tmp.c.res$y
          tmp2$Lon.Start<-tmp.c.res$x
          tmp2$Lat.Start<-tmp.c.res$y
          tmp1$Leg.Length<-hubeny(tmp1$Lon.Start,tmp1$Lat.Start,tmp1$Lon.End,tmp1$Lat.End)
          tmp2$Leg.Length<-hubeny(tmp2$Lon.Start,tmp2$Lat.Start,tmp2$Lon.End,tmp2$Lat.End)
          tmp1$area<-label.s
          tmp2$area<-label.e
          adleg<-rbind(adleg,tmp1,tmp2)
        }else{
          use.ind<-which((((area.border$area1==label.s)&(area.border$area2==label.e))|
                            ((area.border$area2==label.s)&(area.border$area1==label.e)))&
                           (area.border$border%in%tmp.c.res$no))
          ab<-as.vector(area.border[use.ind,-1])
          tmp.c.res<-tmp.c.res[tmp.c.res$no==use.ind,]
          tmp1<-tmp2<-tmp
          tmp1$Lon.End<-tmp.c.res$x
          tmp1$Lat.End<-tmp.c.res$y
          tmp2$Lon.Start<-tmp.c.res$x
          tmp2$Lat.Start<-tmp.c.res$y
          tmp1$Leg.Length<-hubeny(tmp1$Lon.Start,tmp1$Lat.Start,tmp1$Lon.End,tmp1$Lat.End)
          tmp2$Leg.Length<-hubeny(tmp2$Lon.Start,tmp2$Lat.Start,tmp2$Lon.End,tmp2$Lat.End)
          if(length(ab[ab==label.s])){
            tmp1$area<-ab[ab==label.s]
            tmp2$area<-ab[ab!=label.s]
          }else{
            tmp1$area<-ab[ab==label.e]
            tmp2$area<-ab[ab!=label.e]
          }#else#if(ab[ab==label.s])
          adleg<-rbind(adleg,tmp1,tmp2)
        }
      }#else#if((label.s==99|label.e==99)&length(tmp.c.res$x)==0)
    }#else#if(label.s==label.e)
  }#for(i1)
  adleg<-adleg[adleg$Leg.Length>border.length,]


  tmp.data<-adleg
  tmp.den<-as.data.frame(matrix(NA,length(area.number),3,dimnames=list(c(),c("Area","Density","Total.Length"))))
  for(i2 in area.number){
    #i2<-1
    tmp.data2<-tmp.data[tmp.data$area==i2,]
    tmp.d<-tmp.data2[,"Density"]*tmp.data2$Leg.Length
    total.length<-sum(tmp.data2$Leg.Length)
    if(is.null(tmp.d)){
      tmp.den[i2,]<-c(i2,NA,total.length)
    }else{
      tmp.den[i2,]<-c(i2,sum(tmp.d)/total.length,total.length)
    }#else#if(is.null(dim(tmp.d)))
  }#for(i2)
  ad<-tmp.den
  return(list(area.density=ad,area.density.leg=adleg))
}#area.D


