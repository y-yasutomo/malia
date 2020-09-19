#08.00.Data.handling
#'Create sighting data for MALIA
#'@description The data for malia package is craeated from 'raw' data
#'
#'@param Data.name
#'@param Rev.name
#'@param Breaks
#'
#'@import openxlsx
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
Sight.hand<-function(Data.name,Rev.name,
                     Breaks=c(0,5,10,15,20,25,30,35,40,45,50,75,100,150,200)){

  if(F){
    Data.name<-"海鷹　目視　全まとめ（UT+9ｈ）"
    Rev.name<-"UU"
  }

  full.path<-paste(getwd(),"/",Data.name,".xlsx",sep="")
  Data <- read.xlsx(full.path, sheet = 1, check.names = F)

  use.c.name<-c("max","min","distance","leg.number","lat","lon","type","size","weather","occo","number")

  hand.Data<-data.frame(matrix(NA,nrow(Data),length(use.c.name),dimnames=
                                 list(c(),use.c.name)))
  binned.PD<-bin(x=Data$distance,Breaks=Breaks)
  hand.Data[,1:3]<-binned.PD
  hand.Data$lat<-Data$lat
  hand.Data$lon<-Data$lon
  hand.Data$leg.number<-Data[,1]
  hand.Data$type<-Data$item_kind
  hand.Data$size<-Data$size
  hand.Data$weather<-Data$weather
  hand.Data$occo<-Data$sea_conditions
  hand.Data$number<-Data$debris_number

  if(!is.factor(hand.Data$type))
    hand.Data$type<-factor(hand.Data$type,levels=unique(hand.Data$type))
  if(!is.factor(hand.Data$size))
    hand.Data$size<-factor(hand.Data$size,levels=unique(hand.Data$size))
  if(!is.factor(hand.Data$weather))
    hand.Data$weather<-factor(hand.Data$weather,levels=unique(hand.Data$weather))

  csv.list<-list.files(getwd(),"debris.csv")
  ind<-paste(Rev.name,".debris.csv",sep="")%in%csv.list
  if(!ind){
    cat("\n",paste(Rev.name,".debris.csv",sep=""),"was created.")
    write.csv(hand.Data,paste(Rev.name,".debris.csv",sep=""),row.names=F)
  }else{
    write.csv(hand.Data,paste(Rev.name,"2.debris.csv",sep=""),row.names=F)
    warning("Since ",paste(Rev.name,".debris.csv",sep=""),
            " already exists, \n",paste(Rev.name,"2.debris.csv",sep=""),
            " was created.",immediate.=T)
  }
}#Sight.hand


#'Create effort data for MALIA
#'@description The data for malia package is craeated from 'raw' data
#'
#'@param Data.name
#'@param Rev.name
#'
#'@import openxlsx
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
Effort.hand<-function(Data.name,Rev.name){

  helper<-function(vec){
    if(!is.numeric(vec)){
      return(as.numeric(as.character(vec)))
    }else{
      return(vec)
    }
  }

  full.path<-paste(getwd(),"/",Data.name,".xlsx",sep="")
  f.data <- read.xlsx(full.path, sheet = 1, check.names = F)

  df<-data.frame(leg.number=f.data$No.,
                 lat.start=f.data$start.lat.,
                 lon.start=f.data$start.lon.,
                 lat.end=f.data$end.lat.,
                 lon.end=f.data$end.lon.)

  df$leg.number<-helper(df$leg.number)
  df$lat.start<-helper(df$lat.start)
  df$lat.end<-helper(df$lat.end)
  df$lon.start<-helper(df$lon.start)
  df$lon.end<-helper(df$lon.end)
  leg.length<-numeric()
  for(i in 1:nrow(df)){
    leg.length[i]<-hubeny(lon0=df$lon.start[i],
                          lat0=df$lat.start[i],
                          lon1=df$lon.end[i],
                          lat1=df$lat.end[i])
  }#for(i)
  df$leg.length<-leg.length

  csv.list<-list.files(getwd(),"effort.csv")
  ind<-paste(Rev.name,".effort.csv",sep="")%in%csv.list
  if(!ind){
    cat("\n",paste(Rev.name,".effort.csv",sep=""),"was created.")
    write.csv(df,paste(Rev.name,".effort.csv",sep=""),row.names=F)
  }else{
    write.csv(df,paste(Rev.name,"2.effort.csv",sep=""),row.names=F)
    warning("Since ",paste(Rev.name,".effort.csv",sep=""),
            " already exists, \n",paste(Rev.name,"2.effort.csv",sep=""),
            " was created.",immediate.=T)
  }
}#effort.handling

#08.01 Data check before conducting analysis
#'Check data type
#'@description Check and redefine data type for covariate
#'
#'@param Sight.Data Sighting data of debris
#'@param Effort.Data Effort data of survey
#'@param numeric_covariate character vector of covariate to be used as numeric
#'@param factor_covariate character vector of covariate to be used as factor
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
Data.check<-function(Sight.Data,
                     numeric_covariate=c("occo"),
                     factor_covariate=c("weather","size")){
  ###
  if(F){
    Sight.Data<-read.csv(paste(Voyage.name,".debris.csv",sep=""))
    Sight.Data$type<-as.character(Sight.Data$type)
    Sight.Data$weather<-as.character(Sight.Data$weather)
    Sight.Data$size<-as.character(Sight.Data$size)
    Sight.Data$occo<-as.character(Sight.Data$occo)
    Sight.Data$max <- as.character(Sight.Data$max)
    Sight.Data$min <- as.character(Sight.Data$min)

    numeric_covariate=c("occo")
    factor_covariate=c("weather","size")

    Data.check(Sight.Data)
  }#IF(F)
  ###

  #check numeric distance
  if(!(is.numeric(Sight.Data$max) &&  is.numeric(Sight.Data$min))){
    stop("max and min shoule be numeric",call.=F)
  }

  #check numeric covariate
  idx <- logical()
  for(i in 1:length(numeric_covariate)){
    if(!(is.numeric(Sight.Data[,numeric_covariate[i]]))){
      idx <- c(idx ,1)
    }else{
      idx <- c(idx ,0)
    }
  }

  if(sum(idx)>0){
    message<-paste(numeric_covariate[which(idx==1)],"should be numeric.")
    stop(paste(message,collapse = "\n"),call.=F)
  }

  #check debris type
  if(!(is.factor(Sight.Data$type))){
    stop("type should be factor",call.=F)
  }

  #check factor covariate
  idx <- logical()
  for(i in 1:length(factor_covariate)){
    if(!(is.factor(Sight.Data[,factor_covariate[i]]))){
      idx <- c(idx ,1)
    }else{
      idx <- c(idx ,0)
    }
  }

  if(sum(idx)>0){
    message<-paste(factor_covariate[which(idx==1)],"should be factor.")
    stop(paste(message,collapse = "\n"),call.=F)
  }

}#Data.check


bin<-function(x,Breaks=c(0,5,10,15,20,25,30,35,40,45,50,75,100,150,200)){
  xmin<-xmax<-numeric()
  for(i in 1:length(x)){
    if(is.na(x[i])){
      xmin[i]<-NA;xmax[i]<-NA
    }else{
      ind.min<-max(which(x[i]>Breaks))

      if(ind.min==length(Breaks)){
        xmin[i]<-Inf;xmax[i]<-Inf
      }else if(x[i]>=Breaks[length(Breaks)]){
        xmax[i]<-xmin[i]<-Inf
      }else{
        ind.max<-min(which(x[i]<Breaks))
        xmin[i]<-Breaks[ind.min]
        xmax[i]<-Breaks[ind.max]
      }
    }
  }#for(i)

  PDmat<-cbind(xmax,xmin,x)
  colnames(PDmat)<-c("max","min","distance")
  PDmat

}#bin
