##05.01 TCDM
#'Estimate detection function
#'@description Inner function of SDAM.
#'
#'@param Voyage.name
#'@param type
#'@param key
#'@param td
#'@param cp
#'@param colist
#'@param inits
#'@param area
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'

TCDM<-function(Voyage.name,type=NULL,key,td,cp=10,colist=NULL,inits=NULL,area=T,
               para.scale=0.05,rescale=10^(-3),ignore=0.01){
  if(F){
    Voyage.name
    type<-list("EPS")
    type=list("EPS","DW")
    type=list(c("EPS","DW"),c("DW"),c("EPS","PC"))
    type<-NULL
    key<-"hn"
    td<-200
    cp<-10
    colist<-"weather"
    para.scale<-0.05
    rescale<-10^(-3)
    ignore<-0.01
    Sight.Data<-read.csv(paste(Voyage.name,".debris.csv",sep=""))
    colnames(Sight.Data)
    Sight.Data<-Sight.Data[,c("max","leg.number","min","type","weather")]
    inits<-NULL
    Effort.Data<-read.csv(paste(Voyage.name,".effort.csv",sep=""))
    colnames(Effort.Data)
    Effort.Data<-Effort.Data[,c("lat.start","lon.start","leg.length","leg.number","lat.end","lon.end")]
    names(Effort.Data)[1]<-"Leg.number"
  }

  Sight.Data<-read.csv(paste(Voyage.name,".debris.csv",sep=""))
  Effort.Data<-read.csv(paste(Voyage.name,".effort.csv",sep=""))

  if(is.list(type)){
    Sight<-vector("list",length(type))
    for(i in 1:length(Sight)){
      Sight[[i]]<-Sight.Data[Sight.Data$type%in%type[[i]],]
      tmp<-sapply(type,paste,collapse="_")
      names(Sight)[i]<-tmp[i]
    }
  }else{
    Type<-levels(Sight.Data$type)
    Sight<-vector("list",length(Type))
    for(i in 1:length(Sight)){
      Sight[[i]]<-Sight.Data[Sight.Data$type==Type[i],]
    }
    names(Sight)<-Type
  }

  RESULT.obj<-vector("list",length(Sight))

  for(i in 1:length(Sight)){
    Sight.<-Sight[[i]]
    Sight.<-factor.arrange(Sight.,colist)
    cat(paste("Fitting",key,"key function for",names(Sight)[i],"\n",sep=" "))
    #res.<-try(MALIA(Sight.,Effort.Data,key,td,cp=cp,colist=colist,
    #            para.scale=para.scale,rescale=rescale,ignore=ignore),silent=T)
    res.<-MALIA(Sight.,Effort.Data,key,td,cp=cp,colist=colist,inits=inits,area=area,
          para.scale=para.scale,rescale=rescale,ignore=ignore)

    RESULT.obj[[i]]<-res.
    }#for(i)

   names(RESULT.obj)<-names(Sight)

  for(i in 1:length(Sight)){
   voyage.inf<-list(Voyage.name=Voyage.name,debris.type=names(Sight)[i])
   RESULT.obj[[i]]$Voyage.inf<-voyage.inf
   }
   RESULT.obj
}#TCDM

if(F){
 Voyage.name<-"s14y1"
 key<-"hn"
 td<-200

 res<-SDAM(Voyage.name,type=NULL,key,td,cp=10,colist=NULL,
              para.scale=0.5,rescale=10^(-3),ignore=0.01)
 res$DW$Voyage.inf
 #saveRDS(res,"Sample.malia.obj")
}

#
#factor arrange
 factor.arrange<-function(Sight.Data,colist){
  if(F){
    Sight.Data<-Sight.
  }
   if(is.null(colist)){
     return(Sight.Data)
   }else{
for(i in 1:length(colist)){
  tmp<-Sight.Data[,colist[i]]
  if(is.numeric(tmp))next
  Table<-table(tmp)
  if(sum(Table==0)>0){
    Ori.levels<-levels(tmp)
    rm.ind<-which(Table==0)
    Rev.levels<-Ori.levels[-rm.ind]
    Sight.Data[,colist[i]]<-factor(Sight.Data[,colist[i]],levels=Rev.levels)
  }else{
    next
  }

}#for(i)
   return(Sight.Data)
     }
}#factor.arrenge







