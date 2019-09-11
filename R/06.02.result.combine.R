#06.02 result.combine
#'Combine multiple survey results
#'@description Return ...
#'
#'@param vname character vector of Voyage name
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
 leg.D.comb<-function(vname){
   ###
   if(F){
     vname<-c("S18y1","O18y1")
   }#IF(F)
   ###
   aic.list<-best.list<-vector("list",length(vname))

 for(i in 1:length(vname)){
  #i<-1
  res<-readRDS(paste(vname[i],".result.obj",sep=""))
  ##model summary
  aic.list[[i]]<-aic.summary(res)
  best.list[[i]]<-model.extract(aic.list[[i]],res)
 }#for(i)
  #n.type<-max(sapply(best.list,function(x)length(x$best.list)))

  tmp.name<-c()
  for(i in 1:length(best.list)){
    tmp.name<-c(tmp.name,names(best.list[[i]]$best.list))
  }#for(i)

  all.name<-unique(tmp.name)
  com.leg.D<-vector("list",length(all.name))
  com.leg.D<-lapply(com.leg.D,function(x)x<-data.frame())
  for(i in 1:length(best.list)){
    tmp<-best.list[[i]]$best.list
    for(j in 1:length(all.name)){
      #j<-1
      tmp2<-tmp[[all.name[j]]]
      if(is.null(tmp2))next
      tmp.leg.D<-tmp2$leg.D.obs$leg.result
      tmp.df<-cbind(Voyage_name=tmp2$Voyage.inf$Voyage.name,tmp.leg.D)
      com.leg.D[[j]]<-rbind(com.leg.D[[j]],tmp.df)
    }#for(j)
  }#for(i)
  names(com.leg.D)<-all.name
  com.leg.D
 }#leg.D.comb
