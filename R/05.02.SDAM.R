#05.02 Single Data Analysis by MALIA
#'Estimate detection function and calculate density
#'@description Sequence analysis will be conducted
#'
#'@param Voyage.name name of voyage
#'@param Sight.Data Sighting data of debris
#'@param Effort.Data Effort data of survey
#'@param COVARIATE character vector of covariate
#'@param key.list character vector of detection function to be used
#'@param td truncated distance
#'@param cp cut point
#'@param area
#'@param save
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
SDAM<-function(Voyage.name,Sight.Data,Effort.Data,COVARIATE=c("conv","occo","weather","size"),key.list=c("hn","hr","hhn","hhr"),
               td=200,cp=10,area=T,control=list(para.scale=0.05,rescale=10^(-3),
                                                ignore=0.01),inits=NULL,save=T){
  ###
  if(F){
    Voyage.name<-"SE18y3"
    key.list<-c("hn","hr","hhn","hhr")
    COVARIATE<-c("conv","occo","weather","size")
    td=200
    cp=10
    control=list(para.scale=0.05,rescale=10^(-3),
                 ignore=0.01)
    inits=NULL
    save=T
  }#IF(F)
  ###
  Voyage.result<-vector("list",length(key.list))
  if(1!=length(key.list))pb<-txtProgressBar(min=1,max=length(key.list),style=3)
  system.time({
    for(i in 1:length(key.list)){
      key<-key.list[i]
      RES.<-vector("list",length(COVARIATE))
      for(j in 1:length(COVARIATE)){
        colist<-COVARIATE[j]
        if(colist=="conv")colist<-NULL
        RES.[[j]]<-TCDM(Voyage.name=Voyage.name,
                        Sight.Data=Sight.Data,
                        Effort.Data=Effort.Data,
                        type=NULL,key,td,cp=cp,colist=colist,inits=inits,area=area,
                        para.scale=control$para.scale,rescale=control$rescale,ignore=control$ignore)
      }#for(j)
      names(RES.)<-COVARIATE
      Voyage.result[[i]]<-RES.
      if(1!=length(key.list))setTxtProgressBar(pb,i)
    }#for(i)
    names(Voyage.result)<-key.list
  })
  if(save){
    obj.list<-list.files(getwd(),"result.obj")
    ind<-paste(Voyage.name,".result.obj",sep="")%in%obj.list
    if(!ind){
      saveRDS(Voyage.result,paste(Voyage.name,".result.obj",sep=""))
      cat("\n",paste(Voyage.name,".result.obj",sep="")," is created.")
    }else{
      saveRDS(Voyage.result,paste(Voyage.name,".result2.obj",sep=""))
      warning("Since ",paste(Voyage.name,".result.obj",sep=""),
              " already exists, \n",paste(Voyage.name,".result2.obj",sep=""),
              " is created.",immediate.=T)
    }
  }#IF(SAVE)
  invisible(Voyage.result)
}#SDAM



