#04.01.leg.D.table
#' Create table of leg densities
#'@description Summarize densities of legs in each debris type into the table
#'
#'@param best.list
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'

leg.D.table<-function(best.list){
  if(F){
    best.list<-best.model$best.list
  }

  cNAME<-c("Leg.No.","Leg.Length","Density","Abundance","Detected.Number")

  leg.table<-data.frame()
  tmp<-best.list[[1]]
  tmp2<-tmp$leg.D.obs$leg.result[,cNAME]
  colnames(tmp2)[c(-1,-2)]<-paste(names(best.list)[1],colnames(tmp2)[c(-1,-2)],sep="_")

  tmp2<-cbind(data.frame(Voyage_name=tmp$Voyage.inf$Voyage.name),tmp2)
  last<-tmp$leg.D.obs$leg.result[,c("Lat.Start","Lon.Start","Lat.End","Lon.End")]

  for(i in 2:length(best.list)){
    tmp<-best.list[[i]]
    if(is.null(tmp)){
      next
    }else{
      tmp3<-tmp$leg.D.obs$leg.result[,cNAME[c(-1,-2)]]
      colnames(tmp3)<-paste(names(best.list)[i],colnames(tmp3),sep="_")
      tmp2<-cbind(tmp2,tmp3)
    }
  }#for(i)
  leg.table<-cbind(tmp2,last)
  leg.table
}#leg.D.table
