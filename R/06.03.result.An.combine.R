#06.03 result.combine
#'Combine multiple survey results
#'@description Artificial
#'
#'@param vname character vector of Voyage name
#'@param Ar character vector of artificial product
#'@param Nt character vector of natural product
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
 vanc<-function(vname,Ar=c("FGN","FGF","FGO","EPS","PBA","PBO","FP","PC","G","M","W","UO"),
               Nt=c("SW","DW","NO")){
  if(F){
    vname<-c("S18y1","O18y1")
  }

   Ar.leg.D<-Nt.leg.D<-data.frame()

   for(i in 1:length(vname)){

   res<-readRDS(paste(vname[i],".result.obj",sep = ""))

   ##model summary
   aic.mat<-aic.summary(res)
   best.model<-model.extract(aic.mat,res)
   ##leg table
   leg.table<-leg.D.table(best.model$best.list)
   ##compaund artificial and natural products
   An.table<-ls.(leg.table,vname[i],Ar,Nt)
   Artificial.leg<-An.table[,c("Leg.No.","Leg.Length","Lat.Start",
                               "Lon.Start","Lat.End",
                               "Lon.End","Artificial_Density")]
   colnames(Artificial.leg)[ncol(Artificial.leg)]<-"Density"
   Artificial.leg<-cbind(vname=vname[i],Artificial.leg)
   Ar.leg.D<-rbind(Ar.leg.D,Artificial.leg)

   Natural.leg<-An.table[,c("Leg.No.","Leg.Length","Lat.Start",
                            "Lon.Start","Lat.End",
                            "Lon.End","Natural_Density")]
   colnames(Natural.leg)[ncol(Natural.leg)]<-"Density"
   Natural.leg<-cbind(vname=vname[i],Natural.leg)
   Nt.leg.D<-rbind(Nt.leg.D,Natural.leg)
   }
   return(list(Ar.leg.D=Ar.leg.D,Nt.leg.D=Nt.leg.D))
 }#vanc


