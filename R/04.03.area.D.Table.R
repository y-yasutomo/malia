#04.03.area.D.table
#' Create table of area density
#'@description Summarize densities of area in each debris type into the table
#'
#'@param best.list
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
area.D.table<-function(best.list){

  cNAME<-c("Area","Total.Length")

  tmp<-best.list[[1]]
  area.table<-tmp$area.D.obs$area.density[,cNAME]
  area.table<-cbind(data.frame(Voyage_name=tmp$Voyage.inf$Voyage.name),area.table)
  for(i in 1:length(best.list)){
    tmp<-best.list[[i]]
    if(is.null(tmp)){
      next
    }else{
      tmp.name<-names(best.list)[i]
      area.d<-tmp$area.D.obs$area.density$Density
      area.table<-cbind(area.table,area.d)
      colnames(area.table)[ncol(area.table)]<-paste(tmp.name,"Density",sep="_")
    }
  }#for(i)

  #area.table<-area.table[!is.nan(area.table[,ncol(area.table)]),]
  area.table
}#area.D.table
