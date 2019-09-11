#04.02.grid.D.table
#' Create table of grid densities
#'@description Summarize densities of grids in each debris type into the table
#'
#'@param best.list
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
grid.D.table<-function(best.list){

  cNAME<-c("lat.min","lat.max","lon.min","lon.max","Total.length")

  tmp<-best.list[[1]]
  grid.table<-tmp$grid.D.res[,cNAME]
  grid.table<-cbind(data.frame(Voyage_name=tmp$Voyage.inf$Voyage.name),grid.table)

  for(i in 1:length(best.list)){
    tmp<-best.list[[i]]
    if(is.null(tmp)){
      next
    }else{
      tmp.name<-names(best.list)[i]
      grid.d<-tmp$grid.D.res$Density
      grid.table<-cbind(grid.table,grid.d)
      colnames(grid.table)[ncol(grid.table)]<-paste(tmp.name,"Density",sep="_")
    }
  }#for(i)
  grid.table<-grid.table[!is.nan(grid.table[,ncol(grid.table)]),]
  grid.table
}#grid.table
