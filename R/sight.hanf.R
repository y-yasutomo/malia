#sight hand

sight.hand<-function(Sight.Data,colist,td){
  check.list<-c("max","min",colist)
  check.mat<-matrix(0,nrow(Sight.Data),length(check.list))
  for(i in 1:2){
    check.mat[,i]<-is.na(Sight.Data[[check.list[i]]])|
      is.infinite(Sight.Data[[check.list[i]]])|
      (Sight.Data[[check.list[i]]]>td)
  }
  if(!is.null(colist)){
    for(i in 3:length(check.list)){
      check.mat[,i]<-is.na(Sight.Data[[check.list[i]]])
    }
  }
  #na.ind<-which(rowSums(check.mat)>0)
  na.ind<-c(which(rowSums(check.mat)>0),nrow(Sight.Data)+1)

  Comp.Data<-Sight.Data[-na.ind,]
  Lack.Data<-Sight.Data[na.ind,]
  list(Comp.Data=Comp.Data,Lack.Data=Lack.Data)
}#sight.hand
