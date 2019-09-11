#06.01 aic.summary
#'Sumamrize aic in each model
#'@description Return ...
#'
#'@param result.obj SDAM object
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
aic.summary<-function(result.obj){
  if(F){
    result.obj<-res
  }
  n.type<-length(result.obj[[1]][[1]])
  n.key<-length(result.obj)
  n.cov<-length(result.obj[[1]])
  summary.list<-vector("list",n.type)
  summary.mat<-data.frame(matrix(NA,n.key*n.cov,
                                 3,dimnames=list(c(),c("Key","Covariate","AIC"))))
  for(i in 1:n.type){
    #i<-1
    tmp.mat<-summary.mat
    for(j in 1:n.key){
      #j<-1
      tmp<-result.obj[[j]]
      for(k in 1:n.cov){
        #k<-1
        tmp2<-tmp[[k]][[i]]
        if(length(tmp2$est)==1){
          next
        }else{
          tmp.mat[j+n.key*(k-1),1]<-names(result.obj)[j]
          tmp.mat[j+n.key*(k-1),2]<-names(tmp)[k]
          tmp.mat[j+n.key*(k-1),3]<-tmp2$est$AIC
        }
      }#for(k)
    }#for(j)
    summary.list[[i]]<-tmp.mat
  }#for(i)
  names(summary.list)<-names(result.obj[[1]][[1]])

  summary.list

}#aic.summary

#res.mat<-aic.summary(result.obj)
#'Return survey information
#'@description Return debris and voyage information as a list type object
#'
#'@param aic.mat .debris data
#'@param result.obj .effort data
#'@param Order Order can designate ranking of model which we have an interest
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
model.extract<-function(aic.mat,result.obj,Order=rep(1,length(aic.mat))){
  ###
  if(F){
    aic.mat<-res.mat
  }#IF(F)
  ###
  best.list<-vector("list",length(aic.mat))
  best.mat<-data.frame(matrix(NA,length(best.list),3,
                              dimnames=list(names(aic.mat),c("Key","Covariate","AIC"))))

  for(i in 1:length(aic.mat)){
    tmp<-aic.mat[[i]]
    target<-order(tmp$AIC)[Order[i]]
    key.type<-tmp[target,1]
    cov.type<-tmp[target,2]
    best.list[[i]]<-result.obj[[key.type]][[cov.type]][[i]]
    best.mat[i,1]<-key.type
    best.mat[i,2]<-cov.type
    best.mat[i,3]<-tmp[target,3]
  }#for(i)
  names(best.list)<-names(aic.mat)
  list(best.list=best.list,best.mat=best.mat)
}#model.extract

#best.<-model.extract(aic.mat,result.obj)
#best.$best.mat
