
#####01.01.xz.maker
xz.maker<-function(mdspec,Sight.Data){

  cov.link<-mdspec$cov.link
  colist<-mdspec$colist
  td<-mdspec$td

  ###xmat
  #x<-Sight.Data[,c(1,2)] #c(max,min)
  x<-Sight.Data[,c("max","min")] #c(max,min)

  xmat<-data.matrix(x[x[,1]<=td,])

  ###zmat
  ########
  if(!is.null(colist)){
    cov<-Sight.Data[,colist,drop=F]
    tmp<-nume.check<-numeric()
    for(i in 1:ncol(cov)){
      nume.check[i]<-is.numeric(cov[,i])
      if(is.numeric(cov[,i])){
        tmp[i]<-1
      }else{
        tmp[i]<-length(levels(cov[,i]))
      }
    }#for(i)
    n.para<-sum(tmp)
    if(sum(nume.check)==n.para)n.para<-1+n.para
    #########
    zmat<-matrix(NA,nrow(Sight.Data),n.para)
    for(k in 1:nrow(Sight.Data)){
      cov<-Sight.Data[k,colist,drop=F]
      reg<-1
      e1<-0
      for(i in 1:length(colist)){
        if(is.numeric(cov[,i])){
          tmp<-cov[,i]
        }else{
          if(e1==0){
            tmp<-numeric(length(levels(cov[,i])))
            tmp[as.numeric(cov[,i])]<-1
            tmp<-tmp[-1]
            e1<-1
          }else{
            tmp<-numeric(length(levels(cov[,i])))
            tmp[as.numeric(cov[,i])]<-1
          }#IF(e1==0)
        }#IF(is.numeric(cov[,i]))
        reg<-c(reg,tmp)
      }#for(i)
      reg<-matrix(reg,1)
      zmat[k,]<-reg
    }#for(k)
  }else{
    zmat<-NULL
    #zmat<-matrix(1,nrow(xmat))
  }
  return(list(
    xmat=xmat,
    zmat=zmat
  ))
}#xz.maker


