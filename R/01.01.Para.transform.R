
para.transform<-function(gx.args,x,z,neo.para){
  if(is.null(z)){
    matrix(rep(exp(neo.para),nrow(x)),nrow(x))
  }else{
    n.data<-nrow(z)
    n.para<-gx.args$n.para
    paleo.gx<-gx.args$paleo.gx
    cov.link<-gx.args$cov.link
    paleo.para<-matrix(0,n.para,n.data)
    nocov.ind<-cov.link[!cov.link$cov,]
    cov.ind<-cov.link[cov.link$cov,]
    #paleo.para[nocov.ind$paleo,]<-neo.para[nocov.ind$neo]
    paleo.para[nocov.ind$paleo,]<-exp(neo.para[nocov.ind$neo])
    if(nrow(cov.ind)==0){
      paleo.para[1,]<-exp(paleo.para[1,])
    }
    if(nrow(cov.ind)!=0)paleo.para[cov.ind$paleo[1],]<-exp(z%*%neo.para[cov.ind$neo])
    t(paleo.para)
  }


}#para.transform
