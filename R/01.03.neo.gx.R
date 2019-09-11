
neo.gx<-function(x,z,neo.para,mdspec,...){
  n.data<-length(x)
  cov.link<-mdspec$cov.link
  paleo.gx<-mdspec$paleo.gx
  n.para<-mdspec$n.para
  paleo.para<-matrix(0,n.para,n.data)
  nocov.ind<-cov.link[!cov.link$cov,]
  cov.ind<-cov.link[cov.link$cov,]
  paleo.para[nocov.ind$paleo,]<-neo.para[nocov.ind$neo]
  if(nrow(cov.ind)!=0){
  paleo.para[cov.ind$paleo[1],]<-exp(z%*%neo.para[cov.ind$neo])
  }
  paleo.gx(x,paleo.para,...)
}#neo.gx

