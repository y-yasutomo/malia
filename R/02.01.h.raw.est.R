#02.01 h.row.est

h.raw.est<-function(raw.est,mdspec){
  cov.link<-mdspec$cov.link
  est.val<-raw.est$par
  est.val[!cov.link$cov]<-exp(raw.est$par[!cov.link$cov])

  hesse<-solve(raw.est$hessian)
  ##jaco
  jaco<-matrix(0,nrow(hesse),ncol(hesse))
  diag(jaco)<-ifelse(cov.link$cov==T,1,exp(raw.est$par))
  hesse<-jaco%*%hesse%*%t(jaco)
  est.var<-diag(hesse)
  ##AIC
  aic<-2*raw.est$value+2*length(raw.est$par)

  return(list(
    raw.est=raw.est,
    est.val=est.val,
    est.var=est.var,
    hesse=hesse,
    AIC=aic
  ))
}#h.row.est

