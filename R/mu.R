#'Calculate effective strip half width
#'
#'
mu<-function(neo.para,z,mdspec,td,rescale=1,...){
  integrate(neo.gx,0,td,z,neo.para,mdspec,c.p=10)$value*rescale
}#mu
