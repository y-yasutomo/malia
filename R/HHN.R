#'Half half normal detection function
#'
#'@param x vector of perpendicular distance
#'@param para parameter vector of detection function (sigma,delta)
#'@param c.p cut point
#'
#'@export
#'@examples
#'HHN(10,c(20,10),10)
#'@author Hiroaki Murata
HHN<-function(x,para,c.p){
  sig<-para[1]
  delta<-para[2]
  exp(-(x-c.p)^2/(2*delta^2))*(x<=c.p)+(exp(-(x-c.p)^2/(2*sig^2)))*(x>c.p)
}#HHN
