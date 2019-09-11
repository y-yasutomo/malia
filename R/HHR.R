#'Half hazard rate detection function
#'
#'@param x vector of perpendicular distance
#'@param para parameter vector of detection function (sigma,b,delta)
#'@param c.p cut point
#'
#'@export
#'@examples
#'HHR(10,c(20,2,10),10)
#'@author Hiroaki Murata
HHR<-function(x,para,c.p){
  theta<-para[1]
  b<-para[2]+1
  delta<-para[3]
  exp(-(x-c.p)^2/(2*delta^2))*(x<=c.p)+(1-exp(-(abs(x-c.p)/theta)^(-b)))*(x>c.p)
}#HHR
