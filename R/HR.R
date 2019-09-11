#'Hazard rate detection function
#'
#'@param x vector of perpendicular distance
#'@param para parameter vector of detection function (sigma,b)
#'
#'@export
#'@examples
#'HR(0,c(20,2))
#'@author Hiroaki Murata

HR<-function(x,para,...){
  theta<-para[1]
  b<-para[2]+1
  1-exp(-((x/theta)^(-b)))
}
