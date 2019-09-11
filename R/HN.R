#'Half normal detection function
#'
#'@param x vector of perpendicular distance
#'@param para parameter of detection function (sigma)
#'
#'@export
#'@examples
#'HN(0,20)
#'@author Hiroaki Murata
#'
HN<-function(x,para,...){
  sig<-para
  exp(-x^2/(2*sig^2))
}#HN
