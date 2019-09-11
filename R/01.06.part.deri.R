#partial derivative
part.deri<-function(para,target,h,func,...){
  upper.para<-lower.para<-para
  upper.para[target]<-para[target]+h
  lower.para[target]<-para[target]-h
  #(func(upper.para,gx=gx,td=td,c.p=c.p)-func(lower.para,gx=gx,td=td,c.p=c.p))/(2*accuracy)
  (func(upper.para,...)-func(lower.para,...))/(2*h)
}
