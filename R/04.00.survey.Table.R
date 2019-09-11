##04.00.survey.inf
#'Return survey information
#'@description Return debris and voyage information as a list type object
#'
#'@param Sight.Data .debris data
#'@param Effort.Data .effort data
#'@param Artificial character vector of artificial product
#'@param Natural character vector of natural product
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
survey.inf<-function(Sight.Data,Effort.Data,
                     Arttificail=c("FGN","FGF","FGO","EPS","PBA","PBO","FP","PC","G","M","W","UO"),
                     Natural=c("SW","DW","NO")){

  #Artificial<-c("FGN","FGF","FGO","EPS","PBA","PBO","FP","PC","G","M","W","UO")
  #Natural<-c("SW","DW","NO")

  leg.number<-length(Effort.Data$leg.number)
  leg.length<-sum(Effort.Data$leg.length)

  n.debris<-Sight.Data$number
  n.debris[n.debris>=20]<-20
  sight.debris<-sum(n.debris,na.rm = T)

  voyage.inf<- data.frame(Number_of_legs=leg.number,Total_leg_length=leg.length,
             Number_of_debris=sight.debris)

  #Each debris
  tmp.df<-data.frame(n=n.debris,type=Sight.Data$type)
  summary.df<-data.frame(matrix(NA,length(levels(Sight.Data$type))+2,1,
                               dimnames=list(c(levels(Sight.Data$type),"Artificial","Natural"),
                               c("Number"))))
  for(i in 1:length(levels(Sight.Data$type))){
    summary.df[i,1]<-sum(tmp.df[tmp.df$type==levels(Sight.Data$type)[i],]$n,na.rm=T)
  }#for(i)

  summary.df["Artificial",]<-sum(tmp.df[tmp.df$type%in%Artificial,]$n,na.rm=T)
  summary.df["Natural",]<-sum(tmp.df[tmp.df$type%in%Natural,]$n,na.rm=T)

  return(list(voyage.inf=voyage.inf,debris.inf=summary.df))


}#survey.inf
