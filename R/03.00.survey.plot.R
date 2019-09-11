#03.00.Survey plot
#'Plot survey track line
#'
#'@param Effort.Data
#'@param xl ranges of longitude to be plotted
#'@param yl ranges of latitude to be plotted
#'@param col color of trackline
#'@param myfill color of continent
#'@param multi if True, multiple surveys will be plotted with legend
#'
#'@import ggplot2
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'

survey.plot<-function(Effort.Data,xl=c(120,180),yl=c(10,50),col="orange",myfill="gray40",
                       multi=F){


  world.map2 <- map_data("world")
  values2<-data.frame(id=factor(labels(table(world.map2$group))[[1]]),value=1:length(table(world.map2$group)))
  positions2<-data.frame(id=factor(world.map2$group),x=world.map2$long,y=world.map2$lat)
  #posi<-c(0.02,0.98)
  clear<-rgb(0,0,0,0)
  tmp.plot<-ggplot()+geom_map(data=values2,aes(map_id=id),map=positions2,fill=myfill)+
    xlim(xl)+ylim(yl)+coord_fixed(ratio=1)+labs(x="",y="")
  tmp.plot<-malia.label(tmp.plot)+theme(
  plot.title=element_text(hjust=0.5),
  panel.background=element_rect(fill="white"),
  panel.grid.major=element_line(colour="gray80"),
  panel.grid.minor=element_line(colour="gray90"),
  legend.background=element_rect(fill="#F7FAF7F5",size=0.5),
  legend.justification =c("left","top"),
  legend.key.size=unit(0.3,"cm"),
  legend.key=element_rect(fill=clear,colour=clear),
  legend.title.align=0.5,
  legend.text.align=0,
  legend.text=element_text(size=8),
  legend.title=element_text(size=10)
)

  Effort.Data$lon<-apply(Effort.Data[,c("lon.end","lon.start")],1,mean)
  Effort.Data$lat<-apply(Effort.Data[,c("lat.end","lat.start")],1,mean)

  if(multi){

    tmp.plot<-tmp.plot+geom_point(data=Effort.Data,aes(x=lon,y=lat,col=voyage_name),
                                  size=2,shape=1)+
      theme(legend.justification = c(1,0),
            legend.position = c(.98,.2),
            legend.key.size=unit(0.3,"cm"),
            legend.key=element_rect(fill=clear,colour=clear),
            legend.title.align=0.5,
            legend.text.align=0,
            legend.text=element_text(size=10),
            legend.title=element_text(size=12)
      )
  }else{
    tmp.plot<-tmp.plot+geom_point(data=Effort.Data,aes(x=lon,y=lat,col=voyage_name),
                                  size=2,col=col,shape=1)
  }
  print(tmp.plot)
}#survey.plot
