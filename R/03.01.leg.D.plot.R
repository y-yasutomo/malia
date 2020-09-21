##03.01.leg.D.plot
#'Plot densities of legs
#'
#'@param leg.D.res leg.D object
#'@param xl ranges of longitude to be plotted
#'@param yl ranges of latitude to be plotted
#'@param Type debris type is also plotted in the figure
#'@param myfill color of continent
#'@param text.size size of Type
#'@param save defoult is set as false, if true, the figure will be saves in current directory
#'
#'@import ggplot2
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'

leg.D.plot<-function(leg.D.res,xl=c(120,180),yl=c(10,50),Type=NULL,myfill="gray40",
                     text.size=6,save=F){

  world.map2 <- ggplot2::map_data ("world")
  values2<-data.frame(id=factor(labels(table(world.map2$group))[[1]]),value=1:length(table(world.map2$group)))
  positions2<-data.frame(id=factor(world.map2$group),x=world.map2$long,y=world.map2$lat)
  posi<-c(0.02,0.98)
  clear<-rgb(0,0,0,0)

  tmp.plot<-ggplot()+geom_map(data=values2,aes(map_id=id),map=positions2,fill=myfill)+
    xlim(xl)+ylim(yl)+coord_fixed(ratio=1)+labs(x="",y="")
  pinf<-ggplot_build(tmp.plot)
  tmp.plot<-malia.label(tmp.plot)

  x.<-xl[2]-(xl[2]-xl[1])*0.2
  y.<-yl[1]+(yl[2]-yl[1])*0.2
  leg.D.res$lat<-apply(leg.D.res[,c("Lat.Start","Lat.End")],1,mean)
  leg.D.res$lon<-apply(leg.D.res[,c("Lon.Start","Lon.End")],1,mean)

  my.breaks<-c(0,25,50,75,100)
  my.labels<-c("0","25","50","75","100+")
  leg.D.res$Density[leg.D.res$Density>100]<-100
  tmp.plot<-tmp.plot+geom_point(data=leg.D.res,aes(x=lon,y=lat,size=Density),shape=1,colour="#FF8282FF")+
    scale_size_continuous(limits = c(0,100),range=c(1,5),breaks=my.breaks,labels=my.labels,guide=guide_legend(reverse=T),
                          name=expression(Density(ind/km^2)))
  if(!is.null(Type)) tmp.plot<-tmp.plot+geom_text(data=data.frame(x=x.+1,y=y.,m=Type),aes(x=x,y=y,label=m),size=text.size)

  tmp.plot<-tmp.plot+
    theme(
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
      legend.title=element_text(size=10),
      legend.position=posi,
      #legend.margin=margin(3,5,0.1,0.1),
      legend.box.spacing=unit(0.1,"cm")
    )
  print(tmp.plot)
  if(save){
    ppi<-200
    png("leg.png",width=6*ppi,height=4*ppi,res=ppi)
    print(tmp.plot)
    dev.off()
  }
}#leg.D.plot
