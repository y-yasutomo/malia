##03.02 grid.D.plot
#'Plot densities of grids
#'
#'@param grid.D.res grid.D object
#'@param xl ranges of longitude to be plotted
#'@param yl ranges of latitude to be plotted
#'@param Type debris type is also plotted in the figure
#'@param text.size size of Type
#'@param save defoult is set as false, if true, the figure will be saves in current directory
#'
#'@import ggplot2
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'
grid.D.plot<-function(grid.D.res,xl=c(120,180),yl=c(10,50),
                      Type=NULL,text.size=6,save=F){

  my.cols<-c("#7987FF" ,"#D118FF" ,"#FF0000")
  my.breaks<-c(0,25,50,75,100)
  my.labels<-c("0","25","50","75","100+")

    world.map2 <- map_data ("world")
    values2<-data.frame(id=factor(labels(table(world.map2$group))[[1]]),value=1:length(table(world.map2$group)))
    positions2<-data.frame(id=factor(world.map2$group),x=world.map2$long,y=world.map2$lat)
    posi<-c(0.02,0.98)
    clear<-rgb(0,0,0,0)

    grid.D.res$Density[grid.D.res$Density>100]<-100
    tmp.plot<-ggplot()+geom_rect(data=grid.D.res[!is.nan(grid.D.res$Density),],aes(xmin=lon.min,xmax=lon.max,ymin=lat.min,ymax=lat.max,fill=Density))+
      geom_map(data=values2,aes(map_id=id),map=positions2,fill="gray40")+
      xlim(xl)+ylim(yl)+coord_fixed(ratio=1)+
      scale_fill_gradientn(colours=my.cols,limits=c(0,100),breaks=my.breaks,labels=my.labels)+
      guides(fill=guide_legend(title=expression(Density(ind/km^2)),reverse=T))+
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
        legend.box.spacing=unit(0.1,"cm"))

    if(!is.null(Type)){
      x.<-xl[2]-(xl[2]-xl[1])*0.2
      y.<-yl[1]+(yl[2]-yl[1])*0.2
      tmp.plot<-tmp.plot+geom_text(data=data.frame(x=x.,y=y.,m=Type),aes(x=x,y=y,label=m),size=text.size)
    }
    pinf<-ggplot_build(tmp.plot)
    tmp.plot<-malia.label(tmp.plot)

    print(tmp.plot)
    if(save){
      ppi<-200
      png("grid.png",width=6*ppi,height=4*ppi,res=ppi)
      print(tmp.plot)
      dev.off()
    }

}#grid.D.plot
