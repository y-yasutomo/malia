#'@import ggplot2
#'
legend.position<-function(posi){
  theme(plot.title=element_text(hjust=0.5),
        panel.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="gray80"),
        panel.grid.minor=element_line(colour="gray90"),
        legend.position=posi,legend.margin=margin(0.5,0.5,0.5,0.5),
        legend.background=element_rect(fill="#F7FAF7F5",size=0.5),
        legend.justification =c("left","top"),
        legend.key=element_rect(fill=rgb(0,0,0,0),colour=rgb(0,0,0,0))
  )
}#legend.position


#'@import ggplot2
#'
malia.label<-function(tmp.plot){
  pinf<-ggplot_build(tmp.plot)

  #old ver of ggplot2?
  #x.tmp<-pinf$layout$panel_ranges[[1]]$x.major_source
  #y.tmp<-pinf$layout$panel_ranges[[1]]$y.major_source

  x.tmp<-pinf$layout$panel_params[[1]]$x.major_source
  y.tmp<-pinf$layout$panel_params[[1]]$y.major_source

  tmp.plot+scale_x_continuous(limits=range(x.tmp),breaks=x.tmp,
                              labels=ifelse(x.tmp<180,parse(text=paste(x.tmp,"*degree*E",sep="")),
                                            ifelse(x.tmp==180,parse(text=paste(180,"*degree",sep="")),
                                                   parse(text=paste(360-x.tmp,"*degree*W",sep="")))))+
    scale_y_continuous(limits=range(y.tmp),breaks=y.tmp,
                       labels=ifelse(y.tmp<0,parse(text=paste(-1.0*y.tmp,"*degree*S",sep="")),
                                     ifelse(y.tmp==0,parse(text=paste(0,"*degree",sep="")),
                                            parse(text=paste(y.tmp,"*degree*N",sep="")))))
}
