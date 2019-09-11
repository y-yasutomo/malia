#77.77 MArine LItter Analysis
#'Estimate detection function and calculate density
#'@description Sequence analysis will be conducted
#'
#'@param Sight.Data
#'@param Effort.Data
#'@param key
#'@param td
#'@param cp
#'@param colist
#'@param area
#'
#'@author Hiroaki Murata and Tomoki Yasuhara
#'@export
#'
#'@examples
#'
 MALIA<-function(Sight.Data,Effort.Data,key,td,cp=10,colist=NULL,area=T,
                  inits=NULL,para.scale=0.05,rescale=10^(-3),ignore=0.01){
  if(F){
   Sight.Data<-Sight.
   Effort.Data<-Effort.Data
   key<-key
   td<-td
   cp<-cp
   colist<-colist
   inits<-NULL
   para.scale<-para.scale
   rescale<-rescale
   ignore<-ignore
  }
   h.Sight.Data<-sight.hand(Sight.Data,colist,td)
   Comp.Data<-h.Sight.Data$Comp.Data
   Lack.Data<-h.Sight.Data$Lack.Data
   mdspec<-Model.spec(Sight.Data=Comp.Data,key=key,colist=colist,td=td,cp=cp)

   if(dim(Comp.Data)[1]==0){
     xz.obs<-NA
     est<-leg.D.obs<-grid.D.res<-area.D.obs<-df.info<-NA
     cat(paste("Fitting",key,"key function for",Sight.Data$type[1],"\n",
               "was skipped because of no complete sighting.",sep=" "))

   }else{
     xz.obs<-xz.maker(mdspec,Comp.Data)
     x<-xz.obs$xmat
     x<-cbind(x[,2],x[,1])
     z<-xz.obs$zmat

   raw.est<-try(run.optim.c(inits=inits,mdspec,x,z,para.scale=para.scale),silent=T)
   if(class(raw.est)!="try-error"){
   if(0%in%diag(raw.est$hessian)){
     est<-leg.D.obs<-grid.D.res<-area.D.obs<-df.info<-NA
   }else{
     tmp<-try(solve(raw.est$hessian),silent=T)
     if(class(tmp)=="try-error"){
       est<-leg.D.obs<-grid.D.res<-area.D.obs<-df.info<-NA
     }else{
     est<-h.raw.est(raw.est,mdspec)
     leg.D.obs<-leg.D(est,x,z,mdspec,Comp.Data,Lack.Data,Effort.Data,rescale=rescale)
     leg.D.res<-leg.D.obs$leg.result
     grid.D.res<-grid.D(leg.D.res,ignore=ignore)
     if(area){
       area.D.obs<-area.D(leg.D.res)
     }else{
       area.D.obs<-NULL
     }
    }
     #area.D.res<-area.D.obs$area.density
     #df.info<-df.inf(est,Comp.Data,mdspec)
   }
   }else{
     est<-leg.D.obs<-grid.D.res<-area.D.obs<-df.info<-NA
   }
   }
   #Sight.Data$pa<-leg.D.obs$mu.list

   return(list(
     xz.obs=xz.obs,
     est=est,
     leg.D.obs=leg.D.obs,
     grid.D.res=grid.D.res,
     area.D.obs=area.D.obs,
     mdspec=mdspec,
     Sight.Data=Sight.Data
   ))
 }#MALIA

 if(F){
 Sight.<-Sight.Data[Sight.Data$type%in%c("PB"),]
 res<-MALIA(Sight.,Effort.Data,"hhr",td)
 res$xz.obs
 res$est
 res$leg.D.obs$leg.result
 res$leg.D.obs$all.result
 res$grid.D.res
 res$df.inf
 res$mdspec
 }


