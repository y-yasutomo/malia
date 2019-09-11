
##01.02.Model.spec

Model.spec<-function(Sight.Data,key,colist,td,cp){

  if(key%in%c("hn","HN")){
    gx<-HN
    NLL<-NLL.HN
    n.para<-1
  }

  if(key%in%c("hr","HR")){
    gx<-HR
    NLL<-NLL.HR
    n.para<-2
  }

  if(key%in%c("hhn","HHN")){
    gx<-HHN
    NLL<-NLL.HHN
    n.para<-2
  }

  if(key%in%c("hhr","HHR")){
    gx<-HHR
    NLL<-NLL.HHR
    n.para<-3
  }
  coind<-1
if(!is.null(colist)){
  cov<-Sight.Data[,colist,drop=F]
  variables<-sapply(cov,levels)
  fac.var<-sapply(variables,length)
  nume.var<-sum(fac.var==0)
  n.cov.para<-sum(fac.var,nume.var)
  #######
  if(nume.var==n.cov.para){
    n.cov.para<-1+n.cov.para
  }
}

  #cov.link
  cov.<-rep(F,n.para)
  if(!is.null(colist)){
  cov.<-append(cov.,rep(T,n.cov.para),after=coind)
  cov.<-cov.[-coind]
  }
  #neo.ind
  neo.ind<-1:length(cov.)
  #paleo.ind
  paleo.ind<-1:n.para
  if(!is.null(colist))paleo.ind<-append(paleo.ind,rep(coind,n.cov.para-1),after=coind)
  cov.link<-data.frame(cov=cov.,neo.ind,paleo.ind)
  #colist
  if(!is.null(colist)){
  colist.ind<-which(names(Sight.Data)%in%colist)
  }else{
  colist.ind<-NULL
  }

  return(list(
    colist=colist,
    cov.link=cov.link,
    paleo.gx=gx,
    n.para=n.para,
    NLL=NLL,
    td=td,
    key=key,
    cp=cp
  ))
  }#model.spec

#Model.spec(Sight.Data,key,colist,td)
