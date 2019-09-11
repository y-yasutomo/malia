#04.05.combine
#'Combine natural and artificial individuals into same category
#'@description density ...
#'
#'@param leg.table
#'@param Voyage.name
#'@param Ar character vector of artificial product
#'@param Nt character vector of natural product
#'
#'@author Tomoki Yasuhara
#'@export
#'
#'@examples
#'

ls.<-function(leg.table,Voyage.name,Ar=c("FGN","FGF","FGO","EPS","PBA","PBO","FP","PC","G","M","W","UO"),
              Nt=c("SW","DW","NO")){
  if(F){
    head(leg.table)
    Ar<-c("FGN","FGF","FGO","EPS","PBA","PBO","FP","PC","G","M","W","UO")
    Nt<-c("SW","DW","NO")
  }
  ###
  d.colmn<-grep("Density",names(leg.table))
  n.colmn<-grep("Detected.Number",names(leg.table))

  d.<-leg.table[,d.colmn]
  n.<-leg.table[,n.colmn]

  NAME<-sapply(strsplit(names(d.),split="_"),"[")[1,]
  n.NAME<-sapply(strsplit(names(n.),split="_"),"[")[1,]

  D.Ar.ind<-which(NAME%in%Ar)
  D.Nt.ind<-which(NAME%in%Nt)

  D.Ar.tmp<-as.numeric(apply(d.[,D.Ar.ind],1,sum))
  D.Nt.tmp<-as.numeric(apply(d.[,D.Nt.ind],1,sum))

  n.Ar.ind<-which(n.NAME%in%Ar)
  n.Nt.ind<-which(n.NAME%in%Nt)

  n.Ar<-as.numeric(apply(n.[,n.Ar.ind],1,sum))
  n.Nt<-as.numeric(apply(n.[,n.Nt.ind],1,sum))
  tmp<-cbind(n.Ar,n.Nt)
  sp<-apply(tmp,1,sum) #total number of observations(including natural and artificial products)
  r.mat<-matrix(NA,nrow(tmp),ncol(tmp))
  for(i in 1:nrow(r.mat)){
    r.mat[i,]<-tmp[i,]/sp[i] #proportion of each category
  }#for(i)

  #allocate unknown objects into Ar and Nt
  D.uk.ind<-which(!NAME%in%c(Ar,Nt))

  if(0!=length(D.uk.ind)){
  sp.mat<-matrix(NA,nrow(tmp),ncol(tmp))
  for(i in 1:nrow(sp.mat)){
    sp.mat[i,]<-ifelse(is.nan(r.mat[i,]*d.[,D.uk.ind][i]),0,r.mat[i,]*d.[,D.uk.ind][i])
  }#for(i)
  }

  D.Ar<-D.Ar.tmp+ifelse(0==length(D.uk.ind),0,sp.mat[,1])
  D.Nt<-D.Nt.tmp+ifelse(0==length(D.uk.ind),0,sp.mat[,2])

  ttmp<-cbind(n.Ar,D.Ar,n.Nt,D.Nt)
  colnames(ttmp)<-c("Artificial_Number","Artificial_Density","Natural_Number",
                    "Natural_Density")
  cbind(leg.table[,c("Leg.No.","Leg.Length","Lat.Start",
                       "Lon.Start","Lat.End","Lon.End","Voyage_name")],ttmp)
}#ls.
