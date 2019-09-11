#leg.D
leg.D<-function(est,x,z,mdspec,Comp.Data,Lack.Data,Effort.Data,rescale=1){
  td<-mdspec$td
  cp<-mdspec$cp
  n.data<-nrow(x)
  neo.para<-est$est.val
  cmu.list<-numeric(n.data)

  if(!is.null(mdspec$colist)){
  z.list<-apply(z,1,paste,collapse=".")
  z.table<-table(z.list)
  z.names<-names(z.table)
  z.npat<-length(z.names)

  #z.ind<-numeric(n.data)
  for(i.z in 1:z.npat){
    tmp.char<-z.names[i.z]
    tmp.ind<-which(z.list==tmp.char)
    #z.ind[tmp.ind]<-i.z
    #cmu.list[tmp.ind]<-mu(neo.para,z[tmp.ind[1],],mdspec,td,...)
    cmu.list[tmp.ind]<-mu(neo.para,z[tmp.ind[1],],mdspec,td,10^(-3),c.p=10)
  }#for(i.z)
  lz<-Lack.z.maker(Lack.Data,mdspec)
  n.ldata<-nrow(Lack.Data)
  lz.list<-apply(lz,1,paste,collapse=".")
  lz.table<-table(lz.list)
  lz.names<-names(lz.table)
  lz.npat<-length(lz.names)
  lmu.list<-numeric(n.ldata)
  for(i.z in 1:lz.npat){
    tmp.char<-lz.names[i.z]
    tmp.ind<-which(lz.list==tmp.char)
    tmp.lz<-lz[tmp.ind[1],]
    match.ind<-p.match(z,tmp.lz,which(is.na(tmp.lz)))
    tmp.mu<-mean(cmu.list[match.ind])
    lmu.list[tmp.ind]<-tmp.mu
  }#for(i.z)
  }else{
    n.ldata<-nrow(Lack.Data)
    lmu.list<-numeric(n.ldata)
    cmu.list<-rep(mu(neo.para,z,mdspec,td,10^(-3),c.p=10),n.data)
    lmu.list<-rep(cmu.list[1],n.ldata)
  }
  n.leg<-nrow(Effort.Data)
  leg_colnames<-c("Leg.No.","Density","Abundance","Detected.Number","Leg.Length","Lat.Start","Lon.Start","Lat.End","Lon.End")
  leg.D.res<-matrix(0,n.leg,9,dimnames=list(NULL,leg_colnames))
  for(i in 1:n.leg){
    tmp.eff<-Effort.Data[i,]
    #tmp.leg<-leg_list[i]
    comp.ind<-which(Comp.Data$leg.number==tmp.eff$leg.number)
    lack.ind<-which(Lack.Data$leg.number==tmp.eff$leg.number)
    tmp.mu.list<-c(cmu.list[comp.ind],lmu.list[lack.ind])
    tmp.N<-sum(1/tmp.mu.list)
    tmp.D<-tmp.N/tmp.eff$leg.length
    leg.D.res[i,1]<-tmp.eff$leg.number
    leg.D.res[i,2]<-tmp.D
    leg.D.res[i,3]<-tmp.N
    leg.D.res[i,4]<-length(tmp.mu.list)
    leg.D.res[i,5]<-tmp.eff$leg.length
    leg.D.res[i,6]<-tmp.eff$lat.start
    leg.D.res[i,7]<-tmp.eff$lon.start
    leg.D.res[i,8]<-tmp.eff$lat.end
    leg.D.res[i,9]<-tmp.eff$lon.end
  }#for(i)

  leg.D.res<-data.frame(leg.D.res)
  mu.list<-c(cmu.list,lmu.list)
  L<-sum(leg.D.res$Leg.Length)
  D.est<-sum(leg.D.res$Abundance)/(L*(td*rescale))

  ###########
  ##Variance
  ##unit:meter
  encount.var<-encount(leg.D.res)

  hesse<-est$hesse
  tmp.dev<-matrix(NA,length(cmu.list),length(neo.para))
  if(!is.null(mdspec$colist)){
  for(i.z in 1:z.npat){
    tmp.char<-z.names[i.z]
    tmp.ind<-which(z.list==tmp.char)
    #z.ind[tmp.ind]<-i.z
    #cmu.list[tmp.ind]<-mu(neo.para,z[tmp.ind[1],],mdspec,td,...)
    tmp<-numeric(length(neo.para))
    for(i.target in 1:length(neo.para)){
      tmp[i.target]<-part.deri(para=neo.para,target=i.target,
                               h=10^(-10),mu2,z=z[i.z,],mdspec=mdspec,
                               td=td,c.p=cp)
    }#for(i.target)
    tmp.dev[tmp.ind,]<-tmp
  }#for(i.z)
  }else{
      tmp<-numeric(length(neo.para))
      for(i.target in 1:length(neo.para)){
        tmp[i.target]<-part.deri(para=neo.para,target=i.target,
                                 h=10^(-10),mu2,z=z,mdspec=mdspec,
                                 td=td,c.p=cp)
      }#for(i.target)
    for(i in 1:nrow(tmp.dev))tmp.dev[i,]<-tmp
  }#else
  dd<-t(apply(tmp.dev,2,sum))
  mu.var<-dd%*%hesse%*%t(dd)
  mu.var<-mu.var*rescale^2
  tmp.var<-mu.var+encount.var
  D.var<-tmp.var/(L*(td*rescale))^2
  CV.D<-sqrt(D.var)/(sum(leg.D.res$Abundance)/(L*(td*rescale)))

  ##Var N in covered region
  if(F){
    ss<-sum((1-mu.list)/(mu.list)^2)
    Var.N.in.c<-ss+mu.var
    CV.N.in.c<-sqrt(Var.N.in.c)/sum(leg.D.res$Abundance)
  }

  return(list(
    leg.result=leg.D.res,
    all.result=data.frame(matrix(c(D.est,D.var,CV.D),1,
                                 dimnames=list(c(),c("Density","Variance","CV")))),
    mu.list=mu.list
  ))
}#leg.D


#p.match
p.match<-function(origin,patt,ignore){
  ###
  if(F){
    origin<-z
    patt<-c(1,1,0,0,0,NA)
    ignore<-which(is.na(tmp.lz))
  }
  ###
  ignore<-c(ignore,length(patt))
  u_ori<-origin[,-ignore,drop=F]
  u_pat<-patt[-ignore]
  u_pat_list<-paste(u_pat,collapse=".")
  u_ori_list<-apply(u_ori,1,paste,collapse=".")
  which(u_ori_list==u_pat_list)
}

##Lack.z.maker
Lack.z.maker<-function(Lack.Data,mdspec){
  if(F){
    Lack.Data<-h.Sight.Data$Lack.Data
    Lack.Data$weather[c(1,9)]<-NA
    Lack.Data
  }
  cov.link<-mdspec$cov.link
  #########
  colist<-mdspec$colist
  cov<-Lack.Data[,colist,drop=F]
  tmp<-nume.check<-numeric()
  for(i in 1:ncol(cov)){
    nume.check[i]<-is.numeric(cov[,i])
    if(is.numeric(cov[,i])){
      tmp[i]<-1
    }else{
      tmp[i]<-length(levels(cov[,i]))
    }
  }#for(i)
  n.para<-sum(tmp)
  if(sum(nume.check)==n.para)n.para<-1+n.para
  zmat<-matrix(NA,nrow(Lack.Data),n.para)
  #zmat<-matrix(NA,nrow(Lack.Data),nrow(cov.link[cov.link$cov,]))
  ###########
  for(k in 1:nrow(Lack.Data)){
    cov<-Lack.Data[k,colist,drop=F]
    reg<-1
    e1<-0
    for(i in 1:length(colist)){
      if(is.numeric(cov[,i])){
        tmp<-cov[,i]
      }else{
        if(e1==0){
          tmp<-numeric(length(levels(cov[,i])))
          e2<-as.numeric(cov[,i])
          if(is.na(e2)){
            tmp<-rep(NA,length(levels(cov[,i])))
          }else{
            tmp[as.numeric(cov[,i])]<-1
          }
          tmp<-tmp[-1]
          e1<-1
        }else{
          tmp<-numeric(length(levels(cov[,i])))
          e2<-as.numeric(cov[,i])
          if(is.na(e2)){
            tmp<-rep(NA,length(levels(cov[,i])))
          }else{
            tmp[e2]<-1
          }
        }#IF(e1==0)
      }#IF(is.numeric(cov[,i]))
      reg<-c(reg,tmp)
    }#for(i)
    zmat[k,]<-reg
  }#for(k)
  zmat
}#Lack.z.maker

###
encount<-function(leg.D.res){
  L<-sum(leg.D.res$Leg.Length)
  K<-nrow(leg.D.res)
  Nk<-leg.D.res$Abundance
  Nc<-sum(Nk)
  lk<-leg.D.res$Leg.Length
  encount<-sum((lk*(((Nk/lk)-(Nc/L))^2))/(K-1))
  encount*L
}#encount

mu2<-function(neo.para,z,mdspec,td,...){
  (td)/(integrate(neo.gx,0,td,z,neo.para,mdspec,rel.tol=10^(-5),...)$value)
}#mu2


