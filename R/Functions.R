inits.searcher3<-function(fp,LB.para,UB.para,init.para,nit=50,maxit=100,...){
   n.para<-length(LB.para)

  para.mat<-matrix(0,nit,n.para)
  for(i in 1:n.para){
    para.mat[,i]<-seq(LB.para[i],UB.para[i],length.out=nit)
  }
  use.pind<-rep(1,n.para)
  #pi.list<-1:n.para
  #use.p<-sapply(pi.list,function(ipind)para.mat[use.pind[ipind],ipind])
  use.p<-init.para
  #pre.value<-fp(use.p,rpar=rpar,BYY=0,EYY=nrow(sim.data$B),sim.data=sim.data)
  pre.value<-fp(use.p,...)
  cat(paste("Start from ",pre.value,"\n",sep=""))
  for(i.tit in 1:maxit){
    for(i.para in 1:n.para){
      #i.para<-1
      tmp.value1<-numeric(nit)
      for(i.it in 1:nit){
        #i.it<-1
        use.p[i.para]<-para.mat[i.it,i.para]
        #tmp.value1[i.it]<-fp(use.p,rpar=rpar,BYY=0,EYY=nrow(sim.data$B),sim.data=sim.data)
        tmp.value1[i.it]<-fp(use.p,...)
      }#for(i.it)
      use.p[i.para]<-head(para.mat[order(tmp.value1),i.para],1)
    }#for(i.para)
    #neo.value<-fp(use.p,rpar=rpar,BYY=0,EYY=nrow(sim.data$B),sim.data=sim.data)
    neo.value<-fp(use.p,...)
    if(neo.value==pre.value){
      cat(paste("ite ",i.tit," value = ",neo.value,"\n","generated initial value!\n",sep=""))
      break
    }else{
      cat(paste("ite ",i.tit," value = ",neo.value,"\n",sep=""))
      pre.value<-neo.value
    }#if
  }#for(i.tit)
  return(use.p)
}#init.searcher3

#pl : para loader
pl<-function(para){
  para.names<-names(para)
  for(p.name in para.names)assign(p.name,eval(parse(text=paste("para$",p.name,sep=""))),env=parent.frame())
}#pl

integral<-function(f,lower,upper,...,devide=length(seq(lower,upper,0.00001))){
  f<-match.fun(f)
  fn<-function(x)f(x,...)
  n<-2*devide+1
  h<-(upper-lower)/n
  xs<-seq(lower,upper,length.out=n)
  (h/3)*(fn(xs[1])+2*sum(fn(xs[seq(3,n-2,2)]))+4*sum(fn(xs[seq(2,n,2)]))+fn(xs[n]))
}

#calculate leg length that is included in some grid
eff.grid<-function(s.lon,s.lat,e.lon,e.lat,g.lon,g.lat,as.median=T){
  meru.res<-meru(s.lon,s.lat,e.lon,e.lat,g.lon,g.lat)
  glon<-meru.res$c.theta
  glat<-meru.res$c.phai
  res.lat<-matrix(c(g.lon,glat),ncol=2)
  res.lon<-matrix(c(glon,g.lat),ncol=2)
  result<-as.data.frame(rbind(res.lat,res.lon,c(s.lon,s.lat),c(e.lon,e.lat)))
  names(result)<-c("lon","lat")
  result<-result[order(result$lon),]
  result<-result[min(c(s.lon,e.lon))<=result$lon&result$lon<=max(c(s.lon,e.lon))&min(c(s.lat,e.lat))<=result$lat&result$lat<=max(c(s.lat,e.lat)),]
  M<-matrix(0,ncol=2,nrow=length(result$lon))
  init<-rep(0,length(result$lon)-1)
  if(as.median){
    effort.res<-data.frame(lon=init,lat=init,effort=init)
    for(i in 1:(length(result$lon)-1)){
      effort.res$lon[i]<-(result$lon[i]+result$lon[i+1])/2
      effort.res$lat[i]<-(result$lat[i]+result$lat[i+1])/2
      effort.res$effort[i]<-hubeny(result$lon[i],result$lat[i],result$lon[i+1],result$lat[i+1],radian=F)
    }#for(i)
  }else{
    effort.res<-data.frame(lon.start=init,lat.start=init,lon.end=init,lat.end=init,effort=init)
    for(i in 1:(length(result$lon)-1)){
      effort.res$lon.start[i]<-result$lon[i]
      effort.res$lat.start[i]<-result$lat[i]
      effort.res$lon.end[i]<-result$lon[i+1]
      effort.res$lat.end[i]<-result$lat[i+1]
      effort.res$effort[i]<-hubeny(result$lon[i],result$lat[i],result$lon[i+1],result$lat[i+1],radian=F)
    }#for(i)
  }
  #list(cross.point=result,effort=effort.res)
  effort.res
}#eff.grid


meru<-function(theta1,phai1,theta2,phai2,Theta,Phai){
  Min.theta<-90
  theta1<-theta1-Min.theta
  theta2<-theta2-Min.theta
  Theta<-Theta-Min.theta
  Theta<-pi*Theta/180
  Phai<-pi*Phai/180
  theta1<-pi*theta1/180
  theta2<-pi*theta2/180
  phai1<-pi*phai1/180
  phai2<-pi*phai2/180
  a<-6378137
  f<-1/298.257222101
  b<-a*(1-f)

  r<-function(phai)sqrt(a^2*cos(phai)^2+b^2*sin(phai)^2)
  Fx<-function(theta,phai)r(phai)*cos(phai)*cos(theta)
  Fy<-function(theta,phai)r(phai)*cos(phai)*sin(theta)
  Fz<-function(phai)r(phai)*sin(phai)
  r1<-sqrt(a^2*cos(phai1)^2+b^2*sin(phai1)^2)
  r2<-sqrt(a^2*cos(phai2)^2+b^2*sin(phai2)^2)
  x1<-r1*cos(phai1)*cos(theta1)
  y1<-r1*cos(phai1)*sin(theta1)
  z1<-r1*sin(phai1)
  x2<-r2*cos(phai2)*cos(theta2)
  y2<-r2*cos(phai2)*sin(theta2)
  z2<-r2*sin(phai2)
  alpha<-(y1/x1)*(z1/y1-(x1*z2-x2*z1)/(x1*y2-x2*y1))
  beta<-(x1*z2-x2*z1)/(x1*y2-x2*y1)
  A<-function(y)(alpha^2+b^2/a^2)^(-1)*alpha*beta*y
  B<-function(y)(alpha^2+b^2/a^2)^(-1)*(-b^2+(beta^2+b^2/a^2)*y^2)
  C<-function(x)(beta^2+b^2/a^2)^(-1)*alpha*beta*x
  D<-function(x)(beta^2+b^2/a^2)^(-1)*(-b^2+(alpha^2+b^2/a^2)*x^2)
  E<-function(z)(beta^2/alpha^2+1)^(-1)*beta/alpha^2*z
  F<-function(z)(beta^2/alpha^2+1)^(-1)*((1/alpha^2+a^2/b^2)*z^2-a^2)
  fy<-function(y)sqrt(A(y)^2-B(y))-A(y)
  gx<-function(x)sqrt(C(x)^2-D(x))-C(x)
  hz<-function(z)sqrt(E(z)^2-F(z))+E(z)
  x12<-seq(x1,x2,length.out=1000)
  x<-x12
  y<-gx(x)
  z<-alpha*x+beta*y
  phai<-asin(sqrt(-sqrt(z^2/(b^2-a^2)+(1/4)*(a^2/(b^2-a^2))^2)-(1/2)*(a^2/(b^2-a^2))))
  theta<-acos(x/(sqrt((a*cos(phai))^2+(b*sin(phai))^2)*cos(phai)))

  Z<-r(Phai)*sin(Phai)
  Y<-hz(Z)
  X<-Z/alpha-hz(Z)*beta/alpha

  res.theta<-acos(X/(r(Phai)*cos(Phai)))+(X/(r(Phai)*cos(Phai))>pi)*2*X/(r(Phai)*cos(Phai))
  tes<-function(phai,theta)Fz(phai)-alpha*Fx(theta,phai)-beta*Fy(theta,phai)
  res.phai<-numeric(length(Theta))
  res.phai<-asin((alpha*cos(Theta)+beta*sin(Theta))/sqrt(1+(alpha*cos(Theta)+beta*sin(Theta))^2))
  list(theta=theta*180/pi+Min.theta,phai=phai*180/pi,c.theta=res.theta*180/pi+Min.theta,c.phai=res.phai*180/pi)
}

#hubeny
hubeny<-function(lon0,lat0,lon1,lat1,radian=F,meter=F){
  if(!radian){
    lat0<-pi*lat0/180
    lon0<-pi*lon0/180
    lat1<-pi*lat1/180
    lon1<-pi*lon1/180
  }
  a<-6378137
  b<-6356752.314245
  dlat<-lat1-lat0
  dlon<-lon1-lon0
  myulat<-(lat1+lat0)/2
  e<-sqrt((a^2-b^2)/a^2)
  W<-sqrt(1-e^2*(sin(myulat)^2))
  N<-a/W
  M<-a*(1-e^2)/W^3
  d<-sqrt((dlat*M)^2+(dlon*N*cos(myulat))^2)
  if(meter){d}
  else{d/1000}
}
