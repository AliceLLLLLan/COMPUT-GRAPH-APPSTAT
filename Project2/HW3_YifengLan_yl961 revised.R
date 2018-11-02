Heart<-read.csv("/Users/Glowie/Downloads/Heart.csv")

my.smooth.forKS<-function(x.str,xindex,yindex,ind.sqrt=T){
##par(mfrow=c(2,2))
##plot(x.str[[xindex]],x.str[[yindex]],main="Raw data and smooth")
##lines(smooth.spline(x.str[[xindex]],x.str[[yindex]]))
  if(ind.sqrt){
    smsp.strcv<-smooth.spline(x.str[[xindex]],x.str[[yindex]])
    smspcv.resid<-x.str[[yindex]]-approx(smsp.strcv$x,smsp.strcv$y,x.str[[xindex]])$y
  }
  else{
    smsp.strcv<-smooth.spline(x.str[[xindex]],(x.str[[yindex]]))
    smspcv.resid<-(x.str[[yindex]])-approx(smsp.strcv$x,smsp.strcv$y,x.str[[xindex]])$y
  }
  sd.resid<-sum(smspcv.resid^2)/(length(x.str[[1]]-smsp.strcv$df))
  stud.resid<-smspcv.resid/sd.resid
  D<-ks.test(stud.resid,pnorm)$statistic
  my.smooth<-approx(smsp.strcv$x,smsp.strcv$y,x.str[[xindex]])$y
  list(D=D,raw.resid=smspcv.resid,sd.resid=sd.resid,smooth=my.smooth)
}
my.smooth.forKS(Heart,3,2,ind.sqrt=T)

my.bootstrapCB<-function(mydata,x.index,y.index,nboot,confidence){
  par(mfrow=c(1,1))
  str0<-my.smooth.forKS(mydata,x.index,y.index)
  smooth.dist<-NULL
  base.smooth<-str0$smooth
  base.sd<-str0$sd.resid
  base.resid<-str0$raw.resid
  my.bootdata<-mydata
  n1<-length(base.smooth)
  for(i in 1:nboot){
    bres<-sample(base.resid,length(base.resid),replace=T)
    boot.dat<-((base.smooth+bres))
    #print(boot.dat)
    my.bootdata[[y.index]]<-boot.dat
    bstr0<-my.smooth.forKS(my.bootdata,x.index,y.index,F)
    boot.smooth<-bstr0$smooth
    smooth.dist<-rbind(smooth.dist,boot.smooth-base.smooth)
    
  }
  n1<-length(smooth.dist[1,])
  alpha<-1-confidence
  LB<-NULL
  UB<-NULL
  for(i in 1:n1){
    s1<-sort(smooth.dist[,i])
    n2<-length(s1)
    v1<-c(1:n2)/n2
    bvec<-approx(v1,s1,c(alpha/2,1-alpha/2))$y
    LB<-c(LB,base.smooth[i]-bvec[2])
    UB<-c(UB,base.smooth[i]-bvec[1])
  }
  plot(rep(mydata[[x.index]],4),c(LB,base.smooth,UB,mydata[[y.index]]),xlab="X",ylab="Y",type="n")
  points(mydata[[x.index]],mydata[[y.index]])
  o1<-order(mydata[[x.index]])
  lines(mydata[[x.index]][o1],LB[o1],col=2)
  lines(mydata[[x.index]][o1],UB[o1],col=2)
  lines(mydata[[x.index]][o1],base.smooth[o1],col=3)
  lines(smooth.spline(mydata[[x.index]],mydata[[y.index]],df=2))
}
my.bootstrapCB(Heart,3,2,1000,0.95)

