HeartFunction<-function(Heart){
  
  plot(Heart[[3]],Heart[[2]],xlab="Cholesterol", ylab="Resting Blood Pressure", main ="Relation between Chol and RestBP" )
  lines(smooth.spline(Heart[[3]],Heart[[2]]),main="SS with the default cross validation")
  lines(smooth.spline(Heart[[3]],Heart[[2]],df=2),col=2, main="SS with 2 degrees of freedom")
  
  o.Heart.Chol<-order(Heart[[3]])
  Io<-(Heart[[3]][o.Heart.Chol])<239
  vec.lowChol<-(Heart[[3]][o.Heart.Chol])[Io]
  vec.lowChol.BP<-(Heart[[2]][o.Heart.Chol])[Io]
  dflo=NROW(vec.lowChol)-2
  
  Ih<-(Heart[[3]])>=239
  vec.highChol<-(Heart[[3]][Ih])
  vec.highChol.BP<-(Heart[[2]][Ih])
  dfhi=NROW(vec.highChol)-2
  
  
  ls.strlo<-lsfit(vec.lowChol,vec.lowChol.BP)
  ls.strhi<-lsfit(vec.highChol,vec.highChol.BP)
  ls.strfull<-lsfit(Heart[[3]],Heart[[2]])
  
  residfull<-ls.strfull$residuals
  residlo<-ls.strlo$resid
  residhi<-ls.strhi$resid
  ssfull<-sum(residfull^2) 
  sslo<-sum(residlo^2)
  sshi<-sum(residhi^2)
  
  SSN<-ssfull
  SSF<-sslo+sshi
  
  dffull = nrow(Heart)-2
  dffnest = dflo+dfhi
  
  F<-((SSN-SSF)/(dffnest-dffull))/(SSF/dffull)
  print("F test = ")
  print(F)
  
  Pvalue<-pf(F,2,dffull)
  
  if(Pvalue<=0.95){
    print("P value < 0.95. Do not reject the null hypothesis that there is no need for nested model.")
  }else{
    print("P value > 0.95. Reject the null hypothesis that there is no need for nested model.")
  }
  ##par(mfrow=c(2,2))
  ##qqnorm(residfull, main="QQnorm for the residuals of full model")
  ##qqnorm(residlo, main="QQnorm for the residuals of nested model for low Chol")
  ##qqnorm(residhi, main="QQnorm for the residuals of nested model for high Chol")
  
  smsp.strcv<-smooth.spline(Heart[[3]],Heart[[2]])
  smspcv.resid<-Heart[[2]]-approx(smsp.strcv$x,smsp.strcv$y,Heart[[3]])$y
  smsp.str2<-smooth.spline(Heart[[3]],Heart[[2]],df=2)
  smsp2.resid<-Heart[[2]]-approx(smsp.str2$x,smsp.str2$y,Heart[[3]])$y
  par(mfrow=c(2,1))
  qqnorm(smspcv.resid)
  qqnorm(smsp2.resid)
  
  }

##Please remeber to change the route of the Heart.csv file.
Heart<-read.csv("/Users/AliceLan/Desktop/R Class/Class 1/HW1/Heart.csv")
HeartFunction(Heart)