# Calculating Kaplan-Meier survival function
calkme=function(dj,nj)
{
  prod=1
  ans=c()
  for(i in 1:length(dj))
  {
    prod=prod*(1-dj[i]/nj[i])
    ans=append(ans,prod)
  }
  return(ans)
}
dj=c(2,2,1,3,1,0,5,2,1)
nj=c(100,98,96,90,87,86,76,71,67) 

ans?calkme(dj,nj)

ans

#Times
tj=c(4,5,8,10,11,15,20,22,24) 

#Plot
plot(c(0,tj),c(1,ans),type="s",xlim=c(0,25),ylim=c(0.5,1),
     main="Kaplan-Meier estimator of S(t)",
     xlab="Time t",ylab="Survival probability") #s is used for step function

#Nelson-Aa?en estimate
calna=function(dj,nj)
{
  prod=0
  ans=c()
  for(i in 1:length(dj))
  {
    prod=prod+(dj[i]/nj[i])
    ans=append(ans,prod)
  }
  return(ans)
}
ans=calna(dj,nj)
ans

#Nelson-Aalen s.d.
calnasd=function(dj,nj)
{
  prod=0
  ans=c()
  for(i in 1:?ength(dj))
  {
    prod=prod+(dj[i]*(nj[i]-dj[i])/nj[i]^3)
    ans=append(ans,sqrt(prod))
  }
  return(ans)
}
ans=calnasd(dj,nj)
ans

#Plot
plot(c(0,tj),c(0,calna(dj,nj)),xlim=c(0,25),ylim=c(0,0.35),type="s",
     main="Nelson-Aalen estimator of the integr?ted hazard",
     xlab="Time t",ylab="Integrated hazard")
lines(tj,calna(dj,nj)-1.96*calnasd(dj,nj),type="s",lty=2)
lines(tj,calna(dj,nj)+1.96*calnasd(dj,nj),type="s",lty=2) 

#Nelson-Aalen Survival Function
sna=exp(-calna(dj,nj))

#Inequality verification?#skm<=sna
skm = calkme(dj,nj)
sna = exp(-calna(dj,nj))

ineq=matrix(c(tj,round(skm,4),round(sna,4)),ncol=3)
colnames(ineq)=c("t","SKM","SNA")
ineq

#Alternative approach for inequality verification
skm<=sna