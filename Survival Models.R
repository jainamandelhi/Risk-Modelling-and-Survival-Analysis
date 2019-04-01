#Calculate mu
calmu=function(x)
{
  t=(x-70)/50
  e=exp(b0+b1*t+b2*(2*t*t-1))
  mu=a0+a1*t+e
  return (mu)
}

a0=0.00005887
a1=-0.0004988
b0=-4.363378
b1=5.544956
b2=-0.620345
mu=calmu(80)

#display mu
mu

#plot
plot(calmu,20,110,log="y",xlab="Age (x)",ylab="mu(x)",main="Mortality
     rates for AM92 (log scale)")  #use log="xy" for log scale on both axes

#qx for different age

x=seq(20,110,by=10)
x

#function to calculate qx
calqx=function(x)
{
  return (1-exp(-calmu(x+0.5)))
}

#rounding the list of ages to 6 decimal places
qx=round(sapply(x,calqx),6)

ans=cbind(x,qx)
ans

#calculate curtate expectaction of life
#lets calculate summation(tpx) where t goes from 1 to 119-x

calcurt=function(x)
{
  ans=0
  z=1
  for(i in 1:(119-x))
  {
    y=1-calqx(x+i-1)
    z=z*y
    ans=ans+z
  }
  return(ans)
}

#example
calcurt(25)