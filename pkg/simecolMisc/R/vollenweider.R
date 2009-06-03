###################################################
#Vollenweider-Diagramm in der Orthophosphat-Version
#


vollenweider <- function(L, z, t, ...) {
  vollw_op <- function (z,t,v,P) {
  	P*v + P*z/t
  }
  v<-c(10,20,20,20)
  P<-c(0.005, 0.01, 0.03, 0.05)
  t1<-1
  tt<-1:9
  zz<-c(0.1*tt,tt,10*tt,100*tt)
  par(mar=c(5,5,4,2)+0.1)
  par(las=1)
  plot(zz,vollw_op(zz,t1,v[4],P[4]),type="n",log="xy",
       xlim=c(0.1,1000),ylim=c(0.01,50),axes=FALSE,
       #xlab=expression(frac(bar(z),bar(t))~(m~a^{-1})),
       xlab=expression(bar(z)/bar(t)~(m~a^{-1})),
       ylab=expression(L[c]~(g~m^{-2}~a^{-1})),
       #main=expression(L[c]==P[c,spring]~v[s,p]+P[c,spring]~frac(bar(z),bar(t))),
       cex.main=1.6,
       cex.lab =1.2)
  lines(zz,vollw_op(zz,t1,v[1],P[1]))
  lines(zz,vollw_op(zz,t1,v[2],P[2]))
  lines(zz,vollw_op(zz,t1,v[3],P[3]))
  lines(zz,vollw_op(zz,t1,v[4],P[4]))
  tt<-1:9
  axis(1,c(0.1*tt,tt,10*tt,100*tt,1000),labels=FALSE)
  axis(2,c(0.01*tt,0.1*tt,tt,50),labels=FALSE)
  axis(1,c(0.1,1,10,100,1000),labels=c(0.1,1,10,100,1000),cex.axis=1.2)
  axis(2,c(0.01,0.1,1,10,50),labels=c(0.01,0.1,1,10,50),cex.axis=1.2)
  box()
  text(0.3,3,"hypertroph",cex=1.2)
  text(0.3,0.8,"polytroph",cex=1.2)
  text(0.3,0.4,"eutroph",cex=1.2)
  text(0.3,0.1,"mesotroph",cex=1.2)
  text(0.3,0.02,"oligotroph",cex=1.2)
  points(z/t, L, ...)
}

#vollenweider(10, 10, 1)
