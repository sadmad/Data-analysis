#install.packages("StatDA")
library(StatDA)
data(ohorizon)
ba <- ohorizon$Ba
n <- length(ba)
par(mfcol=c(2,2),mar=c(4,4,2,2))
edaplotlog(ba, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="Ba [mg/kg]",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=TRUE, map.col = 'rainbow')
edaplot(log10(ba),H.freq=F,box=T,S.pch=3,S.cex=0.5,D.lwd=1.5,P.ylab="Density",
        P.log=T,P.logfine=c(5,10),P.main="",P.xlab="Ba [mg/kg]",B.pch=3,B.cex=0.5)
plot(sort(ba),((1:n)-0.5)/n,pch=3,cex=0.8,
     main="",xlab="Ba [mg/kg]",ylab="Probability",cex.lab=1,cex.lab=1.4)
abline(h=seq(0,1,by=0.1),lty=3,col=gray(0.5))
abline(v=seq(0,1400,by=200),lty=3,col=gray(0.5))
plot(sort(log10(ba)),((1:n)-0.5)/n,pch=3,cex=0.8,
     main="",xlab="Ba [mg/kg]",ylab="Probability",cex.lab=1,xaxt="n",cex.lab=1.4)
axis(1,at=log10(alog<-sort(c((10^(-50:50))%*%t(c(5,10))))),labels=alog)
abline(h=seq(0,1,by=0.1),lty=3,col=gray(0.5))
abline(v=log10(alog),lty=3,col=gray(0.5))

str(ohorizon)
library(ggplot2)
df <- ohorizon <- ohorizon[sample(nrow(ohorizon), 1000, replace = T), ]
head(df)
qplot(HUMTHI,data=df,geom='histogram',binwidth=0.1,alpha=0.8)




#####Mapping Spatial Data
par(mfrow=c(1,1))
data(kola.background)
el=ohorizon[,"As"]
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=T)
bubbleFIN(X,Y,el,S=9,s=2,plottitle="",legendtitle="As [mg/kg]", text.cex=0.60,
          legtitle.cex=0.70,ndigits=2)
text(min(X)+diff(range(X))*5/7,max(Y),"Exponentially",cex=0.70)
text(min(X)+diff(range(X))*5/7,max(Y)-diff(range(Y))/25,"growing dots",cex=0.70)
scalebar(761309,7373050,861309,7363050,shifttext=-0.5,shiftkm=4e4,sizetext=0.8)
Northarrow(362602,7818750,362602,7878750,362602,7838750,Alength=0.15,Aangle=15,Alwd=1.3,Tcex=1.6)


#Surface Maps Constructed With Kriging

el=ohorizon[,"As"]
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=T)
SymbLegend(X,Y,el,type="percentile",qutiles<-c(0,0.05,0.25,0.75,0.95,1),symbtype="EDA",symbmagn=0.8,
leg.position="topright",leg.title="As [mg/kg]",leg.title.cex=0.8,leg.round=2,leg.wid=4,leg.just="rig")
text(min(X)+diff(range(X))*4/7,max(Y),paste(qutiles*100,collapse=","),cex=0.8)
text(min(X)+diff(range(X))*4/7,max(Y)-diff(range(Y))/25,"Percentiles",cex=0.8)
scalebar(761309,7373050,861309,7363050,shifttext=-0.5,shiftkm=37e3,sizetext=0.8)
Northarrow(362602,7818750,362602,7878750,362602,7838750,Alength=0.15,Aangle=15,Alwd=1.3,Tcex=1.6)




X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
el=log10(ohorizon[,"As"])
data(kola.background)
data(bordersKola)
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
SmoothLegend(X,Y,el,resol=200,type="contin",whichcol="gray",
             qutiles=c(0,0.05,0.25,0.50,0.75,0.90,0.95,1),borders="bordersKola",
             leg.xpos.min=7.8e5,leg.xpos.max=8.0e5,leg.ypos.min=77.6e5,leg.ypos.max=78.7e5,
             leg.title="mg/kg", leg.title.cex=0.7, leg.numb.cex=0.7, leg.round=2,leg.wid=4,
             leg.numb.xshift=0.7e5,leg.perc.xshift=0.4e5,leg.perc.yshift=0.2e5,tit.xshift=0.35e5)
plotbg(map.col=c("gray","gray","gray","gray"),map.lwd=c(1,1,1,1),add.plot=T)
text(min(X)+diff(range(X))*4/7,max(Y),"As",cex=1)
text(min(X)+diff(range(X))*4/7,max(Y)-diff(range(Y))/28,"in o-horizon",cex=0.8)
scalebar(761309,7373050,861309,7363050,shifttext=-0.5,shiftkm=37e3,sizetext=0.8)
Northarrow(362602,7818750,362602,7878750,362602,7838750,Alength=0.15,Aangle=15,Alwd=1.3,Tcex=1.6)


#install.packages("geoR")
library(geoR)
X=ohorizon[,"XCOO"]/1000
Y=ohorizon[,"YCOO"]/1000
el=ohorizon[,"As"]
vario.b <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300)
vario.c <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300, op="cloud")
vario.bc <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300, bin.cloud=TRUE)
vario.s <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300, op="sm", band=10)
vario.0 <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300, dir=0, tol=pi/8)
vario.90 <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300, dir=pi/4, tol=pi/8)
vario.45 <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300, dir=pi/8, tol=pi/8)
vario.120 <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300, dir=3*pi/8, tol=pi/8)
data(res.eyefit.As_C)
v5=variofit(vario.b,res.eyefit.As_C,cov.model="spherical",max.dist=300)
