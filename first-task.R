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
