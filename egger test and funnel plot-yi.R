library(metafor)
#library(readxl)
shrink<- read.csv("/Users/yi/Documents/Martin-Meng/1shrinkage.csv")
scar <-  read.csv("/Users/yi/Documents/Martin-Meng/2scar.csv")
bleed <- read.csv("/Users/yi/Documents/Martin-Meng/3bleeding.csv")

dat <- escalc(measure="MD", m1i=vmean, m2i=mean, n1i=vn, sd1i=vsd, sd2i=sd, n2i=n, data=shrink)
res <- rma(yi, vi, data=dat, method="REML")
summary(res)
et1 <- regtest(res, model="rma")

taf<-trimfill(res)
funnel(res,pch=shrink$group)
legend(-10,0 ,c("H. pylori + ","H. pylori - "), pch=c(1,2))
text(25,2, labels=paste("Test for funnel plot asymmetry\n","Z = ",round(et1$zval,3),", p = ",round(et1$pval,3)))



dat2 <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=scar)
res2 <- rma(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat2)
et2 <- regtest(res2, model="rma")
taf2<-trimfill(res2)
funnel(res2, pch=scar$wek/4)
legend(-4,0 ,c("4 weeks","8 weeks"), pch=c(1,2))
text(3,0.3, labels=paste("Test for funnel plot asymmetry\n","Z = ",round(et2$zval,3),", p = ",round(et2$pval,3)))


dat3 <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=bleed)
res3 <- rma(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat3)
et3 <- regtest(res3, model="rma")
taf3<-trimfill(res3)
funnel(taf3, pch=1)
text(2,0.3, labels=paste("Test for funnel plot asymmetry\n","Z = ",round(et3$zval,3),", p = ",round(et3$pval,3)))

