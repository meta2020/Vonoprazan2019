library(metafor)
#library(readxl)
bleed <- read.csv("/Users/yi/Documents/Martin-Meng/3bleeding.csv")

dat <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=week4)
res <- rma(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
regtest(res, model="rma")

dat2 <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=week8)
res2 <- rma(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat2)
regtest(res2, model="rma")

dat3 <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=bleed)
res3 <- rma(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat3)
regtest(res3, model="rma")

taf<-trimfill(res)
taf2<-trimfill(res2)
taf3<-trimfill(res3)
funnel(taf, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0)
funnel(taf2, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0)
funnel(taf3, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0)
