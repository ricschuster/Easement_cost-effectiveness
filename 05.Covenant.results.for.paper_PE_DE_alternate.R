library(ggplot2)
library(reshape)
library("grid")

setwd("D:\\R_files\\15_05_08_Ch3_PeerJ")

load("04.Covenant.analysis.all.runs.RM.const.low.2.RData")
run.sum.low<-run.sum.PE.DE.alter
load("04.Covenant.analysis.all.runs.RM.const.medium.2.RData")
run.sum.med<-run.sum.PE.DE.alter
load("04.Covenant.analysis.all.runs.RM.const.high.2.RData")
run.sum.high<-run.sum.PE.DE.alter

Cst.n.dsp <- data.frame(seq(run.sum.low$Init.cost[1],run.sum.low$Cost.no.disp[1],length.out=years))
names(Cst.n.dsp) <- "low_1"
for(ii in 2:years) {
  Cst.n.dsp <- cbind(Cst.n.dsp,seq(run.sum.low$Init.cost[ii],run.sum.low$Cost.no.disp[ii],length.out=years))
  names(Cst.n.dsp)[ii] <- sprintf("low_%s",ii)
}
for(ii in 1:years) {
  Cst.n.dsp <- cbind(Cst.n.dsp,seq(run.sum.med$Init.cost[ii],run.sum.med$Cost.no.disp[ii],length.out=years))
  names(Cst.n.dsp)[ii+100] <- sprintf("med_%s",ii)
}
for(ii in 1:years) {
  Cst.n.dsp <- cbind(Cst.n.dsp,seq(run.sum.high$Init.cost[ii],run.sum.high$Cost.no.disp[ii],length.out=years))
  names(Cst.n.dsp)[ii+200] <- sprintf("high_%s",ii)
}

Cst.n.dsp.mean <- apply(Cst.n.dsp,1,mean)
Cst.n.dsp.min <- apply(Cst.n.dsp,1,min)
Cst.n.dsp.max <- apply(Cst.n.dsp,1,max)

summary(Cst.n.dsp.mean)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 43900000  73320000 102800000 102800000 132200000 161600000

summary(Cst.n.dsp.min)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 42550000  71050000  99540000  99540000 128000000 156500000
 
summary(Cst.n.dsp.max)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 44990000  75190000 105400000 105400000 135600000 165800000

##### ----------------------------------------------------------------------
##### low dispute rate
##### ----------------------------------------------------------------------
disp.low <- data.frame(mn = as.vector(apply(run.sum.low[1:100,5:(4+years)],2,mean)),
                       low = as.vector(apply(run.sum.low[1:100,5:(4+years)],2,min)),
                       high = as.vector(apply(run.sum.low[1:100,5:(4+years)],2,max)))
summary(disp.low$mn)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 43900000  74020000 104100000 104100000 134300000 164300000
summary(disp.low$low)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 42550000  71850000 100900000 101000000 130500000 159400000
summary(disp.low$high)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 44990000  76440000 107400000 107800000 139300000 170500000
 
##### ----------------------------------------------------------------------
##### medium dispute rate
##### ----------------------------------------------------------------------
disp.med <- data.frame(mn = as.vector(apply(run.sum.med[1:100,5:(4+years)],2,mean)),
                       low = as.vector(apply(run.sum.med[1:100,5:(4+years)],2,min)),
                       high = as.vector(apply(run.sum.med[1:100,5:(4+years)],2,max)))
summary(disp.med$mn)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 43900000  82640000 121400000 121400000 160200000 198800000
summary(disp.med$low)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 42550000  79030000 115900000 116200000 153900000 191200000
summary(disp.med$high)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 44990000  88210000 126900000 127200000 167800000 208800000
 
##### ----------------------------------------------------------------------
##### high dispute rate
##### ----------------------------------------------------------------------
disp.high <- data.frame(mn = as.vector(apply(run.sum.high[1:100,5:(4+years)],2,mean)),
                       low = as.vector(apply(run.sum.high[1:100,5:(4+years)],2,min)),
                       high = as.vector(apply(run.sum.high[1:100,5:(4+years)],2,max)))
summary(disp.high$mn)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 43900000 169500000 294700000 294900000 420600000 546200000
summary(disp.high$low)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 42550000 160300000 276300000 280700000 403600000 524300000
summary(disp.high$high)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 44990000 179100000 310300000 310400000 442700000 570400000



##### ----------------------------------------------------------------------
##### biodiversity low dispute rate
##### ----------------------------------------------------------------------
biod.disp.low <- data.frame(mn = as.vector(apply(run.sum.low[1:100,(5+years):(4+2*years)],2,mean))/total.beta,
                       low = as.vector(apply(run.sum.low[1:100,(5+years):(4+2*years)],2,min))/total.beta,
                       high = as.vector(apply(run.sum.low[1:100,(5+years):(4+2*years)],2,max))/total.beta)
summary((1-biod.disp.low$mn/0.2)*100)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#0.0007211 0.2030000 0.3672000 0.3772000 0.5564000 0.7454000
summary((1-biod.disp.low$low/0.2)*100)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.006298 0.772900 0.969100 0.995100 1.221000 1.494000
summary((1-biod.disp.low$high/0.2)*100)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
#-8.600e-07  5.993e-03  3.539e-02  4.503e-02  8.636e-02  1.125e-01

##### ----------------------------------------------------------------------
##### biodiversity medium dispute rate
##### ----------------------------------------------------------------------
biod.disp.med <- data.frame(mn = as.vector(apply(run.sum.med[1:100,(5+years):(4+2*years)],2,mean))/total.beta,
                       low = as.vector(apply(run.sum.med[1:100,(5+years):(4+2*years)],2,min))/total.beta,
                       high = as.vector(apply(run.sum.med[1:100,(5+years):(4+2*years)],2,max))/total.beta)
summary((1-biod.disp.med$mn/0.2)*100)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.000721 1.908000 3.746000 3.738000 5.588000 7.313000
summary((1-biod.disp.med$low/0.2)*100)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.006298 3.465000 5.425000 5.267000 7.398000 9.249000
summary((1-biod.disp.med$high/0.2)*100)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#-0.000001  1.093000  1.975000  2.265000  3.518000  5.246000
 
##### ----------------------------------------------------------------------
##### biodiversity high dispute rate
##### ----------------------------------------------------------------------
biod.disp.high <- data.frame(mn = as.vector(apply(run.sum.high[1:100,(5+years):(4+2*years)],2,mean))/total.beta,
                       low = as.vector(apply(run.sum.high[1:100,(5+years):(4+2*years)],2,min))/total.beta,
                       high = as.vector(apply(run.sum.high[1:100,(5+years):(4+2*years)],2,max))/total.beta)
summary((1-biod.disp.high$mn/0.2)*100)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.00072 17.58000 31.93000 30.23000 43.95000 53.62000
summary((1-biod.disp.high$low/0.2)*100)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0063 19.8800 35.3100 33.2200 47.5500 57.7000
summary((1-biod.disp.high$high/0.2)*100)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   0.00   14.70   28.41   27.03   40.13   49.33
   
