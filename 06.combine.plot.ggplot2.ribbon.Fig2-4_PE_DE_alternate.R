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

#mn <- mean(marx.acqu$sums[[2]]$Cost[1:100])
#std <- sd(marx.acqu$sums[[2]]$Cost[1:100])

#high <- mn + 2*std
#low <- mn - 2*std

#cost=rep(mn,years)
#lwr=rep(low,years)
#upr=rep(high,years)
#year <- seq(1,years,1)

#predframe <- data.frame(year,cost,lwr,upr)
#(p1 <- ggplot(predframe, aes(year, cost))+
#    geom_line()+
#    geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.3))


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
#plot(Cst.n.dsp.mean)
#lines(Cst.n.dsp.min)
#lines(Cst.n.dsp.max)

#geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha=0.3))

#----------------------------------------------------------------------------
#mean
tt <- data.frame( Acqu.cst=rep(mean(marx.acqu$sums[[2]]$Cost[1:years]),years),
                  Cst.n.dsp=Cst.n.dsp.mean,
                  y.cost.low=as.vector(apply(run.sum.low[1:100,5:(4+years)],2,mean)),
                  y.cost.med=as.vector(apply(run.sum.med[1:100,5:(4+years)],2,mean)),
                  y.cost.high=as.vector(apply(run.sum.high[1:100,5:(4+years)],2,mean)),
                  x= seq(1,years,1))
names(tt) <- c("Baseline: Land Purchase Cost",
               "Easement cost, no disputes",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

#----------------------------------------------------------------------------
#min
tt.min <- data.frame( Acqu.cst=rep(min(marx.acqu$sums[[2]]$Cost[1:years]),years),
                  Cst.n.dsp=Cst.n.dsp.min,
                  y.cost.low=as.vector(apply(run.sum.low[1:100,5:(4+years)],2,min)),
                  y.cost.med=as.vector(apply(run.sum.med[1:100,5:(4+years)],2,min)),
                  y.cost.high=as.vector(apply(run.sum.high[1:100,5:(4+years)],2,min)),
                  x= seq(1,years,1))
names(tt.min) <- c("Baseline: Land Purchase Cost",
               "Easement cost, no disputes",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

#----------------------------------------------------------------------------
#max
tt.max <- data.frame( Acqu.cst=rep(max(marx.acqu$sums[[2]]$Cost[1:years]),years),
                  Cst.n.dsp=Cst.n.dsp.max,
                  y.cost.low=as.vector(apply(run.sum.low[1:100,5:(4+years)],2,max)),
                  y.cost.med=as.vector(apply(run.sum.med[1:100,5:(4+years)],2,max)),
                  y.cost.high=as.vector(apply(run.sum.high[1:100,5:(4+years)],2,max)),
                  x= seq(1,years,1))
names(tt.max) <- c("Baseline: Land Purchase Cost",
               "Easement cost, no disputes",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

Acqu <- data.frame(x=tt.min$x, low=tt.min[,1], high=tt.max[,1])
no.dis <- data.frame(x=tt.min$x, low=tt.min[,2], high=tt.max[,2])
low <- data.frame(x=tt.min$x, low=tt.min[,3], high=tt.max[,3])
med <- data.frame(x=tt.min$x, low=tt.min[,4], high=tt.max[,4])
high <- data.frame(x=tt.min$x, low=tt.min[,5], high=tt.max[,5])


F2.melt <- melt(tt, id = "x")
F2.melt.min <- melt(tt.min, id = "x")
F2.melt.max <- melt(tt.max, id = "x")

F2.melt$low <- F2.melt.min$value
F2.melt$high <- F2.melt.max$value

ggplot(subset(F2.melt)) +
  geom_line(aes(x=x,y=value,color=variable,size=variable)) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="black",data=Acqu,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="brown",data=no.dis,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="green",data=low,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="blue",data=med,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="red",data=high,alpha=0.3) +
#  geom_ribbon(aes(x=x,ymin=lwr,ymax=upr),color=variable,size=variable,alpha=0.3) +
  scale_size_manual(values =c(rep(1.2,5))) +
  scale_color_manual(values=c("black",
                              "brown",
                              "green",
                              "blue",
                              "red")) +
  scale_fill_manual(values=c("black",
                              "brown",
                              "green",
                              "blue",
                              "red")) +

#  scale_color_manual(values=c(TLV,StC,SeqC,StSeqC,
#                              TLV,StC,SeqC,StSeqC)) +
#  ggtitle("Plant growth with\ndifferent treatments") +
#    theme(plot.title = element_text(lineheight=.8, face="bold"))
    xlab("Year") +
    ylab("Reserve Cost [Million $]") +
#    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    scale_y_continuous(breaks=seq(1e+08,5e+08,1e+08),
          labels=c("100","200","300","400","500")) +
    scale_x_continuous(breaks=seq(0,100,10),
          labels=seq(0,100,10))  +
    theme(legend.position=c(0.35, 0.7)) +
    theme(legend.title=element_blank()) + #remove legend title
    theme(legend.background = element_rect(color= "black", fill="white", size=.2, linetype="solid"),
          legend.key=element_blank(),
          legend.key.size = unit(1,"cm"),
          legend.text.align=0,
          legend.text = element_text(size= 16)) +

    theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
#      panel.border = element_blank(),
#      panel.border = element_rect(colour = "black"),
      panel.background = element_blank()) +
    theme(axis.text = element_text(colour='black', size=20),
          axis.title = element_text(size=20),
          axis.title.y=element_text(vjust=0.3),
          axis.title.x=element_text(vjust=0))
#    theme(legend.text = element_text(size = 14))
#theme(legend.text = element_text(colour = 'red', angle = 45, size = 10, hjust = 3, vjust = 3, face = 'bold'))

#ggsave("F2_PE_DE_alter.tiff")
#ggsave("F2_PE_DE_alter.png")
##############################################################################
##
##  Fig 3: Biodiversity loss
##
##############################################################################

#----------------------------------------------------------------------------
#mean
tt.b <- data.frame( Acqu.cst=rep(mean(run.sum.low$Init.Beta[1:years])/total.beta,years),
                  y.biodiv.low=as.vector(apply(run.sum.low[1:100,(5+years):(4+2*years)],2,mean))/total.beta,
                  y.biodiv.med=as.vector(apply(run.sum.med[1:100,(5+years):(4+2*years)],2,mean))/total.beta,
                  y.biodiv.high=as.vector(apply(run.sum.high[1:100,(5+years):(4+2*years)],2,mean))/total.beta,
                  x= seq(1,years,1))

names(tt.b) <- c("Baseline: Land Purchase Cost",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

#----------------------------------------------------------------------------
#min
tt.b.min <- data.frame( Acqu.cst=rep(min(run.sum.low$Init.Beta[1:years])/total.beta,years),
                  y.biodiv.low=as.vector(apply(run.sum.low[1:100,(5+years):(4+2*years)],2,min))/total.beta,
                  y.biodiv.med=as.vector(apply(run.sum.med[1:100,(5+years):(4+2*years)],2,min))/total.beta,
                  y.biodiv.high=as.vector(apply(run.sum.high[1:100,(5+years):(4+2*years)],2,min))/total.beta,
                  x= seq(1,years,1))

names(tt.b.min) <- c("Baseline: Land Purchase Cost",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

#----------------------------------------------------------------------------
#mmax
tt.b.max <- data.frame( Acqu.cst=rep(max(run.sum.low$Init.Beta[1:years])/total.beta,years),
                  y.biodiv.low=as.vector(apply(run.sum.low[1:100,(5+years):(4+2*years)],2,max))/total.beta,
                  y.biodiv.med=as.vector(apply(run.sum.med[1:100,(5+years):(4+2*years)],2,max))/total.beta,
                  y.biodiv.high=as.vector(apply(run.sum.high[1:100,(5+years):(4+2*years)],2,max))/total.beta,
                  x= seq(1,years,1))

names(tt.b.max) <- c("Baseline: Land Purchase Cost",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

Acqu.b <- data.frame(x=tt.b.min$x, low=tt.b.min[,1], high=tt.b.max[,1])
low.b <- data.frame(x=tt.b.min$x, low=tt.b.min[,2], high=tt.b.max[,2])
med.b <- data.frame(x=tt.b.min$x, low=tt.b.min[,3], high=tt.b.max[,3])
high.b <- data.frame(x=tt.b.min$x, low=tt.b.min[,4], high=tt.b.max[,4])


F3.melt <- melt(tt.b, id = "x")
F3.melt.min <- melt(tt.b.min, id = "x")
F3.melt.max <- melt(tt.b.max, id = "x")

F3.melt$low <- F3.melt.min$value
F3.melt$high <- F3.melt.max$value

ggplot(subset(F3.melt)) +
  geom_line(aes(x=x,y=value,color=variable,size=variable)) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="black",data=Acqu.b,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="green",data=low.b,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="blue",data=med.b,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="red",data=high.b,alpha=0.3) +
  scale_size_manual(values =c(rep(1.2,4))) +
  scale_color_manual(values=c("black",
                              "green",
                              "blue",
                              "red")) +

#  scale_color_manual(values=c(TLV,StC,SeqC,StSeqC,
#                              TLV,StC,SeqC,StSeqC)) +
#  ggtitle("Plant growth with\ndifferent treatments") +
#    theme(plot.title = element_text(lineheight=.8, face="bold"))
    xlab("Year") +
    ylab("Biodiversity protected [%]") +
#    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    scale_y_continuous(breaks=seq(0.1,0.2,0.02),
          labels=seq(0.1,0.2,0.02)*100) +
    scale_x_continuous(breaks=seq(0,100,10),
          labels=seq(0,100,10))  +
    theme(legend.position=c(0.35, 0.2)) +
    theme(legend.title=element_blank()) + #remove legend title
    theme(legend.background = element_rect(color= "black", fill="white", size=.2, linetype="solid"),
          legend.key=element_blank(),
          legend.key.size = unit(1,"cm"),
          legend.text.align=0,
          legend.text = element_text(size= 16)) +

    theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
#      panel.border = element_blank(),
#      panel.border = element_rect(colour = "black"),
      panel.background = element_blank()) +
    theme(axis.text = element_text(colour='black', size=20),
          axis.title = element_text(size=20),
          axis.title.y=element_text(vjust=0.3),
          axis.title.x=element_text(vjust=0))
#theme(legend.text = element_text(colour = 'red', angle = 45, size = 10, hjust = 3, vjust = 3, face = 'bold'))

#ggsave("F3_PE_DE_alter.tiff")
#ggsave("F3_PE_DE_alter.png")

##############################################################################
##
##  Fig 4: Cost effectiveness
##
##############################################################################

#----------------------------------------------------------------------------
#mean

temp.mean <- data.frame(tt.b[,1],dummy=tt.b[,1],tt.b[,2:length(tt.b[1,])])
names(temp.mean) <- c("Baseline: Land Purchase Cost",
               "Easement cost, no disputes",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

tt.cb <- temp.mean[,1:5]/tt[,1:5]
rel.max <- max(tt.cb[,1])
tt.cb <- tt.cb/rel.max
tt.cb$x <- temp.mean[,6]

#----------------------------------------------------------------------------
#min

temp.min <- data.frame(tt.b.min[,1],dummy=tt.b.min[,1],tt.b.min[,2:length(tt.b.min[1,])])
names(temp.min) <- c("Baseline: Land Purchase Cost",
               "Easement cost, no disputes",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

tt.cb.min <- temp.min[,1:5]/tt[,1:5]
tt.cb.min <- tt.cb.min/rel.max
tt.cb.min$x <- temp.min[,6]

#----------------------------------------------------------------------------
#max

temp.max <- data.frame(tt.b.max[,1],dummy=tt.b.max[,1],tt.b.max[,2:length(tt.b.max[1,])])
names(temp.max) <- c("Baseline: Land Purchase Cost",
               "Easement cost, no disputes",
               "Low dispute rate (0.028% of easements/year)",
               "Moderate dispute rate (0.28% of easements/year)",
               "High dispute rate (2.8% of easements/year)",
               "x")

tt.cb.max <- temp.max[,1:5]/tt[,1:5]
tt.cb.max <- tt.cb.max/rel.max
tt.cb.max$x <- temp.max[,6]


Acqu <- data.frame(x=tt.cb.min$x, low=tt.cb.min[,1], high=tt.cb.max[,1])
no.dis <- data.frame(x=tt.cb.min$x, low=tt.cb.min[,2], high=tt.cb.max[,2])
low <- data.frame(x=tt.cb.min$x, low=tt.cb.min[,3], high=tt.cb.max[,3])
med <- data.frame(x=tt.cb.min$x, low=tt.cb.min[,4], high=tt.cb.max[,4])
high <- data.frame(x=tt.cb.min$x, low=tt.cb.min[,5], high=tt.cb.max[,5])


F4.melt <- melt(tt.cb, id = "x")
F4.melt.min <- melt(tt.cb.min, id = "x")
F4.melt.max <- melt(tt.cb.max, id = "x")

F4.melt$low <- F4.melt.min$value
F4.melt$high <- F4.melt.max$value

ggplot(subset(F4.melt)) +
  geom_line(aes(x=x,y=value,color=variable,size=variable)) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="black",data=Acqu,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="brown",data=no.dis,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="green",data=low,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="blue",data=med,alpha=0.3) +
  geom_ribbon(aes(x=x,ymin=low,ymax=high),fill="red",data=high,alpha=0.3) +
#  geom_ribbon(aes(x=x,ymin=lwr,ymax=upr),color=variable,size=variable,alpha=0.3) +
  scale_size_manual(values =c(rep(1.2,5))) +
  scale_color_manual(values=c("black",
                              "brown",
                              "green",
                              "blue",
                              "red")) +
  scale_fill_manual(values=c("black",
                              "brown",
                              "green",
                              "blue",
                              "red")) +

#  scale_color_manual(values=c(TLV,StC,SeqC,StSeqC,
#                              TLV,StC,SeqC,StSeqC)) +
#  ggtitle("Plant growth with\ndifferent treatments") +
#    theme(plot.title = element_text(lineheight=.8, face="bold"))
    xlab("Year") +
    ylab("Cost effectiveness relative to Land Purchase") +
#    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
#    scale_y_continuous(breaks=seq(1e+08,5e+08,1e+08),
#          labels=c("100","200","300","400","500")) +
    scale_x_continuous(breaks=seq(0,100,10),
          labels=seq(0,100,10))  +
    theme(legend.position=c(0.6, 0.8)) +
    theme(legend.title=element_blank()) + #remove legend title
    theme(legend.background = element_rect(color= "black", fill="white", size=.2, linetype="solid"),
          legend.key=element_blank(),
          legend.key.size = unit(1,"cm"),
          legend.text.align=0,
          legend.text = element_text(size= 16)) +

    theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
#      panel.border = element_blank(),
#      panel.border = element_rect(colour = "black"),
      panel.background = element_blank()) +
    theme(axis.text = element_text(colour='black', size=20),
          axis.title = element_text(size=20),
          axis.title.y=element_text(vjust=0.3),
          axis.title.x=element_text(vjust=0))
#    theme(legend.text = element_text(size = 14))
#theme(legend.text = element_text(colour = 'red', angle = 45, size = 10, hjust = 3, vjust = 3, face = 'bold'))

#ggsave("F4_PE_DE_alter.tiff")
#ggsave("F4_PE_DE_alter.png")

sapply(tt.cb,min)
#                   Baseline: Land Purchase Cost
#                                       1.000000
#                     Easement cost, no disputes
#                                       3.253356
#    Low dispute rate (0.028% of easements/year)
#                                       3.226736
#Moderate dispute rate (0.28% of easements/year)
#                                       2.811134
#     High dispute rate (2.8% of easements/year)
#                                       1.012342
