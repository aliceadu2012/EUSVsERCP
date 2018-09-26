##EUS Vs ERCP Outcome: Migration
setwd("C:/Users/ydu6c9/R/library")
getwd()

install.packages(c("metafor","meta","weightr"))
#r loading packages
library(metafor)
library(meta)
library(weightr)

#r read data
library(openxlsx)
dat.Combine.RR=read.xlsx("H:/Fhc/FHC_GME_DATA_ANALYST/Alice(Yuan) Du/GME/General Surgery/EUS Vs ERCP/Sas Data/EUS_Combine.xlsx")


#r calculating overall rr;
ies.logit.RR.Migration=escalc(ai=MigrationEUS,bi=(SizeEUS-MigrationEUS),ci=MigrationERCP,di=(SizeERCP-MigrationERCP),measure="RR",data=dat.Combine.RR)
pes.logit.RR.Migration=rma(yi,vi,data=ies.logit.RR.Migration,method="DL",slab = paste(Study,AuthorYear,sep = ", "))
pes.RR.Migration.RR=predict(pes.logit.RR.Migration,transf=exp,digits=5)
print(pes.RR.Migration.RR,digits=2);
print(pes.logit.RR.Migration,digits=2);confint(pes.logit.RR.Migration,digits=2)

help("escalc")
#r Forest plot using meta
png("forestplot_meta_Migration_RR.png",width=700,height=600)
forest(pes.logit.RR.Migration,xlim = c(-20, 10), at = log(c(0.02, 1, 1.5, 10)), atransf = exp,
       ilab = cbind(dat.Combine.RR$MigrationEUS, dat.Combine.RR$SizeEUS,dat.Combine.RR$MigrationERCP, dat.Combine.RR$SizeERCP),
       ilab.xpos = c(-13, -11, -9, -7), cex = 1)
op <- par(cex = 1, font = 2)
text(c(-13, -11, -9, -7), 3.5, c("Event", "Total", "Event", "Total"))
text(c(-11.75, -8), 4.5, c("EUS", "ERCP"))
text(-20, 3.5, "Study AuthorYear", pos = 4)
text(10,3.5, "Relative Risk [95% CI]", pos = 2)
par(op)
dev.off()


#Cumulative analysis
#Comp.RR.Cul=cumul(pes.logit.RR.Migration,order=order(dat.Combine.RR$Study))
#forest(Comp.RR.Cul,atransf = exp)


#r Funnel plot
funnel(pes.logit.RR.Migration,yaxis="sei")
funnel(pes.logit.RR.Migration,yaxis="sei",atransf=exp,xlab="RR",digits=4)


#r Egger's regression test
regtest(pes.logit.RR.Migration,model="lm",predictor = "sei")


#Moderator analysis subgroup analysis or meta regression
sub.samplesize.RR=rma(yi,vi,data=ies.logit.RR.Migration,mods=~SampleSize,method="REML")
print(sub.samplesize.RR,digits=2)

sub.FemalePercent.RR=rma(yi,vi,data=ies.logit.RR.Migration,mods=~FemalePercent,method="REML")
print(sub.FemalePercent.RR,digits=2)

sub.PancPercent.RR=rma(yi,vi,data=ies.logit.RR.Migration,mods=~PancPercent,method="REML")
print(sub.PancPercent.RR,digits=2)