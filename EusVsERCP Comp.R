
##EUS Vs ERCP Comp
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
ies.logit.RR.Comp=escalc(ai=CompEUS,bi=(SizeEUS-CompEUS),ci=CompERCP,di=(SizeERCP-CompERCP),measure="RR",data=dat.Combine.RR)
pes.logit.RR.Comp=rma(yi,vi,data=ies.logit.RR.Comp,method="DL",slab = paste(Study,AuthorYear,sep = ", "))
pes.RR.Comp.RR=predict(pes.logit.RR.Comp,transf=exp,digits=5)
print(pes.RR.Comp.RR,digits=2);
print(pes.logit.RR.Comp,digits=2);confint(pes.logit.RR.Comp,digits=2)

help("escalc")
#r Forest plot using meta
png("forestplot_meta_comp_RR.png",width=700,height=600)
forest(pes.logit.RR.Comp,xlim = c(-20, 10), at = log(c(0.02, 0.75, 1, 6)), atransf = exp,
       ilab = cbind(dat.Combine.RR$CompEUS, dat.Combine.RR$SizeEUS,dat.Combine.RR$CompERCP, dat.Combine.RR$SizeERCP),
       ilab.xpos = c(-13, -11, -9, -7), cex = 1)
op <- par(cex = 1, font = 2)
text(c(-13, -11, -9, -7), 7.5, c("Event", "Total", "Event", "Total"))
text(c(-11.75, -8), 8, c("EUS", "ERCP"))
text(-20, 7.5, "Study AuthorYear", pos = 4)
text(10,7.5, "Relative Risk [95% CI]", pos = 2)
par(op)
dev.off()


#Cumulative analysis
Comp.RR.Cul=cumul(pes.logit.RR.Comp,order=order(dat.Combine.RR$Study))
forest(Comp.RR.Cul,atransf = exp)


#r Funnel plot
funnel(pes.logit.RR.Comp,yaxis="sei")
funnel(pes.logit.RR.Comp,yaxis="sei",atransf=exp,xlab="RR",digits=4)


#r Egger's regression test
regtest(pes.logit.RR.Comp,model="lm",predictor = "sei")


#Moderator analysis subgroup analysis or meta regression
sub.samplesize.RR=rma(yi,vi,data=ies.logit.RR.Comp,mods=~SampleSize,method="REML")
print(sub.samplesize.RR,digits=2)

sub.FemalePercent.RR=rma(yi,vi,data=ies.logit.RR.Comp,mods=~FemalePercent,method="REML")
print(sub.FemalePercent.RR,digits=2)

sub.PancPercent.RR=rma(yi,vi,data=ies.logit.RR.Comp,mods=~PancPercent,method="REML")
print(sub.PancPercent.RR,digits=2)


#test
test=metabin(CompEUS, SizeEUS, CompERCP, SizeERCP,
        data=dat.Combine.RR, studlab=Study,
        sm="RR", method="I")
forest(test)
png("TEST",width=700,height=600)
forest(test,
       xlim=c(0,1),pscale=1,
       rightcols=c("effect","ci","w.random"),
       rihtlabs=c("RR","95% C.I.","weights"),
       leftcols=c("Study","CompEUS", "SizeEUS", "CompERCP", "SizeERCP"),
       leftlabs=c("Study ID","Event", "Total", "Event", "Total"),
       xlab = "Prevalence",
       fs.xlab=12,
       fs.study=12,
       fs.study.lables=12,
       fs.heading=12,
       squaresize=0.5, col.square="navy",col.square.lines="navy",
       col.diamond="navy", col.diamond.lines="navy",
       comb.fixed=FALSE,
       lty.fixed=0,
       lty.random=2,
       type.study="square",
       type.random="diamond",
       ff.fixed="bold.italic",
       ff.random="bold.italic",
       hetlab="Heterogeneity:",
       fs.hetstat=10,
       smlab="",
       print.Q=TRUE,
       print.pval.Q=TRUE,
       print.I2=TRUE,
       print.tau2=FALSE,
       col.by="grey",
       digits=5)
dev.off()







