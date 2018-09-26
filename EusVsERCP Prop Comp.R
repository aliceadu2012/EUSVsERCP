##EUS Vs ERCP Comp Prop
setwd("C:/Users/ydu6c9/R/library")
getwd()

install.packages(c("metafor","meta","weightr"))
#r loading packages
library(metafor)
library(meta)
library(weightr)

#r read data
library(openxlsx)
dat.Combine=read.xlsx("H:/Fhc/FHC_GME_DATA_ANALYST/Alice(Yuan) Du/GME/General Surgery/EUS Vs ERCP/GME Analyses List-Yuan Du by year by author.xlsx")
dat.Comp.EUS=dat.Combine[which(dat.Combine$Procedure=="EUS"),]
dat.Comp.ERCP=dat.Combine[which(dat.Combine$Procedure=="ERCP"),]

#r calculating overall summary proportion; effect size by study type
ies.logit.Comp.EUS=escalc(xi=Complications,ni=Sample_Size,measure="PLO",data=dat.Comp.EUS)
pes.logit.Comp.EUS=rma(yi,vi,data=ies.logit.Comp.EUS,method="DL")
pes.Comp.EUS=predict(pes.logit.EUS,transf=transf.ilogit,digits=5)
print(pes.Comp.EUS,digits=2);print(pes.logit.Comp.EUS,digits=2);confint(pes.logit.Comp.EUS,digits=2)

#r Forest plot using meta
pes.forest.comp.EUS=metaprop(Complications,Sample_Size,AuthorYear,data=dat.Comp.EUS,sm="PLO",method.ci="NASm",method.tau = "DL",incr=0.5,allincr = FALSE,addincr = FALSE,title="")
png("forestplot_meta_comp_EUS.png",width=800,height=600)
forest(pes.forest.ocmp.EUS,
       xlim=c(0,1),pscale=1,
       rightcols=c("effect","ci","w.random"),
       rihtlabs=c("Proportion","95% C.I.","weights"),
       leftcols=c("Study","studlab","Complications","Sample_Size"),
       leftlabs=c("Study ID","Author","Complication","Sample Size"),
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

###########################################################################################################
#r calculating overall summary proportion; effect size by study type
ies.logit.Comp.ERCP=escalc(xi=Complications,ni=Sample_Size,measure="PLO",data=dat.Comp.ERCP)
pes.logit.Comp.ERCP=rma(yi,vi,data=ies.logit.Comp.ERCP,method="DL")
pes.Comp.ERCP=predict(pes.logit.ERCP,transf=transf.ilogit,digits=5)
print(pes.Comp.ERCP,digits=2);print(pes.logit.Comp.ERCP,digits=2);confint(pes.logit.Comp.ERCP,digits=2)

#r Forest plot using meta
pes.forest.comp.ERCP=metaprop(Complications,Sample_Size,AuthorYear,data=dat.Comp.ERCP,sm="PLO",method.ci="NASm",method.tau = "DL",incr=0.5,allincr = FALSE,addincr = FALSE,title="")
png("forestplot_meta_comp_ERCP.png",width=800,height=600)
forest(pes.forest.comp.ERCP,
       xlim=c(0,1),pscale=1,
       rightcols=c("effect","ci","w.random"),
       rihtlabs=c("Proportion","95% C.I.","weights"),
       leftcols=c("Study","studlab","Complications","Sample_Size"),
       leftlabs=c("Study ID","Author","Complication","Sample Size"),
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

##################################################################Combine by group
#r calculating overall summary proportion; effect size by study type; See above

#r Forest plot using meta
pes.forest.comp =metaprop(Complications,Sample_Size,AuthorYear,byvar=Procedure,data=dat.Combine,sm="PLO",method.ci="NASm",method.tau = "DL",incr=0.5,allincr = FALSE,addincr = FALSE,title="")
png("forestplot_meta_comp_Combine.png",width=800,height=600)
forest(pes.forest.comp ,
       xlim=c(0,1),pscale=1,
       rightcols=c("effect","ci","w.random"),
       rihtlabs=c("Proportion","95% C.I.","weights"),
       leftcols=c("Study","studlab","Complications","Sample_Size"),
       leftlabs=c("Study","AuthorYear","Complication","Sample Size"),
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