##EUS Vs ERCP Procedural Success Prop
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
dat.ProSuccess.EUS=dat.Combine[which(dat.Combine$Procedure=="EUS"),]
dat.ProSuccess.ERCP=dat.Combine[which(dat.Combine$Procedure=="ERCP"),]

#r calculating overall summary proportion; effect size by study type
ies.logit.ProSuccess.EUS=escalc(xi=Procedural_Success,ni=Sample_Size,measure="PLO",data=dat.ProSuccess.EUS)
pes.logit.ProSuccess.EUS=rma(yi,vi,data=ies.logit.ProSuccess.EUS,method="DL")
pes.ProSuccess.EUS=predict(pes.logit.EUS,transf=transf.ilogit,digits=5)
print(pes.ProSuccess.EUS,digits=2);print(pes.logit.ProSuccess.EUS,digits=2);confint(pes.logit.ProSuccess.EUS,digits=2)


##################################################################Combine by group
#r calculating overall summary proportion; effect size by study type; See above

#r Forest plot using meta
pes.forest.ProSuccess =metaprop(Procedural_Success,Sample_Size,AuthorYear,byvar=Procedure,data=dat.Combine,sm="PLO",method.ci="NASm",method.tau = "DL",incr=0.5,allincr = FALSE,addincr = FALSE,title="")
png("forestplot_meta_ProSuccess_Combine.png",width=800,height=600)
forest(pes.forest.ProSuccess ,
       xlim=c(0,1),pscale=1,
       rightcols=c("effect","ci","w.random"),
       rihtlabs=c("Proportion","95% C.I.","weights"),
       leftcols=c("Study","studlab","Procedural_Success","Sample_Size"),
       leftlabs=c("Study","AuthorYear","Procedural Success","Sample Size"),
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