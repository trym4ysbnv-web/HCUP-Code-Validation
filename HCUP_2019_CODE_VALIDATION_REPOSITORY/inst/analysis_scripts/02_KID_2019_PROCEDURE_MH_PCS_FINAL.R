# ================================================================
# KID 2019 FINAL PROCEDURE + MH-PCS ANALYSIS
# Pediatric KID population
#
# FINAL / VALIDATED VERSION
# - Exact ICD-10 matching across I10_DX1-I10_DX15
# - RareMed excludes E83.42
# - Procedure bins: 0 / 1-2 / 3-5 / 6+
# - MH DRG: 880-887
# - Corrected percentage logic (no recycled first-row percentages)
#
# INPUTS SELECTED:
#   1) KID_2019_Core_function (1)(2).R
#   2) KID_2019_Core.ASC
#
# OUTPUTS:
#   01 Cohort Audit
#   02 Overall Summary
#   03 Service Domains
#   04 Individual Prefixes
#   05 Treated Intensity
#   06 Complexity x MH
#   07 MH DRG x MH PCS
#   08 MH Count Distribution
#   09 Overall Procedure Bins
#   10 Procedure Bin QA
# ================================================================

options(stringsAsFactors=FALSE)
if(!requireNamespace("data.table",quietly=TRUE))
  install.packages("data.table",repos="https://cloud.r-project.org")
library(data.table)

cat("\nKID 2019 FINAL PROCEDURE + MH-PCS ANALYSIS — AGE 18-64\n")

cat("\nSTEP 1 OF 2: Select KID_2019_Core_function.R\n")
core_function_file <- file.choose()
source(core_function_file)
if(!exists("KID_2019_Core_function")) stop("KID_2019_Core_function not loaded.")

cat("\nSTEP 2 OF 2: Select KID_2019_Core.ASC\n")
core_file <- file.choose()

KID_2019_Core <- read.table(core_file,header=FALSE,sep=",",
                            stringsAsFactors=FALSE,quote="",comment.char="")
D <- KID_2019_Core_function(KID_2019_Core)
rm(KID_2019_Core); gc()
names(D) <- trimws(names(D)); setDT(D)

# Canonical final definitions
RareNP_codes <- c("E80.21","E71.522","G31.86","Q93.51","E72.21","I67.850","E71.520","Q79.61","D82.1","Q79.60","E75.21","G43.401","G43.409","Q99.2","G11.11","G31.01","E75.22","E74.810","E72.02","E76.1","E76.01","Q79.62","Q93.59","Q98.4","E75.23","G40.309","G40.C01","G40.C09","G40.C11","G40.C19","G31.82","G93.44","Q87.4","D47.02","E88.41","E75.25","E71.120","E76.21","E72.12","E75.4","E75.24","Q79.69","Q93.52","Q87.11","F84.2","E75.01","E76.22","F78.A1","E75.02","Q77.1","Q85.1","Q79.63","Q93.82","E83.01","E34.8","E71.528","E71.529")
RareMed_codes <- c("Q77.4","D81.30","E71.521","E88.01","Q87.81","G12.21","D68.61","E72.22","Q61.2","E26.81","M35.2","D47.Z2","G11.3","G60.0","Q77.3","E72.23","E74.03","M34.1","E84.11","E84.19","E84.8","E84.0","E84.9","E72.01","G71.01","Q81.0","Q81.2","G71.02","D68.1","E74.21","I78.0","G11.4","D58.0","E85.2","E72.11","Q82.3","H49.81","E85.81","I45.81","E71.0","E74.04","E88.42","G71.11","E71.511","Q61.5","Q85.01","Q85.02","Q78.0","M34.89","D59.5","E70.0","E74.02","E71.121","E20.1","H35.52","Q85.03","D81.0","D81.1","D81.2","G12.0","M34.81","M34.82","M34.83","Q96.9","E74.01","Q85.83","D68.0","D82.0","Q82.1","E71.510")
CommonMH_codes <- c("F90.0","F90.1","F90.2","F84.0","F32.0","F32.1","F32.2","F32.3","F33.1","F41.1","F33.3","F33.4","F43.12","F20.0","F20.1","F20.2","F20.3","F42.2","F42.8")
CommonMed_codes <- c("N18.1","N18.2","N18.30","N18.31","N18.32","N18.4","N18.5","J44.0","J44.1","J44.89","J44.9","I50.9","I25.10","I25.110","I25.119","G35.A","G35.B1","G35.B2","G20.A1","G20.A2","G20.B1","G20.B2","E10.9")

dx <- paste0("I10_DX",1:15)
pr <- paste0("I10_PR",1:25)
required <- c(dx,pr,"DISCWT","I10_NPR","DRG")
miss <- setdiff(required,names(D))
if(length(miss)) stop(paste("Missing:",paste(miss,collapse=", ")))

norm <- function(x) {
  x <- toupper(trimws(as.character(x))); x <- gsub("\\.","",x)
  x[x %in% c("","NA")] <- NA_character_; x
}
for(v in dx) set(D,j=v,value=norm(D[[v]]))
for(v in pr) set(D,j=v,value=norm(D[[v]]))
for(v in c("DISCWT","I10_NPR","DRG"))
  set(D,j=v,value=suppressWarnings(as.numeric(D[[v]])))
D[is.na(I10_NPR) | I10_NPR<0,I10_NPR:=NA_real_]

flag <- function(codes) {
  use <- norm(codes); z <- rep(FALSE,nrow(D))
  for(v in dx) z <- z | D[[v]] %chin% use
  as.integer(z)
}
D[,RareNP:=flag(RareNP_codes)]
D[,RareMed:=flag(RareMed_codes)]
D[,CommonMH:=flag(CommonMH_codes)]
D[,CommonMed:=flag(CommonMed_codes)]
groups <- c("CommonMed","CommonMH","RareMed","RareNP")

MH_CRISIS <- c("GZ2")
MH_THERAPY <- c("GZ5","GZ6","GZ7","GZH")
MH_ASSESSMENT <- c("GZ1","GZC")
MH_MEDICAL <- c("GZ3","GZB")
MH_PREFIXES <- c("GZ1","GZ2","GZ3","GZ5","GZ6","GZ7","GZB","GZC","GZH")
prefix_labels <- c(GZ1="Psychological Tests",GZ2="Crisis Intervention",
                   GZ3="Medication Management",GZ5="Individual Psychotherapy",
                   GZ6="Counseling",GZ7="Family Psychotherapy",
                   GZB="Electroconvulsive Therapy",GZC="Biofeedback",
                   GZH="Group Psychotherapy")

count_prefix <- function(prefixes) {
  z <- integer(nrow(D))
  for(v in pr) {
    p <- substr(D[[v]],1,3)
    z <- z + as.integer(!is.na(p) & p %chin% prefixes)
  }
  z
}

D[,MH_Crisis_Count:=count_prefix(MH_CRISIS)]
D[,MH_Therapy_Count:=count_prefix(MH_THERAPY)]
D[,MH_Assessment_Count:=count_prefix(MH_ASSESSMENT)]
D[,MH_Medical_Count:=count_prefix(MH_MEDICAL)]
D[,MH_Total_Count:=MH_Crisis_Count+MH_Therapy_Count+MH_Assessment_Count+MH_Medical_Count]
for(p in MH_PREFIXES) D[,(paste0("Count_",p)):=count_prefix(p)]

D[,Any_MH_PCS:=as.integer(MH_Total_Count>=1)]
D[,Any_Crisis:=as.integer(MH_Crisis_Count>=1)]
D[,Any_Therapy:=as.integer(MH_Therapy_Count>=1)]
D[,Any_Assessment:=as.integer(MH_Assessment_Count>=1)]
D[,Any_Medical:=as.integer(MH_Medical_Count>=1)]
D[,NonMH_PCS_Count:=ifelse(!is.na(I10_NPR),pmax(I10_NPR-MH_Total_Count,0),NA_real_)]
D[,MH_PCS_Share:=ifelse(!is.na(I10_NPR)&I10_NPR>0,100*MH_Total_Count/I10_NPR,NA_real_)]
D[,Procedure_Bin:=fcase(is.na(I10_NPR),NA_character_,
                        I10_NPR==0,"0",
                        I10_NPR>=1&I10_NPR<=2,"1-2",
                        I10_NPR>=3&I10_NPR<=5,"3-5",
                        I10_NPR>=6,"6+")]
D[,MH_DRG:=as.integer(!is.na(DRG)&DRG%in%880:887)]
bin_order <- c("0","1-2","3-5","6+")

wmean <- function(x,w) {
  ok <- !is.na(x)&!is.na(w)&w>0
  if(!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok])/sum(w[ok])
}


weighted_quantile <- function(x,w,probs=c(.25,.50,.75,.90,.95)) {
  ok <- !is.na(x)&!is.na(w)&w>0
  x<-x[ok]; w<-w[ok]
  if(!length(x)||sum(w)<=0) return(rep(NA_real_,length(probs)))
  o<-order(x); x<-x[o]; w<-w[o]; cw<-cumsum(w)/sum(w)
  sapply(probs,function(p)x[which(cw>=p)[1]])
}

Procedure_Summary <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & !is.na(I10_NPR)&!is.na(DISCWT)]
  tw<-sum(x$DISCWT,na.rm=TRUE); wm<-wmean(x$I10_NPR,x$DISCWT)
  wsd<-sqrt(sum(x$DISCWT*(x$I10_NPR-wm)^2,na.rm=TRUE)/tw)
  q<-weighted_quantile(x$I10_NPR,x$DISCWT)
  data.table(Group=g,Unweighted_N=nrow(x),Weighted_N=tw,
             Unweighted_Mean=mean(x$I10_NPR,na.rm=TRUE),Weighted_Mean=wm,
             Weighted_SD=wsd,Weighted_Q1=q[1],Weighted_Median=q[2],
             Weighted_Q3=q[3],Weighted_P90=q[4],Weighted_P95=q[5],
             Percent_Zero_Procedures=100*sum(x$DISCWT[x$I10_NPR==0],na.rm=TRUE)/tw,
             Percent_Any_Procedure=100*sum(x$DISCWT[x$I10_NPR>=1],na.rm=TRUE)/tw)
}))

Cohort_Audit <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1]
  data.table(Group=g,Unweighted_Stays=nrow(x),
             Weighted_Stays=sum(x$DISCWT,na.rm=TRUE),
             Missing_I10_NPR_N=sum(is.na(x$I10_NPR)),
             Any_MH_PCS_N=sum(x$Any_MH_PCS==1,na.rm=TRUE),
             Any_MH_PCS_Weighted=sum(x$DISCWT[x$Any_MH_PCS==1],na.rm=TRUE),
             MH_Count_Greater_Than_NPR=sum(!is.na(x$I10_NPR)&x$MH_Total_Count>x$I10_NPR,na.rm=TRUE))
}))

MH_Overall_Summary <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & !is.na(DISCWT)]
  tw<-sum(x$DISCWT,na.rm=TRUE); mh<-x[Any_MH_PCS==1]; mw<-sum(mh$DISCWT,na.rm=TRUE)
  data.table(Group=g,Unweighted_Stays=nrow(x),Weighted_Stays=tw,
             MH_PCS_Unweighted=nrow(mh),MH_PCS_Weighted=mw,
             MH_PCS_Percent=if(tw>0)100*mw/tw else NA_real_,
             Mean_Total_PCS=wmean(x$I10_NPR,x$DISCWT),
             Mean_MH_PCS_All_Stays=wmean(x$MH_Total_Count,x$DISCWT),
             Mean_NonMH_PCS=wmean(x$NonMH_PCS_Count,x$DISCWT),
             Mean_MH_PCS_Among_MH_Treated=if(nrow(mh))wmean(mh$MH_Total_Count,mh$DISCWT) else NA_real_,
             Mean_MH_Share_Among_MH_Treated=if(nrow(mh))wmean(mh$MH_PCS_Share,mh$DISCWT) else NA_real_)
}))

domain_list <- list(Crisis_Intervention="Any_Crisis",Therapy="Any_Therapy",
                    Assessment_Diagnostic="Any_Assessment",Medical_Intervention="Any_Medical")
MH_Domain_Summary <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & !is.na(DISCWT)]; tw<-sum(x$DISCWT,na.rm=TRUE)
  rbindlist(lapply(names(domain_list),function(label){
    y<-x[get(domain_list[[label]])==1]; wy<-sum(y$DISCWT,na.rm=TRUE)
    data.table(Group=g,MH_Service_Category=label,Unweighted_N=nrow(y),
               Weighted_N=wy,Percent_of_Cohort=if(tw>0)100*wy/tw else NA_real_)
  }))
}))

MH_Prefix_Breakdown <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & !is.na(DISCWT)]; tw<-sum(x$DISCWT,na.rm=TRUE)
  rbindlist(lapply(MH_PREFIXES,function(p){
    v<-paste0("Count_",p); y<-x[get(v)>=1]; wy<-sum(y$DISCWT,na.rm=TRUE)
    data.table(Group=g,PCS_Prefix=p,Procedure=prefix_labels[[p]],
               Unweighted_Stays=nrow(y),Weighted_Stays=wy,
               Percent_of_Cohort=if(tw>0)100*wy/tw else NA_real_,
               Weighted_Procedure_Count=sum(x[[v]]*x$DISCWT,na.rm=TRUE))
  }))
}))

MH_Treated_Intensity <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & Any_MH_PCS==1 & !is.na(DISCWT)]
  if(!nrow(x)) return(data.table(Group=g))
  data.table(Group=g,MH_Treated_Unweighted_N=nrow(x),
             MH_Treated_Weighted_N=sum(x$DISCWT,na.rm=TRUE),
             Mean_MH_PCS=wmean(x$MH_Total_Count,x$DISCWT),
             Mean_Total_PCS=wmean(x$I10_NPR,x$DISCWT),
             Mean_NonMH_PCS=wmean(x$NonMH_PCS_Count,x$DISCWT),
             Mean_MH_PCS_Share=wmean(x$MH_PCS_Share,x$DISCWT),
             Mean_Crisis_Count=wmean(x$MH_Crisis_Count,x$DISCWT),
             Mean_Therapy_Count=wmean(x$MH_Therapy_Count,x$DISCWT),
             Mean_Assessment_Count=wmean(x$MH_Assessment_Count,x$DISCWT),
             Mean_Medical_Count=wmean(x$MH_Medical_Count,x$DISCWT))
}),use.names=TRUE,fill=TRUE)

Complexity_x_MH <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & !is.na(Procedure_Bin)&!is.na(DISCWT)]
  rbindlist(lapply(bin_order,function(b){
    y<-x[Procedure_Bin==b]; tw<-sum(y$DISCWT,na.rm=TRUE); mh<-y[Any_MH_PCS==1]
    mw<-sum(mh$DISCWT,na.rm=TRUE)
    data.table(Group=g,Procedure_Bin=b,Unweighted_Total=nrow(y),Weighted_Total=tw,
               MH_PCS_Unweighted=nrow(mh),MH_PCS_Weighted=mw,
               Percent_With_MH_PCS=if(tw>0)100*mw/tw else NA_real_,
               Mean_MH_PCS_Among_Treated=if(nrow(mh))wmean(mh$MH_Total_Count,mh$DISCWT) else NA_real_,
               Mean_MH_Share_Among_Treated=if(nrow(mh))wmean(mh$MH_PCS_Share,mh$DISCWT) else NA_real_)
  }))
}))

MH_DRG_x_MH_PCS <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & !is.na(DISCWT)]; tw<-sum(x$DISCWT,na.rm=TRUE)
  cmb<-CJ(MH_DRG=c(0L,1L),MH_PCS=c(0L,1L))
  rbindlist(lapply(seq_len(nrow(cmb)),function(i){
    y<-x[MH_DRG==cmb$MH_DRG[i] & Any_MH_PCS==cmb$MH_PCS[i]]
    wy<-sum(y$DISCWT,na.rm=TRUE)
    data.table(Group=g,MH_DRG=cmb$MH_DRG[i],MH_PCS=cmb$MH_PCS[i],
               Unweighted_N=nrow(y),Weighted_N=wy,
               Percent_of_Cohort=if(tw>0)100*wy/tw else NA_real_)
  }))
}))

MH_Count_Distribution <- rbindlist(lapply(groups,function(g){
  x<-copy(D[get(g)==1 & !is.na(DISCWT)])
  x[,MH_Count_Category:=fifelse(MH_Total_Count>=6,"6+",as.character(MH_Total_Count))]
  cats<-c("0","1","2","3","4","5","6+"); tw<-sum(x$DISCWT,na.rm=TRUE)
  out<-x[,.(Unweighted_N=.N,Weighted_N=sum(DISCWT,na.rm=TRUE)),by=MH_Count_Category]
  out<-merge(data.table(MH_Count_Category=cats),out,by="MH_Count_Category",all.x=TRUE,sort=FALSE)
  out[is.na(Unweighted_N),Unweighted_N:=0L]; out[is.na(Weighted_N),Weighted_N:=0]
  out[,Percent_of_Cohort:=if(tw>0)100*Weighted_N/tw else NA_real_]
  out[,Group:=g]; setcolorder(out,c("Group","MH_Count_Category","Unweighted_N","Weighted_N","Percent_of_Cohort"))
  out
}))

Overall_Procedure_Bins <- rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1 & !is.na(Procedure_Bin)&!is.na(DISCWT)]
  tu<-nrow(x); tw<-sum(x$DISCWT,na.rm=TRUE)
  out<-x[,.(Unweighted_N=.N,Weighted_N=sum(DISCWT,na.rm=TRUE)),by=Procedure_Bin]
  out<-merge(data.table(Procedure_Bin=bin_order),out,by="Procedure_Bin",all.x=TRUE,sort=FALSE)
  out[is.na(Unweighted_N),Unweighted_N:=0L]; out[is.na(Weighted_N),Weighted_N:=0]
  out[,Group:=g]
  if(tu>0) out[,Percent_Unweighted:=100*Unweighted_N/tu] else out[,Percent_Unweighted:=NA_real_]
  if(tw>0) out[,Percent_Weighted:=100*Weighted_N/tw] else out[,Percent_Weighted:=NA_real_]
  setcolorder(out,c("Group","Procedure_Bin","Unweighted_N","Weighted_N","Percent_Unweighted","Percent_Weighted"))
  out
}))

Procedure_Bin_QA <- Overall_Procedure_Bins[,.(Sum_Unweighted_Percent=sum(Percent_Unweighted,na.rm=TRUE),
                                             Sum_Weighted_Percent=sum(Percent_Weighted,na.rm=TRUE)),by=Group]

output_dir <- dirname(core_file)
fwrite(Procedure_Summary,file.path(output_dir,"KID_2019_PCS_00_Procedure_Summary.csv"))
fwrite(Cohort_Audit,file.path(output_dir,"KID_2019_MH_PCS_01_Cohort_Audit.csv"))
fwrite(MH_Overall_Summary,file.path(output_dir,"KID_2019_MH_PCS_02_Overall_Summary.csv"))
fwrite(MH_Domain_Summary,file.path(output_dir,"KID_2019_MH_PCS_03_Service_Domains.csv"))
fwrite(MH_Prefix_Breakdown,file.path(output_dir,"KID_2019_MH_PCS_04_Individual_Prefixes.csv"))
fwrite(MH_Treated_Intensity,file.path(output_dir,"KID_2019_MH_PCS_05_Treated_Intensity.csv"))
fwrite(Complexity_x_MH,file.path(output_dir,"KID_2019_MH_PCS_06_Complexity_x_MH.csv"))
fwrite(MH_DRG_x_MH_PCS,file.path(output_dir,"KID_2019_MH_PCS_07_MH_DRG_x_MH_PCS.csv"))
fwrite(MH_Count_Distribution,file.path(output_dir,"KID_2019_MH_PCS_08_MH_Count_Distribution.csv"))
fwrite(Overall_Procedure_Bins,file.path(output_dir,"KID_2019_MH_PCS_09_Overall_Procedure_Bins.csv"))
fwrite(Procedure_Bin_QA,file.path(output_dir,"KID_2019_MH_PCS_10_Procedure_Bin_QA.csv"))

cat("\nDONE: 10 validated KID outputs written to",output_dir,"\n")
print(Procedure_Bin_QA)
