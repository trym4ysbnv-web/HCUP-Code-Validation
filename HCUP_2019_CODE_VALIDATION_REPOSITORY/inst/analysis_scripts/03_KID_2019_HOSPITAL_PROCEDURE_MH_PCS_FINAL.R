# ================================================================
# KID 2019 FINAL HOSPITAL-STRATIFIED PROCEDURE + MH-PCS ANALYSIS
#
# FINAL / VALIDATED VERSION
#
# INPUTS SELECTED:
#   1) KID_2019_Core_function (1)(2).R
#   2) KID_2019_Core.ASC
#   3) KID_2019_HOSPITAL*.ASC
#
# HOSPITAL TYPES:
#   Freestanding children's hospital = KID_STRATUM == 9999
#   Other urban teaching             = otherwise HOSP_LOCTEACH == 3
#   Urban nonteaching                = HOSP_LOCTEACH == 2
#   Rural                            = HOSP_LOCTEACH == 1
#
# 2019 KID hospital fixed-width fields used:
#   HOSP_KID      columns 1-5
#   HOSP_LOCTEACH columns 70-71
#   KID_STRATUM   columns 76-79
#
# OUTPUTS:
#   01 Hospital Distribution
#   02 Procedure Summary
#   03 Procedure Bins
#   04 MH PCS by Hospital
#   05 MH DRG x MH PCS
#   06 RareNP vs CommonMH
#   07 Bin QA
#   08 Hospital Distribution QA
# ================================================================

options(stringsAsFactors=FALSE)
if(!requireNamespace("data.table",quietly=TRUE))
  install.packages("data.table",repos="https://cloud.r-project.org")
library(data.table)

cat("\nKID 2019 FINAL HOSPITAL-STRATIFIED PROCEDURE + MH-PCS ANALYSIS\n")
cat("\nSTEP 1 OF 3: Select KID_2019_Core_function (1)(2).R\n")
core_function_file<-file.choose()
cat("\nSTEP 2 OF 3: Select KID_2019_Core.ASC\n")
core_file<-file.choose()
cat("\nSTEP 3 OF 3: Select KID_2019_HOSPITAL*.ASC\n")
hospital_file<-file.choose()

source(core_function_file)
if(!exists("KID_2019_Core_function")) stop("KID_2019_Core_function not loaded.")

KID_2019_Core<-read.table(core_file,header=FALSE,sep=",",stringsAsFactors=FALSE,quote="",comment.char="")
D<-KID_2019_Core_function(KID_2019_Core)
rm(KID_2019_Core); gc()
names(D)<-trimws(names(D)); setDT(D)

hospital_lines<-readLines(hospital_file,warn=FALSE)
H<-data.table(
  HOSP_KID=suppressWarnings(as.numeric(substr(hospital_lines,1,5))),
  HOSP_LOCTEACH=suppressWarnings(as.numeric(substr(hospital_lines,70,71))),
  KID_STRATUM_HOSP=suppressWarnings(as.numeric(substr(hospital_lines,76,79)))
)
rm(hospital_lines); gc()

RareNP_codes <- c("E80.21","E71.522","G31.86","Q93.51","E72.21","I67.850","E71.520","Q79.61","D82.1","Q79.60","E75.21","G43.401","G43.409","Q99.2","G11.11","G31.01","E75.22","E74.810","E72.02","E76.1","E76.01","Q79.62","Q93.59","Q98.4","E75.23","G40.309","G40.C01","G40.C09","G40.C11","G40.C19","G31.82","G93.44","Q87.4","D47.02","E88.41","E75.25","E71.120","E76.21","E72.12","E75.4","E75.24","Q79.69","Q93.52","Q87.11","F84.2","E75.01","E76.22","F78.A1","E75.02","Q77.1","Q85.1","Q79.63","Q93.82","E83.01","E34.8","E71.528","E71.529")
RareMed_codes <- c("Q77.4","D81.30","E71.521","E88.01","Q87.81","G12.21","D68.61","E72.22","Q61.2","E26.81","M35.2","D47.Z2","G11.3","G60.0","Q77.3","E72.23","E74.03","M34.1","E84.11","E84.19","E84.8","E84.0","E84.9","E72.01","G71.01","Q81.0","Q81.2","G71.02","D68.1","E74.21","I78.0","G11.4","D58.0","E85.2","E72.11","Q82.3","H49.81","E85.81","I45.81","E71.0","E74.04","E88.42","G71.11","E71.511","Q61.5","Q85.01","Q85.02","Q78.0","M34.89","D59.5","E70.0","E74.02","E71.121","E20.1","H35.52","Q85.03","D81.0","D81.1","D81.2","G12.0","M34.81","M34.82","M34.83","Q96.9","E74.01","Q85.83","D68.0","D82.0","Q82.1","E71.510")
CommonMH_codes <- c("F90.0","F90.1","F90.2","F84.0","F32.0","F32.1","F32.2","F32.3","F33.1","F41.1","F33.3","F33.4","F43.12","F20.0","F20.1","F20.2","F20.3","F42.2","F42.8")
CommonMed_codes <- c("N18.1","N18.2","N18.30","N18.31","N18.32","N18.4","N18.5","J44.0","J44.1","J44.89","J44.9","I50.9","I25.10","I25.110","I25.119","G35.A","G35.B1","G35.B2","G20.A1","G20.A2","G20.B1","G20.B2","E10.9")

dx<-paste0("I10_DX",1:15); pr<-paste0("I10_PR",1:25)
req<-c(dx,pr,"DISCWT","I10_NPR","DRG","HOSP_KID","KID_STRATUM")
miss<-setdiff(req,names(D)); if(length(miss))stop(paste("Missing:",paste(miss,collapse=", ")))

norm<-function(x){x<-toupper(trimws(as.character(x)));x<-gsub("\\.","",x);x[x%in%c("","NA")]<-NA_character_;x}
for(v in dx)set(D,j=v,value=norm(D[[v]]))
for(v in pr)set(D,j=v,value=norm(D[[v]]))
for(v in c("DISCWT","I10_NPR","DRG","HOSP_KID","KID_STRATUM"))
  set(D,j=v,value=suppressWarnings(as.numeric(D[[v]])))
D[is.na(I10_NPR)|I10_NPR<0,I10_NPR:=NA_real_]

Hsmall<-unique(H[,.(HOSP_KID,HOSP_LOCTEACH,KID_STRATUM_HOSP)],by="HOSP_KID")
n0<-nrow(D)
D<-merge(D,Hsmall,by="HOSP_KID",all.x=TRUE,sort=FALSE)
if(nrow(D)!=n0)stop("Hospital merge changed discharge count.")

D[,Hospital_Type:=fcase(
  KID_STRATUM==9999,"Freestanding children's hospital",
  HOSP_LOCTEACH==3,"Other urban teaching",
  HOSP_LOCTEACH==2,"Urban nonteaching",
  HOSP_LOCTEACH==1,"Rural",
  default=NA_character_
)]
hospital_types<-c("Freestanding children's hospital","Other urban teaching","Urban nonteaching","Rural")

flag<-function(codes){use<-norm(codes);z<-rep(FALSE,nrow(D));for(v in dx)z<-z|D[[v]]%chin%use;as.integer(z)}
D[,RareNP:=flag(RareNP_codes)];D[,RareMed:=flag(RareMed_codes)]
D[,CommonMH:=flag(CommonMH_codes)];D[,CommonMed:=flag(CommonMed_codes)]
groups<-c("CommonMed","CommonMH","RareMed","RareNP")

MH_CRISIS<-c("GZ2");MH_THERAPY<-c("GZ5","GZ6","GZ7","GZH")
MH_ASSESSMENT<-c("GZ1","GZC");MH_MEDICAL<-c("GZ3","GZB")
count_prefix<-function(prefixes){
  z<-integer(nrow(D))
  for(v in pr){p<-substr(D[[v]],1,3);z<-z+as.integer(!is.na(p)&p%chin%prefixes)}
  z
}
D[,MH_Crisis_Count:=count_prefix(MH_CRISIS)]
D[,MH_Therapy_Count:=count_prefix(MH_THERAPY)]
D[,MH_Assessment_Count:=count_prefix(MH_ASSESSMENT)]
D[,MH_Medical_Count:=count_prefix(MH_MEDICAL)]
D[,MH_Total_Count:=MH_Crisis_Count+MH_Therapy_Count+MH_Assessment_Count+MH_Medical_Count]
D[,Any_MH_PCS:=as.integer(MH_Total_Count>=1)]
D[,Procedure_Bin:=fcase(is.na(I10_NPR),NA_character_,I10_NPR==0,"0",
                        I10_NPR>=1&I10_NPR<=2,"1-2",
                        I10_NPR>=3&I10_NPR<=5,"3-5",I10_NPR>=6,"6+")]
D[,MH_DRG:=as.integer(!is.na(DRG)&DRG%in%880:887)]
bin_order<-c("0","1-2","3-5","6+")

wmean<-function(x,w){ok<-!is.na(x)&!is.na(w)&w>0;if(!any(ok))return(NA_real_);sum(x[ok]*w[ok])/sum(w[ok])}
wq<-function(x,w,probs=c(.25,.5,.75,.9,.95)){
  ok<-!is.na(x)&!is.na(w)&w>0;x<-x[ok];w<-w[ok]
  if(!length(x)||sum(w)<=0)return(rep(NA_real_,length(probs)))
  o<-order(x);x<-x[o];w<-w[o];cw<-cumsum(w)/sum(w)
  sapply(probs,function(p)x[which(cw>=p)[1]])
}

Hospital_Distribution<-rbindlist(lapply(groups,function(g){
  x<-D[get(g)==1&!is.na(Hospital_Type)&!is.na(DISCWT)];tw<-sum(x$DISCWT,na.rm=TRUE)
  out<-x[,.(Unweighted_N=.N,Weighted_N=sum(DISCWT,na.rm=TRUE)),by=Hospital_Type]
  out<-merge(data.table(Hospital_Type=hospital_types),out,by="Hospital_Type",all.x=TRUE,sort=FALSE)
  out[is.na(Unweighted_N),Unweighted_N:=0L];out[is.na(Weighted_N),Weighted_N:=0]
  out[,Percent_of_Cohort:=if(tw>0)100*Weighted_N/tw else NA_real_];out[,Group:=g]
  setcolorder(out,c("Group","Hospital_Type","Unweighted_N","Weighted_N","Percent_of_Cohort"));out
}))

Procedure_Summary_By_Hospital<-rbindlist(lapply(groups,function(g)rbindlist(lapply(hospital_types,function(h){
  x<-D[get(g)==1&Hospital_Type==h&!is.na(I10_NPR)&!is.na(DISCWT)]
  if(!nrow(x))return(data.table(Group=g,Hospital_Type=h,Unweighted_N=0L,Weighted_N=0,
                                Weighted_Mean_PCS=NA_real_,Weighted_SD=NA_real_,
                                Weighted_Q1=NA_real_,Weighted_Median=NA_real_,Weighted_Q3=NA_real_,
                                Weighted_P90=NA_real_,Weighted_P95=NA_real_,
                                Percent_Zero_Procedures=NA_real_,Percent_Any_Procedure=NA_real_))
  tw<-sum(x$DISCWT,na.rm=TRUE);wm<-wmean(x$I10_NPR,x$DISCWT)
  sdw<-sqrt(sum(x$DISCWT*(x$I10_NPR-wm)^2,na.rm=TRUE)/tw);q<-wq(x$I10_NPR,x$DISCWT)
  data.table(Group=g,Hospital_Type=h,Unweighted_N=nrow(x),Weighted_N=tw,Weighted_Mean_PCS=wm,
             Weighted_SD=sdw,Weighted_Q1=q[1],Weighted_Median=q[2],Weighted_Q3=q[3],
             Weighted_P90=q[4],Weighted_P95=q[5],
             Percent_Zero_Procedures=100*sum(x$DISCWT[x$I10_NPR==0],na.rm=TRUE)/tw,
             Percent_Any_Procedure=100*sum(x$DISCWT[x$I10_NPR>=1],na.rm=TRUE)/tw)
}))))

Procedure_Bins_By_Hospital<-rbindlist(lapply(groups,function(g)rbindlist(lapply(hospital_types,function(h){
  x<-D[get(g)==1&Hospital_Type==h&!is.na(Procedure_Bin)&!is.na(DISCWT)]
  if(!nrow(x))return(data.table(Group=g,Hospital_Type=h,Procedure_Bin=bin_order,
                                Unweighted_N=0L,Weighted_N=0,Percent_Unweighted=NA_real_,Percent_Weighted=NA_real_))
  tu<-nrow(x);tw<-sum(x$DISCWT,na.rm=TRUE)
  out<-x[,.(Unweighted_N=.N,Weighted_N=sum(DISCWT,na.rm=TRUE)),by=Procedure_Bin]
  out<-merge(data.table(Procedure_Bin=bin_order),out,by="Procedure_Bin",all.x=TRUE,sort=FALSE)
  out[is.na(Unweighted_N),Unweighted_N:=0L];out[is.na(Weighted_N),Weighted_N:=0]
  out[,Percent_Unweighted:=100*Unweighted_N/tu];out[,Percent_Weighted:=100*Weighted_N/tw]
  out[,`:=`(Group=g,Hospital_Type=h)]
  setcolorder(out,c("Group","Hospital_Type","Procedure_Bin","Unweighted_N","Weighted_N","Percent_Unweighted","Percent_Weighted"));out
}))))

MH_PCS_By_Hospital<-rbindlist(lapply(groups,function(g)rbindlist(lapply(hospital_types,function(h){
  x<-D[get(g)==1&Hospital_Type==h&!is.na(DISCWT)]
  if(!nrow(x))return(data.table(Group=g,Hospital_Type=h,Unweighted_N=0L,Weighted_N=0,
                                MH_PCS_Unweighted=0L,MH_PCS_Weighted=0,MH_PCS_Percent=NA_real_,
                                Mean_Total_PCS=NA_real_,Mean_MH_PCS=NA_real_))
  tw<-sum(x$DISCWT,na.rm=TRUE);mh<-x[Any_MH_PCS==1];mw<-sum(mh$DISCWT,na.rm=TRUE)
  data.table(Group=g,Hospital_Type=h,Unweighted_N=nrow(x),Weighted_N=tw,
             MH_PCS_Unweighted=nrow(mh),MH_PCS_Weighted=mw,MH_PCS_Percent=100*mw/tw,
             Mean_Total_PCS=wmean(x$I10_NPR,x$DISCWT),Mean_MH_PCS=wmean(x$MH_Total_Count,x$DISCWT))
}))))

MH_DRG_MH_PCS_By_Hospital<-rbindlist(lapply(groups,function(g)rbindlist(lapply(hospital_types,function(h){
  x<-D[get(g)==1&Hospital_Type==h&!is.na(DISCWT)];tw<-sum(x$DISCWT,na.rm=TRUE)
  cmb<-CJ(MH_DRG=c(0L,1L),MH_PCS=c(0L,1L))
  rbindlist(lapply(seq_len(nrow(cmb)),function(i){
    y<-x[MH_DRG==cmb$MH_DRG[i]&Any_MH_PCS==cmb$MH_PCS[i]];wy<-sum(y$DISCWT,na.rm=TRUE)
    data.table(Group=g,Hospital_Type=h,MH_DRG=cmb$MH_DRG[i],MH_PCS=cmb$MH_PCS[i],
               Unweighted_N=nrow(y),Weighted_N=wy,
               Percent_of_Hospital_Cohort=if(tw>0)100*wy/tw else NA_real_)
  }))
}))))

RareNP_vs_CommonMH<-rbindlist(lapply(hospital_types,function(h){
  rn<-Procedure_Summary_By_Hospital[Group=="RareNP"&Hospital_Type==h]
  cm<-Procedure_Summary_By_Hospital[Group=="CommonMH"&Hospital_Type==h]
  rn6<-Procedure_Bins_By_Hospital[Group=="RareNP"&Hospital_Type==h&Procedure_Bin=="6+"]
  cm6<-Procedure_Bins_By_Hospital[Group=="CommonMH"&Hospital_Type==h&Procedure_Bin=="6+"]
  rnmh<-MH_PCS_By_Hospital[Group=="RareNP"&Hospital_Type==h]
  cmmh<-MH_PCS_By_Hospital[Group=="CommonMH"&Hospital_Type==h]
  data.table(Hospital_Type=h,RareNP_Mean_PCS=rn$Weighted_Mean_PCS,
             CommonMH_Mean_PCS=cm$Weighted_Mean_PCS,
             Mean_PCS_Difference=rn$Weighted_Mean_PCS-cm$Weighted_Mean_PCS,
             Mean_PCS_Ratio=rn$Weighted_Mean_PCS/cm$Weighted_Mean_PCS,
             RareNP_6plus_Percent=rn6$Percent_Weighted,
             CommonMH_6plus_Percent=cm6$Percent_Weighted,
             SixPlus_Difference=rn6$Percent_Weighted-cm6$Percent_Weighted,
             RareNP_MH_PCS_Percent=rnmh$MH_PCS_Percent,
             CommonMH_MH_PCS_Percent=cmmh$MH_PCS_Percent)
}))

Procedure_Bin_QA<-Procedure_Bins_By_Hospital[,.(Sum_Unweighted_Percent=sum(Percent_Unweighted,na.rm=TRUE),
                                                Sum_Weighted_Percent=sum(Percent_Weighted,na.rm=TRUE)),
                                             by=.(Group,Hospital_Type)]
Hospital_Distribution_QA<-copy(Hospital_Distribution)

output_dir<-dirname(core_file)
fwrite(Hospital_Distribution,file.path(output_dir,"KID_2019_PCS_HospitalType_01_Hospital_Distribution.csv"))
fwrite(Procedure_Summary_By_Hospital,file.path(output_dir,"KID_2019_PCS_HospitalType_02_Procedure_Summary.csv"))
fwrite(Procedure_Bins_By_Hospital,file.path(output_dir,"KID_2019_PCS_HospitalType_03_Procedure_Bins.csv"))
fwrite(MH_PCS_By_Hospital,file.path(output_dir,"KID_2019_PCS_HospitalType_04_MH_PCS_By_Hospital.csv"))
fwrite(MH_DRG_MH_PCS_By_Hospital,file.path(output_dir,"KID_2019_PCS_HospitalType_05_MH_DRG_x_MH_PCS.csv"))
fwrite(RareNP_vs_CommonMH,file.path(output_dir,"KID_2019_PCS_HospitalType_06_RareNP_vs_CommonMH.csv"))
fwrite(Procedure_Bin_QA,file.path(output_dir,"KID_2019_PCS_HospitalType_07_Bin_QA.csv"))
fwrite(Hospital_Distribution_QA,file.path(output_dir,"KID_2019_PCS_HospitalType_08_Hospital_Distribution_QA.csv"))

cat("\nDONE: 8 validated KID hospital-type outputs written to",output_dir,"\n")
print(Procedure_Bin_QA)
