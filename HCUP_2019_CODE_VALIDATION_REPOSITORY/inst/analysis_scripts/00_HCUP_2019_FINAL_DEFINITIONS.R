# ================================================================
# HCUP 2019 FINAL COHORT AND MH-PCS DEFINITIONS
# Canonical definitions used in final procedure analyses
# ================================================================

norm_code <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\.", "", x)
  x[x %in% c("", "NA")] <- NA_character_
  x
}

RareNP_codes <- c(
  "E80.21","E71.522","G31.86","Q93.51","E72.21","I67.850",
  "E71.520","Q79.61","D82.1","Q79.60","E75.21","G43.401",
  "G43.409","Q99.2","G11.11","G31.01","E75.22","E74.810",
  "E72.02","E76.1","E76.01","Q79.62","Q93.59","Q98.4",
  "E75.23","G40.309","G40.C01","G40.C09","G40.C11","G40.C19",
  "G31.82","G93.44","Q87.4","D47.02","E88.41","E75.25",
  "E71.120","E76.21","E72.12","E75.4","E75.24","Q79.69",
  "Q93.52","Q87.11","F84.2","E75.01","E76.22","F78.A1",
  "E75.02","Q77.1","Q85.1","Q79.63","Q93.82","E83.01",
  "E34.8","E71.528","E71.529"
)

RareMed_codes <- c(
  "Q77.4","D81.30","E71.521","E88.01","Q87.81","G12.21",
  "D68.61","E72.22","Q61.2","E26.81","M35.2","D47.Z2",
  "G11.3","G60.0","Q77.3","E72.23","E74.03","M34.1",
  "E84.11","E84.19","E84.8","E84.0","E84.9","E72.01",
  "G71.01","Q81.0","Q81.2","G71.02","D68.1","E74.21",
  # E83.42 intentionally excluded
  "I78.0","G11.4","D58.0","E85.2","E72.11","Q82.3",
  "H49.81","E85.81","I45.81","E71.0","E74.04","E88.42",
  "G71.11","E71.511","Q61.5","Q85.01","Q85.02","Q78.0",
  "M34.89","D59.5","E70.0","E74.02","E71.121","E20.1",
  "H35.52","Q85.03","D81.0","D81.1","D81.2","G12.0",
  "M34.81","M34.82","M34.83","Q96.9","E74.01","Q85.83",
  "D68.0","D82.0","Q82.1","E71.510"
)

CommonMH_codes <- c(
  "F90.0","F90.1","F90.2","F84.0","F32.0","F32.1",
  "F32.2","F32.3","F33.1","F41.1","F33.3","F33.4",
  "F43.12","F20.0","F20.1","F20.2","F20.3","F42.2","F42.8"
)

CommonMed_codes <- c(
  "N18.1","N18.2","N18.30","N18.31","N18.32","N18.4","N18.5",
  "J44.0","J44.1","J44.89","J44.9","I50.9",
  "I25.10","I25.110","I25.119",
  "G35.A","G35.B1","G35.B2",
  "G20.A1","G20.A2","G20.B1","G20.B2",
  "E10.9"
)

MH_CRISIS <- c("GZ2")
MH_THERAPY <- c("GZ5","GZ6","GZ7","GZH")
MH_ASSESSMENT <- c("GZ1","GZC")
MH_MEDICAL <- c("GZ3","GZB")
MH_PREFIXES <- c("GZ1","GZ2","GZ3","GZ5","GZ6","GZ7","GZB","GZC","GZH")

prefix_labels <- c(
  GZ1 = "Psychological Tests",
  GZ2 = "Crisis Intervention",
  GZ3 = "Medication Management",
  GZ5 = "Individual Psychotherapy",
  GZ6 = "Counseling",
  GZ7 = "Family Psychotherapy",
  GZB = "Electroconvulsive Therapy",
  GZC = "Biofeedback",
  GZH = "Group Psychotherapy"
)

groups <- c("CommonMed","CommonMH","RareMed","RareNP")
bin_order <- c("0","1-2","3-5","6+")

wmean_safe <- function(x,w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok]) / sum(w[ok])
}

weighted_quantile <- function(x,w,probs=c(.25,.50,.75,.90,.95)) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x) || sum(w)<=0) return(rep(NA_real_,length(probs)))
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w)/sum(w)
  sapply(probs,function(p) x[which(cw>=p)[1]])
}
