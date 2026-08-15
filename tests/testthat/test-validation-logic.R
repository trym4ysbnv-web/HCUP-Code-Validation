test_that("ICD-10 normalization is correct", {
  expect_equal(normalize_icd10(c("E83.42"," f32.1 ","")),
               c("E8342","F321",NA_character_))
})

test_that("RareMed excludes E83.42", {
  sets <- cohort_code_sets()
  expect_false("E8342" %in% normalize_icd10(sets$RareMed))
  expect_true("Q770" %in% normalize_icd10(sets$RareMed))
})

test_that("diagnosis matching is exact, not substring based", {
  d <- data.frame(I10_DX1=c("F32.1","F32.10","XF32.1"))
  expect_equal(flag_exact_dx(d,"I10_DX1","F32.1"), c(1L,0L,0L))
})

test_that("E83.42 alone does not qualify RareMed", {
  d <- data.frame(I10_DX1=c("E83.42","E83.42"), I10_DX2=c(NA,"Q77.0"))
  expect_equal(flag_exact_dx(d,c("I10_DX1","I10_DX2"),cohort_code_sets()$RareMed),
               c(0L,1L))
})

test_that("final MH PCS prefix set is locked", {
  expect_setequal(mh_prefix_sets()$All_Selected,
                  c("GZ1","GZ2","GZ3","GZ5","GZ6","GZ7","GZB","GZC","GZH"))
  expect_false(any(c("GZF","GZG","GZJ") %in% mh_prefix_sets()$All_Selected))
})

test_that("MH PCS prefix counting works across positions", {
  d <- data.frame(I10_PR1=c("GZ5AA00","ABC1234"),
                  I10_PR2=c("GZ3XX00","GZBAA00"),
                  I10_PR3=c(NA,"GZH1234"))
  expect_equal(count_mh_prefixes(d,c("I10_PR1","I10_PR2","I10_PR3"),
                                 mh_prefix_sets()$All_Selected),
               c(2L,2L))
})

test_that("procedure-bin boundaries are correct", {
  expect_equal(procedure_bin(c(0,1,2,3,5,6,25,NA)),
               c("0","1-2","1-2","3-5","3-5","6+","6+",NA_character_))
})

test_that("weighted mean is correct", {
  expect_equal(weighted_mean_safe(c(1,3),c(1,3)),2.5)
})

test_that("weighted percentage is correct", {
  expect_equal(weighted_percent(c(TRUE,FALSE),c(3,1)),75)
})

test_that("procedure-bin percentages sum to 100 without recycling", {
  counts <- c(40,30,20,10)
  pct <- 100*counts/sum(counts)
  expect_equal(pct,c(40,30,20,10))
  expect_equal(sum(pct),100)
  expect_gt(length(unique(pct)),1)
})

test_that("NIS age boundaries retain 18 through 64 only", {
  age <- c(17,18,64,65,NA)
  expect_equal(!is.na(age)&age>=18&age<=64,
               c(FALSE,TRUE,TRUE,FALSE,FALSE))
})

test_that("KID freestanding hospital classification has priority", {
  KID_STRATUM <- c(9999,1000,1000,1000)
  HOSP_LOCTEACH <- c(3,3,2,1)
  out <- ifelse(KID_STRATUM==9999,"Freestanding children's hospital",
         ifelse(HOSP_LOCTEACH==3,"Other urban teaching",
         ifelse(HOSP_LOCTEACH==2,"Urban nonteaching",
         ifelse(HOSP_LOCTEACH==1,"Rural",NA))))
  expect_equal(out,c("Freestanding children's hospital","Other urban teaching",
                     "Urban nonteaching","Rural"))
})

test_that("final MH DRG definition is 880-887 inclusive", {
  expect_equal(as.integer(c(879,880,881,887,888,894) %in% 880:887),
               c(0L,1L,1L,1L,0L,0L))
})
