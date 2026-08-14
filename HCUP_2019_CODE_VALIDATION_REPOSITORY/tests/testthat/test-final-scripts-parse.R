test_that("all archived final R scripts parse", {
  d <- system.file("analysis_scripts",package="HCUPCodeValidation")
  scripts <- list.files(d,pattern="\\.R$",full.names=TRUE)
  expect_gte(length(scripts),4)
  for(f in scripts) expect_error(parse(file=f),NA,info=basename(f))
})
