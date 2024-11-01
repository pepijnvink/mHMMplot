viterbi <- readRDS(test_path("fixtures", "viterbi.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
test_that("Plotting works",{
  expect_s3_class(plot_viterbi(viterbi), "ggplot")
  expect_s3_class(plot_viterbi(viterbi,
                               ID = 1:10), "ggplot")
  expect_s3_class(plot_viterbi(viterbi,
                               ID = 1), "ggplot")
}
                    )
test_that("Warning and errors",{
  expect_error(plot_viterbi(out_2st_cat))
}
          )
