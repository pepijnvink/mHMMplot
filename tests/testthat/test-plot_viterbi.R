viterbi <- readRDS(test_path("fixtures", "viterbi.rds"))
test_that("Plotting works",{
  expect_s3_class(plot_viterbi(viterbi), "ggplot")
  expect_s3_class(plot_viterbi(viterbi,
                               color = c("red", "green"),
                               ID = 1:10), "ggplot")
  expect_s3_class(plot_viterbi(viterbi,
                               ID = 1,
                               state_labels = c("red", "green")), "ggplot")
  expect_s3_class(plot_viterbi(viterbi,
                               ID = 1,
                               state_labels = c("red", "green", "blue")), "ggplot")
}
                    )
test_that("Warning and errors",{
  expect_warning(plot_viterbi(viterbi,
                              ID = 1,
                              state_labels = c("red", "green", "blue")))
}
          )
