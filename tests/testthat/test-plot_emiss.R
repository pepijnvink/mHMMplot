out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
out_3st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
test_that("Obtain plots (continuous)", {
  expect_s3_class(
    plot_emiss(
      out_3st_cont,
      type = "bar",
      subject_effects = FALSE
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cont,
      type = "bar",
      subject_effects = TRUE,
      alpha = 0.5
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cont,
      type = "boxplot"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cont,
      type = "point"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cont,
      type = "point",
      subject_effects = TRUE,
      errorbar = "ci"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cont,
      type = "point",
      subject_effects = TRUE,
      errorbar = "sd"
    ),
    "ggplot"
  )
})
test_that("Obtain plots (categorical)", {
  expect_s3_class(
    plot_emiss(
      out_3st_cat,
      type = "bar",
      errorbar = "ci"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cat,
      type = "bar",
      subject_effects = TRUE,
      errorbar = "ci",
      vrb = "p_looking"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cat,
      type = "point",
      subject_effects = TRUE,
      errorbar = "ci",
      vrb = "p_looking"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_emiss(
      out_3st_cat,
      type = "bar",
      subject_effects = TRUE,
      errorbar = "ci",
      vrb = "t_looking"
    ),
    "ggplot"
  )
})

test_that("Other Error and warning messages", {
  expect_error(plot_emiss(c(1, 2)))
  expect_error(plot_emiss(
    out_3st_cat,
    type = "bar",
    errorbar = "sd", # cannot plot this for categorical data
    vrb = "p_vocalizing"
  ))
})
