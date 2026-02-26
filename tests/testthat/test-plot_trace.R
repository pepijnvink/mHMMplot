out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
out_2st_cat_2 <- readRDS(test_path("fixtures", "mhmm_cat_2.rds"))
test_that("Categorical, group level", {
  expect_s3_class(plot_trace(out_2st_cat, component = "gamma", param = 'prob'), "ggplot")
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "gamma",
      param = "varint"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      param = "varint"
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_s3_class(
    plot_trace(out_2st_cat, component = "emiss", vrb = "p_looking", param = 'prob'),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      param = "varint",
      vrb = "p_looking"
    ),
    "ggplot"
  )
})
test_that("Categorical Subject-level", {
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "gamma",
      level = "subject",
      param = 'prob',
      subject = 1
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "gamma",
      level = "subject",
      param = 'int',
      subject = 1
    ),
    "ggplot"
  )

  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      level = "subject",
      vrb = "p_looking",
      param = 'int',
      subject = 1
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      level = "subject",
      vrb = "p_looking",
      param = 'prob',
      subject = 1
    ),
    "ggplot"
  )
})
test_that("Continuous", {
  expect_s3_class(
    plot_trace(out_3st_cont, component = "emiss", param = 'mu', vrb = "observation 1"),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "emiss",
      param = "varmu",
      vrb = "observation 1"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "emiss",
      param = "sd",
      vrb = "observation 1"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "emiss",
      level = "subject",
      param = 'mu',
      vrb = "observation 1",
      subject = 1
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      param = 'prob'
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      param = 'int'
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      param = 'varint'
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      param = 'int',
      level = 'subject',
      subject = 1
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      param = 'int',
      level = 'subject',
      subject = 1
    ),
    "ggplot"
  )
})
test_that("Other errors", {
  expect_error(plot_trace(c(1, 2)))
})
