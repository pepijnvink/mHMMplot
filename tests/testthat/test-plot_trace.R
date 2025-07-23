out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
out_2st_cat_2 <- readRDS(test_path("fixtures", "mhmm_cat_2.rds"))
test_that("Categorical, group level", {
  expect_s3_class(plot_trace(out_2st_cat, component = "gamma"), "ggplot")
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "gamma",
      param = "var",
      prob = TRUE,
      state_labels = c("red", "green")
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "gamma",
      param = "var",
      prob = FALSE,
      state_labels = c("red", "green")
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(out_2st_cat, component = "emiss", vrb = "p_looking"),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      param = "var",
      vrb = "p_looking",
      cat_labels = c("a", "b"),
      state_labels = c("red", "green")
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      param = "var",
      vrb = "p_looking",
      cat_labels = c("a", "b"),
      state_labels = c("red", "green"),
      prob = TRUE
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      vrb = "p_looking",
      cat_labels = c("red", "green", "blue")
    ),
    "ggplot"
  ) %>%
    expect_warning()
})
test_that("Categorical Subject-level", {
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "gamma",
      level = "subject",
      prob = TRUE,
      state_labels = c("red", "green"),
      subject = 1
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "gamma",
      level = "subject",
      prob = FALSE,
      state_labels = c("red", "green"),
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
      cat_labels = c("a", "b"),
      state_labels = c("red", "green"),
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
      cat_labels = c("a", "b"),
      state_labels = c("red", "green", "blue"),
      subject = 1,
      prob = TRUE
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_s3_class(
    plot_trace(
      list(out_2st_cat, out_2st_cat_2),
      component = "emiss",
      level = "subject",
      vrb = "p_looking",
      cat_labels = c("a", "b"),
      state_labels = c("red", "green"),
      subject = 1,
      prob = TRUE
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      list(out_2st_cat, out_2st_cat_2),
      component = "emiss",
      level = "subject",
      vrb = "p_looking",
      cat_labels = c("a", "b"),
      state_labels = c("red", "green"),
      subject = 1,
      prob = FALSE
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      list(out_2st_cat, out_2st_cat_2),
      component = "gamma",
      level = "subject",
      cat_labels = c("a", "b"),
      state_labels = c("red", "green"),
      subject = 1,
      prob = FALSE
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_s3_class(
    plot_trace(
      list(out_2st_cat, out_2st_cat_2),
      component = "gamma",
      level = "subject",
      vrb = "p_looking",
      state_labels = c("red", "green"),
      subject = 1,
      prob = FALSE
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_s3_class(
    plot_trace(
      out_2st_cat,
      component = "emiss",
      level = "subject",
      param = "var",
      vrb = "p_looking",
      subject = 1
    ),
    "ggplot"
  ) %>%
    expect_warning()
})
test_that("Continuous", {
  expect_s3_class(
    plot_trace(out_3st_cont, component = "emiss", vrb = "observation 1"),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "emiss",
      param = "var",
      vrb = "observation 1",
      state_labels = c("red", "green", "blue")
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "emiss",
      param = "sd",
      vrb = "observation 1",
      state_labels = c("red", "green", "blue")
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "emiss",
      level = "subject",
      vrb = "observation 1",
      state_labels = c("red", "green", "blue"),
      subject = 1
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      state_labels = c("red", "green")
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      cat_labels = c("red", "green")
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_s3_class(
    plot_trace(
      out_3st_cont,
      component = "gamma",
      state_labels = c("red", "green")
    ),
    "ggplot"
  ) %>%
    expect_warning()
  expect_error(plot_trace(out_3st_cont, component = "gamma", level = "subject"))
})
test_that("Other errors", {
  expect_error(plot_trace(c(1, 2)))
})
