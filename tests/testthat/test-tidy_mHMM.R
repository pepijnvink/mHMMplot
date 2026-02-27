## Test continuous data
out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
test_that('Gives output for continuous', {
  expect_s3_class(tidy_mHMM(out_3st_cont), 'tbl_df')
  expect_s3_class(
    tidy_mHMM(out_3st_cont, param = 'gamma', level = 'subject'),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(out_3st_cont, param = 'emiss', level = 'subject'),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_3st_cont,
      param = 'emiss',
      level = 'subject',
      quantiles = c(0.025, 0.05, 0.95, 0.975),
      subjects = 1:5
    ),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_3st_cont,
      param = 'emiss',
      level = 'group',
      quantiles = c(0.025, 0.05, 0.95, 0.975)
    ),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_3st_cont,
      param = 'gamma',
      level = 'subject',
      quantiles = c(0.025, 0.05, 0.95, 0.975),
      subjects = 1:5
    ),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_3st_cont,
      param = 'gamma',
      level = 'group',
      quantiles = c(0.025, 0.05, 0.95, 0.975)
    ),
    'tbl_df'
  )
})
test_that('Gives warning or error for continuous', {
  expect_warning(tidy_mHMM(out_3st_cont, level = 'sdfkjh'))
  expect_warning(tidy_mHMM(out_3st_cont, param = 'sdfkjh'))
  expect_warning(tidy_mHMM(out_3st_cont, level = 'subject', subjects = 1:100))
  expect_warning(tidy_mHMM(out_3st_cont, level = 'subject', subjects = -1:10))
  expect_warning(tidy_mHMM(
    out_3st_cont,
    level = 'subject',
    subjects = c("a", "b")
  ))
  expect_error(tidy_mHMM(out_3st_cont, quantiles = c('a', 'b')))
  expect_error(tidy_mHMM(out_3st_cont, quantiles = c(-1, 1)))
})

# test categorical data
out_2st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
test_that('Gives output for categorical model', {
  expect_s3_class(tidy_mHMM(out_2st_cat), 'tbl_df')
  expect_s3_class(
    tidy_mHMM(out_2st_cat, param = 'gamma', level = 'subject'),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(out_2st_cat, param = 'emiss', level = 'subject'),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_2st_cat,
      param = 'gamma',
      level = 'subject',
      quantiles = c(0.025, 0.05, 0.95, 0.975)
    ),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_2st_cat,
      param = 'emiss',
      level = 'subject',
      quantiles = c(0.025, 0.05, 0.95, 0.975),
      subjects = 1:5
    ),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_2st_cat,
      param = 'gamma',
      level = 'subject',
      quantiles = c(0.025, 0.05, 0.95, 0.975),
      subjects = 1:5
    ),
    'tbl_df'
  )
  expect_s3_class(
    tidy_mHMM(
      out_2st_cat,
      param = 'gamma',
      level = 'group',
      quantiles = c(0.025, 0.05, 0.95, 0.975)
    ),
    'tbl_df'
  )
})

test_that('Gives warning or error for categorical', {
  expect_warning(tidy_mHMM(out_2st_cat, level = 'sdfkjh'))
  expect_warning(tidy_mHMM(out_2st_cat, param = 'sdfkjh'))
  expect_warning(tidy_mHMM(out_2st_cat, level = 'subject', subjects = 1:100))
  expect_warning(tidy_mHMM(out_2st_cat, level = 'subject', subjects = -1:10))
  expect_warning(tidy_mHMM(
    out_2st_cat,
    level = 'subject',
    subjects = c("a", "b")
  ))
  expect_error(tidy_mHMM(out_2st_cat, quantiles = c('a', 'b')))
  expect_error(tidy_mHMM(out_2st_cat, quantiles = c(-1, 1)))
})
