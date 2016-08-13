
context("ntile")

test_that("ntile",
{
  expect_equal(ntile(c(-10:10, rep(0, 7)), 4, checkBleed = FALSE),
               structure(list(`1` = c(-10, -9, -8, -7, -6, -5, -4),
                              `2` = c(-3, -2, -1, 0, 0, 0, 0),
                              `3` = c(1, 2, 3, 0, 0, 0, 0),
                              `4` = c(4, 5, 6, 7, 8, 9, 10)),
                         .Names = c("1", "2", "3", "4")))
  expect_equal(ntile(c(1:4, NA), 1, na.rm=TRUE, result="numeric"),
               c(1L, 1L, 1L, 1L))
  expect_equal(ntile(setNames(state.area, state.name), 10, result="factor", reverse=TRUE),
               structure(c(5L, 10L, 9L, 5L, 10L, 9L, 1L, 1L, 6L, 6L, 1L, 8L,
                           6L, 3L, 6L, 8L, 3L, 4L, 3L, 2L, 2L, 6L, 8L, 4L, 7L, 10L, 8L,
                           9L, 2L, 1L, 10L, 5L, 5L, 7L, 4L, 7L, 9L, 4L, 1L, 3L, 7L, 4L,
                           10L, 8L, 2L, 3L, 7L, 2L, 5L, 9L),
                         .Names = c("Alabama", "Alaska",
                                    "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                                    "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                                    "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                                    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                    "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                                    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
                                    "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                    "Wyoming"),
                         .Label = c("10", "9", "8", "7", "6", "5", "4", "3",  "2", "1"),
                         class = c("ordered", "factor")))
})

