context("remove rows full of NAs")

# tests -------------------------------------------------------------------

df <- tibble::tribble(
  ~x, ~y, ~z,
  1, "a", FALSE,
  NA, NA, NA,
  5, "b", TRUE,
  NA, NA, NA,
  NA, "c", NA
)

test_that("is_na_row outputs a logical vector of lengh equal to n of rows", {
  expect_type(is_na_row(df), "logical")
  expect_length(is_na_row(df), nrow(df))
  df <- as.matrix(df)
  expect_type(is_na_row(df), "logical")
  expect_length(is_na_row(df), nrow(df))
})

test_that("rm_na_row output is of same type as input", {
  expect_true(is.data.frame(rm_na_row(df)))
  expect_false(
    df %>% 
      as.matrix() %>% 
      rm_na_row() %>% 
      is.data.frame()
  )
  expect_true(
    df %>% 
      as.matrix() %>% 
      rm_na_row() %>% 
      is.matrix()
  )
})

test_that("is_na_row detects rows in a data frame full of NAs", {
  na_rows_manual <- c(2, 4)
  na_rows_fun <-   df %>% is_na_row() %>% which()
  expect_equal(na_rows_fun, na_rows_manual)
})


test_that("rm_na_row removes only rows full of NAs, not partial", {
  df <- tibble::tribble(
    ~x, ~y, ~z,
    1, "a", FALSE,
    NA, NA, NA,
    5, "b", TRUE,
    NA, NA, NA,
    NA, "c", NA
  )
  expect_equal(nrow(rm_na_row(df)), 3)
})
