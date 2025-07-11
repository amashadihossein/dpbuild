testthat::test_that("Passing unobfuscated AWS keys directly to fn_dry(creds_set_aws()) raises an error", {
  testthat::expect_error(
    fn_dry(
      creds_set_aws(key = "abc", secret = "defg")
    )
  )
})

testthat::test_that("Passing unobfuscated Labkey API key directly to fn_dry(creds_set_labkey()) raises an error", {
  testthat::expect_error(
    fn_dry(
      creds_set_labkey(api_key = "abcd")
    )
  )
})

testthat::test_that("fn_dry output is properly deparsed", {
  testthat::expect_equal(
    fn_dry(
      creds_set_aws(key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
    ),
    "creds_set_aws(key = Sys.getenv(\"AWS_ACCESS_KEY_ID\"), secret = Sys.getenv(\"AWS_SECRET_ACCESS_KEY\"))"
  )
})
