testthat::test_that("This would expect the error with creds_set_aws in fn_dry function",{
  testthat::expect_error(
    fn_dry(
      creds_set_aws(key = "98huyIAQ6IBZYNWDP5KS", secret = "BQLvB3ofdHL5eMZlcXM5A397hfk97h!R")
    )
  )
})

testthat::test_that("This would expect the equality in fn_dry function",{
  testthat::expect_equal(
    fn_dry(
      creds_set_aws(key = Sys.getenv("AWS_KEY"), secret = Sys.getenv("AWS_SECRET"))
    ),
    "creds_set_aws(key = Sys.getenv(\"AWS_KEY\"), secret = Sys.getenv(\"AWS_SECRET\"))"
  )
})

testthat::test_that("This would expect the error with creds_set_labkey in fn_dry function",{
  testthat::expect_error(
    fn_dry(
      creds_set_labkey(api_key = "98huyIAQ6IBZYNWDP5KS")
    )
  )
})

testthat::test_that("This would expect the equality in fn_dry function",{
  testthat::expect_equal(
    fn_dry(
      creds_set_labkey(api_key = Sys.getenv("jedi-test"))
    ),
    "creds_set_labkey(api_key = Sys.getenv(\"jedi-test\"))"
  )
})


testthat::test_that("This would test if labkey api credentials from keyring pass in fn_dry function",{
  testthat::expect_equal(
    fn_dry(
      creds_set_labkey(keyring::key_get(service = "jedi-test", keyring="remote"))
    ),
    "creds_set_labkey(keyring::key_get(service = \"jedi-test\", keyring = \"remote\"))"
  )
})

testthat::test_that("This would test if aws credentials from keyring pass in fn_dry function",{
  testthat::expect_equal(
    fn_dry(
      creds_set_aws(keyring::key_get(service = "AWS_KEY", keyring="remote"))
    ),
    "creds_set_aws(keyring::key_get(service = \"AWS_KEY\", keyring = \"remote\"))"
  )
})
