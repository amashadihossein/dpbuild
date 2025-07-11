testthat::test_that("fn_hydrate output matches specified environment variables", {
  withr::local_envvar(.new=list(
    "AWS_ACCESS_KEY_ID" = "abc",
    "AWS_SECRET_ACCESS_KEY" = "defgh"
  ))
  expected_output <- data.frame(
    profile_name = "",
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY")
  )
  class(expected_output) <- c("aws_creds", "data.frame")
  testthat::expect_equal(
    fn_hydrate(
      fn_dry(
        creds_set_aws(key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
      )
    ),
    expected_output
  )
})
