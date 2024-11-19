
test_that("properly checks valid repository ", {
  local_mocked_bindings(is_valid_dp_repository = function(path) FALSE )
  expect_snapshot(error = TRUE, {
    data_object = list()
    path <- '.'
    dp_write(data_object, project_path = path)
  })
})

test_that("properly checks data object class", {
    local_mocked_bindings(is_valid_dp_repository = function(path) TRUE )
    expect_snapshot(error = TRUE, {
    data_object = list()
    path <- withr::local_tempfile()
    dp_write(data_object, project_path = path)
  })
})

test_that("properly creates data object paths", {
    path = withr::local_tempfile()
    data_object = structure(list(), class = "dp")
    local_mocked_bindings(is_valid_dp_repository = function(path) TRUE)
    expect_equal(
                save_object(data_object, path, type = 'rds'), 
                file.path(path, "output_files/RDS_format/data_object.RDS"))
    expect_equal(
                save_object(data_object, path, type = 'qs'), 
                file.path(path, "output_files/qs_format/data_object.qs"))

})