
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
    unlink(file.path(path, "output_files/RDS_format/"), recursive = TRUE)
    expect_equal(
                save_object(data_object, path, type = 'qs'), 
                file.path(path, "output_files/qs_format/data_object.qs"))

})

test_that("properly errors when different format directory exists", {
  # Set up temporary project path
  path <- withr::local_tempdir()
  data_object <- structure(list(), class = "dp")
  
  # Mock validation function
  local_mocked_bindings(is_valid_dp_repository = function(path) TRUE)
  
  # Create directory structure for RDS format first
  dir.create(file.path(path, "output_files/RDS_format"), recursive = TRUE)
  
  # Now try to save as qs format, which should error
  expect_error(
    save_object(data_object, path, type = 'qs'),
    "Directory for RDS format already exists while trying to save as qs. Please try again with the existing daap format."
  )
  
  # Clean up and test the reverse case
  unlink(file.path(path, "output_files/RDS_format"), recursive = TRUE)
  dir.create(file.path(path, "output_files/qs_format"), recursive = TRUE)
  
  expect_error(
    save_object(data_object, path, type = 'rds'),
    "Directory for qs format already exists while trying to save as RDS. Please try again with the existing daap format."
  )
})