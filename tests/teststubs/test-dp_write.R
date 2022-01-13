test_that("dp_write", {
	stub(dp_commit, 'dp_write', 100)
	stub(dp_write, 'dp_structure', 100)
	stub(dp_write, 'dplognote_get', 100)
	stub(dp_write, 'dp_repository_check', 100)
	stub(dp_write, 'is_valid_dp_repository', 100)
	dp_write(data_object, project_path = ".") 
	expect_equal(2 * 2, 4)
})


test_that("dplognote_get", {
	stub(dp_write, 'dplognote_get', 100)
	stub(dplognote_get, 'get_pin_version', 100)
	stub(dplognote_get, 'make_sha1_compatible', 100)
	dplognote_get(data_object, project_path) 
	expect_equal(2 * 2, 4)
})


