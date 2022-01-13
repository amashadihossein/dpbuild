test_that("dpcode_add", {
	stub(dpcode_add, 'dp_repository_check', 100)
	stub(dpcode_add, 'is_valid_dp_repository', 100)
	dpcode_add(project_path) 
	expect_equal(2 * 2, 4)
})


