test_that("dp_activate", {
	stub(dp_activate, 'dp_init', 100)
	stub(dp_activate, 'is_valid_dp_repository', 100)
	dp_activate(project_path = .) 
	expect_equal(2 * 2, 4)
})


