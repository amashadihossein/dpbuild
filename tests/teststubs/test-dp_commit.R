test_that("dp_commit", {
	stub(dp_commit, 'dp_write', 100)
	stub(dp_commit, 'dp_repository_check', 100)
	stub(dp_commit, 'is_valid_dp_repository', 100)
	dp_commit(project_path = fs::path_wd(), commit_description) 
	expect_equal(2 * 2, 4)
})


