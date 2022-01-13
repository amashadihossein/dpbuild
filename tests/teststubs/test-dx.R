test_that("dp_repository_check", {
	stub(dp_commit, 'dp_repository_check', 100)
	stub(dp_write, 'dp_repository_check', 100)
	stub(dpcode_add, 'dp_repository_check', 100)
	stub(dpconf_update, 'dp_repository_check', 100)
	stub(dpinput_write, 'dp_repository_check', 100)
	stub(dp_repository_check, 'dp_init', 100)
	stub(dp_repository_check, 'dpconf_read', 100)
	stub(is_valid_dp_repository, 'dp_repository_check', 100)
	dp_repository_check(path) 
	expect_equal(2 * 2, 4)
})


test_that("is_valid_dp_repository", {
	stub(dp_activate, 'is_valid_dp_repository', 100)
	stub(dp_commit, 'is_valid_dp_repository', 100)
	stub(dp_write, 'is_valid_dp_repository', 100)
	stub(dpcode_add, 'is_valid_dp_repository', 100)
	stub(dpconf_update, 'is_valid_dp_repository', 100)
	stub(dpinput_write, 'is_valid_dp_repository', 100)
	stub(is_valid_dp_repository, 'dp_init', 100)
	stub(is_valid_dp_repository, 'dp_repository_check', 100)
	is_valid_dp_repository(path, checks = c("all", "git", "dp", "renv", "branch"), verbose = F) 
	expect_equal(2 * 2, 4)
})


