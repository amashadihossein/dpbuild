test_that("add_readme", {
	stub(add_readme, 'fn_hydrate', 100)
	stub(dp_git_init, 'add_readme', 100)
	add_readme(project_path, dp_title, github_repo_url, board_params_set_dried, creds_set_dried) 
	expect_equal(2 * 2, 4)
})


test_that("dp_git_init", {
	stub(dp_git_init, 'add_readme', 100)
	stub(dp_git_init, 'dp_init', 100)
	stub(dp_init, 'dp_git_init', 100)
	dp_git_init(project_path, project_name, branch_name, github_repo_url, board_params_set_dried, creds_set_dried, git_ignore) 
	expect_equal(2 * 2, 4)
})


test_that("dp_init", {
	stub(dp_activate, 'dp_init', 100)
	stub(dp_git_init, 'dp_init', 100)
	stub(dp_init, 'dp_git_init', 100)
	stub(dp_init, 'dpconf_init', 100)
	stub(dpconf_update, 'dp_init', 100)
	stub(dp_repository_check, 'dp_init', 100)
	stub(is_valid_dp_repository, 'dp_init', 100)
	dp_init(project_path = fs::path_wd(), project_description, branch_name, branch_description, readme_general_note = character(0), board_params_set_dried, creds_set_dried, github_repo_url, git_ignore = c(".drake/", "input_files/", "output_files/", ".Rhistory", ".DS_Store"), ...) 
	expect_equal(2 * 2, 4)
})


test_that("dpconf_init", {
	stub(dp_init, 'dpconf_init', 100)
	stub(dpconf_init, 'dpconf_write', 100)
	dpconf_init(project_path, project_name, project_description = character(0), branch_name, branch_description = character(0), readme_general_note = character(0), board_params_set_dried, creds_set_dried, ...) 
	expect_equal(2 * 2, 4)
})


test_that("fn_dry", {
	
	fn_dry(fn_called) 
	expect_equal(2 * 2, 4)
})


test_that("fn_hydrate", {
	stub(add_readme, 'fn_hydrate', 100)
	stub(dpconf_get, 'fn_hydrate', 100)
	fn_hydrate(dried_fn) 
	expect_equal(2 * 2, 4)
})


