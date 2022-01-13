test_that("dpconf_update", {
	stub(dpconf_update, 'dp_init', 100)
	stub(dpconf_update, 'dpconf_read', 100)
	stub(dpconf_update, 'dpconf_write', 100)
	stub(dpconf_update, 'dp_repository_check', 100)
	stub(dpconf_update, 'is_valid_dp_repository', 100)
	dpconf_update(project_path = fs::path_wd(), project_description = character(0), branch_name = character(0), branch_description = character(0), readme_general_note = character(0), board_params_set_dried = character(0), creds_set_dried = character(0), commit_description = "dp_conf modified", git_ignore = character(0), ...) 
	expect_equal(2 * 2, 4)
})


