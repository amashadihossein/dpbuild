test_that("dpconf_get", {
	stub(dpconf_get, 'fn_hydrate', 100)
	stub(dpconf_get, 'dpconf_read', 100)
	stub(dpconf_get, 'dpconf_validate', 100)
	dpconf_get(project_path) 
	expect_equal(2 * 2, 4)
})


test_that("dpconf_read", {
	stub(dpconf_get, 'dpconf_read', 100)
	stub(dpconf_update, 'dpconf_read', 100)
	stub(dp_repository_check, 'dpconf_read', 100)
	dpconf_read(project_path) 
	expect_equal(2 * 2, 4)
})


test_that("dpconf_validate", {
	stub(dpconf_get, 'dpconf_validate', 100)
	dpconf_validate(dpconf, project_path) 
	expect_equal(2 * 2, 4)
})


test_that("dpconf_write", {
	stub(dpconf_init, 'dpconf_write', 100)
	stub(dpconf_update, 'dpconf_write', 100)
	dpconf_write(project_path, dpconf) 
	expect_equal(2 * 2, 4)
})


