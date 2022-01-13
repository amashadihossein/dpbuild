test_that("dp_structure", {
	stub(dp_structure, 'dpname_get', 100)
	stub(dp_structure, 'readme_get', 100)
	stub(dp_write, 'dp_structure', 100)
	dp_structure(data_files_read, config, output = list(), metadata = list()) 
	expect_equal(2 * 2, 4)
})


