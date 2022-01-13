test_that("dp_push", {
	
	dp_push(project_path = ".", remote_alias = character(0), remote_url = character(0)) 
	expect_equal(2 * 2, 4)
})


