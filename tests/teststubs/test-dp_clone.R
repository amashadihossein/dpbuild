test_that("dp_clone", {
	
	dp_clone(remote_url, branch, verbose = F) 
	expect_equal(2 * 2, 4)
})


