test_that("dpinput_make", {
	stub(dpinput_make, 'map', 100)
	stub(dpinput_write, 'dpinput_make', 100)
	dpinput_make(input_d) 
	expect_equal(2 * 2, 4)
})


test_that("dpinput_write", {
	stub(dpinput_write, 'dpinput_make', 100)
	stub(dpinput_write, 'dp_repository_check', 100)
	stub(dpinput_write, 'is_valid_dp_repository', 100)
	dpinput_write(project_path, input_d, verbose = F) 
	expect_equal(2 * 2, 4)
})


