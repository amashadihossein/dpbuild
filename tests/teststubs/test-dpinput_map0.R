test_that("dpinput_flatten", {
	stub(dpinput_flatten, 'map', 100)
	stub(dpinput_map0, 'dpinput_flatten', 100)
	dpinput_flatten(read_dpinput0) 
	expect_equal(2 * 2, 4)
})


test_that("dpinput_map0", {
	stub(dpinput_map, 'dpinput_map0', 100)
	stub(dpinput_map0, 'map', 100)
	stub(dpinput_map0, 'dpinput_map', 100)
	stub(dpinput_map0, 'dpinput_flatten', 100)
	stub(dpinput_map0, 'dpinput_read', 100)
	dpinput_map0(project_path = ".") 
	expect_equal(2 * 2, 4)
})


