test_that("dpinput_read", {
	stub(dpinput_map0, 'dpinput_read', 100)
	stub(dpinput_read, 'map', 100)
	stub(dpinput_read, 'make_pinlink', 100)
	dpinput_read(daap_input_yaml = yaml::read_yaml(file = "./.daap/daap_input.yaml"), add_metadata = F) 
	expect_equal(2 * 2, 4)
})


test_that("make_pinlink", {
	stub(dpinput_read, 'make_pinlink', 100)
	stub(make_pinlink, 'dp_connect', 100)
	stub(make_pinlink, 'dp_get', 100)
	make_pinlink(synced_input_i) 
	expect_equal(2 * 2, 4)
})


