test_that("dpinputnames_simplify", {
	stub(inputmap_clean, 'dpinputnames_simplify', 100)
	dpinputnames_simplify(x, make_unique = FALSE) 
	expect_equal(2 * 2, 4)
})


test_that("dpname_get", {
	stub(dp_structure, 'dpname_get', 100)
	stub(dpname_get, 'dpname_make', 100)
	dpname_get(data_object) 
	expect_equal(2 * 2, 4)
})


test_that("dpname_make", {
	stub(dpname_get, 'dpname_make', 100)
	dpname_make(project_name, branch_name) 
	expect_equal(2 * 2, 4)
})


test_that("get_pin_version", {
	stub(dplognote_get, 'get_pin_version', 100)
	get_pin_version(d, pin_name, pin_description) 
	expect_equal(2 * 2, 4)
})


test_that("inputmap_clean", {
	stub(inputmap_clean, 'map', 100)
	stub(inputmap_clean, 'dpinputnames_simplify', 100)
	inputmap_clean(input_map, force_cleanname = F) 
	expect_equal(2 * 2, 4)
})


test_that("make_names_codefriendly", {
	stub(dirTree_build, 'make_names_codefriendly', 100)
	make_names_codefriendly(x, make_unique = T) 
	expect_equal(2 * 2, 4)
})


test_that("make_sha1_compatible", {
	stub(dplognote_get, 'make_sha1_compatible', 100)
	make_sha1_compatible(l) 
	expect_equal(2 * 2, 4)
})


test_that("purge_local_cache", {
	
	purge_local_cache(path_cache = pins::board_cache_path()) 
	expect_equal(2 * 2, 4)
})


test_that("readme_get", {
	stub(dp_structure, 'readme_get', 100)
	readme_get(d, general_note) 
	expect_equal(2 * 2, 4)
})


test_that("tbsig_get", {
	stub(dir_process, 'tbsig_get', 100)
	tbsig_get(d) 
	expect_equal(2 * 2, 4)
})


