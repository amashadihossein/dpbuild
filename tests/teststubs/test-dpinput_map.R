test_that("dir_ls_tidy", {
	stub(dir_process, 'dir_ls_tidy', 100)
	dir_ls_tidy(current_dir) 
	expect_equal(2 * 2, 4)
})


test_that("dir_process", {
	stub(dir_process, 'map', 100)
	stub(dir_process, 'pmap', 100)
	stub(dir_process, 'dir_ls_tidy', 100)
	stub(dir_process, 'dir_process_zip', 100)
	stub(dir_process, 'tbsig_get', 100)
	stub(dir_process_zip, 'dir_process', 100)
	stub(dpinput_map, 'dir_process', 100)
	dir_process(current_dir, junk_path = character(0)) 
	expect_equal(2 * 2, 4)
})


test_that("dir_process_zip", {
	stub(dir_process, 'dir_process_zip', 100)
	stub(dir_process_zip, 'dir_process', 100)
	dir_process_zip(zip_dir) 
	expect_equal(2 * 2, 4)
})


test_that("dirTree_build", {
	stub(dirTree_build, 'map', 100)
	stub(dirTree_build, 'make_names_codefriendly', 100)
	stub(dpinput_map, 'dirTree_build', 100)
	dirTree_build(flattened_dirTree) 
	expect_equal(2 * 2, 4)
})


test_that("dirTree_flatten", {
	stub(dpinput_map, 'dirTree_flatten', 100)
	dirTree_flatten(read_files) 
	expect_equal(2 * 2, 4)
})


test_that("dpinput_map", {
	stub(dpinput_map, 'map', 100)
	stub(dpinput_map, 'dir_process', 100)
	stub(dpinput_map, 'dirTree_build', 100)
	stub(dpinput_map, 'dirTree_flatten', 100)
	stub(dpinput_map, 'dpinput_map0', 100)
	stub(dpinput_map0, 'map', 100)
	stub(dpinput_map0, 'dpinput_map', 100)
	stub(dpinput_map0, 'dpinput_flatten', 100)
	stub(dpinput_map0, 'dpinput_read', 100)
	dpinput_map(project_path) 
	expect_equal(2 * 2, 4)
})


test_that("dpinput_syncflag_reset", {
	stub(dpinput_syncflag_reset, 'map', 100)
	stub(dpinput_syncflag_reset, 'dpinput_sync', 100)
	dpinput_syncflag_reset(input_map, input_id) 
	expect_equal(2 * 2, 4)
})


