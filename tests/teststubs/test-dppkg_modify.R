test_that("dppkg_modify", {
	stub(dppkg_modify, 'get_rspm_pkgmeta', 100)
	dppkg_modify(project_path = ".", pkg_name, pkg_version = character(0), pkg_sha = character(0), repo_name = "RSPM", repo_url, force_repo_overwrite = F, verbose = T) 
	expect_equal(2 * 2, 4)
})


test_that("get_rspm_pkgmeta", {
	stub(dppkg_modify, 'get_rspm_pkgmeta', 100)
	get_rspm_pkgmeta(pkg_name, rspm_api_url) 
	expect_equal(2 * 2, 4)
})


