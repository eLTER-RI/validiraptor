test_that("schema can be fetched from GitHub", {
    expect_no_error(get_schema_from_repo("shared"))    
})

test_that("run_app works", expect_no_error(run_app()))
