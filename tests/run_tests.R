# Run from the app root:  Rscript tests/run_tests.R
testthat::test_dir("tests/testthat", reporter = "summary")
