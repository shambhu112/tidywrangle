library(testthat)

test_that("test tidy loader ", {
    test_ds <- "d:/wsl/Rproj/bcm_sfdc/sfdc_export_nov17.xlsx"
    ds <- readxl::read_xlsx(test_ds)
    dic <- create_dictory_df(ds)

    cnames <- c("original_colnm" , "newcol_nm" , "drop" , "na_threshhold")
    expect_equal(colnames(dic) , cnames)

    expect_gt(nrow(dic) , 1)
})

test_that("write dictionary", {
    test_ds <- "d:/wsl/Rproj/bcm_sfdc/sfdc_export_nov17.xlsx"
    ds <- readxl::read_xlsx(test_ds)
    dic <- create_dictory_df(ds)
    ret <- write_dictionary(dic , to = "excel" , filename = "d:\\temp\\dictionary.xlsx")
    expect_equal(ret , "d:\\temp\\dictionary.xlsx")

    ret <- write_dictionary(dic , filename = "d:\\temp\\dictionary.csv")
    expect_equal(ret , "d:\\temp\\dictionary.csv")

    ## google is not working
   # ret <- write_dictionary(dic , to = "google")

})

test_that(" create and prep tidy_loader" , {
    test_ds <- "d:/wsl/Rproj/bcm_sfdc/sfdc_export_nov17.xlsx"
    ds <- readxl::read_xlsx(test_ds)
    dic <- create_dictory_df(ds)


    tl <- tidy_loader$new(dic)
    tl$print()

    tl$prep_df(ds)
    expect_equal(nrow(tl$data) , 5186)
    expect_equal(ncol(tl$data) , 54)
    expect_true("ebit_percent" %in% colnames(tl$data))
    tl$print()
})
