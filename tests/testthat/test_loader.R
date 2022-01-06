library(testthat)

test_that("test tidy loader ", {
    test_ds <- "test_ds.csv"

    ds <- readr::read_csv(test_ds)
    dic <- create_dictory_df(ds)

    cnames <- c("original_colnm" , "newcol_nm" , "drop" , "na_threshhold")
    expect_equal(colnames(dic) , cnames)

    expect_gt(nrow(dic) , 1)
})

test_that("write dictionary", {
    test_ds <- "test_ds.csv"
    ds <- readr::read_csv(test_ds)

    dic <- create_dictory_df(ds)
    ret <- write_dictionary(dic , to = "excel" , filename = tempfile("dictionary" , fileext = ".xlsx"))
    dic1 <- read.csv(ret)
    expect_equal(ncol(dic1) , 4)

    ret <- write_dictionary(dic , filename = tempfile("dictionary" , fileext = ".csv"))
    dic2 <- read.csv(ret)
    expect_equal(ncol(dic2) , 4)


})

test_that(" create and prep tidy_loader" , {
 #   test_ds <- "d:/wsl/Rproj/bcm_sfdc/sfdc_export_nov17.xlsx"
 #   ds <- readxl::read_xlsx(test_ds)

    test_ds <- "test_ds.csv"
    ds <- readr::read_csv(test_ds)

    dic <- create_dictory_df(ds)


    tl <- tidy_loader$new(dic)
    tl$print()

    tl$prep_df(ds)
    expect_equal(nrow(tl$data) , 49)
    expect_equal(ncol(tl$data) , 9)
    expect_true("project_status" %in% colnames(tl$data))
    tl$print()
})
