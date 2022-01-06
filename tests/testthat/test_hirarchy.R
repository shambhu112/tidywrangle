library(testthat)
library(dplyr)
test_that("test hirarchy ", {
  ds <- readr::read_csv("test_ds.csv")
  dic <- create_dictory_df(ds)

  tl <- tidy_loader$new(dic)
  tl$prep_df(ds)

   hmap <- readr::read_csv("h_map.csv")
   hw <- hirarchy_wrangler$new(hmap)

   d <- tl$data

   key_cols_inmap <- c("study_population" , "research_focus")
   target_cols_inmap <- c("population" , "school" , "simple_focus")

   hw$map_hirarchy(d , key_cols_inmap , target_cols_inmap )
   dd <- hw$data
   welbeing_count  <- nrow(dd[dd$simple_focus == "Wellbeing",])
   expect_equal(14 , welbeing_count)

})
