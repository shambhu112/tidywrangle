
hirarchy_wrangler <- R6::R6Class("hirarchy_wrangler",
                      public = list(
                        map = NULL ,
                        data = NULL ,
                        initialize = function(map) {
                          self$map <- map
                        },

                        map_hirarchy = function(.data , key_cols_inmap , target_cols_inmap , key_cols_inds = NULL , target_colnames_inds = NULL){

                          # Case when Keys Cols are the same in map and in DS
                          if(is.null(key_cols_inds))
                            key_cols_inds <- key_cols_inmap

                          # Case when target cols are same as in map
                          if(is.null(target_colnames_inds))
                            target_colnames_inds <- target_cols_inmap

                          if(!all(all_of(key_cols_inmap) %in% names(self$map))){
                            cli::cli_alert_danger("all keys from  :** {all_of(key_cols_inmap)} ** do not exist in map ** {names(h)} ** ")
                            return(FALSE)
                          }

                          if(!all(all_of(key_cols_inds) %in% names(self$map))){
                            cli::cli_alert_danger("all keys from ds:** {all_of(key_cols_inds)} ** do not exist in map ** {names(h)} ** ")
                            return(FALSE)
                          }

                          if(length(key_cols_inds) != length(key_cols_inmap)){
                            cli::cli_alert_danger("Keys in Map and Datasource (DS) should be the same. ")
                            return(FALSE)
                          }

                          map_sub_select <- select(self$map , all_of(key_cols_inmap) , all_of(target_cols_inmap))
                          ds_sub_select <- select(.data , all_of(key_cols_inds))

                          names(map_sub_select) <- c(all_of(key_cols_inmap) , all_of(target_cols_inmap))

                          self$data <- left_join(ds_sub_select , map_sub_select , by = all_of(key_cols_inmap))
                          return(TRUE)

                        },

                        print = function() {
                          cli::cli_alert_success("map loaded , variable count = {nrow(self$map)}")
                        }
                      )
)
