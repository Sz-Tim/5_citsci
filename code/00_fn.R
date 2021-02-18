# Opération Fourmis
# Functions for 5_citsci
# Tim Szewczyk



intersect_with_composition <- function(ant_buffer, landcover, category_col) {
  library(sf); library(tidyverse)
  st_intersection(ant_buffer %>% select(TubeNo), 
                  landcover %>% select(category_col)) %>% 
    mutate(area=st_area(.)) %>% group_by(TubeNo) %>%
    mutate(prop=as.numeric(area/sum(area)))
}



categorize_buffers <- function(intsct.df, category_col, col_suffix) {
  library(sf); library(tidyverse)
  full_join(intsct.df %>% st_set_geometry(NULL) %>%
              group_by(TubeNo) %>% slice_max(prop) %>% ungroup %>%
              select(TubeNo, category_col, prop) %>%
              rename(cat_=category_col, prLargest_=prop) %>%
              rename_with(., ~paste0(.x, col_suffix), 
                          one_of("cat_", "prLargest_")),
            intsct.df %>% st_set_geometry(NULL) %>% 
              group_by(TubeNo) %>% summarise(n_=n()) %>% ungroup %>% 
              rename_with(., ~paste0(.x, col_suffix), starts_with("n_")), 
            by="TubeNo")
}
