# Opération Fourmis
# Functions for 5_citsci
# Tim Szewczyk


#' Intersect buffered points with habitat layer
#'
#' Row for every point x overlapping habitat, with the area of each overlapping
#' habitat and proportianal compasition calculated
#' @param ant_buffer Polygon sf object with buffered points
#' @param landcover Polygon landcover layer to intersect
#' @param category_col Scalar character with the column name in \code{landcover}
#'   that contains the habitat categories
#'
#' @return Intersected sf object

intersect_with_composition <- function(ant_buffer, landcover, category_col) {
  library(sf); library(tidyverse)
  st_intersection(ant_buffer %>% select(TubeNo), 
                  landcover %>% select(category_col[1])) %>% 
    mutate(area=st_area(.)) %>% group_by(TubeNo) %>%
    mutate(prop=as.numeric(area/sum(area)))
}



#' Categorize habitats for buffered points
#' 
#' Using the output from \code{intersect_with_composition()}, categorize each
#' point with the dominant habitat type, keeping the proportion of the point
#' buffer covered by that habitat as well as the number of habitats intersecting
#' the point buffer
#' @param intsct.df Output from \code{intersect_with_composition()}
#' @param category_col Scalar character with the column name in \code{landcover}
#'   that contains the habitat categories
#' @param col_suffix Suffix to append to new columns (\code{'cat_',
#'   'prLargest_', 'n_'})
#'
#' @return Updated sf object with a row for each tube and new columns indicating
#'   the dominant category, proportion of the buffer it covers, and the number
#'   of habitat types intersecting the buffer

categorize_buffers <- function(intsct.df, category_col, col_suffix) {
  library(sf); library(tidyverse)
  full_join(intsct.df %>% st_set_geometry(NULL) %>%
              group_by(TubeNo) %>% slice_max(prop) %>% ungroup %>%
              select(TubeNo, category_col[1], prop) %>%
              rename(cat_=category_col[1], prLargest_=prop) %>%
              rename_with(., ~paste0(.x, col_suffix), 
                          one_of("cat_", "prLargest_")),
            intsct.df %>% st_set_geometry(NULL) %>% 
              group_by(TubeNo) %>% summarise(n_=n()) %>% ungroup %>% 
              rename_with(., ~paste0(.x, col_suffix), starts_with("n_")), 
            by="TubeNo")
}
