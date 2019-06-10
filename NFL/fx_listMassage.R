
library(dplyr)
library(purrr)

# subfunction 
rid_row_names_list_to_df <- function(dummy.list){
  test              <- dummy.list %>% data.frame() 
  rownames(test)    <- c()
  return(test)
}

# for do loops of nested lists
# get dimenstion of each sub list
Dim_List_Objs  <- function(split_list){
  dummy.list <- 0
  for(xx in 1:length(split_list)){
    dummy.list[[xx]] <- length(split_list[[xx]])
  }
  return(rid_row_names_list_to_df(dummy.list))
}

# Col_Bind_All unpacks all sublists into a list with degree one

Col_Bind_All <- function(list_to_unitize){
  dummy.list <- list()
  dummy.dims <- Dim_List_Objs(list_to_unitize)
  for(xx in 1:length(list_to_unitize)){
    if(dummy.dims[xx,]>= 2){
      dummy.list[[xx]] <- bind_cols(list_to_unitize[[xx]]) 
    }
    else{                    
      dummy.list[[xx]] <- bind_cols(list_to_unitize[[xx]]) 
    }
  }
  return(dummy.list)
}
