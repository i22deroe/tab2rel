library(dplyr)
library(reshape2)
library(yaml)
library(purrr)

tab2rel <- function(tabular_dataset = data.frame(), 
                    hierarchy_depth = numeric(),
                    hierarchy_levels = list()){
  disclaimer()
  hierarchy_levels <- `if`(length(hierarchy_levels) == 0,
                                colnames(tabular_dataset)[1:hierarchy_depth],
                                hierarchy_levels)
  
  hierarchy_depth <- `if`(length(hierarchy_depth) == 0,
                                length(hierarchy_levels),
                                hierarchy_depth)

  relational_dataset = tabular_dataset %>%
    split(f = .[[hierarchy_levels[1]]]) %>%
    build_relationship(hierarchy_depth, rev(hierarchy_levels)) %>%
    bottom_level(hierarchy_depth, hierarchy_levels[length(hierarchy_levels)]) %>%
    top_up(hierarchy_depth, rev(hierarchy_levels[2:length(hierarchy_levels)])) %>%
    list(tmpname = .) %>%
    restructure_list(hierarchy_levels[1]) %>%
    as.yaml() 
  
  return(relational_dataset)
  
}

rel2tab = function(relational_dataset){
  disclaimer()
  tabular_dataset = melt(yaml.load(yaml.load_file(relational_dataset)))

  return(tabular_dataset)
  
}

build_relationship = function(x, depth, levels){
  # Recursively create 0:(n-1)th levels of hierarchy
  depth = depth-1
  if(depth > 0){
    category_column = levels[2]
    content_column = levels[1]
    levels = levels[2:length(levels)]
    x = build_relationship(x, depth, levels) %>%
      map_depth(depth, dplyr::select, -category_column) %>%
      map_depth(depth, ~ split(.x, f = .x[[content_column]]))   
  }else{
    x = x
  }
  return(x)
}

bottom_level = function(x, level, last_column){
  # Define the nth level of hierarchy
  x %>%
    map_depth(level, dplyr::select, -last_column) %>%
    map_depth(level, as.list, -last_column) %>%
    map_depth(level+1, unique) 
}

top_up = function(x, level, colnames, ilevel=-1){
  # Add the variable names 
  nlevel = ilevel + 2
  colname = colnames[length(colnames)]
  
  if(nlevel < level){
    top_up(x, level, colnames[-length(colnames)], nlevel-1) %>%
    map_depth(nlevel, ~ restructure_list(list(tmp = .), colname)) 
  } else {
    x
  }
}

restructure_list = function(x, listname){
  # Give a name to a list inside a pipeline.
  names(x) = listname
  return(x)
}

disclaimer = function(rlang_v = as.numeric(stringr::str_extract(as.character(packageVersion("rlang") ), 
                                     "([0-9]+)$"))/10){
  if(rlang_v >= 2){
    rlang::warn("Please take into account that the right depth / hierarchy levels 
                for your tabular dataset to make sense in relational dataset may 
                require further tweaking depending on factors like the degree of 
                aggregation of your table and the order of columns. You may need 
                to check once or twice.\n
                Likewise, when converting a relational dataset into a tabular 
                dataset you will need to further adjust the resulting dataframe.\n
                This warning message will be displayed only once per session.",
                .frequency = 'once')
  } else {
    rlang::warn("Please take into account that the right depth / hierarchy levels 
                for your tabular dataset to make sense in relational dataset may 
                require further tweaking depending on factors like the degree of 
                aggregation of your table and the order of columns. You may need 
                to check once or twice.\n
                Likewise, when converting a relational dataset into a tabular 
                dataset you will need to further adjust the resulting dataframe.\n")

  }
}

