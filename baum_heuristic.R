
baum_step_1 <- function(forest) {
  # TODO: Incorporate equal highest imporantance
  # TODO: Check tree importance measure (first split)
  most_important_selector <- variable_importance(test_forest) == variable_importance(test_forest) %>% max()
  most_important_variable <- which(most_important_selector)
  
  # Get all the trees in a list
  trees <- map(1:forest$`_num_trees`, ~get_tree(forest, .x))
  
  # Iterate through, filter for those with correct highest node
  trees_filtered <- map_lgl(trees, ~.x$nodes[[1]]$split_variable == most_important_variable)
  return(trees_filtered)
}

baum_step_2 <- function(forest, data) {
  selected_trees <- baum_step_1(forest)
  which_trees <- selected_trees %>% which()
  
  dfs <- map(1:length(which_trees), ~predict_casual_tree(get_tree(test_forest, which_trees[.x]), data))
  expanded_dfs <- map(dfs, ~bind_cols(.x, data))
  expanded_dfs
  
  # TEs_by_tree <- 
}