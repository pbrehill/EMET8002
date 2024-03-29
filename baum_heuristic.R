
baum_step_1 <- function(forest) {
  # TODO: Incorporate equal highest imporantance
  # TODO: Check tree importance measure (first split)
  most_important_selector <- variable_importance(forest) == variable_importance(forest) %>% max()
  most_important_variable <- which(most_important_selector)
  
  # Get all the trees in a list
  trees <- map(1:forest$`_num_trees`, ~get_tree(forest, .x))
  
  # Iterate through, filter for those with correct highest node
  trees_filtered <- map(trees, ~.x$nodes[[1]]$split_variable) %>%
    unlist()
  trees_filtered <- trees_filtered == most_important_variable
  return(trees_filtered)
}

baum_step_2 <- function(forest, data) {
  selected_trees <- baum_step_1(forest)
  which_trees <- selected_trees %>% which()
  predictions <- predict(forest)$predictions
  
  trees <- map(which_trees, ~get_tree(forest, .x))
  
  selected_tree_samples <- map(trees, function (x) {
      drawn <- x$drawn_samples
      1:nrow(data) %in% drawn
    }) %>%
    bind_cols()
  
  prediction_variance <- selected_tree_samples %>%
    map_dbl(~var(predictions[.x]))
  
  best_tree <- prediction_variance %>% order(decreasing = TRUE)
  
  return(best_tree[1])
}

baum_step_3 <- function(forest, data) {
  get_tree(forest, baum_step_2(forest, data))
}
# baum_trees <- map(forest_outputs, ~baum_step_3(.x, data))