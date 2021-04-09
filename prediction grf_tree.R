library(tidyverse)
library(grf)
data <- read_csv('titanic.csv')

evaluate_node <- function (datapoint, fit, node_num = 1) {
  if (class(fit) != "grf_tree") stop("Input is not a causal tree")
  
  node <- fit$nodes[[node_num]]
  
  # Check if node is leaf
  if (node$is_leaf) return(node$leaf_stats)
  
  test_value <- datapoint[node$split_variable]
  
  if (!is.na(test_value)) {
    evaluation <- test_value <= node$split_value
  } else {
    evaluation <- node$send_missing_left
  }
  
  if (evaluation) {
    evaluate_node(datapoint, fit, node$left_child)
  } else {
    evaluate_node(datapoint, fit, node$right_child)
  }
}


predict_casual_tree <- function (fit, data1) {
  data1 %>%
    select(fit$columns) %>%
    apply(1, evaluate_node, fit = fit) %>%
    t()
}

predict_causal_trees <- function(fit, data1) {
  if (class(fit) == "grf_tree") {
    predict_casual_tree(fit, data1)
  } else if (class(fit) == "list") {
    predicts_list <- map(fit, ~predict_casual_tree(.x, data1))
    Reduce("+", predicts_list) / length(predicts_list)
  } else {
    stop("Incomptable data type")
  }
}

# output_df <- predict_casual_tree(test_tree, data)

trees <- c(1:(cf['_num_trees'] %>% unlist())) %>%
  map(~get_tree(cf, .x))

outs <- predict_causal_trees(trees, data)
