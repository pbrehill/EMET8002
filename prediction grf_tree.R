library(tidyverse)
library(grf)
library(progress)
library(microbenchmark)
# data <- read_csv('titanic.csv')
# load("~/R-projects/EMET8002/broockman_kalla_replication_data.RData")
# 
# data <- read_dta('macoursetal_main.dta')
# 
# # trans <- x %>%
# #   select(treatment,
# #          trans_therm_post,
# #          trans_therm_pre,
# #          age,
# #          party,
# #          race,
# #          voted14,
# #          voted12,
# #          voted10,
# #          sdocanvass_minutes) %>%
# #   Sex1 = recode(Sex, 
# #                 "male" = 0, 
# #                 "female" = 1)
# 
# Rcpp::sourceCpp('forest_cpp.cpp')
# 
# data <- data %>%
#   mutate(Sex1 = recode(Sex,
#                        "male" = 0,
#                        "female" = 1))
# 
# # Split data
# train_ind <- sample(seq_len(nrow(data)), size = floor(0.75 * nrow(data)))
# 
# data_train <- data[train_ind,] %>% select_if(is.numeric)
# data_test <- data[-train_ind,] %>% select_if(is.numeric)


get_p <-  function (cf, data = NULL) {
  # TODO: Add out of sample
  listed_forest <- map(1:cf$`_num_trees`, ~get_tree(cf, .x))
  if (is.null(data)) data <- cf$X.orig
  
  tree_tables <- evaluate_forest(data %>% as.list(),
                                 listed_forest) %>%
    map(~setNames(.x, c('group', 'avg_Y', 'avg_W')) %>%
          rownames_to_column())
  
  tree_tables
}

evaluate_node_R <- function (datapoint, fit, node_num = 1) {
  if (!("grf_tree" %in% class(fit))) stop("Input is not a causal tree")
  
  node <- fit$nodes[[node_num]]
  
  # Check if node is leaf
  if (node$is_leaf) {
    node_stats <- c(node_num, node$leaf_stats)
    names(node_stats) <- c('node_num', 'avg_Y', 'avg_W')
    return(node_stats)
  }
  
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



find_next_path <- function(decision_tree, node_num, nodes = nodes, nodes_covered = nodes_covered) {

  node = nodes[[node_num]] ;
  left_child = node[["left_child"]] ;
  right_child = node[["right_child"]] ;
  parent = node[["parent"]] ;

  if (!nodes_covered[left_child]) {
    return(c(child = left_child, parent = parent)) ;
  } else if (!nodes_covered[right_child]) {
    return(c(child = right_child, parent = parent)) ;
  } else if (parent != -1) {
    return (find_next_path(decision_tree, node_num, parent, nodes_covered, nodes)) ;
  } else {
    # TODO: Change this
    return (find_next_path(decision_tree, node_num, parent, nodes_covered, nodes)) ;
  }
}


rules_iteration <- function(decision_tree, node_num, parent_node, leaves = leaves, nodes_covered = nodes_covered) {

  nodes <- decision_tree$nodes
  
  # Get node
  node = nodes[[node_num]] ;

  # Tick off node and initialise children
  local_nodes <- nodes_covered
  local_nodes[node_num] <- TRUE
  nodes_covered <<- local_nodes;

  left_child = node[["left_child"]] ;
  right_child = node[["right_child"]] ;

  # Set child node
  node[["parent"]] = parent_node ;

  # Checks next action
  if (node[["is_leaf"]]) { # If node is a leaf, we return cs
    leaves <- c(leaves, node_num) ; # Update leaves

  } else if (!nodes_covered[left_child]){
    return (rules_iteration(decision_tree, left_child, node_num)) ; # Go left
  } else if (!nodes_covered[left_child]) {
    return (rules_iteration(decision_tree, right_child, node_num)) ; # Go right
  } else if (sum(nodes_covered) == length(nodes)) {
    return (leaves) ;
  } else {
    next_node <- find_next_path(decision_tree, node_num, parent_node, nodes_covered., nodes) ;
    if (next_node == -1) {
      return (leaves) ;
    } else {
      return (rules_iteration(decision_tree, right_child, -1)) ;
    }
  }
}


equals_number <- function (x, target) {
  if (is.null(x)) {
    return (FALSE)
  } else if (x == target) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}



get_rules <- function(decision_tree, node_num = 1, parent_node = -1) {

  # Taking care of initial assignment
  nodes = decision_tree[["nodes"]]
  leaves = integer(length = 0L)
  nodes_covered = logical(length(nodes))

  leaves = rules_iteration(decision_tree, node_num, parent_node, leaves = leaves, nodes_covered = nodes_covered)
  # Handle rule compilation here?
  return (leaves)
}

# Get rules without recursion
loop_rules <- function(decision_tree, node_num = 1, parent_node = -1) {
  
  # Taking care of initial assignment
  nodes = decision_tree[["nodes"]]
  leaves = integer(length = 0L)
  nodes_covered = logical(length(nodes))
  last_node <- -1
  node_num <- 1
  i <- 0
  
  
  # Get leaves and parents
  while (!all(nodes_covered)) {
    if (i != 0) node_num <- next_node
    
    # Set current node to covered an select new node
    nodes_covered[node_num] <- TRUE
    node = nodes[[node_num]]
    
    # Set child node
    decision_tree$nodes[[node_num]][["parent"]] <- last_node
    
    # Set last node to current
    last_node <- node_num
    
    # Checks next action
    if (node[["is_leaf"]]) { # If node is a leaf, we return cs
      leaves <- c(leaves, node_num) # Update leaves
      next_node <- which(!nodes_covered) %>% min()
      last_node_left <- nodes %>% map(~.x$left_child) %>% map(~equals_number(.x, next_node)) %>% unlist() %>% which()
      last_node_right <- nodes %>% map(~.x$right_child) %>% map(~equals_number(.x, next_node)) %>% unlist() %>% which()
      last_node <- c(last_node_left, last_node_right)
    } else if (!nodes_covered[node$left_child]){
      next_node <- node$left_child; # Go left
    } else if (!nodes_covered[node$left_child]) {
      next_node <- node$right_child ; # Go right
    } else if (!all(nodes_covered)) {
      # TODO: Make parent node assignment work here
      next_node <- which(!nodes_covered) %>% min()
      last_node_left <- nodes %>% map(~.x$left_child) %>% map(~equals_number(.x, next_node)) %>% unlist() %>% which()
      last_node_right <- nodes %>% map(~.x$right_child) %>% map(~equals_number(.x, next_node)) %>% unlist() %>% which()
      last_node <- c(last_node_left, last_node_right)
    }
    
    i <- i + 1
  }
  decision_tree[["leaves"]] <- leaves
  return(decision_tree)
  }


predict_casual_tree <- function (fit, data1) {
  data1 %>%
    select(fit$columns) %>%
    apply(1, evaluate_node, fit = fit) %>%
    t() %>%
    setNames(c('node_num', 'avg_Y', 'avg_W'))
}

get_leaves_contained <- function (fit) {
  for (i in length(fit$nodes):1) {
    if (!fit$nodes[[i]]$is_leaf) {
      # Add child leaves
      if (fit$nodes[[fit$nodes[[i]]$left_child]]$is_leaf) {
        responsible_leaves_left <- fit$nodes[[i]]$left_child
      } else {
        responsible_leaves_left <- fit$nodes[[fit$nodes[[i]]$left_child]]$descendents
      }
      
      if (fit$nodes[[fit$nodes[[i]]$right_child]]$is_leaf) {
        responsible_leaves_right <- fit$nodes[[i]]$right_child
      } else {
        responsible_leaves_right <- fit$nodes[[fit$nodes[[i]]$right_child]]$descendents
      }
      
      # Add kids to attribute
      fit$nodes[[i]]$descendents <- c(responsible_leaves_left,
                                      responsible_leaves_right)
    }
  }
  
  fit
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





fit_cf_progressively_notau <- function (X, Y, W, num.trees, test_X = NULL) {
  # Remember ci.group = 2
  
  # Initialise i
  i <- 1
  
  # Set test_X
  if (is.null(test_X)) test_X <- X
  
  # Initialise PB
  pb <- progress_bar$new(
    format = " calculating [:bar] :percent time elapsed: :elapsedfull",
    total = num.trees, clear = FALSE, width= 60)
  
  
  while (i <= num.trees) {
    # Fit a new tree
    new_tree <- grf::causal_forest(X, Y, W, num.trees = 1)
    
    # Merge with existing forest
    if (i == 1) {
      # Assign tree to forest, don't merge
      new_forest <- new_tree
      
      # Initialise changes vector
      changes <- c()
      mean_debiased <- c()
      mean_excess <- c()
      predictions <- list()
      new_forest_predictions <- predict(new_forest, test_X)$predictions
      
    } else {
      # Merge forests
      old_forest <- new_forest
      new_forest <- merge_forests(list(old_forest, new_tree))
      
      # Get forest predictions
      new_forest_predictions <- predict(new_forest, test_X)$predictions
    
    
      # Compute change
      individual_differences <- predict(old_forest, test_X)$predictions -
        new_forest_predictions
      
      debiased_error_i <- predict(new_forest, test_X)$debiased.error
      excess_error_i <- predict(new_forest, test_X)$excess.error
        
      # Square and average
      changes <- c(changes, mean(individual_differences ** 2, na.rm = T))
      mean_debiased <- c(mean_debiased, mean(debiased_error_i, na.rm = T))
      mean_excess <- c(mean_excess, mean(excess_error_i, na.rm = T))
      predictions[[i]] <- new_forest_predictions
    }
    
    # Increment i
    i <- i + 1
    pb$tick()
  }
  
  # Calculate error
  
  # Get MSE
  
  # Get probability treatments
  p <- get_p(new_forest)

  # Get MSE for each forest
  # TODO: Change this to OOB
  ## Initialise new tidy dataframe
  error_df <- expand_grid(c(1:new_forest$`_num_trees`), c(1:length(new_forest$W.orig))) %>%
    setNames(c('tree', 'case')) %>%
    mutate(Y.star = NA,
           treatment_probability = NA)
  
  error_df$Y.star <- as.numeric(error_df$Y.star)
  error_df$treatment_probability <- as.numeric(error_df$treatment_probability)
  
  # TODO: Fix errors here, we seem to be picking up the wrong leaf
  # TODO: Check why we're having issues with the number of trees here
  for (i in 1:new_forest$`_num_trees`) {
    for (j in 1:length(new_forest$W.orig)) {
      if (error_df$case[j] %in% p[[i]]$rowname) {
        pi <- p[[i]]$avg_W[p[[i]]$rowname == error_df$case[j]]
        W <- new_forest$W.orig[error_df$case[j]]
        Y <- new_forest$Y.orig[error_df$case[j]]
        new_Y.star <- ((W - pi) / (pi*(1-pi))) * Y
      } else {
        new_Y.star <- NA
        pi <- NA
        W <- NA
        Y <- NA
      }
      index <- (i-1) * length(new_forest$W.orig) + j
      error_df[index, 'Y.star'] <- new_Y.star
      error_df[index, 'tau.hat'] <- new_forest_predictions[j]
      error_df[index, 'treatment_probability'] <- pi
      error_df[index, 'sq_err'] <- (error_df$tau.hat[index] - error_df$Y.star[index]) ** 2
    }
    
    # Compare with predictions
  }
  
  # Get MSE from error_df
  # TODO: Potentially set na.rm to false so that we only get models for which we have valid Y.stars
  MSEs <- error_df %>%
    group_by(tree) %>%
    summarise(MSE = mean(sq_err, na.rm = TRUE))

  return(list(forest = new_forest, changes = changes, predictions = predictions, MSE_values = MSEs, error_df = error_df, p = p))
}


get_rules_from_leaves <- function(decision_tree) {
  map(decision_tree$leaves, function(x) {
    node_num <- decision_tree$nodes[[x]]$parent
    split_vars <- c()
    split_condition <- c()
    while (node_num != -1) {
      node <- decision_tree$nodes[[node_num]]
      split_vars <- c(split_vars, node$split_variable)
      split_condition <- c(split_condition, node$split_value)
      node_num <- node$parent
    }
    
    # TODO: Add support for exporting outcome for leaves
    data.frame(variables = split_vars, conditions = split_condition)
  }
  )
}


combine_rules <- function (rules1, rules2) {
  
}


fit_cf_progressively <- function (X, Y, W, tau, num.trees, test_X = NULL) {
  # Remember ci.group = 2
  
  # Initialise i
  i <- 1
  
  # Set test_X
  if (is.null(test_X)) test_X <- X
  
  # Initialise PB
  pb <- progress_bar$new(
    format = " calculating [:bar] :percent time elapsed: :elapsedfull",
    total = num.trees, clear = FALSE, width= 60)
  
  
  while (i <= num.trees) {
    # Fit a new tree
    new_tree <- grf::causal_forest(X, Y, W, num.trees = 1)
    
    # Merge with existing forest
    if (i == 1) {
      # Assign tree to forest, don't merge
      new_forest <- new_tree
      
      # Initialise changes vector
      changes <- c()
      mean_debiased <- c()
      mean_excess <- c()
      predictions <- list()
      new_forest_predictions <- predict(new_forest, test_X)$predictions
      
    } else {
      # Merge forests
      old_forest <- new_forest
      new_forest <- merge_forests(list(old_forest, new_tree))
      
      # Get forest predictions
      new_forest_predictions <- predict(new_forest, test_X)$predictions
      
      
      # Compute change
      individual_differences <- predict(old_forest, test_X)$predictions -
        new_forest_predictions
      
      debiased_error_i <- predict(new_forest, test_X)$debiased.error
      excess_error_i <- predict(new_forest, test_X)$excess.error
      
      # Square and average
      changes <- c(changes, mean(individual_differences ** 2, na.rm = T))
      mean_debiased <- c(mean_debiased, mean(debiased_error_i, na.rm = T))
      mean_excess <- c(mean_excess, mean(excess_error_i, na.rm = T))
      predictions[[i]] <- new_forest_predictions
    }
    
    # Increment i
    i <- i + 1
    pb$tick()
  }
  
  mse <- map_dbl(predictions, ~mean((.x - tau) ** 2))
  
  errors.df <- data.frame(debiased = mean_debiased,
                          excess = mean_excess)
  
  return(list(forest = new_forest,
              changes = changes,
              predictions = predictions,
              errors.df = errors.df,
              mse = mse))
}



change_tree_var_names <- function(tree, data) {
  labels <- data[tree$columns] %>% map_chr(~attributes(.x)$label)
  tree$column_labels <- labels[tree$columns] %>% unname
  
  return(tree)
}


get_lineage <- function(tree) {
  lineages_list <- list()
  which_child_list <- list()
  for (i in 1:length(tree$leaves)) {
    leaf_lineage <- tree$leaves[i]
    current_node <- tree$nodes[[leaf_lineage]]
    while (current_node$parent != -1) {
      leaf_lineage <- c(leaf_lineage, current_node$parent)
      current_node <- tree$nodes[[current_node$parent]]
    }
    lineages_list[[i]] <- leaf_lineage
  }
  
  return(lineages_list)
}

which_child <- function(tree, lineage_list = get_lineage(tree)) {
  child_list <- list()
  for (i in 1:length(lineage_list)) {
    child_vector <- logical()
    current_vector <- lineage_list[[i]]
    for (j in 1:(length(current_vector) - 1)) {
      node_num <- current_vector[j]
      node <- tree$nodes[[node_num]]
      left_child <- tree$nodes[[node$parent]]$left_child
      right_child <- tree$nodes[[node$parent]]$right_child
      
      if (left_child == node_num) {
        child_vector <- c(child_vector, TRUE)
      } else if (right_child == node_num) {
        child_vector <- c(child_vector, FALSE)
      } else {
        child_vector <- c(child_vector, NA)
      }
    }
    child_list[[i]] <- child_vector
  }
  return(child_list)
}


get_rule_list <- function (tree) {
  tree <- loop_rules(tree)
  lineage <- get_lineage(tree)
  which_children <- which_child(tree)
  
  lineage_split_vars <- lineage %>%
    map(function (i) {
      map_dbl(i, function (j) {
        value <- tree$nodes[[j]]$split_variable
        if (is.null(value)) return(NA)
        value
      })
    })
  
  lineage_split_values <- lineage %>%
    map(function (i) {
      map_dbl(i, function (j) {
        value <- tree$nodes[[j]]$split_value
        if (is.null(value)) return(NA)
        value
      })
    })
  
  # TODO: Incorporate direction in here.
  
  rules_df_list <- list()
  for (i in 1:length(lineage)) {
    rules_df_list[[i]] <- data.frame(parents = lineage[[i]],
               variable = lineage_split_vars[[i]],
               values = lineage_split_values[[i]],
               result = c(NA, which_children[[i]])
               ) %>%
      mutate(variable_name = tree$columns[variable])
  }
  # rules_df_list <- pmap(lineage_split_vars, lineage_split_values, lineage, ~data.frame(variables = ..1, values = ..2, parent = ..3) #%>%
  #                         # filter(!is.na(variables)) %>%
  #                         # mutate(plain_text = paste0(tree$columns[variables], " <= ", format(round(.$values, 2), nsmall = 2)))
  #                         )
  
  names(rules_df_list) <- paste0("node_", tree$leaves)
  rules_df_list
}


# test_forest <- causal_forest(data_train %>% select(-Survived, -Sex1),
#                                            data_train$Survived,
#                                            data_train$Sex1,
#                                            num.trees = 1000)
# 
# test_tree <- get_tree(test_forest, 1)

prune_causal_tree <- function (tree, node, data) {
  if (!tree$nodes[[node]]$is_leaf) {
    print("Node is not a leaf, returning NA")
    return (NA)
  }
  
  samples <- tree$nodes %>%
    map(~.x$samples)
  
  # Set new sample for parent
  parent <- tree$nodes[[node]]$parent
  # left <- tree$nodes[[parent]]$left_child
  # right <- tree$nodes[[parent]]$right_child
  
  tree$nodes[[parent]]$samples <- tree$nodes[[parent]]$descendents %>%
    map(~tree$nodes[[.x]]$samples) %>%
    unlist()
  
  # Calculate new leaf stats
  tree$nodes[[parent]]$leaf_stats <- c(
    data[tree$nodes[[parent]]$samples,] %>%
      summarise(avg_Y = mean(z_all_08, na.rm = TRUE),
                avg_W = mean(`T`, na.rm = TRUE)) %>%
      unlist()
  )
  print(tree$nodes[[parent]]$leaf_stats['avg_W'])
  tree$nodes[[parent]]$big_enough <- (tree$nodes[[parent]]$leaf_stats['avg_W'] * length(tree$nodes[[parent]]$sample) > 29) & ((1 - tree$nodes[[parent]]$leaf_stats['avg_W']) * length(tree$nodes[[parent]]$sample) > 29)
  
  # Set leaf to true
  tree$nodes[[parent]]$is_leaf <- TRUE
  
  # Null old children
  descendents <- tree$nodes[[parent]]$descendents
  for (i in 1:length(descendents)) {
    tree$nodes[[descendents[i]]]$samples <- NULL
    tree$nodes[[descendents[i]]]$is_leaf<- FALSE
  }
  
  # Return tree
  tree
}


prune_whole_tree <- function (tree, data, min.size) {
  total_nodes <- length(tree$nodes)
  tree <- loop_rules(tree) %>%
    get_leaves_contained()
  
  for (i in total_nodes:1) {
    # Check if node has enough T and C
    if (tree$nodes[[i]]$is_leaf) {
      W_stats <- tree$nodes[[i]]$leaf_stats['avg_W']
      leaf_sample <- tree$nodes[[i]]$samples
      if (tree$nodes[[i]]$big_enough) {
        tree <- prune_causal_tree(tree, i, data)
      }
    }
  }
  
  return(tree)
}

best_tree <- prune_whole_tree(good_tree, data, 30)



# Get Y* for an observation

# set.seed(1993)
# 
# pcf1 <- fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
#                             data_train$Survived,
#                             data_train$Sex1,
#                             tau = tau_noise,
#                             num.trees = 1000,
#                             test_X = data_test %>% select(-Survived, -Sex1))
# 
# pcf2 <- fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
#                              data_train$Survived,
#                              data_train$Sex1,
#                              num.trees = 5000,
#                              test_X = data_test %>% select(-Survived, -Sex1))

# benchmark <- microbenchmark(fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
#                              data_train$Survived,
#                              data_train$Sex1,
#                              num.trees = 40,
#                              test_X = NULL), times = 5L)
# pcf5 <- fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
#                              data_train$Survived,
#                              data_train$Sex1,
#                              num.trees = 7,
#                              test_X = NULL)

# pcf4 <- fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
#                              data_train$Survived,
#                              data_train$Sex1,
#                              num.trees = 5000,
#                              test_X = NULL)


# benchmark_Cpp <- microbenchmark(
#   evaluate_nodes(pcf3$forest$X.orig %>% as.list(), pcf3$forest %>% get_tree(5)),
#   times = 10L
# )
# 
# benchmark_R <- microbenchmark(
#   for (i in nrow(data_train)) {
#     evaluate_node_R(pcf3$forest$X.orig[i,], pcf3$forest %>% get_tree(5))
#   },
#   times = 10L
# )

# pcf1$mse %>% qplot(y = ., x = 1:length(.))

# test_p_out <- get_p(cf_test)

## Create a conjunction set

# conjunction_set <- function (cf) {
#   cs <- list()
#   for (i in 1:cf$`_num_trees`) {
#     if (i == 0) {
#       cs[[0]] <- 
#     }
#   }
# }


# get_rules(test_tree)


# # output_test <- loop_rules(test_tree)
# 
# trees <- map(1:test_forest$`_num_trees`, ~get_tree(test_forest, .x))
# 
# trees_w_parents <- list()
# rules_list <- list()
# 
# pb <- progress_bar$new(
#   format = " calculating [:bar] :percent time elapsed: :elapsedfull",
#   total = test_forest$`_num_trees`, clear = FALSE, width= 60)
# 
# for (i in 1:test_forest$`_num_trees`) {
#   trees_w_parents[[i]] <- loop_rules(trees[[i]])
#   rules_list[[i]] <- get_rules_from_leaves(trees_w_parents[[i]])
#   pb$tick()
# }
# 
# 
# rules_df <- rules_list %>% map(~bind_rows(.x, .id = "leaf")) %>% bind_rows(.id = "tree")
# # get_rules_from_leaves(trees_w_parents[[2]])
# 
# # loop_rules(test_tree)
