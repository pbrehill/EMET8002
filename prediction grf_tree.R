library(tidyverse)
library(grf)
library(progress)
library(microbenchmark)
data <- read_csv('titanic.csv')

Rcpp::sourceCpp('forest_cpp.cpp')

data <- data %>% 
  mutate(Sex1 = recode(Sex, 
                       "male" = 0, 
                       "female" = 1))

# Split data
train_ind <- sample(seq_len(nrow(data)), size = floor(0.75 * nrow(data)))

data_train <- data[train_ind,] %>% select_if(is.numeric)
data_test <- data[-train_ind,] %>% select_if(is.numeric)


get_leaves <- function(cf) {
    map(1:cf$`_num_trees`, function (i) {
    cf$`_drawn_samples`[[i]] %>% 
      map(function (x) map_lgl(cf$`_leaf_samples`[[i]], function (y) x %in% y) %>% which()) %>%
      setNames(cf$`_drawn_samples`[[i]]) %>%
      unlist() %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      setNames(c('rowname', 'group')) %>%
      mutate(group = as.numeric(group))
  })
}


get_p <-  function (cf, cpp = TRUE) {
  leaves <- get_leaves(cf)
  
  tree_tables <- map(1:cf$`_num_trees`, function (i) {
    # Compute in_sample ps
    tree_table <- leaves[[i]] %>% setNames(c('rowname', 'leaf')) %>% split(.$leaf) %>% 
      map(pull, rowname) %>%
      map(as.numeric) %>%
      map(~mean(cf$W.orig[.x])) %>%
      unlist() %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      setNames(c('group', 'w_mean')) %>%
      mutate(group = as.integer(group),
             in_sample = TRUE) %>%
      right_join(leaves[[i]], by = 'group') %>%
      mutate(rowname = as.integer(rowname)) %>%
      complete(rowname = 1:length(cf$W.orig))
    
    # Compute out-of-sample
    tree <- get_tree(cf, i)
    missing_from_sample <- is.na(tree_table$group)
    
    # Ideally make conditional on missing value but I'm just prototyping for now
    for (g in 1:nrow(tree_table)) {
      if (is.na(tree_table[g, 'group'])) {
        tree_table[g, 'group'] <- evaluate_node(cf$X.orig[tree_table$rowname[g],], tree)['node_num']
        tree_table[g, 'in_sample'] <- FALSE
      }
    }
      
    # Fill in missing p
    tree_table <- tree_table %>%
      group_by(group) %>%
      fill(w_mean, .direction = "downup")
    
  })
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





fit_cf_progressively <- function (X, Y, W, num.trees, test_X = NULL) {
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
        pi <- p[[i]]$w_mean[p[[i]]$rowname == error_df$case[j]]
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

# Get Y* for an observation

set.seed(1993)

# pcf1 <- fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
#                             data_train$Survived,
#                             data_train$Sex1,
#                             num.trees = 500,
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

