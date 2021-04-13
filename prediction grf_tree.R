library(tidyverse)
library(grf)
library(progress)
data <- read_csv('titanic.csv')

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


get_p <-  function (cf) {
  leaves <- get_leaves(cf)
  
  map(1:cf$`_num_trees`, function (i) {
    leaves[[i]] %>% setNames(c('rowname', 'leaf')) %>% split(.$leaf) %>% 
      map(pull, rowname) %>%
      map(as.numeric) %>%
      map(~mean(cf$W.orig[.x])) %>%
      unlist() %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      setNames(c('group', 'w_mean')) %>%
      mutate(group = as.integer(group)) %>%
      right_join(leaves[[i]], by = 'group')
  })
}

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





fit_cf_progressively <- function (X, Y, W, num.trees, test_X = NULL) {
  # As ci.group.size = 2, we'll increment by 2s
  num.trees = num.trees/2
  
  # Initialise i
  i <- 1
  
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
      
    } else {
      # Merge forests
      old_forest <- new_forest
      new_forest <- merge_forests(list(old_forest, new_tree))
    
    
      # Compute change
      individual_differences <- predict(old_forest, test_X)$predictions -
        predict(new_forest, test_X)$predictions
      
      debiased_error_i <- predict(new_forest, test_X)$debiased.error
      excess_error_i <- predict(new_forest, test_X)$excess.error
        
      # Square and average
      changes <- c(changes, mean(individual_differences ** 2, na.rm = T))
      mean_debiased <- c(mean_debiased, mean(debiased_error_i, na.rm = T))
      mean_excess <- c(mean_excess, mean(excess_error_i, na.rm = T))
      
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
  ## Initialise new tidy dataframe
  error_df <- expand_grid(c(1:num.trees), c(1:length(W.orig))) %>%
    setNames(c('tree', 'case'))
  
  for (i in 1:nrow(error_df)) {
    if (error_df$case[i] in p[[i]]$rowname) {
      pi <- p[[i]]$w_mean[p[[i]]$rowname == error_df$case[i]]
      W <- W.orig[error_df$case[i]]
      Y <- Y.orig[error_df$case[i]]
      new_Y.star <- ((W - p) / (p*(1-p))) * Y
    } else {
      new_Y.star <- NA
    }
    
    # Compare with predictions
  }
  
  # 
  
  map(1:cf$`_num_trees`, function (i) {
    # Get mean treatment in group for tress 1:i
    for (i in 1:length(cf$W.orig)) {
      
    }
  }
  )
  
  ## Get Y*
  # Ystar = Y * (W-p) / p(1 - p)
  # 

  return(list(forest = new_forest, changes = changes))#, debiased = mean_debiased, excess = mean_excess))
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

pcf3 <- fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
                             data_train$Survived,
                             data_train$Sex1,
                             num.trees = 500,
                             test_X = NULL)

# pcf4 <- fit_cf_progressively(data_train %>% select(-Survived, -Sex1),
#                              data_train$Survived,
#                              data_train$Sex1,
#                              num.trees = 5000,
#                              test_X = NULL)
