#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// Testing git

// [[Rcpp::export]]
NumericVector evaluate_node(List datapoint, List fit, int node_num = 1) {

  // Adapt from R by reducing node_num by 1
  node_num -= 1 ;
  
  List nodes = fit["nodes"] ; 
  List node = nodes[node_num] ;
  NumericVector node_stats(3) ;
  
  // Check if node is leaf
  if (node["is_leaf"]) {
    NumericVector node_stats = node["leaf_stats"] ;
    NumericVector node_results = NumericVector::create( _["node_num"] = node_num, 
                             _["avg_Y"] = node_stats[0],
                             _["avg_Y"] = node_stats[1]);                               
    return node_results ;
  } else {
    
  
    int test_variable = node["split_variable"] ;
    test_variable -= 1 ;
    double test_value = datapoint[test_variable] ;
    double split_value = node["split_value"] ;
    bool evaluation = NA_LOGICAL ;
  
    if (true) { // Replaced with true condition for testing
      evaluation = test_value <= split_value ;
      // Rprintf("Test variable: %i\n", test_variable) ;
      // Rprintf("Test value: %f\n", test_value) ;
      // Rprintf("Split value: %f\n", split_value) ;
    } else {
      evaluation = node["send_missing_left"] ;
    }
    
    NumericVector x(3) ;
  
    if (evaluation) {
      int next_node = node["left_child"] ;
      // Rprintf("Evaluated TRUE, moving left to node %i\n", next_node) ;
      x = evaluate_node(datapoint, fit, node_num = next_node) ;
      return x ;
    } else {
      int next_node = node["right_child"] ;
      // Rprintf("Evaluate%sd FALSE, moving right\n") ;
      x = evaluate_node(datapoint, fit, node_num = next_node) ;
      return x ;
    }
  }
}




// [[Rcpp::export]]
DataFrame evaluate_nodes(List datapoints, List fit) {
  int ncols = datapoints.length() ;
  NumericVector first_vector = datapoints[0] ;
  int nrows = first_vector.length() ;
  NumericVector temp_output(3) ;
  NumericVector long_vector(3 * nrows) ;
  // NumericMatrix output(list_length , 3) ;
  // int index = 0 ;
  List tempinput(ncols) ;
  
  // Create temp vectors
  NumericVector node_num(nrows) ;
  NumericVector avg_Y(nrows) ;
  NumericVector avg_w(nrows) ;

  for (int i = 0; i < nrows; i++) {
    for (int j=0; j<ncols; j++) {
      NumericVector column = datapoints[j] ;
      tempinput[j] = column[i] ;
    }
    
    temp_output = evaluate_node(tempinput, fit, 1) ;
    node_num[i] = temp_output[0] ;
    avg_Y[i] = temp_output[1] ;
    avg_w[i] = temp_output[2] ;
  }
  
  // Create dataframe
  DataFrame df = DataFrame::create( Named("node_num") = node_num,
                                    Named("avg_Y") = avg_Y,
                                    Named("avg_w") = avg_w) ;
  
  return df ;
}


// Doesn't yet work due to naming issue
// [[Rcpp::export]]
List evaluate_forest(List datapoints, List forest) {
  int forest_length = forest.length() ;
  List results(forest_length) ;
  List current_tree(5) ;
  
  for (int i = 0; i < forest_length; i++) {
    current_tree = forest[i] ;
    DataFrame result = evaluate_nodes(datapoints, current_tree) ;
    results[i] = result ;
  }
  
  return results ;
}


/*** R
# library(tidyverse)
# library(grf)
# print(evaluate_node(pcf3$forest$X.orig[30,] %>% as.list(), pcf3$forest %>% get_tree(3), 1))
*/


// Rcpp::List get_leaves(Rcpp::List cf) {
//   int num_trees = cf["_num_trees"] ;
//   
//   for(int i = 0; i < num_trees; i++) {
//     int cf_sample = cf["_drawn_samples"][i] ;
//     int number_leaves = cf["_leaf_samples"][i].size() ;
//     int index_in_leaf = 0 ;
//     
//     for(int j = 0; j < number_leaves; j++) {
//       // Start a loop to test if values within leaves are our value
//       int leaf_samples_length = cf["_leaf_samples"][i][j].size() ;
//       int k = 0 ;
//       
//       // Here we are finding a match within the leaves
//       while(true) // We can set this condition because we have breaks for every condition
//         {
//         if(cf["_leaf_samples"][i][j][k] == cf_sample) break ;
//         k++ ;
//         if (k < leaf_samples_length) {
//           k = 0 ;
//           break ;
//         }
//       }
//       index_in_leaf += k ; // we can get the leaf index because there will only be one non-zero number
//     }
//     
//     
//     int index_in_leaf = 0
//     for(int m = 0; m < number_leaves; m++) index_in_leaf +=   
//       map(function (x) map_lgl(cf$`_leaf_samples`[[i]], function (y) x %in% y) %>% which()) %>%
//       setNames(cf$`_drawn_samples`[[i]]) %>%
//       unlist() %>%
//       as.data.frame() %>%
//       rownames_to_column() %>%
//       setNames(c('rowname', 'group')) %>%
//       mutate(group = as.numeric(group))
//   })
// }
