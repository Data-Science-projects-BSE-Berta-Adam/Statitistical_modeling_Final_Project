library(MASS)
library(dplyr)
library(tidyverse) # multipurpose package developped by Posit
library(data.table)
set.seed(1234)

darwin <- read.csv("DARWIN.csv", sep = ",", header = TRUE)


darwin_Y <- as.numeric(darwin$class %in% c("P")) #generate a dichotomous variable
darwin_x <- data.frame(darwin[, !colnames(darwin) %in% c("ID", "class")])

drop_darwin_x <- darwin_x %>% dplyr::select(-starts_with(c("total_time", "mean_gmrt")))

# Extract substrings excluding numbers using gsub to obtain unique column names
unique_variable_names <- unique(
  
  gsub("\\d", "", names(drop_darwin_x))
)
unique_variable_names

#darwin_test <- darwin_x %>% dplyr::select(ends_with(test_names))

unique_variable_names
colnames(darwin_x)
##############################

transformed_darwin_generator <- function(input_df, unique_variable_names, list_of_pairs, diff = TRUE) {
  stopifnot(is.logical(diff), !is.na(diff))
  

  rowmean_diff_df_gen <- function(test_pairs) {
    test_1 <- test_pairs[1]
    test_2 <- test_pairs[2]
  
    n_obs_individual <- nrow(input_df) #observations
    num_variable_names <- length(unique_variable_names) #number of unique metrics evaluated across experiments

  
    output_df_1 <- data.frame(matrix(ncol = num_variable_names, nrow = n_obs_individual))
    colnames(output_df_1) <- unique_variable_names #create an empty dataframe
  
    output_df_2 <- data.frame(matrix(ncol = num_variable_names, nrow = n_obs_individual))
    colnames(output_df_2) <- unique_variable_names #create an empty dataframe
  
  # compute averages 
    for (variable_name in unique_variable_names){
      nm_vector <- paste0(variable_name, "", c(test_1, test_2))
      output_df_1[variable_name] <- rowMeans(input_df[nm_vector], na.rm = TRUE)
    }
    
    mean_names_test_1_2 <- paste0("mean_", unique_variable_names, "_" ,test_1, "_", test_2)
    colnames(output_df_1) <- mean_names_test_1_2
  
    # compute differences
    if (diff == TRUE){
      for (variable_name in unique_variable_names){
        nm_vector1 <- paste0(variable_name, "", c(test_1))
        nm_vector2 <- paste0(variable_name, "", c(test_2))
        output_df_2[variable_name] <- input_df[nm_vector1] - input_df[nm_vector2]
      }
    
  
    diff_names_test_1_2 <- paste0("diff_", unique_variable_names, "_" ,test_1, "_", test_2)
    colnames(output_df_2) <- diff_names_test_1_2
    
    # merge both metrics
    
    output_df <- cbind(output_df_1, output_df_2)
    return(output_df)
    
    } else {
      output_df <- output_df_1
      return(output_df)
    }
  
    # merge both metrics
  
    output_df <- cbind(output_df_1, output_df_2)
  
    return(output_df)
  }
  ### now create the final dataset with the function we created inside this function

  #iterate over list of pairs to generate new transformed columns
  mean_diff_darwin_x <- data.frame() #empty dataframe to pour values into

    for (pair in list_of_pairs) {
      output <- rowmean_diff_df_gen(pair)
      mean_diff_darwin_x <- data.frame(append(output, mean_diff_darwin_x))
    }

  # merge with original dataset darwin_x and remove processed columns 

  complete_vector <-c()

  for (element in unlist(list_of_pairs)){
    string_vector <- paste0(unique_variable_names, element)
    complete_vector <- append(complete_vector, string_vector)
  }

input_df <- input_df %>% dplyr::select(-ends_with(complete_vector))

mean_diff_darwin_x <- cbind(mean_diff_darwin_x, input_df)

return(mean_diff_darwin_x) #return output

}
##############################################3
list_of_pairs <- list(c(2,3), c(4,5))

functional_darwin <- transformed_darwin_generator(darwin_x, unique_variable_names, list_of_pairs, diff = TRUE)

#################################3