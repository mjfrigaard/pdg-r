library(dplyr)


create_new_column <- function(data, cols) {
    
     if (length(cols) == 3) {
        
      return_data <- mutate(data, 
                           new_col = paste(
                               .data[[cols[1]]], .data[[cols[2]]], 
                               .data[[cols[3]]], sep = "-")) 
      
    } else if (length(cols) == 2) {
        
     return_data <- mutate(data, 
                           new_col = paste(
                               .data[[cols[1]]], .data[[cols[2]]], sep = "-"))  
     
    } else {
        
     return_data <- mutate(data, 
                           new_col = paste(.data[[cols[1]]], sep = "-")) 
     
    }
    
    return(return_data)
    
}

DFX <- data.frame(a = rep(LETTERS[1:5], 2),
                  b = sample(LETTERS[1:2], 10, replace = TRUE),
                  c = sample(LETTERS[1:5], 10, replace = TRUE),
                  x = 1:10,
                  y = rep(seq(2, 10, by = 2), 2),
                  z = sample(1:10, 10, replace = FALSE))
DFX

create_new_column(data = DFX, cols = c("b", "c"))
create_new_column(data = DFX, cols = c("b", "c" , "y"))
