# Bootstrap function, this function simulates bootstrapping a dataset for the probability 
#of survival for red or blue birds
#this is a comment it doesn't run unless you remove the '#' symbol 
#if you didn't install the package ggplot2 before, uncomment the line below and run it
#install.packages("ggplot2")
library(ggplot2) #you have to run this each time you open R if you want to use this package
options(warn = -1) #getting warning on scale_x_continuous that's annoying
rm(list = ls()) # Clears all variables in the environment
run_bootstrap_simulation <- function(n_bootstrap=n_bootstrap) {
  # Create the groups
  red_birds <- rep("Red", 100)   # Makes a variable of Red birds, rep stands for "repeat"
  blue_birds <- rep("Blue", 100)  # Blue birds
  
  # Combine both groups into one vector
  group <- c(red_birds, blue_birds)
  
  # Create a data frame for plotting
  group_df <- data.frame(ID = 1:200, Group = group)
  
  # Assign probabilities: higher probabilities mean more likely to not survive
  survive_probs <- ifelse(group_df$Group == "Blue", 0.75, 0.25)  # chance for Blue, chance for Red
  
  # Shuffle the group and randomly select individuals to not survive based on probabilities
  survived_indices <- sample(1:200, 100, prob = survive_probs)  # Randomly select survival with unequal probabilities
  
  # Mark Individuals that did not survive in the data frame
  group_df$NotSurvive <- FALSE  # Initialize the not survive column
  group_df$NotSurvive[survived_indices] <- TRUE  # Mark the individuals who did not survive
  
  # Initialize a numeric vector to store bootstrap results
  bootstrap_results <- numeric(n_bootstrap)
  
  # Bootstrap sampling loop
  for (i in 1:n_bootstrap) {
    # Sample with replacement from the original data, respecting survival status
    bootstrap_sample <- sample(1:200, 50, replace = TRUE)
    
    # Get the Groups and Killed status for the sampled indices
    sampled_groups <- group_df$Group[bootstrap_sample]
    sampled_notsurvive <- group_df$NotSurvive[bootstrap_sample]
    
    # Calculate the proportion of surviving Red birds
    surviving_red <- sampled_groups[sampled_notsurvive == FALSE] == "Red"
    if (length(surviving_red) > 0) {
      proportion_red <- mean(surviving_red)
    } else {
      proportion_red <- 1e-10  # Use a small value instead of NA
    }
    bootstrap_results[i] <- proportion_red
  }
  
  # Create a data frame for bootstrap results
  bootstrap_df <- data.frame(Proportion_Surviving_Red = bootstrap_results)
  # Check the data frame to ensure it's correct
  #print(summary(bootstrap_df))  # prints the mean,median, and IQR
  
  # Plot the distribution of bootstrap results
  #all plotting code is stored in the variable 'p'
    p <- ggplot(bootstrap_df, aes(x = Proportion_Surviving_Red)) +
      geom_histogram(binwidth = 0.01, fill = "lightpink", color = "black", alpha = 0.7) +
      labs(title = paste("Bootstrapped Proportion of Surviving Red Birds", n_bootstrap, "X"),
           x = "Proportion of Surviving Red Birds",
           y = "Frequency") +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  # Remove major grid lines
            panel.grid.minor = element_blank(),  # Remove minor grid lines
            axis.text.x = element_text(size = 14),  # Increase x-axis text size
            axis.text.y = element_text(size = 14))
   p  # show plot
}
# Call the function, increase n_bootstrap to increase the number of times you resample
run_bootstrap_simulation(n_bootstrap=1)


