# Load the dplyr package using the library() function
?library
library(dplyr)

# Import and read in the MechaCar_mpg.csv file
mechacar_mpg <- read.csv(file = 'MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

# Perform linear regression on all columns using the lm() function
?lm
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechacar_mpg)

# Use the summary() function to get the p-value and the r-squared value for the model
?summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechacar_mpg))

# Import and read in the Suspension_Coil.csv file
suspension_coil <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

# Create use the summarize() function to get the mean, median, variance, and standard deviation of the PSI column
?summarize
total_summary <- suspension_coil %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# Use the group_by() and summarize() functions to group each manufacturing lot by the mean, median, variance, and standard deviation of the PSI column
?group_by
?summarize
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# Use the t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch
?t.test()
all_lot <- t.test(suspension_coil$PSI, mu = 1500)

# Use the t.test() function and its subset() argument to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch

# Lot 1
lot1 <- t.test(subset(suspension_coil, Manufacturing_Lot == "Lot1")$PSI, mu = 1500)

# Lot 2
lot2 <- t.test(subset(suspension_coil, Manufacturing_Lot == "Lot2")$PSI, mu = 1500)

# Lot 3
lot3 <- t.test(subset(suspension_coil, Manufacturing_Lot == "Lot3")$PSI, mu = 1500)
