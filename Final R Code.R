library(readxl)
library(dplyr) 
library(tidyr)
library(ggplot2)  

# to predict 2026 values, variables were graph to identify trends 
# then they were modelled based on a lm() or avg() 

file_path <- "2025 Case Comp Data CLEAN.xlsx"   
data <- read_excel(file_path, sheet = "Data")

# 1) Predicting and Graphing the future number of employees 

# summarising the number of employees by industry for each year
employee_summary <- data %>%
  group_by(Industry) %>%  #Group the data by industry
  summarise(
    `2023 Employees` = sum(`Number of employees 2023`),  
    `2024 Employees` = sum(`Number of employees 2024`),  
    `2025 Employees` = sum(`Number of employees 2025`)  
  )

# prep data for linear modeling by reshaping it into long format after extracting from excel 
employee_summary_long <- employee_summary %>%
  pivot_longer(cols = c(`2023 Employees`, `2024 Employees`, `2025 Employees`), 
               names_to = "Year",           #create column Year
               values_to = "Employees")     # create column called employees

#clean the year column to remove the text and convert it to numeric
employee_summary_long <- employee_summary_long %>%
  mutate(Year = as.numeric(sub(" Employees", "", Year)))  # remove text and convert to numeric

#Use linear regression for each industry to predict the number of employees in 2026
lm_results <- lapply(unique(employee_summary_long$Industry), function(sector) {
  sector_data <- filter(employee_summary_long, Industry == sector)  # Filter the date
  fit <- lm(Employees ~ Year, data = sector_data)  #use linear regression for th two variables
  prediction_2026 <- predict(fit, newdata = data.frame(Year = 2026))  # predicts the number of employees for 2026
  data.frame(Industry = sector, Predicted_Employees_2026 = prediction_2026)  # Store the result in a new data frame
})

#combine all the individual linear model results into a single data frame
predictions_df <- bind_rows(lm_results)  # bind the rows of each model result into a single data frame 

#join the predictions with the original employee summary by industry with 2026 on the right 
final_summary <- left_join(employee_summary, predictions_df, by = "Industry")

#view the final table with predictions for 2026 to check values 
print(final_summary)

# Reshape the final summary into long format again, this time to include the predicted 2026 data
employee_summary_long <- final_summary %>%
  pivot_longer(cols = c(`2023 Employees`, `2024 Employees`, `2025 Employees`, `Predicted_Employees_2026`), 
               names_to = "Year",           # Create column year
               values_to = "Employees")     #create a column employees

#clean the year column again for proper formatting when put into the graph 
employee_summary_long <- employee_summary_long %>%
  mutate(Year = case_when(  
    Year == "Predicted_Employees_2026" ~ 2026,   #replace "Predicted_Employees_2026" with 2026 for plotting on the graph nicer
    TRUE ~ as.numeric(sub(" Employees", "", Year))  # convert other years to numeric format
  ))

#subset actual data for 2023-2025 and predicted data (2026) portions
data_actual <- filter(employee_summary_long, Year != 2026)  # filter the actual data (2023-2025)
data_predicted <- filter(employee_summary_long, Year == 2026)  # Filter the predicted data (2026)

#create a helper table to store the segments connecting 2025 and 2026
segment_data <- left_join(
  data_actual %>% filter(Year == 2025),  # Filter the data for 2025
  data_predicted,  # Join with the predicted 2026 data
  by = "Industry"  #join by the industry column
) %>%
  select(Industry, Employees.x, Employees.y) %>%  # Select relevant columns: Industry, Employees for 2025 and 2026
  rename(Employees_2025 = Employees.x, Employees_2026 = Employees.y) %>% 
  mutate(Year_2025 = 2025, Year_2026 = 2026)  #manually add the year columns for segment plotting

#create the plot using
ggplot() +
  #plot actual data (2023–2025) as a solid line and points
  geom_line(data = data_actual, 
            aes(x = Year, y = Employees, color = Industry, group = Industry),  
            linewidth = 1) +  #set line width for the solid line
  geom_point(data = data_actual, 
             aes(x = Year, y = Employees, color = Industry),  
             size = 3) +  # set the size of points
  #add a dashed line segment from the actual value in 2025 to the predicted value in 2026
  geom_segment(data = segment_data, 
               aes(x = Year_2025, xend = Year_2026, 
                   y = Employees_2025, yend = Employees_2026, 
                   color = Industry),  
               linetype = "dashed",   
               linewidth = 1) +       #set the line width for the dashed line
  #plot the predicted 2026 values 
  geom_point(data = data_predicted, 
             aes(x = Year, y = Employees, color = Industry),  
             shape = 17,   # use triangle shape for 2026 data points
             size = 3) +   #size of points
  labs(title = "Actual (2023–2025) and Predicted (2026) Employees by Industry",
       x = "Year", 
       y = "Number of Employees") +
  theme_minimal() + 
  theme(legend.position = "right")


# 2) Predicting average weekly wage and graphing the predictions 

#Summarising the average weekly wage by industry for each year
wage_summary <- data %>%
  group_by(Industry) %>% 
  summarise(
    `2023 Wages` = sum(`2023 wages`) / sum(`Number of employees 2023`) / 52,  
    `2024 Wages` = sum(`2024 wages`) / sum(`Number of employees 2024`) / 52,  
    `2025 Wages` = sum(`2025 wages`) / sum(`Number of employees 2025`) / 52   
  )


# Prep the data for linear modeling by reshaping it into long format
wage_summary_long <- wage_summary %>%
  pivot_longer(cols = c(`2023 Wages`, `2024 Wages`, `2025 Wages`), 
               names_to = "Year",           # Create year column
               values_to = "Wages")        # Create a new column wages

# Clean the year column to remove the wages text and convert it to numeric format
wage_summary_long <- wage_summary_long %>%
  mutate(Year = as.numeric(sub(" Wages", "", Year)))  # Remove Wages text and convert to numeric

#apply linear regression for each industry to predict the average weekly wage in 2026
lm_results <- lapply(unique(wage_summary_long$Industry), function(sector) {
  sector_data <- filter(wage_summary_long, Industry == sector)  #filter data for each sector
  fit <- lm(Wages ~ Year, data = sector_data)  # perform linear regression
  prediction_2026 <- predict(fit, newdata = data.frame(Year = 2026))  # Predict the average weekly wage for 2026
  data.frame(Industry = sector, Predicted_Wages_2026 = prediction_2026)  #store the result in a new data frame
})

#combine all the individual linear model results into a single data frame
predictions_df <- bind_rows(lm_results)  #bind the rows of each model result into a single data frame

# Join the predictions with the original wage summary by industry
final_summary <- left_join(wage_summary, predictions_df, by = "Industry")

#view final table 
print(final_summary)

# Reshape the final summary into long format again, this time to include the predicted 2026 data
wage_summary_long <- final_summary %>%
  pivot_longer(cols = c(`2023 Wages`, `2024 Wages`, `2025 Wages`, `Predicted_Wages_2026`), 
               names_to = "Year",           # Create column year
               values_to = "Wages")         # Create column wages

#clean year column
wage_summary_long <- wage_summary_long %>%
  mutate(Year = case_when(  
    Year == "Predicted_Wages_2026" ~ 2026,   # Replace "Predicted_Wages_2026" with 2026 for plotting
    TRUE ~ as.numeric(sub(" Wages", "", Year))  # Convert other years to numeric format
  ))

#subset actual data (2023–2025) and predicted data (2026) portions
data_actual <- filter(wage_summary_long, Year != 2026)  # Filter the actual data
data_predicted <- filter(wage_summary_long, Year == 2026)  #filter the predicted data (2026)

#Create helper table to store the segments connecting 2025 and 2026
segment_data <- left_join(
  data_actual %>% filter(Year == 2025),  # Filter the data
  data_predicted,  # Join with the predicted 2026 data
  by = "Industry"  #join by the industry column
) %>%
  select(Industry, Wages.x, Wages.y) %>%  # Select relevant columns that are industr and wages for 2025 and 2026
  rename(Wages_2025 = Wages.x, Wages_2026 = Wages.y) %>%  # Rename 
  mutate(Year_2025 = 2025, Year_2026 = 2026)  # Manually add the year columns for segment plotting

# Create plot 
ggplot() +
  # Plot actual data (2023–2025) as a solid line and points
  geom_line(data = data_actual, 
            aes(x = Year, y = Wages, color = Industry, group = Industry),  
            linewidth = 1) +  #set line width for the solid line
  geom_point(data = data_actual, 
             aes(x = Year, y = Wages, color = Industry),  
             size = 3) +  # sset the size of points
  # Add a dashed line segment from the actual value in 2025 to the predicted value in 2026
  geom_segment(data = segment_data, 
               aes(x = Year_2025, xend = Year_2026, 
                   y = Wages_2025, yend = Wages_2026, 
                   color = Industry),  
               linetype = "dashed",  
               linewidth = 1) +       #Set the line width for the dashed line
  # Plot the predicted 2026 values 
  geom_point(data = data_predicted, 
             aes(x = Year, y = Wages, color = Industry),  
             shape = 17,   # Use triangle shape FOR 2026 DATA points
             size = 3) +   
  labs(title = "Actual (2023–2025) and Predicted (2026) Average Weekly Wages by Industry",
       x = "Year", 
       y = "Average Weekly Wage") +
  theme_minimal() +  
  theme(legend.position = "right") 


# 3) Medical costs per claim prediction for 2026 

# Summarising  medical cost per claim by industry for each year
cost_summary <- data %>%
  group_by(Industry) %>%  # Group the data by industry
  summarise(
    `2023 Medical Cost per Claim` = sum(`2023 medical costs`) / sum(`2023 number of claims`),  
    `2024 Medical Cost per Claim` = sum(`2024 medical costs`) / sum(`2024 number of claims`),  
    `2025 Medical Cost per Claim` = sum(`2025 medical costs`) / sum(`2025 number of claims`)   
  )

#calculate the average for 2023–2025 and use it as the prediction for 2026
cost_summary <- cost_summary %>%
  mutate(
    `Predicted Medical Cost per Claim 2026` = (`2023 Medical Cost per Claim` + `2024 Medical Cost per Claim` + `2025 Medical Cost per Claim`) / 3
  )

#prepare the data for graphing with correct axis labels by reshaping it into long format
cost_summary_long <- cost_summary %>%
  pivot_longer(cols = c(`2023 Medical Cost per Claim`, `2024 Medical Cost per Claim`, `2025 Medical Cost per Claim`, `Predicted Medical Cost per Claim 2026`), 
               names_to = "Year",            # Create a new column for year names
               values_to = "Medical Cost per Claim")  #create column

#clean the year column to remove the text and convert it to numeric format
cost_summary_long <- cost_summary_long %>%
  mutate(Year = case_when(  
    Year == "Predicted Medical Cost per Claim 2026" ~ 2026,   # Set the year for predicted value to 2026
    TRUE ~ as.numeric(sub(" Medical Cost per Claim", "", Year))  #convert other years to numeric format
  ))

# subset actual data (2023–2025) and predicted data (2026) portions
data_actual <- filter(cost_summary_long, Year != 2026)  #filter the actual data (2023-2025)
data_predicted <- filter(cost_summary_long, Year == 2026)  # Filter the predicted data (2026)

#create a helper table to store the segments connecting 2025 and 2026
segment_data <- left_join(
  data_actual %>% filter(Year == 2025),  #filter the data for 2025
  data_predicted,  #join with the predicted 2026 data
  by = "Industry"  # Join by the industry column
) %>%
  select(Industry, `Medical Cost per Claim.x`, `Medical Cost per Claim.y`) %>%  # Select relevant columns, indutry and Cost for 2025 and 2026
  rename(Medical_Cost_2025 = `Medical Cost per Claim.x`, Medical_Cost_2026 = `Medical Cost per Claim.y`) %>%  # Rename
  mutate(Year_2025 = 2025, Year_2026 = 2026)  # Manually add the year columns for segment plotting

# Create the plot
ggplot() +
  #plot actual data (2023–2025) as a solid line and points
  geom_line(data = data_actual, 
            aes(x = Year, y = `Medical Cost per Claim`, color = Industry, group = Industry),  
            linewidth = 1) +  # set line width for the solid line
  geom_point(data = data_actual, 
             aes(x = Year, y = `Medical Cost per Claim`, color = Industry),  
             size = 3) +  # Set the size
  # add a dashed line segment from the actual value in 2025 to the predicted value in 2026
  geom_segment(data = segment_data, 
               aes(x = Year_2025, xend = Year_2026, 
                   y = Medical_Cost_2025, yend = Medical_Cost_2026, 
                   color = Industry),  
               linetype = "dashed",   # Set the line type
               linewidth = 1) +       # Set the line width for the dashed line
  # Plot the predicted 2026 values 
  geom_point(data = data_predicted, 
             aes(x = Year, y = `Medical Cost per Claim`, color = Industry),  
             shape = 17,   # Use triangle shape for 2026 data points
             size = 3) +   # Set the size
  labs(title = "Average Medical Cost per Claim by Industry (2023–2025) and Predicted (2026)",
       x = "Year", 
       y = "Average Medical Cost per Claim") +
  theme_minimal() +  # Use minimal theme
  theme(legend.position = "right")  # Place the legend on the right side


# 4) Predicting the total number of claims and graphing 

# Summarising the total number of claims by industry for each year
claim_summary <- data %>%
  group_by(Industry) %>%  #group the data by industry
  summarise(
    `2023 Total Claims` = sum(`2023 number of claims`),  
    `2024 Total Claims` = sum(`2024 number of claims`),  
    `2025 Total Claims` = sum(`2025 number of claims`)   
  )

# Prepare the data by reshaping it into long format for modeling
claim_summary_long <- claim_summary %>%
  pivot_longer(cols = c(`2023 Total Claims`, `2024 Total Claims`, `2025 Total Claims`), 
               names_to = "Year",            # Create the cololum year
               values_to = "Total Claims")  # Create column

# Clean the year column to remove the text and convert it to numeric format
claim_summary_long <- claim_summary_long %>%
  mutate(Year = as.numeric(sub(" Total Claims", "", Year)))  #remove text and convert to numeric

# Apply linear regression for each industry to predict the total claims in 2026
lm_results_claims <- lapply(unique(claim_summary_long$Industry), function(sector) {
  sector_data <- filter(claim_summary_long, Industry == sector)  # Filter data for each sector
  fit <- lm(`Total Claims` ~ Year, data = sector_data)  # Perform linear regression
  prediction_2026 <- predict(fit, newdata = data.frame(Year = 2026))  #predict the total claims for 2026
  data.frame(Industry = sector, Predicted_Total_Claims_2026 = prediction_2026)  # Store the result in a new data frame
})

# Combine all the individual linear model results into a single data frame
predictions_claims_df <- bind_rows(lm_results_claims)  #Bind the rows of each model result into a single data frame

#join the predictions with the original claim summary by industry
final_claims_summary <- left_join(claim_summary, predictions_claims_df, by = "Industry")

#reshape the final summary into long format again, this time to include the predicted 2026 data
claim_summary_long <- final_claims_summary %>%
  pivot_longer(cols = c(`2023 Total Claims`, `2024 Total Claims`, `2025 Total Claims`, `Predicted_Total_Claims_2026`), 
               names_to = "Year",            # Create a column year and total claims
               values_to = "Total Claims")  

# Clean the year column again for proper formatting
claim_summary_long <- claim_summary_long %>%
  mutate(Year = case_when(  
    Year == "Predicted_Total_Claims_2026" ~ 2026,   # Replace "Predicted_Total_Claims_2026 with 2026
    TRUE ~ as.numeric(sub(" Total Claims", "", Year))  # Convert other years to numeric format
  ))

# Subset actual data (2023–2025) and predicted data (2026) portions
data_actual_claims <- filter(claim_summary_long, Year != 2026)  # Filter the actual data 2023-2025
data_predicted_claims <- filter(claim_summary_long, Year == 2026)  # Filter the predicted data (2026)

# Create the plot with an area plot
ggplot(claim_summary_long, aes(x = Year, y = `Total Claims`, fill = Industry)) +
  geom_area(alpha = 0.7) +  #create the area plot
  geom_line(data = data_predicted_claims, aes(x = Year, y = `Total Claims`, color = Industry), 
            linetype = "dashed", size = 1) +  #add a dashed line for the predicted 2026
  labs(title = "Total Claims by Industry Over Time with 2026 Prediction",
       x = "Year", 
       y = "Total Claims") +
  theme_minimal() +  # Use minimal theme
  theme(legend.position = "right")  #place the legend on the right side
