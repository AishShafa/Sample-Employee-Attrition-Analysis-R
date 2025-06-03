#Aishath Shafa Saudulla
#TP066434



#Step 1: DATA IMPORT (Import CSV file)


# Load the required libraries
library(data.table)
library(dplyr)
library(lubridate)
library(tidyverse)
library(lattice)
library(treemap)
library(gganimate)



# Set the path to CSV file
csv_file <- "C:\\Users\\aisha\\OneDrive\\Documents\\Assignments\\2nd Year\\first sem\\PFDA\\Assignment\\employee_attrition.csv"

# Import the CSV file using data.tables
data <- fread(csv_file, header = TRUE)

#Assigning Headers
names(data)=c("Employee_ID","Record_Date","Birth_Date","Hired_Date","Termination_Date","Age","Length_of_Service","City","Department","Job","Store","Gender_Short","Gender","Reason_Termination","Type_Termination","Status_Year","Status","Business_Unit")



#STEP 2: DATA CLEANING


# Remove duplicate records based on employee ID
data <- unique(data, by = "Employee_ID")



#STEP 3: DATA PRE-PROCESSING



# Perform encoding for job titles
encoded_titles <- data.table::dcast(data, Employee_ID + Age ~ Job, value.var = "Job", fun.aggregate = length, fill = 0)

# Convert data$Age to numeric if it is not already numeric
if (!is.numeric(data$Age)) {
  data$Age <- as.numeric(data$Age)
}

# Create age groups
data$age_group <- cut(data$Age, breaks = c(0, 20, 30, 40, 50, Inf), labels = c("Under 20", "20-29", "30-39", "40-49", "50+"))


#STEP 4: DATA TRANSFORMATION

# Create a new variable for month of birth
# Convert data$Birth_Date to date format
data$Birth_Date <- as.Date(data$Birth_Date)

# Extract the birth month using lubridate::month() to create the variable
data$birth_month <- lubridate::month(data$Birth_Date, label = TRUE)


#STEP 5: DATA EXPLORATIOM

# Viewing methods

#view only 5
head(data, 5)

#view only title\heading
names(data)

#how data stored
class(data)

#summary min/max/average
summary(data$Age)
summary(data)

#glimpse of data
glimpse(data)


-------------------------------------------------------------------------------++---------------------------------------------------

#ANALYSIS BEGINS (15)
  
  
#1 How many employees exist in the age groups
  
# Plot a histogram for age
hist(data$Age, col = "#B0E2FF", border = "black", 
     xlab = "Age", ylab = "Count",
     main = "Histogram of Employee Ages by Count")


#2 relationship between the reasons for termination and the length of service of employees

filtered_data <- data %>%
  filter(Reason_Termination != "Not Applicable")

ggplot(filtered_data, aes(x = Reason_Termination, y = Length_of_Service)) +
  geom_point(color = "red", size = 3) +
  xlab("Reason for Termination") +
  ylab("Length of Service") +
  theme(axis.line = element_line(colour = "black", size = 0.24))


#3 Boxplot Analysis: Gender vs Top 12 Departments


# Filter the data for the top 12 departments
top_departments <- head(sort(table(data$Department), decreasing = TRUE), 12)
filtered_data <- data[data$Department %in% names(top_departments), ]

# Create the boxplot
ggplot(filtered_data, aes(x = Gender, y = Department)) +
  geom_boxplot(fill = "white", color = "black") +
  xlab("Gender") +
  ylab("Department") +
  ggtitle("Boxplot Analysis: Gender vs Top 12 Departments") +
  theme_minimal()



#4 Top 3 City locations and their employee ages compared

top_city <- head(sort(table(data$City), decreasing = TRUE), 3)
filtered_city <- data[data$City %in% names(top_city), ]

# Basic violin plot
p <- ggplot(filtered_city, aes(x = City, y = Age)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
 
# Set trim argument to FALSE
ggplot(filtered_city, aes(x = City, y = Age)) + 
  geom_violin(trim = FALSE)



#5 Boxplot Analysis: Age by Department (Bottom 12)

# Filter the data for the bottom 12 departments
bottom_departments <- tail(sort(table(data$Department), decreasing = TRUE), 12)
filtered_data <- data[data$Department %in% names(bottom_departments), ]

# Create the boxplot with separate plots for each department
ggplot(filtered_data, aes(x = Department, y = Age)) +
  geom_boxplot(fill = "#CD5B45", color = "black") +
  xlab("Department") +
  ylab("Age") +
  ggtitle("Boxplot Analysis: Age by Department (Bottom 12)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Department, scales = "free")



#6 Comparing the Job to how many employees exist in the job titles

# Count the number of job rows for each job category
job_counts <- table(data$Job)

# Create the dot chart with job counts
dotchart(job_counts, labels = names(job_counts), pch = 21, bg = "#FF82AB", pt.cex = 1.5,
         xlab = "Job Count", ylab = "Job Category", main = "Job Counts")



#7 Analysis to see all business units ratio of employees

ggplot(data, aes(x = Business_Unit)) +
  geom_bar(fill = "thistle1", color = "black") +
  labs(x = "Business Unit", y = "Count", title = "Count of Business Units")





#8 Strip Plot of Hired Dates over the years 


# Convert Hired_Date to Date format
data$Hired_Date <- as.Date(data$Hired_Date, format = "%m/%d/%Y")

# Extract year from Hired_Date
data$Hired_Year <- format(data$Hired_Date, format = "%Y")

# Set individual colors for each year
colors <- c("#E69F00", "#56B4E9", "#F0E442", "#009E73", "#D55E00")

# Create strip plot with separate colors for each year
stripplot(Hired_Year ~ as.numeric(Hired_Date), data = data,
          pch = 16, col = colors, aspect = 1,
          main = "Range of Hired Dates", xlab = "Count", ylab = "Hired Date")


#9 Count of Employee ID by Hiring Year

# Convert Hired_Date to Date format
data$Hired_Date <- as.Date(data$Hired_Date, format = "%m/%d/%Y")

# Extract year from Hired_Date
data$Hired_Year <- format(data$Hired_Date, format = "%Y")

# Calculate the count of Employee IDs by Hiring Year
count_data <- as.data.frame(table(data$Hired_Year))

# Create a bar plot for Employee_ID count by Hiring_Year
ggplot(data = count_data, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#E6E6FA", color = "purple") +
  xlab("Hiring Year") +
  ylab("Count of Employee ID") +
  ggtitle("Count of Employee ID by Hiring Year")




#10 Status by Status Year for Employees


ggplot(data, aes(x=Status_Year, y=Status)) +
  geom_line()+
  xlab("Status Year")+
  ggtitle("Status by Status Year for Employees")+
  geom_point()



#11 Length of Service by Gender fill 


ggplot(data, aes(x = Length_of_Service, y= Length_of_Service, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Length of Service by Gender",
       x = "Length of Service",
       y = "Count") +
  scale_fill_manual(values = c("#3366CC", "#FF6666"), labels = c("Male", "Female")) +
  theme_minimal()



#12 Distribution of length of service by department

ggplot(data, aes(y = Length_of_Service, fill = Department)) +
  geom_boxplot() +
  labs(title = "Length of Service by Department", x = "Department", y = "Length of Service") +
  theme_minimal()

#13 Employee Age Distribution

ggplot(data, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, alpha = 0.8, color = "black") +
  labs(title = "Employee Age Distribution", x = "Age", y = "Count") +
  scale_fill_manual(values = c("#3366CC", "#FF6666"), labels = c("Male", "Female")) +
  theme_minimal()


#14 Employee Birth Month Distribution

ggplot(na.omit(data), aes(x = birth_month)) +
  geom_bar(fill = "#B0E2FF") +
  labs(title = "Employee Birth Month Distribution", x = "Month", y = "Count") +
  theme_minimal()


#15 Employee count in top 10 stores

# Calculate employee count by store
store_counts <- table(data$Store)

# Sort the store counts in descending order
sorted_store_counts <- sort(store_counts, decreasing = TRUE)

# Select the top 10 stores with the highest employee count
top_10_stores <- head(sorted_store_counts, 10)

# Visualize employee count by store (top 10 stores)
barplot(top_10_stores, col = "#B9A2EC", border = "black",
        xlab = "Store", ylab = "Count",
        main = "Employee Count by Store (Top 10)")





---------------------------------------------------------------------++----------
#EXTRA FEATURES
  
#1 Pie Chart of top 5 departments and their employee count
  
# Calculate the count of each department
department_counts <- table(data$Department)

# Sort the department counts in descending order
sorted_counts <- sort(department_counts, decreasing = TRUE)

# Select the top 5 departments
top_departments <- names(sorted_counts)[1:5]

# Filter the data for the top 5 departments
top_department_data <- data[data$Department %in% top_departments, ]

# Count the occurrences of the top 5 departments
top_department_counts <- table(top_department_data$Department)

# Create a pie chart for the top 5 departments
pie(top_department_counts, labels = top_department_counts,
    col = rainbow(length(top_department_counts)),
    main = "Top 5 Departments")

# Add a legend
legend("topright", legend = names(top_department_counts),
       fill = rainbow(length(top_department_counts)))






#2 Count of Employees in City's (Treemap)


# Calculate the count of employees in each city
city_counts <- table(data$City)

# Convert city_counts to a data frame
city_counts_df <- as.data.frame(city_counts)

# Rename the columns
colnames(city_counts_df) <- c("City", "Count")

# Create the treemap
treemap(city_counts_df, index = "City", vSize = "Count", title = "Employee Count by City")




#3 Number of Employees by Department


# Calculate count of employees in each department
department_counts <- table(data$Department)

# Convert department counts to data frame
df_department <- data.frame(Department = names(department_counts),
                            Department_Count = as.numeric(department_counts))

# Lollipop chart
ggplot(df_department, aes(x = Department_Count, y = Department)) +
  geom_segment(aes(x = 0, xend = Department_Count, y = Department, yend = Department),
               color = "gray", size = 0.5) +
  geom_point(size = 7) +
  geom_text(aes(label = Department_Count), color = "white", size = 4, nudge_x = 2) +
  labs(title = "Number of Employees by Department",
       x = "Department Count",
       y = "Department Name") +
  theme_minimal()




#4 Employee Length of Service vs Age Scatterplot


# Create animated scatterplot
theme_set(theme_bw())  # Pre-set the bw theme.
g <- ggplot(data, aes(x = Age, y = Length_of_Service)) +
  labs(subtitle = "Employee Length of Service vs Age",
       title = "Moving Scatterplot")

# Animate the scatterplot
p <- g + geom_point(size = 3) +
  transition_time(Length_of_Service) +
  ease_aes("linear")

# Save the animation as a GIF
anim <- animate(p, nframes = 50, fps = 10)
anim_save("Scatterplot.gif", anim)


