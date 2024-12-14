
#AI and HRM in Nigeria Banks: Temmy
library(skimr)
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(corrplot)
library(stargazer)
library(ggpubr)
library(rstatix)
library(dplyr)
library(readxl)

#load data
temmy<-read_excel("temmy_data.xlsx", sheet = 1)
skim(temmy)
colnames(temmy)

#remove all missing values
temmy <- temmy[complete.cases(temmy),]  #143 remains after removing missing values.
write.csv(temmy, "cleaned_temmy.csv")

#demographic
count <- geom_text(stat='count', aes(label=after_stat(count)), 
                   vjust=1.2)
perc <- geom_text(stat='count', aes(label=after_stat(round(count/sum(count)*100,1))),
                  vjust=2.6,color='white')
#Gender
gender_table <- table(temmy$Gender)

# Create a data frame for plotting
gender_df <- data.frame(Gender = names(gender_table), Count = as.numeric(gender_table))

# Create a pie chart for 'Gender' with frequency and count labels
ggplot(gender_df, aes(x = "", y = Count, fill = Gender)) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar("y", start = 0) +
   geom_text(aes(label = paste0(Gender, "\n", Count, " (", scales::percent(Count / sum(Count)), ")")),
             position = position_stack(vjust = 0.5)) +
   theme_void() +
   labs(title = "Distribution of Respondents by Gender", fill= "Gender")

#Training_AI
gender_table <- table(temmy$Training_AI)

# Create a data frame for plotting
gender_df <- data.frame(Gender = names(gender_table), Count = as.numeric(gender_table))

# Create a pie chart for 'Gender' with frequency and count labels
ggplot(gender_df, aes(x = "", y = Count, fill = Gender)) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar("y", start = 0) +
   geom_text(aes(label = paste0(Gender, "\n", Count, " (", scales::percent(Count / sum(Count)), ")")),
             position = position_stack(vjust = 0.5)) +
   theme_void() +
   labs(title = "", fill= "AI Training in Organization?")

#Education
education <- ggplot(temmy, aes(Education, fill=Education))+
   geom_bar() + 
   count + perc+
   labs(title = "Distribution of Respondents by Education", fill= "Education")  + 
   theme_bw()
education

age <- ggplot(temmy, aes(Age, fill=Age))+
   geom_bar() + 
   count + perc+
   labs(title = "Distribution of Respondents by Age", fill= "Age")  + 
   theme_bw()
age

experience <- ggplot(temmy, aes(Year_Experience, fill=Year_Experience))+
   geom_bar() + 
   count + perc+
   labs(title = "Distribution of Respondents by Years of Experience", fill= "Years of Experience")  + 
   theme_bw()
experience

position <- ggplot(temmy, aes(Job_position, fill=Job_position))+
   geom_bar() + 
   count + perc+
   labs(title = "Distribution of Respondents by Job position", fill= "Job position")  + 
   theme_bw()
ggsave(filename = "position.png", plot =position, device = "png", 
       width = 10, height = 5)

familiarity <- ggplot(temmy, aes(Familiarity_AI, fill=Familiarity_AI))+
   geom_bar() + 
   count + perc+
   labs(title = "Familiarity with AI in Professional Context", fill= "Response")  + 
   theme_bw()
ggsave(filename = "familiarity.png", plot =familiarity, device = "png", 
       width = 10, height = 5)


extent <- ggplot(temmy, aes(Extent_AI, fill=Extent_AI))+
   geom_bar() + 
   count + perc+
   labs(title = "Extent of AI Adoption in the Banks", fill= "Response")  + 
   theme_bw()
ggsave(filename = "extent.png", plot =extent, device = "png", 
       width = 8, height = 5)

function_ai <- ggplot(temmy, aes(Function_AI, fill=Function_AI))+
   geom_bar() + 
   count + perc+
   labs(title = "Function of AI in the Banks", fill= "Response")  + 
   theme_bw()
ggsave(filename = "function_ai.png", plot =function_ai, device = "png", 
       width = 8, height = 5)


####===============COUNT RESPONSES FOR LIKERT SCALE===========####
df <- temmy %>% select(-c(Age,Gender, Education, Year_Experience, Job_position,
                          Familiarity_AI, Extent_AI,Function_AI, Training_AI))

# Create an empty data frame to store the counts
count_table <- matrix(0, nrow = 23, ncol = 5)
colnames(count_table) <- c("1", "2", "3", "4", "5")
rownames(count_table) <- paste0("q", 1:23)

# Loop through each variable and calculate counts
for (i in 1:23) {
   # Create a table for the current variable
   tbl <- table(df[[paste0("q", i)]])
   
   # Check if all response categories are present
   if (length(names(tbl)) < 5) {
      # Fill in missing response categories with counts of 0
      missing <- setdiff(c("1", "2", "3", "4", "5"), names(tbl))
      tbl[missing] <- 0
   }
   
   # Assign the table to the count table
   count_table[i, ] <- tbl
}

# Print the count table
print(count_table)
write.csv(count_table, "response_counts.csv")


##==============CHI-SQAURE MEASURE OF DEPENDENCE============###

#(q1, q2, ..., q7) vs Familiarity_AI, Do the same for q8-q13
# Create an empty matrix to store chi-square and p-values
chi_square_matrix <- matrix(NA, nrow = 23, ncol = 2, 
                            dimnames = list(paste0("q", 1:23), c("Chi-square", "P-value")))
# Loop through each question (q1, q2, ..., q7)
for (i in 1:23) {
   # Calculate chi-square and p-value
   chi_sq_result <- chisq.test(table(temmy[[paste0("q", i)]], temmy$Familiarity_AI))
   chi_square_matrix[i, "Chi-square"] <- format(chi_sq_result$statistic, digits = 4)
   chi_square_matrix[i, "P-value"] <- format(chi_sq_result$p.value, digits = 4)
}

# Print the resulting matrix
print(chi_square_matrix)

#(q1, q2, ..., q7) vs Training_AI
# Loop through each question (q1, q2, ..., q7)
chi_square_matrix1 <- matrix(NA, nrow = 23, ncol = 2, 
                            dimnames = list(paste0("q", 1:23), c("Chi-square", "P-value")))
for (i in 1:23) {
   # Calculate chi-square and p-value
   chi_sq_result1 <- chisq.test(table(temmy[[paste0("q", i)]], temmy$Training_AI))
   chi_square_matrix1[i, "Chi-square"] <- format(chi_sq_result1$statistic, digits = 4)
   chi_square_matrix1[i, "P-value"] <- format(chi_sq_result1$p.value, digits = 4)
}

# Print the resulting matrix
print(chi_square_matrix1)

df_chi_result<-data.frame(chi_square_matrix,chi_square_matrix1)
write.csv(df_chi_result, "df_chi_result.csv")

