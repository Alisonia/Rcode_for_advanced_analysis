#loading library
library(readxl)
df<-read_excel("ready_data.xlsx", sheet=1)
head(df)
dim(df)
df <- df[complete.cases(df),] #remove all missing values
dim(df)

#===================subsetting data for manova, analysis 1=====================#
library(dplyr)
colnames(df)
manova_1 <- df %>% select(-c(positive,negative,neutral,single_factor))
colnames(manova_1)

#descriptive statistics
library(psych)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(car)

# Descriptive statistics
#treatment
treat_sum <- manova_1 %>% 
   group_by(treatment)%>%
   get_summary_stats(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,
                     q11,q12,q13,q14,q15,q16,q17,q18,q19,
                     q20,q21,q22,q23,q24, type="mean_sd")
group_sum <- manova_1 %>% 
   group_by(group)%>%
   get_summary_stats(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,
                     q11,q12,q13,q14,q15,q16,q17,q18,q19,
                     q20,q21,q22,q23,q24, type="mean_sd")
write.csv(treat_sum, "summary_trt.csv")
write.csv(group_sum, "summary_group.csv")

#assumption checks
#adequate sample size, each group should > number of dep. vars (24)
manova_1 %>% group_by(treatment) %>% summarise(N=n()) #84, 94
manova_1 %>% group_by(group) %>% summarise(N=n()) #gp 90, mh 88

#independent of obs....Yes,satisfied

# Multivariate outliers
manova_1 %>%
   group_by(treatment) %>%
   mahalanobis_distance() %>%
   filter(is.outlier == TRUE) %>%
   as.data.frame()

#multivariate normality
manova_1 %>% select(-c(treatment,group)) %>% mshapiro_test() #assumption violated


#multicollinearity, no existence
library(corrplot)
cor_job<-cor(manova_1 %>% select(-c(treatment,group)))
corrplot(cor_job, type='lower',order='hclust',tl.col='black',tl.srt=45, method = "color")

#homgeneity of covariance ...violated but run the test Pillai's multivariate instead of wilks
box_m(manova_1[, c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10",
                   "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18", "q19",
                   "q20", "q21", "q22", "q23", "q24")], manova_1$group)

#homogeneity of variance, violated for some items
print(manova_1 %>% 
   gather(key = "variable", value = "value", q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,
          q11,q12,q13,q14,q15,q16,q17,q18,q19,
          q20,q21,q22,q23,q24) %>%
   group_by(variable) %>% levene_test(value ~ treatment), n=24)

#========================computation==================================#
# Define the MANOVA model with the dependent variables (q1 to q24)
manova_model <- manova(cbind(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, 
                             q11, q12, q13, q14, q15, q16, q17, q18, q19, 
                             q20, q21, q22, q23, q24) ~ treatment * group, 
                       data = manova_1)

# Summarize the MANOVA model
summary(manova_model, test = "Pillai")

# Subset the data for MH group
mh1_data <- manova_1 %>% filter(group == "MH")
mh1_data$mean_combined <- rowMeans(mh1_data[, 1:24])

# Perform pairwise t-test for MH group
pwc_mh1 <- mh1_data %>%
   gather(key = "variables", value = "value", q5, q9, 
          q20) %>%
   group_by(variables) %>%
   pairwise_t_test(value ~ treatment, pool.sd = FALSE, var.equal = FALSE)
print(pwc_mh1, n=24)

pwc_mh1 <- pwc_mh1 %>% add_xy_position(x = "treatment")
test.label <- create_test_label(
   description = "MANOVA", statistic.text = quote(italic("F")),
   statistic = 3.31, p = "<0.0001", parameter = "24,151",
   type = "expression", detailed = TRUE
)
ggboxplot(
   mh1_data, x = "treatment", y = c("q5","q9","q20"), 
   merge = TRUE, palette = "aaas"
) + 
   stat_pvalue_manual(
      pwc_mh1, hide.ns = FALSE, tip.length = 0, 
      step.increase = 0.1, step.group.by = "variables",
      color = "variables"
   ) +
   labs(
      subtitle = test.label, title = "Difference in Treatment vs Control in MH Group",
      caption = get_pwc_label(pwc_mh1, type = "expression")
   ) + theme_pubr()


# Subset the data for GP group
gp1_data <- manova_1 %>% filter(group == "GP")
#gp1_data$mean_combined <- rowMeans(gp1_data[, 1:24])

# Perform pairwise t-test for MH group
pwc_gp1 <- gp1_data %>%
   gather(key = "variables", value = "value", q8, q13, q19, q22) %>%
   group_by(variables) %>%
   pairwise_t_test(value ~ treatment, pool.sd = FALSE, var.equal = FALSE)
print(pwc_gp1,n=24)

pwc_gp1 <- pwc_gp1 %>% add_xy_position(x = "treatment")
test.label <- create_test_label(
   description = "MANOVA", statistic.text = quote(italic("F")),
   statistic = 3.31, p = "<0.0001", parameter = "24,151",
   type = "expression", detailed = TRUE
)
ggboxplot(
   gp1_data, x = "treatment", y = c("q8","q13","q19","q22"), 
   merge = TRUE, palette = "aaas"
) + 
   stat_pvalue_manual(
      pwc_gp1, hide.ns = FALSE, tip.length = 0, 
      step.increase = 0.1, step.group.by = "variables",
      color = "variables"
   ) +
   labs(
      subtitle = test.label, title = "Difference in Treatment vs Control in GP Group",
      caption = get_pwc_label(pwc_gp1, type = "expression")
   ) + theme_pubr()

#===================MANOVA 2; ROBUSTNESS CHECK OF THE RESULT=============#
manova_2 <- df %>% select(c(positive,negative,neutral,treatment,group))
colnames(manova_2)

# Descriptive statistics
#treatment
treat_sum2 <- manova_2 %>% 
   group_by(treatment)%>%
   get_summary_stats(positive,negative,neutral, type="mean_sd")
group_sum2 <- manova_2 %>% 
   group_by(group)%>%
   get_summary_stats(positive,negative,neutral, type="mean_sd")
write.csv(treat_sum2, "summary_trt2.csv")
write.csv(group_sum2, "summary_group2.csv")

#visualization
ggboxplot(
   manova_2, x = "treatment", y = c("positive", "negative","neutral"), 
   merge = TRUE, palette = "aaas"
) + theme_bw()

ggboxplot(
   manova_2, x = "group", y = c("positive", "negative","neutral"), 
   merge = TRUE, palette = "aaas"
) + theme_bw()


#assumption checks
#adequate sample size, each group should > number of dep. vars (24)
manova_2 %>% group_by(treatment) %>% summarise(N=n()) #84, 94
manova_2 %>% group_by(group) %>% summarise(N=n()) #gp 90, mh 88

#independent of obs....Yes,satisfied

# Calculate Mahalanobis distances for each observation in the data
md_distances <- manova_2 %>%
   mutate(mahalanobis_distance = mahalanobis(
      select(., -treatment, -group),
      colMeans(select(., -treatment, -group)),
      cov(select(., -treatment, -group))
   ),
   row_number = row_number())

# Define the threshold for identifying outliers
p <- ncol(select(md_distances, -treatment, -group, -mahalanobis_distance, -row_number))
threshold <- qchisq(0.99, df = p)

# Create the plot
ggplot(md_distances, aes(x = row_number, y = mahalanobis_distance)) +
   geom_point() +
   geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
   labs(title = "Mahalanobis Distances for Multivariate Outlier Detection",
        x = "Observation",
        y = "Mahalanobis Distance") +
   theme_minimal() +
   annotate("text", x = max(md_distances$row_number), y = threshold, 
            label = paste("Threshold =", round(threshold, 2)), 
            hjust = 1, vjust = -1, color = "red")

#univariate assumption, QQ plots
ggqqplot(manova_2, "positive",facet.by = "treatment",ylab="Positive Study Factor", ggtheme = theme_bw())
ggqqplot(manova_2, "negative",facet.by = "treatment",ylab="Negative Study Factor", ggtheme = theme_bw())
ggqqplot(manova_2, "neutral",facet.by = "treatment",ylab="Neutral Study Factor", ggtheme = theme_bw())

#linearity assumption
library(tidyr)
# Assuming 'manova_2' is your dataframe with the relevant columns
# Reshape the data for ggplot2
plot_data <- manova_2 %>%
   select(positive, negative, neutral) %>%
   pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Function to create pairwise scatterplots with ggplot2
create_pairwise_plot <- function(data, var1, var2) {
   ggplot(data, aes_string(x = var1, y = var2)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      labs(title = paste("Scatterplot of", var1, "vs", var2)) +
      theme_minimal()
}

# Create pairwise scatterplots
plot_positive_negative <- create_pairwise_plot(manova_2, "positive", "negative")
plot_positive_neutral <- create_pairwise_plot(manova_2, "positive", "neutral")
plot_negative_neutral <- create_pairwise_plot(manova_2, "negative", "neutral")

# Combine the plots into one grid
library(gridExtra)
grid.arrange(plot_positive_negative, plot_positive_neutral, plot_negative_neutral, nrow = 1)

#multivariate normality
manova_2 %>% select(-c(treatment,group)) %>% mshapiro_test() #assumption violated

#multicollinearity, no existence
manova_2 %>% cor_mat(positive,negative,neutral) #no existence of multicollinearity

#homgeneity of covariance ... not violated, p > .05
box_m(manova_2[, c("positive","negative","neutral")], manova_2$group)

#homogeneity of variance, not violated,  p > .05 for each items
manova_2 %>% 
   gather(key = "variable", value = "value", positive,negative,neutral) %>%
   group_by(variable) %>% levene_test(value ~ treatment)

#========================computation==================================#
# Define the MANOVA model with the dependent variables (p, neu, and ne)
manova_model2 <- manova(cbind(positive,negative,neutral) ~ treatment * group, 
                       data = manova_2)

# Summarize the MANOVA model
summary(manova_model2, test = "Wilks")

# Calculate effect sizes (partial eta squared) and confidence intervals
effect_sizes <- effectsize::effectsize(manova_model2, type = "eta", ci = 0.95)

# Print effect sizes and confidence intervals
print(effect_sizes)

#only treatment is significant: compute t-test and visualize result

# Visualization: box plots with p-values
# Subset the data for MH group
mh_data <- manova_2 %>% filter(group == "MH")

# Perform pairwise t-test for MH group
pwc_mh <- mh_data %>%
   gather(key = "variables", value = "value", positive, negative, neutral) %>%
   group_by(variables) %>%
   pairwise_t_test(value ~ treatment, pool.sd = FALSE, var.equal = FALSE, conf.level=0.95) %>%
   adjust_pvalue(method = "bonferroni") %>%
   add_significance()
pwc_mh

# Calculate effect sizes for each pairwise comparison
effect_sizes_mh <- mh_data %>%
   gather(key = "variables", value = "value", positive, negative, neutral) %>%
   group_by(variables) %>%
   reframe(effect_size = cohens_d(value ~ treatment, data = .))

pwc_mh <- pwc_mh %>% add_xy_position(x = "treatment")
test.label <- create_test_label(
   description = "MANOVA", statistic.text = quote(italic("F")),
   statistic = 4.24, p="0.0064", parameter = "3,172",
   type = "expression", detailed = TRUE
)
ggboxplot(
   mh_data, x = "treatment", y = c("positive", "negative","neutral"), 
   merge = TRUE, palette = "aaas"
) + 
   stat_pvalue_manual(
      pwc_mh, hide.ns = FALSE, tip.length = 0, 
      step.increase = 0.1, step.group.by = "variables",
      color = "variables"
   ) +
   labs(
      subtitle = test.label, title = "Difference in Treatment vs Control in MH Group",
      caption = get_pwc_label(pwc_mh, type = "expression")
   ) + theme_pubr()


# Subset the data for GP group
gp_data <- manova_2 %>% filter(group == "GP")

# Perform pairwise t-test for MH group
pwc_gp <- gp_data %>%
   gather(key = "variables", value = "value", positive, negative, neutral) %>%
   group_by(variables) %>%
   pairwise_t_test(value ~ treatment, pool.sd = FALSE, var.equal = FALSE) %>%
   adjust_pvalue(method = "bonferroni") %>%
   add_significance()
pwc_gp

effect_sizes_gp <- gp_data %>%
   gather(key = "variables", value = "value", positive, negative, neutral) %>%
   group_by(variables) %>%
   reframe(effect_size = cohens_d(value ~ treatment, data = .))
print(effect_sizes_gp)


pwc_gp <- pwc_gp %>% add_xy_position(x = "treatment")
test.label <- create_test_label(
   description = "MANOVA", statistic.text = quote(italic("F")),
   statistic = 4.24, p="0.0064", parameter = "3,172",
   type = "expression", detailed = TRUE
)

ggboxplot(
   gp_data, x = "treatment", y = c("positive", "negative","neutral"), 
   merge = TRUE, palette = "aaas"
) + 
   stat_pvalue_manual(
      pwc_gp, hide.ns = FALSE, tip.length = 0, 
      step.increase = 0.1, step.group.by = "variables",
      color = "variables"
   ) +
   labs(
      subtitle = test.label, title = "Difference in Treatment vs Control in GP Group",
      caption = get_pwc_label(pwc_gp, type = "expression")
   ) + theme_pubr()

pwc_gp



#===================Frequenc Analysis 3=============#
freq_anal<- df %>% select(c(single_factor, treatment, group))
colnames(freq_anal)
table(freq_anal$treatment)
table(freq_anal$group)
table(freq_anal$single_factor)

# Subset the data for MH group
mh3_data <- freq_anal %>% filter(group == "MH")

# Create contingency table
contingency_table_mh <- table(mh3_data$treatment, mh3_data$single_factor)

# Perform chi-square test
chi_square_mh <- chisq.test(contingency_table_mh)

# Extract the p-value and chi-square statistic
p_value <- chi_square_mh$p.value
chi_square_stat <- chi_square_mh$statistic

# Create a data frame for counts and percentages
counts1 <- as.data.frame(contingency_table_mh)
counts1$percentage <- round(counts1$Freq / sum(counts1$Freq) * 100, 1)
counts1$label1 <- paste0(counts1$Freq, " (", counts1$percentage, "%)")
counts1 <- counts1 %>%
   rename(treatment = Var1, single_factor = Var2)


# Print results
print(chi_square_mh)

# Plot results
ggplot(mh3_data, aes(x = treatment, fill = single_factor)) +
   geom_bar(position = position_dodge(), stat = "count") +
   labs(title = "Association between Treatment and Single Factor in MH Group",
        y = "Count", x = "Treatment") +
   theme_pubr() +
   geom_text(data = counts1, aes(x = treatment, y = Freq + 1, label = label1, group = single_factor), 
             position = position_dodge(width = 0.9), vjust = 0) +
   annotate("text", x = 1.5, y = max(table(mh3_data$treatment, mh3_data$single_factor)) + 1, 
            label = paste("Chi-square =", round(chi_square_stat, 2),";","p-value =", round(p_value, 3)),
            size = 4, vjust=3)

# Subset the data for GP group
gp3_data <- freq_anal %>% filter(group == "GP")

# Create contingency table
contingency_table_gp <- table(gp3_data$treatment, gp3_data$single_factor)

# Perform chi-square test
chi_square_gp <- chisq.test(contingency_table_gp)
chi_square_stat <- chi_square_gp$statistic
p_value <- chi_square_gp$p.value

# Create a data frame for counts and percentages
counts <- as.data.frame(contingency_table_gp)
counts$percentage <- round(counts$Freq / sum(counts$Freq) * 100, 1)
counts$label <- paste0(counts$Freq, " (", counts$percentage, "%)")
counts <- counts %>%
   rename(treatment = Var1, single_factor = Var2)

# Plot results with counts and percentages
ggplot(gp3_data, aes(x = treatment, fill = single_factor)) +
   geom_bar(position = position_dodge(), stat = "count") +
   labs(title = "Association between Treatment and Single Factor in GP Group",
        y = "Count", x = "Treatment") +
   theme_pubr() +
   geom_text(data = counts, aes(x = treatment, y = Freq + 1, label = label, group = single_factor), 
             position = position_dodge(width = 0.9), vjust = 0) +
   annotate("text", x = 1.5, y = max(counts$Freq) + 5, 
            label = paste("Chi-square =", round(chi_square_stat, 2), ";", "p-value =", round(p_value, 3)),
            size = 4, vjust = 3)


# Subset the data for treatment group
treatment_data <- freq_anal %>% filter(treatment == "Treatment")

# Create contingency table
contingency_table_treatment <- table(treatment_data$group, treatment_data$single_factor)

# Perform chi-square test
chi_square_treatment <- chisq.test(contingency_table_treatment)

# Extract the p-value and chi-square statistic
p_value <- chi_square_treatment$p.value
chi_square_stat <- chi_square_treatment$statistic

# Create a data frame for counts and percentages
counts <- as.data.frame(contingency_table_treatment)
counts$percentage <- round(counts$Freq / sum(counts$Freq) * 100, 1)
counts$label <- paste0(counts$Freq, " (", counts$percentage, "%)")
counts <- counts %>%
   rename(group = Var1, single_factor = Var2)

# Print results
print(chi_square_treatment)

# Plot results with counts and percentages
ggplot(treatment_data, aes(x = group, fill = single_factor)) +
   geom_bar(position = position_dodge(), stat = "count") +
   labs(title = "Association between Group and Single Factor in Treatment Group",
        y = "Count", x = "Group") +
   theme_pubr() +
   geom_text(data = counts, aes(x = group, y = Freq + 1, label = label, group = single_factor), 
             position = position_dodge(width = 0.9), vjust = 0) +
   annotate("text", x = 1.5, y = max(counts$Freq) + 5, 
            label = paste("Chi-square =", round(chi_square_stat, 2), ";", "p-value =", round(p_value, 3)),
            size = 4, vjust = 1)

