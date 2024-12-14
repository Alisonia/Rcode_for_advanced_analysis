#IMPACT OF DIGITAL WORKPLACE ON EMPLOYEES STRESS LEVEL IN AN ORGANIZATION

#loading the neccesary package
library(skimr)
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(corrplot)
library(stargazer)
library(ggpubr)
library(rstatix)


#importing to read the SPSS data
library(haven)

#reading the data
df <- read_sav("dataset.sav")
dim(df)

#subseting the required variables
wrkplace_df <- df %>% select(c(Age,Gender,Income,Work_hrs,
                               DWO_IO_TOT,DWO_SFO_TOT,DWO_CO_TOT,DWOTOTMN, #DWSTOTRV, #wrkplace overload
                               DWANXTMN,DWFOMOTM,DWADDTMR,TMINDTOTRV,CSETOTMN,
                               BURNTOTRV,PSHTOTRV6,MTHTOTRV,HEALTHTOT))
dim(wrkplace_df)  #140  17

wrkplace_df <- wrkplace_df[complete.cases(wrkplace_df),]  #remove all missing values

dim(wrkplace_df) #140 17


#creating dummy variables for the demographics
wrkplace_df <- wrkplace_df %>% mutate(Gender_dummy=ifelse(Gender==1,1,0))  %>%  #1 Male: 0 Female
   mutate(Age_dummy=ifelse(Age<=3,0,1)) %>% #0 34 years and belows: 1 above 34 years
   mutate(Income_dummy=ifelse(Income<=3,0,1)) %>% #0: £30,000 and below; 1 otherwise
   mutate(Work_hrs_dummy=ifelse(Work_hrs<=2,0,1)) # 0: 24 hours and below; 1 otherwise
   
#labeling the categorical data
wrkplace_df <- wrkplace_df %>% mutate(Gender=ifelse(Gender==1, "Male",
                                                 ifelse(Gender==2, "Female",
                                                 ifelse(Gender==3, "Non-binary / third gender",
                                                        "Prefer not to say"))))

wrkplace_df <- wrkplace_df %>% mutate(Age=ifelse(Age==1,"Under 18",
                                                 ifelse(Age==2,"18 - 24",
                                                 ifelse(Age==3,"25 - 34",
                                                 ifelse(Age==4,"35 - 44",
                                                 ifelse(Age==5,"45 - 54",
                                                 ifelse(Age==6,"55 - 64",
                                                 ifelse(Age==7,"65 - 74",
                                                 ifelse(Age==8,"75 - 84","85 or older")))))))))

#renaming variables for easier access
wrkplace_df <- wrkplace_df %>%
   rename(overload_wrk=DWOTOTMN)%>%
   rename(anxiety_work=DWANXTMN)%>%rename(fear_missing_out=DWFOMOTM)%>%rename(addiction_wrk=DWADDTMR)%>%
   rename(trait_mindfulness=TMINDTOTRV)%>%rename(workplace_stress=BURNTOTRV)%>%rename(info_overload=DWO_IO_TOT)%>%
   rename(system_overload=DWO_SFO_TOT)%>%rename(comm_overload=DWO_CO_TOT) %>%
   rename(wellbeing=HEALTHTOT) %>% rename(digital_confidence=CSETOTMN) %>% rename(physical_health=PSHTOTRV6)%>%
   rename(mental_health=MTHTOTRV)
   
   
#save the data to csv file
write.csv(wrkplace_df,"digital_workplace.csv")

View(wrkplace_df)

#Demographic analysis
mycolors<- c("#ff4342", "#00454f")
mycolors2<- c( "#00364f", "#00364f", "#00364f", "#00364f", "#00364f", "#00364f", "#00364f",
               "#00364f", "#00364f", "#00364f")
count <- geom_text(stat='count', aes(label=after_stat(count)), 
                   vjust=1.2)
perc <- geom_text(stat='count', aes(label=after_stat(round(count/sum(count)*100,1))),
                  vjust=2.6,color='white')

#gender
gender <- ggplot(wrkplace_df, aes(Gender, fill= Gender))+
   geom_bar() + 
   count + perc+
   labs(title = "Distribution of Employees by Gender", fill= "Gender")  + 
   theme_get() 

ggsave(filename = "workplace_emp/gender.png", plot =gender, device = "png", 
       width = 5, height = 4)

#Age
age <- ggplot(wrkplace_df, aes(Age, fill= Age))+
   geom_bar() + 
   count + perc+
   labs(title = "Distribution of Employees by Age", fill= "Age")  + 
   theme_get()

ggsave(filename = "workplace_emp/age.png", plot =age, device = "png", 
       width = 8, height = 5)


#===========Digital workplace Stress by income and workhours============#

#workplace STRESS by income, And THEN workhours
bxp <- ggboxplot(wrkplace_df, x = "Work_hrs", y = "workplace_stress") + theme_get()
bxp

res.aov <- anova_test(data = wrkplace_df, dv = workplace_stress,  between = Work_hrs)
get_anova_table(res.aov)

pwc <- wrkplace_df %>%
   pairwise_t_test(
      workplace_stress ~ Work_hrs, paired = FALSE,
      p.adjust.method = "bonferroni"
   )
pwc

pwc <- pwc %>% add_xy_position(x = "Work_hrs")
bxp + 
   stat_pvalue_manual(pwc) +
   labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
   )

#===========Digital workplace overload by income and workhours============#

#workplace overload by income, And THEN workhours
bxp <- ggboxplot(wrkplace_df, x = "Income", y = "overload_wrk") + theme_get()
bxp

res.aov <- anova_test(data = wrkplace_df, dv = overload_wrk,  between = Income)
get_anova_table(res.aov)

pwc2 <- wrkplace_df %>%
   pairwise_t_test(
      overload_wrk ~ Income, paired = FALSE,
      p.adjust.method = "bonferroni"
   )
pwc2

pwc2 <- pwc2 %>% add_xy_position(x = "Income")
bxp + 
   stat_pvalue_manual(pwc2) +
   labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc2)
   )

#Descriptive Statistics

#selecting the variables for model and hypothesis building
model_df <- wrkplace_df %>% select(c(workplace_stress,info_overload,comm_overload,system_overload,
                                     anxiety_work,fear_missing_out,addiction_wrk,wellbeing,trait_mindfulness,
                                     digital_confidence,Gender_dummy, Age_dummy, Income_dummy, Work_hrs_dummy))

head(model_df)

stargazer(as.data.frame(model_df), title= "Descriptive Statistics",
          type = "html", out = "summary_workplace.htm", digits = 3)

#correlation plot
# Plot of workplace stress against info_overload
p1 <- ggplot(model_df, aes(x = info_overload, y = workplace_stress)) +
   geom_point(color = "blue") + 
   geom_smooth(method = "lm", se = FALSE, color="black")+
   labs(x = "Information Overload", y = "Workplace Stress") +
   ggtitle("Workplace Stress vs Information Overload")

# Plot of workplace stress against comm_overload
p2 <- ggplot(model_df, aes(x = comm_overload, y = workplace_stress)) +
   geom_point(color = "magenta") +
   geom_smooth(method = "lm", se = FALSE, color="black")+
   labs(x = "Communication Overload", y = "Workplace Stress") +
   ggtitle("Workplace Stress vs Communication Overload")

# Plot of workplace stress against system_overload
p3 <- ggplot(model_df, aes(x = system_overload, y = workplace_stress)) +
   geom_point(color = "red") +
   geom_smooth(method = "lm", se = FALSE, color="black")+
   labs(x = "System Overload", y = "Workplace Stress") +
   ggtitle("Workplace Stress vs System Overload")

# Arrange plots in a grid
library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1)



cor_job<-cor(model_df)
corrplot(cor_job, type='lower',order='hclust',tl.col='black',tl.srt=45, method = "color" )
tab_corr(model_df,triangle = "lower",p.numeric = T,show.p = T,
         title = "Correlation matrix of Variables", file = "job_corr.htm") #using sjPlot package


#==============running the regression model================#
model_ctrl <- lm(workplace_stress~fear_missing_out+addiction_wrk+anxiety_work+wellbeing 
                 +Gender_dummy+Age_dummy+Income_dummy+Work_hrs_dummy, data=model_df)
summary(model_ctrl)

model_1 <- lm(workplace_stress~info_overload+fear_missing_out+addiction_wrk+anxiety_work+wellbeing 
              +Gender_dummy+Age_dummy+Income_dummy+Work_hrs_dummy, data=model_df)
summary(model_1)

model_2 <- lm(workplace_stress~comm_overload+fear_missing_out+addiction_wrk+anxiety_work+wellbeing 
              +Gender_dummy+Age_dummy+Income_dummy+Work_hrs_dummy, data=model_df)
summary(model_2)

model_3 <- lm(workplace_stress~system_overload+fear_missing_out+addiction_wrk+anxiety_work+wellbeing 
              +Gender_dummy+Age_dummy+Income_dummy+Work_hrs_dummy, data=model_df)
summary(model_3)


#output the regression model
stargazer(model_ctrl,model_1,model_2,model_3, title="Estimation of Regression Result",
          align = TRUE,
          dep.var.labels = c("Workplace Stress"),
          omit.stat = c("LL","ser"),
          type = "html", out = "reg_output_job.htm")

#==============Stepwise backward Regression=================#
library(MASS)
step_df <- model_df %>% select(c('workplace_stress', 'info_overload','comm_overload','system_overload'))

full_model <- lm(workplace_stress~., data=step_df)
null_model <- lm(workplace_stress~1., data=step_df)

backward_stepwise<-stepAIC(full_model, direction = 'backward', 
                scope = list(upper=full_model, lower=null_model), trace=0)

summary(backward_stepwise)

stargazer(backward_stepwise, title="Estimation of Backward Regression Result",
          align = TRUE,
          omit.stat = c("LL","ser"),
          type = "html", out = "stepwise.htm")

#OR Decision tree regression 
library(dplyr)
tree_df <- model_df %>% select(c('workplace_stress', 'info_overload','comm_overload','system_overload'))
tree_df

library(rpart)
library(rpart.plot)

tree_model <- rpart(workplace_stress ~ ., data=tree_df, method="anova")
rpart.plot(tree_model)
printcp(tree_model)
plotcp(tree_model)

#prediction
p<-predict(tree_model, tree_df)
p

#mean absolute error
mean(abs(p-tree_df$workplace_stress)) #0.5844998

#root mean square error
sqrt(mean(tree_df$workplace_stress-p)^2) # 2.201318e-17

#r-square
(cor(tree_df$workplace_stress,p)^2) #0.3042679

var_importance <- tree_model$variable.importance
print(var_importance)

barplot(var_importance, main="Impact of Digital Workplace Overload on Workplace Stress",xlab="Variables",
     ylab="Importance", names.arg=names(var_importance), horiz=TRUE, col = "#00454f")

#======================Moderation regression===================#
moderator_1 <- lm(workplace_stress~info_overload*trait_mindfulness+fear_missing_out+addiction_wrk+anxiety_work+wellbeing 
                  +Gender_dummy+Age_dummy+Income_dummy+Work_hrs_dummy, data=model_df)
summary(moderator_1)

moderator_2 <- lm(workplace_stress~info_overload*digital_confidence+fear_missing_out+addiction_wrk+anxiety_work+wellbeing 
                  +Gender_dummy+Age_dummy+Income_dummy+Work_hrs_dummy, data=model_df)
summary(moderator_2)

stargazer(moderator_1,moderator_2, title="Estimation of Moderation Regression Result",
          align = TRUE,
          omit.stat = c("LL","ser"),
          type = "html", out = "moderator.htm")

library(interactions)
# Define the minimum and maximum values for moderator 1: Trait mindfulness
min_moderator <- round(min(model_df$trait_mindfulness),2)
mean_moderator <-round(mean(model_df$trait_mindfulness),2)
max_moderator <- round(max(model_df$trait_mindfulness),2)

# Plot the interaction using minimum and maximum moderator values
modplot_1<-interact_plot(moderator_1, pred = info_overload, modx = trait_mindfulness,
              legend.main = "Trait Mindfulness", y.label = "Workplace stress",
              x.label = "Information overload", interval = TRUE,
              modx.values = c(min_moderator, mean_moderator, max_moderator),
              modx.labels = c("min", "mean", "max"))

ggsave(filename = "modplot1.png", plot =modplot_1, device = "png", 
       width = 8, height = 6)

# Define the minimum and maximum values for moderator 2: digital confidence
min_moderator <- round(min(model_df$digital_confidence),2)
mean_moderator <-round(mean(model_df$digital_confidence),2)
max_moderator <- round(max(model_df$digital_confidence),2)

# Plot the interaction using minimum and maximum moderator values
modplot_2<-interact_plot(moderator_2, pred = info_overload, modx = digital_confidence,
                         legend.main = "Digital Confidence", y.label = "Workplace stress",
                         x.label = "Digital Confidence", interval = TRUE,
                         modx.values = c(min_moderator, mean_moderator, max_moderator),
                         modx.labels = c("min", "mean", "max"))

ggsave(filename = "modplot2.png", plot =modplot_2, device = "png", 
       width = 8, height = 5)