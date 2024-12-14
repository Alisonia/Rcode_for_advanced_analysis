#loading library
library(ggplot2)
library(readxl)
library(dplyr)   
library(skimr)
library(broom)
library(stargazer)
library(haven)
library(broom)
library(caret)
library(rpart)
library(rattle)
library(car)
library(glmnet)
library(margins)
library(psych)
library(randomForest)
library(stats)
library(sjPlot)
library(DescTools)
library(interactions)


### loading the data

labour_data <- read.csv("EWCS_2015.csv")
head(labour_data)
View(labour_data)

#skim(labour_data)
str(labour_data)

########################## Data Restriction ############################################

### variables selection 

my_data <- labour_data %>% select(Q2a, Q2b, Q7, Q14, Q29a,
                                  Q29b, Q29c, Q29d, Q29e, Q29f, Q29g, Q29h, Q29i, Q30a, Q30b, Q30c, Q30e, Q30g,
                                  Q30h, Q33, Q40, Q44, Q49a, Q49b, Q53c, Q54a, Q54b, Q54c, Q61b, Q61g,
                                  Q61h, Q61i, Q61l, Q63a, Q63b, Q63c, Q63d, Q63e, Q63f, Q64, Q65a, Q65b,
                                  Q71b, Q71c, Q72a, Q72b, Q72c, Q72d, Q72e, Q72f, Q72g, Q75, Q87a, Q87b,
                                  Q87c, Q87d, Q87e, Q88, Q89b,Q89e, Q89h, Q93,ISCED, isco_08_1)
                                  
#skim(my_data)
#summary(my_data)

#filter only the employees respondents and whose age is between 15 and 65years
my_data <- my_data %>% filter(Q7==1, Q2b >14, Q2b <66)
nrow(my_data)

#remove all missing values
my_data <- my_data[complete.cases(my_data),]
write.csv(my_data, "cleaned_ewcs2015.csv")

#removing all DK response and/or refusal in all variables of interest. I used filtering option
my_data <- my_data %>% filter(Q2a != 9,
                              Q14 != 8, Q14 != 9,
                              Q29a != 8, Q29a != 9,
                              Q29b != 8, Q29b != 9,
                              Q29c != 8, Q29c != 9,
                              Q29d != 8, Q29d != 9,
                              Q29e != 8, Q29e != 9,
                              Q29f != 8, Q29f != 9,
                              Q29g != 8, Q29g != 9,
                              Q29h != 8, Q29h != 9,
                              Q29i != 8, Q29i != 9,
                              Q30a != 8, Q30a != 9,
                              Q30b != 8, Q30b != 9,
                              Q30c != 8, Q30c != 9,
                              Q30e != 8, Q30e != 9,
                              Q30g != 8, Q30g != 9,
                              Q30h != 8, Q30h != 9,
                              Q33 != 8,  Q33 != 9,
                              Q40 != 8,  Q40 != 9,
                              Q44 != 8,  Q44 != 9,
                              Q49a != 7, Q49a!= 8, Q49a!= 9,
                              Q49b != 7, Q49b!= 8, Q49b!= 9,
                              Q53c != 8, Q53c != 9,
                              Q54a != 8, Q54a != 9,
                              Q54b != 8, Q54b != 9,
                              Q54c != 8, Q54c != 9,
                              Q61b != 7, Q61b!= 8, Q61b!= 9,
                              Q61g != 7, Q61g!= 8, Q61g!= 9,
                              Q61h != 7, Q61h!= 8, Q61h!= 9,
                              Q61i != 7, Q61i!= 8, Q61i!= 9,
                              Q61l != 7, Q61l!= 8, Q61l!= 9,
                              Q63a != 7, Q63a!= 8, Q63a!= 9,
                              Q63b != 7, Q63b!= 8, Q63b!= 9,
                              Q63c != 7, Q63c!= 8, Q63c!= 9,
                              Q63d != 7, Q63d!= 8, Q63d!= 9,
                              Q63e != 7, Q63e!= 8, Q63e!= 9,
                              Q63f != 7, Q63f!= 8, Q63f!= 9,
                              Q64 != 8, Q64 != 9,
                              Q65a != 8, Q65a != 9,
                              Q65b != 8, Q65b != 9,
                              Q71b != 8, Q71b != 9,
                              Q71c != 8, Q71c != 9,
                              Q72a != 8, Q72a != 9,
                              Q72b != 8, Q72b != 9,
                              Q72c != 8, Q72c != 9,
                              Q72d != 8, Q72d != 9,
                              Q72e != 8, Q72e != 9,
                              Q72f != 8, Q72f != 9,
                              Q72g != 8, Q72g != 9,
                              Q75 != 8, Q75 != 9,
                              Q87a != 8, Q87a != 9,
                              Q87b != 8, Q87b != 9,
                              Q87c != 8, Q87c != 9,
                              Q87d != 8, Q87d != 9,
                              Q87e != 8, Q87e != 9,
                              Q88 != 8, Q88 != 9,
                              Q89b != 7, Q89b != 8, Q89b != 9,
                              Q89e != 7, Q89e != 8, Q89e != 9,
                              Q89h != 7, Q89h != 8, Q89h != 9,
                              Q93 != 8, Q93 != 9,
                              ISCED != 88, ISCED != 99,
                              isco_08_1!=88, isco_08_1!=99)

#recoding variables
my_data <- my_data %>% mutate(job_retention=ifelse(Q93==2,0,1)) %>% mutate(job_retention_q=ifelse(Q93==2,"No","Yes")) %>% 
   mutate(job_satisfaction=ifelse(Q88>=3,0,1)) %>% mutate(Manager_support = ifelse(Q61b>3,0,1)) %>%
   mutate(health_safety = ifelse(Q33>2,0,1)) %>% mutate(training_development = ifelse(Q65a==1,1,0)) %>%
   mutate(working_condition = rowMeans(my_data[, c("Q49a","Q49b")])) %>% 
   mutate(working_condition=ifelse(working_condition<=2,1,0)) %>% #1=too tight, #otherwise, 0
   mutate(job_autonomy=rowMeans(my_data[,c("Q54a","Q54b", "Q54c", "Q61i")])) %>%
   mutate(job_autonomy=ifelse(job_autonomy<=1.5,1,0)) %>% #0 less autonomy, more autonomy
   mutate(immediate_boss_attitude=rowMeans(my_data[,c("Q63a","Q63b","Q63c","Q63d","Q63e","Q63f")])) %>%
   mutate(immediate_boss_attitude=ifelse(immediate_boss_attitude<=2,1,0)) %>%
   mutate(perceived_employability=ifelse(Q89h<=2,1,0)) %>%
   mutate(job_motivation=ifelse(Q89e<=2,1,0)) %>%
   mutate(job_prospect=ifelse(Q89b<=2,1,0)) %>%
   mutate(fair_treatment=ifelse(Q61l<=2,1,0)) %>%
   mutate(health_status=ifelse(Q75<=2,1,0)) %>%
   mutate(air_opinion=ifelse(Q71c==1,1,0)) %>%
   mutate(gender=ifelse(Q2a==1,0,1))%>%
   mutate(gender_q=ifelse(Q2a==1,"Male","Female")) %>%
   mutate(physical_risk=rowMeans(my_data[,c("Q29a", "Q29b", "Q29c", "Q29d","Q29e","Q29f","Q29g","Q29i")])) %>%
   mutate(physical_risk=ifelse(physical_risk<=3,1,0)) %>%
   mutate(mental_risk=ifelse(Q30h<=2,1,0)) %>%
   mutate(emotional_wellbeing=rowMeans(my_data[,c("Q87a","Q87b","Q87c","Q87d","Q87e")])) %>%
   mutate(emotional_wellbeing=ifelse(emotional_wellbeing<=2,1,0)) %>%
   mutate(discrimination=rowMeans(my_data[,c("Q72a","Q72b","Q72c","Q72d","Q72e","Q72f","Q72g")])) %>%
   mutate(discrimination=ifelse(discrimination<2,1,0)) %>%
   mutate(safety_committee=ifelse(Q71b==1,1,0))%>%
   mutate(worklife_balance=ifelse(Q44 <= 2,1,0))

#=============== Cronbach Alpha for Composite variable================#
library(psych)  #for getting alpha for items

#alpha(my_data[,c("Q65a","Q65b","Q64")]) #training development, a = 0.2 too low. Only single variable, Q65a, selected

alpha(my_data[, c("Q49a","Q49b")])  #0.61 good for working condition. Q40 dropped (scale response different). Now alpha =0.79

alpha(my_data[,c("Q54a","Q54b", "Q54c", "Q61i")]) #0.62 for job autonomy

alpha(my_data[,c("Q63a","Q63b","Q63c","Q63d","Q63e","Q63f")])  #0.90 very good for immediate boss attitude.

alpha(my_data[,c("Q29a", "Q29b", "Q29c", "Q29d","Q29e","Q29f",
                 "Q29g","Q29i")]) #0.83, physcial risk. <=3 coded as more exposed, 0 less exposed

alpha(my_data[,c("Q87a","Q87b","Q87c","Q87d","Q87e")]) #0.88, mental wellbeing <=3

alpha(my_data[, c("Q72a","Q72b","Q72c","Q72d","Q72e","Q72f","Q72g")]) #0.68 for discrimination
   
#Joining and cleaning data for exploration purpose

my_data <- my_data %>% mutate(occupation=ifelse(isco_08_1 == 1, "Managers",
                                                ifelse(isco_08_1 == 2, "Professionals",
                                                ifelse(isco_08_1 == 3, "Technicians",
                                                ifelse(isco_08_1 == 4, "Clerical Workers",
                                                ifelse(isco_08_1 == 5, "Services Workers",
                                                ifelse(isco_08_1 == 6, "Skilled Agricultural",
                                                ifelse(isco_08_1 == 7, "Craft Workers",
                                                ifelse(isco_08_1 == 8, "Plant Machine Operators", 
                                                ifelse(isco_08_1 == 9, "Elementary Occupations",
                                                ifelse(isco_08_1 == 0, "Armed Forces",0)))))))))))

my_data<-my_data %>% mutate(sector=ifelse(Q14==1,"Private sector",
                                          ifelse(Q14==2,"Pulbic sector",
                                          ifelse(Q14==3,"Company",
                                          ifelse(Q14==4,"NG0","Others")))))
my_data<-my_data %>% mutate(education=ifelse(ISCED==1,"Early childhood",
                                             ifelse(ISCED==2,"Primary",
                                             ifelse(ISCED==3,"Lower secondary",
                                             ifelse(ISCED==4,"Upper secondary",
                                             ifelse(ISCED==5,"Post-secondary non-tertiary",
                                             ifelse(ISCED==6, "Short-cycle tertiary",
                                             ifelse(ISCED==7,"Bachelor or equivalent",
                                             ifelse(ISCED==8,"Master or equivalent","Doctorate or equivalent")))))))))

my_data <- my_data %>% rename(Age=Q2b)

head(my_data)

#=================Selecting data for model========================================
retention_df <- my_data %>% select(job_retention,job_retention_q,gender,gender_q, Age,education,sector,occupation,job_satisfaction,Manager_support,
                                   health_safety,training_development,working_condition,job_autonomy,immediate_boss_attitude,
                                   perceived_employability,job_motivation,job_prospect,fair_treatment,health_status,air_opinion,
                                   physical_risk,mental_risk,emotional_wellbeing,discrimination,safety_committee,worklife_balance)
write.csv(retention_df, "retention data.csv")


#====================Exploration or Visualization============================================
library(ggplot2)
library(ggpubr)

#for labeling in ggplot, geom_text
count <- geom_text(stat='count', aes(label=after_stat(count)), 
                   vjust=1.2)
perc <- geom_text(stat='count', aes(label=after_stat(round(count/sum(count)*100,1))),
                  vjust=2.6,color='white')

#Job retention
job_retention <- ggplot(retention_df, aes(job_retention_q, fill= job_retention_q))+
   geom_bar() + 
   labs(title = "Job Retention by Employees", fill= "Job Retention")  + 
   theme_bw()+
   count + perc+
   theme(legend.position = "top")
   #facet_wrap(.~gender_q)
ggsave(filename = "job_re_img/job_retention.png", plot = job_retention, device = "png", 
       width = 5, height = 5)

#gender
 gender <- ggplot(retention_df, aes(gender_q, fill= gender_q))+
   geom_bar() + 
   count + perc+
   labs(title = "Composition of Gender", fill= "Gender")  + 
   theme_bw() +
   theme(legend.position = "top")
#facet_wrap(.~gender_q)
ggsave(filename = "job_re_img/gender.png", plot =gender, device = "png", 
       width = 5, height = 5)

#education
educ_status <- ggplot(retention_df, aes(education, fill= education))+
   geom_bar() + 
   labs(title = "Educational Status of the Employees",fill= "Education")  + 
   theme_bw() +coord_flip() + theme(legend.position = "none")
ggsave(filename = "job_re_img/educ_status.png", plot =educ_status, device = "png", 
       width = 6, height = 5)
#sector
sector <- ggplot(retention_df, aes(sector, fill= sector))+
   geom_bar() + 
   labs(title = "Sectors of Employees", fill= "Sector")  + 
   theme_bw() + theme(legend.position = "none") 
ggsave(filename = "job_re_img/sector.png", plot =sector, device = "png", 
       width = 6, height = 5)
#occupation
occupation <- ggplot(retention_df, aes(occupation, fill= occupation))+
   geom_bar() + 
   labs(title = "Occupation of Employees", fill= "Occupation")  + 
   theme_bw() + theme(legend.position = "none") + coord_flip()
ggsave(filename = "job_re_img/occupation.png", plot =occupation, device = "png", 
       width = 8, height = 6)


#===============Descriptive statistics==============
library(stargazer)

stargazer(retention_df, title= "Descriptive Statistics",
type = "html", out = "summarystatistics.htm", digits = 3)

#correlation
library(corrplot)
library(sjPlot)
install.packages("sjPlot")
library(margins)
install.packages("margins")

model_df <- retention_df %>% select(-c(job_retention_q,gender_q,education,sector,occupation))
cor_ret<-cor(model_df)
corrplot(cor_ret, type='lower',order='hclust',tl.col='black',tl.srt=45,method = 'shade')
tab_corr(model_df,triangle = "lower",p.numeric = T,show.p = T,title = "Correlation matrix",
         file = "correlation.htm") #using sjPlot package

#==========================logistic regression model===========================#

mod_ctrl <- glm(job_retention ~ Age + gender + perceived_employability + job_motivation + job_prospect + fair_treatment +
                        health_status+air_opinion + physical_risk + mental_risk + emotional_wellbeing + 
                        discrimination, data = model_df, family = binomial(link='logit'))
summary(mod_ctrl)

mod_predtor_ctrl <- glm(job_retention ~ Age + gender + perceived_employability + job_motivation + job_prospect + fair_treatment +
                           health_status+air_opinion + physical_risk + mental_risk + emotional_wellbeing +discrimination+ 
                           job_satisfaction+Manager_support+health_safety+training_development+working_condition+job_autonomy+
                           immediate_boss_attitude, data = model_df, family = binomial(link='logit'))
summary(mod_predtor_ctrl)
mod_pred_ame<-margins_summary(mod_predtor_ctrl)
write.csv(mod_pred_ame, "AME_summary.csv") 

                       
mod_job_sat_int1 <- glm(job_retention ~ Age + gender + perceived_employability + job_motivation + job_prospect + fair_treatment +
                           health_status+air_opinion + physical_risk + mental_risk + emotional_wellbeing +discrimination+ 
                           job_satisfaction*worklife_balance+Manager_support+health_safety+training_development+working_condition+job_autonomy+
                           immediate_boss_attitude, data = model_df, family = binomial(link='logit'))
summary(mod_job_sat_int1)

mod_job_sat_int2 <- glm(job_retention ~ Age + gender + perceived_employability + job_motivation + job_prospect + fair_treatment +
                           health_status+air_opinion + physical_risk + mental_risk + emotional_wellbeing +discrimination+ 
                           job_satisfaction*safety_committee+Manager_support+health_safety+training_development+working_condition+job_autonomy+
                           immediate_boss_attitude, data = model_df, family = binomial(link='logit'))

summary(mod_job_sat_int2)


mod_job_sat_intc <- glm(job_retention ~ Age + gender + perceived_employability + job_motivation + job_prospect + fair_treatment +
                           health_status+air_opinion + physical_risk + mental_risk + emotional_wellbeing +discrimination+ 
                           job_satisfaction*worklife_balance*safety_committee+Manager_support+health_safety+training_development+working_condition+job_autonomy+
                           immediate_boss_attitude, data = model_df, family = binomial(link='logit'))

summary(mod_job_sat_intc)

#====== Slope test==
# Simple slope test of statistically significant interactions
library(interactions)   
library(jtools)
library("sandwich")
install.packages("sandwich")
sim_slopes(mod_job_sat_int1, pred="job_satisfaction",modx="worklife_balance", johnson_neyman = F)
sim_slopes(mod_job_sat_int2, pred="job_satisfaction",modx="safety_committee", johnson_neyman = F)
sim_slopes(mod_job_sat_intc, pred="job_satisfaction",modx="worklife_balance", mod2="safety_committee",
           modx.values=1,mod2.values=1,johnson_neyman = F)
sim_slopes(mod_job_sat_intc, pred="job_satisfaction",modx="worklife_balance", mod2="safety_committee",
           modx.values=1,mod2.values=0,johnson_neyman = F)
sim_slopes(mod_job_sat_intc, pred="job_satisfaction",modx="worklife_balance", mod2="safety_committee",
           modx.values=0,mod2.values=1,johnson_neyman = F)
sim_slopes(mod_job_sat_intc, pred="job_satisfaction",modx="worklife_balance", mod2="safety_committee",
           modx.values=0,mod2.values=0,johnson_neyman = F)


intr1<-plot_model(mod_job_sat_int1, type = "pred", terms = c("job_satisfaction", "worklife_balance")) + 
   theme_bw()+
   labs( title = "Predicted Probability of Job Retention by Employees",
         x= "Job Satisfaction") + theme(axis.title.y = element_blank())
ggsave(filename = "job_re_img/interaction1.png", plot =intr1, device = "png", 
       width = 8, height = 5)

intr2<-plot_model(mod_job_sat_int2, type = "pred", terms = c("job_satisfaction", "safety_committee")) + 
   theme_bw()+
   labs( title = "Predicted Probability of Job Retention by Employees",
         x= "Job Satisfaction") + theme(axis.title.y = element_blank())
ggsave(filename = "job_re_img/interaction2.png", plot =intr2, device = "png", 
       width = 8, height = 5)

intrcombined<-plot_model(mod_job_sat_intc, type = "pred", terms = c("job_satisfaction", "safety_committee","worklife_balance")) + 
   theme_bw()+
   labs( title = "Predicted Probability of Job Retention by Employees",
         subtitle = "Threeway Interaction with Job Satisfacton",
         x= "Job Satisfaction") + theme(axis.title.y = element_blank())
ggsave(filename = "job_re_img/inter_combined.png", plot =intrcombined, device = "png", 
       width = 8, height = 5)

#=================== Pseudo R2  ==============================================================
PseudoR2(mod_ctrl,which =  "McFaddenAdj")

PseudoR2(mod_predtor_ctrl,which =  "McFaddenAdj")

PseudoR2(mod_job_sat_int1,which =  "McFaddenAdj")

PseudoR2(mod_job_sat_int2,which =  "McFaddenAdj")

PseudoR2(mod_job_sat_intc,which =  "McFaddenAdj")

#OUTPUT THE FILE
stargazer(mod_ctrl, mod_predtor_ctrl,mod_job_sat_int1, mod_job_sat_int2, mod_job_sat_intc,
          type = "html", out = "logit.moderation.htm",
          title= "Logistic Regression and Moderation Analysis", 
          notes.label= "Significance Levels" )

#==========================RANDOM FOREST CLASSIFIER========================#
library(caret)

rforest_df <- retention_df %>% select(-c(job_retention,gender_q,education,sector,occupation))
skim(rforest_df)
head(rforest_df)

myformula_rf <- formula(job_retention_q ~.)

rf.caret <- train(myformula_rf, 
                  data = rforest_df,
                  metric = "Accuracy",
                  method = "rf",
                  trControl = trainControl(method = "cv", number = 10, classProbs = TRUE))

rf.caret
pred_rf.caret <- predict(rf.caret, rforest_df)
table(pred_rf.caret, rforest_df$job_retention_q)
plot(varImp(rf.caret))

