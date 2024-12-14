#load the data 
library(readxl)
library(skimr)
library(stargazer)
library(sjPlot)

car_df <- read_excel("cartype.xls", sheet = 1)
skim(car_df)
head(car_df)

duplicate_rows<-duplicated(car_df)
print(car_df[duplicate_rows, ])

uniq_car_df <- unique(car_df)
skim(uniq_car_df)

library(dplyr)

uniq_car_df <- uniq_car_df %>% select(c(`Carline Class Desc`,`Combined FE`,`Combined CO2`,`Engine Displacement`,`# Cylinders`))
head(uniq_car_df)
uniq_car_df <- uniq_car_df %>% rename(cartype=`Carline Class Desc`,
                                      fuel_economy=`Combined FE`,
                                      co2_emission=`Combined CO2`,
                                      engine_displacement=`Engine Displacement`,
                                      no_of_cylinder=`# Cylinders`)
library(ggplot2)
library(cowplot)
library(gridExtra)

theme_set(theme_gray())

fuel_box <- ggplot(uniq_car_df, aes(y=fuel_economy))+
   geom_boxplot() + labs(title = "Combined Fuel Economy") #+ theme_bw()
   
co2_box <- ggplot(uniq_car_df,aes(y=co2_emission))+
   geom_boxplot() + labs(title="Combined CO2 emission")

engine_box <- ggplot(uniq_car_df,aes(y=engine_displacement))+
   geom_boxplot() + labs(title="Engine Displacement")

cyl_box <- ggplot(uniq_car_df,aes(y=no_of_cylinder))+
   geom_boxplot() + labs(title="Number of Cylinders")


cowplot::plot_grid(fuel_box,co2_box,engine_box,cyl_box, labels = "AUTO")

unique(uniq_car_df$cartype)

#new data with grouping
new_car_df <- uniq_car_df


grouping <- list(
   "Small Cars" = c("Two Seaters", "Minicompact Cars", "Subcompact Cars"),
   "Compact and Midsize Cars" = c("Compact Cars", "Midsize Cars"),
   "Large Cars" = c("Large Cars"),
   "Vans and SUVs" = c("Small Station Wagons", "Vans, Cargo Types", "Vans, Passenger Type",
                       "Special Purpose Vehicle, minivan 2WD", "Small SUV 2WD", "Small SUV 4WD"),
   "Pick-up Trucks" = c("Small Pick-up Trucks 2WD", "Small Pick-up Trucks 4WD",
                        "Standard Pick-up Trucks 4WD", "Special Purpose Vehicle 2WD",
                        "Standard SUV 2WD", "Standard SUV 4WD")
   )

new_car_df <- new_car_df %>% 
   mutate(car_category = case_when(
      cartype %in% grouping[["Small Cars"]] ~ "Small Cars",
      cartype %in% grouping[["Compact and Midsize Cars"]] ~ "Compact and Midsize Cars",
      cartype %in% grouping[["Large Cars"]] ~ "Large Cars",
      cartype %in% grouping[["Vans and SUVs"]] ~ "Vans and SUVs",
      cartype %in% grouping[["Pick-up Trucks"]] ~ "Pick-up Trucks",
      TRUE ~ "Other"
   ))

new_car_df$car_category=as.factor(new_car_df$car_category)

skim(new_car_df)

#visualization
ggplot(new_car_df, aes(car_category, fill= car_category))+
   geom_bar() + 
   labs(title = "Carline Description Type",fill= "Occupation")  + 
   theme_bw() + theme(legend.position = "none") + coord_flip()


ggplot(new_car_df, aes(x=fuel_economy, y=co2_emission)) +
   geom_point(position = position_jitter(width = 0.2, height = 0.2)) +
   #geom_text(vjust = -0.5, hjust = 0.5, size = 2) +
   labs(title = "Scatter Plot: Fuel Economy vs. CO2 Emission",
        x = "Fuel Economy",
        y = "CO2 Emission")


par(mfrow=c(1,1))
cor_car_df <- new_car_df %>% select(-c(cartype,car_category))
cor_car<-cor(cor_car_df)
corrplot(cor_car, type='lower',order='hclust',tl.col='black',tl.srt=45,method = 'number')
tab_corr(cor_car_df,triangle = "lower",p.numeric = T,show.p = T,title = "Correlation matrix",
         file = "correlation.htm") #using sjPlot package

#ANOVA analysis
library(ggplot2)
library(tidyverse)
library(psych)
library(caret)
library(ggpubr)
library(rstatix)
library(car)

bxp <- ggboxplot(new_car_df, x = "car_category", y = "fuel_economy") + theme_bw()
bxp

#check extreme outliers
new_car_df %>%
   group_by(car_category) %>%
   shapiro_test(fuel_economy)

#hypothesis 1

res.aov <- anova_test(data = new_car_df, dv = fuel_economy,  between = car_category)
get_anova_table(res.aov)

pwc <- new_car_df %>%
   pairwise_t_test(
      fuel_economy ~ car_category, paired = FALSE,
      p.adjust.method = "bonferroni"
   )
pwc

pwc <- pwc %>% add_xy_position(x = "car_category")
bxp + 
   stat_pvalue_manual(pwc) +
   labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
   )


#hypohesis 2
bxp2 <- ggboxplot(new_car_df, x = "car_category", y = "co2_emission") + theme_bw()
bxp2

res.aov2 <- anova_test(data = new_car_df, dv = co2_emission,  between = car_category)
get_anova_table(res.aov2)

pwc2 <- new_car_df %>%
   pairwise_t_test(
      co2_emission ~ car_category, paired = FALSE,
      p.adjust.method = "bonferroni"
   )
pwc2

pwc2 <- pwc2 %>% add_xy_position(x = "car_category")
bxp + 
   stat_pvalue_manual(pwc2) +
   labs(
      subtitle = get_test_label(res.aov2, detailed = TRUE),
      caption = get_pwc_label(pwc2)
   )
