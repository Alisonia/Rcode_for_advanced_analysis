#META- ANALYSIS

#loading the neccesary library
library(readxl) #for reading excel file.
library(tidyverse)

#libraries for meta-analysis
library(meta)
library(metafor)
library(devtools)
#install_github("cjvanlissa/metaforest")

#loading the data
df_pes<-read_excel("paper extract on Pesticide.xlsx",sheet = 1)
head(df_pes)
str(df_pes)

#calculating the effect size using escalc from metafor package
df_pes_smd <- escalc(measure = "SMD", 
                     n1i = n.c, n2i = n.t,
                     m1i = m.c, m2i = m.t,
                     sd1i = s.c, sd2i = s.t,
                     data = df_pes)

#show new data with effect size and its variance.
head(df_pes_smd)

#conducting the random effect size, g, using Hedge's method
g_re = rma(yi=df_pes_smd$yi,vi=df_pes_smd$vi, method = "HE")
g_re

#producing the forest plot
forest(g_re, slab = df_pes_smd$Studies, header = TRUE)

funnel(g_re, xlab = "Standardized Mean Difference")
title("Funnel Plot of Standardized Error by Std. Difference in Means")
funnel(g_re, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)

############
new_g_re <- metagen(TE=yi,
                    seTE=vi,
                    studlab=Studies,
                    data = df_pes_smd,
                    sm="SMD",
                    #method.smd="Hedges",
                    fixed=FALSE,
                    random=TRUE,
                    method.tau = "HE",title = "Effect of Pesticides Use on Biodiversity")
summary(new_g_re)   

new_g_re$TE.random
new_g_re$TE.fixed

#plot the forest
png(file = "forestplot2.png", width = 2800, height = 3000, res = 300)
forest(new_g_re, random = TRUE, xlim = c(-2,2),
            #sortvar = TE,
            prediction = TRUE,
            print.tau2 = TRUE,
            leftlabs = c("Author", "g", "SE"))
title("Effect of Pesticides Use on Biodiversity")
dev.off()

#
png(file = "forestJAMA.png", width = 2800, height = 3000, res = 300)
forest(new_g_re, layout = "JAMA",
       leftlabs = c("Author", "g", "SE"))
dev.off()

#NEW FUNNEL PLOT
funnel(new_g_re)

