### Reading the data
## looked for general information, and if there is any issue that has to be fixed.
games <- read.csv("video_games_sales.csv", sep=";",dec = "," ,header = T,na.strings = c("","N/A") ,stringsAsFactors = T)
str(games)
head(games)
games$Name <- as.character(games$Name) games$Platform <- as.character(games$Platform) sum(is.na(games))
library(dplyr)
library(tidyverse)
games_new <- games %>% select(-X, -X.1) %>% na.omit()
sum(is.na(games_new)) dim(games_new) str(games_new)
for (i in 6:8){
  games_new[ ,i] <- gsub(",",".", games_new[,i])
}
games_new[,13] <- gsub(",",".",games_new[,13]) head(games_new)
str(games_new)

games_new[, 6] <- as.numeric(games_new[,6]) games_new[, 7] <- as.numeric(games_new[,7]) games_new[, 8] <- as.numeric(games_new[,8]) games_new[, 13] <- as.numeric(games_new[,13]) games_new[, 14] <- as.numeric(games_new[,14])
str(games_new)
games_new %>% select(Rating) %>% head(100)
games_new <- games_new %>% select(-Rating)
head(games_new)
dim(games_new)
str(games_new)
games_new$Platform <- as.factor(games_new$Platform) summary(games_new)

########## FIRST QUESTION ######## ### TWO SAMPLE MEAN TEST
### Here, first the sample is taken from main data. After that,  
### the two different subsample data is obtained (for ps and xbox platforms).
set.seed(250)
sample_data <- games_new[sample(nrow(games_new), 1500, replace = F),] head(sample_data)
library(sqldf)

ps <- sqldf("SELECT Name, Global_Sales, Platform FROM sample_data WHERE Platform LIKE 'PS%'")
ps
xbox <- sqldf("SELECT Name, Global_Sales, Platform FROM sample_data
WHERE Platform LIKE 'X%'") xbox_new <- xbox
xbox_new[,"Platform"] <- "Xbox" ps_new <- ps
ps_new[ , "Platform"] <- "PS" new_mask <- rbind(xbox_new,ps_new)
## After converting all Platform names (PS for ps_new subdata, and Xbox for xbox_new subdata), 
## started to visualize the findings and applied var.test() for variance equality, and t.test() for comparison of means.
library(ggplot2)
library(ggthemes)
ggplot(new_mask, aes(Global_Sales, color = Platform)) + geom_vline(aes(xintercept = mean(Global_Sales[Platform == "PS"]),
                                                                       color = "PS"), size = 1.25, linetype = "dashed") + geom_vline(aes(xintercept = mean(Global_Sales[Platform == "Xbox"]),
                                                                                                                                         color = "Xbox"), size = 1.25, linetype = "dashed") + scale_color_manual(values = c("PS" = "#2e6db4", "Xbox" = "#2ca243")) + scale_fill_manual(values = c("PS" = "#2e6db4", "Xbox" = "#2ca243")) + geom_density(alpha = .2, size = 1) +
  labs(y = "Density",
       x = "Global Sales (in million)",
       title = "Global Sales of Playstation (PS) & Xbox") + theme_calc() +
  theme(
    plot.title = element_text(size = 15, face="bold"), axis.title.x = element_text(size = 13, face="bold"), axis.title.y = element_text(size = 13, face = "bold"), legend.justification = c("right","top")
  )

var.test(x=ps_new$Global_Sales, y=xbox_new$Global_Sales, conf.level = 0.95)
t.test(x=ps_new$Global_Sales, y=xbox_new$Global_Sales, alternative="two.sided", conf.level = 0.95, var.equal = F )

########## SECOND QUESTION ########
### LINEAR REGRESSION

model <- lm(Global_Sales~ User_Score, data=sample_data)
## To check normality of residuals.
shapiro.test(model$residuals)
plot(model,2)
## For constant variance
plot(model,1)
## The model which is given above seems quite problematic. Hence log transformation for 
## response was applied.
model2 <- lm(log(Global_Sales)~ User_Score, data=sample_data)
# Visualization for linear regression model.
library(dplyr)
ggplot(sample_data,aes(User_Score, Global_Sales))+
  geom_point()+
  labs(x="User Score", y="Global Sales", title = "Relation Between User Score & Global Sales")+ geom_smooth(method="lm", color="red")+
  theme_base()
## To check normality of residuals. shapiro.test(model2$residuals) plot(model2,2)

## For constant variance plot(model2,1)

########## THIRD QUESTION ########
### one-way ANOVA

## Adventure, Shooter and Strategy genres was drawn
anova_question <- sqldf("SELECT Name,Global_Sales, Genre FROM sample_data WHERE Genre IN ( 'Adventure', 'Shooter', 'Strategy')")
## Check the structure of data
str(anova_question)

## Visualize your data to get an idea about the mean of each group install.packages("ggpubr")
library(ggpubr)
ggline(anova_question, x="Genre", y="Global_Sales", add=c("mean_se", "jitter"),
       order=c("Shooter", "Adventure","Strategy"), ylab="Global Sales", xlab="Game Genre")
ggboxplot(anova_question, x="Genre", y="Global_Sales", color="Genre",
          order=c("Shooter", "Adventure","Strategy"),
          ylab="Global Sales", xlab="Game Genre")+ ggtitle("Boxplot Of Global Sales by Game Genre")+ theme_minimal()

## Here it seems there are not significant difference between games genres. Check the assumptions.

##### ASSUMPTIONS OF ANOVA ## Here is the model.
anova <- aov(Global_Sales~Genre, data=anova_question) summary(anova)
### Normality
shapiro.test(residuals(anova))
plot(anova,2)
### Constant variance
plot(anova,1)
bartlett.test(Global_Sales~Genre, data=anova_question)
## Since the model is quite problematic, log transformation is applied for response. anova2 <- aov(log(Global_Sales)~Genre, data=anova_question)
### Normality
shapiro.test(residuals(anova2))
plot(anova2,2)
### Constant variance
plot(anova2,1)
bartlett.test(log(Global_Sales)~Genre, data=anova_question)
## After assumptions are met, here we analyse the data.
## To determine which pairs of groups are different, TukeyHSD() must be applied.

install.packages("multcomp") library(multcomp) TukeyHSD(anova) TukeyHSD(anova2)
Düzgün Ali Özdemir / 2561454 Aişe Esra Gönen / 2561272
