install.packages('skimr')

library('skimr')
library('tidyverse')
library('leaps')
library('forcats')
library('glmnet')
library('glmnetUtils')
library('coefplot')
library('readr')
library('ggthemes')
library('caret')
library('ggthemes')
library('ggplot2')
library('corrplot')
library('coefplot')
library('dplyr')
library('scales')
library('varhandle')
library('hablar')
library('leaps')
library('randomForest')
library('magrittr')
?skimr
skim(Original_Data)

#set seed and scientific notation
set.seed(1861)
options(scipen = 10)

#load csv
Original_Data <-read.csv('datasets/googleplaystore.csv')

playstore_DF <- read.csv('datasets/googleplaystore.csv')

#---------------------------------Data Manipulation---------------------------------------------

#Ratings

table(playstore_DF$Ratings)
str(playstore_DF$Ratings)

playstore_DF <- playstore_DF %>% 
  mutate(Rating = replace( Rating, Reviews == 0,0)) %>%  drop_na (Rating)

#Reviews into numeric
str(playstore_DF$Reviews)
table(playstore_DF$Reviews)



playstore_DF <- playstore_DF %>% mutate( Vector_Reviews = as.vector(Reviews))


playstore_DF <- playstore_DF %>% mutate( Numeric_Reviews = as.numeric(Vector_Reviews))

sapply(playstore_DF, is.numeric)
sapply(playstore_DF, is.factor)


#Price to Numeric
table(playstore_DF$Price)
str(playstore_DF$Price)



playstore_DF$Price <- as.numeric(sub("\\$","", playstore_DF$Price))


#Content Rating Levels
table(playstore_DF$Content.Rating)
Content_rating_levels <- c( "Everyone 10+", "Teen" , "Mature 17+" , "Adults only 18+" , "Unrated")
playstore_DF$Content.Rating <- playstore_DF$Content.Rating <- factor (playstore_DF$Content.Rating,levels = Content_rating_levels)
table(playstore_DF$Content.Rating)
str(playstore_DF$Content.Rating)


#create missing for missing content ratings
playstore_DF$Content.Rating <- fct_explicit_na(playstore_DF$Content.Rating, na_level=
                                        "(missing)")


#Create Category_Main
table(playstore_DF$Category)
playstore_DF <- playstore_DF %>% mutate (Category_Main = fct_lump(Category, n = 8))
table(playstore_DF$Category_Main)
# Category_Main is top 8 categories 


# Clean up Genre
table(playstore_DF$Genre)
str(playstore_DF$Genre)

playstore_DF <- playstore_DF %>%  mutate(Genre_main = 
                                           unlist(map(strsplit(as.character(playstore_DF$Genre),";"),1)))

playstore_DF <- playstore_DF %>% mutate (Genre_main = fct_lump(Genre_main, n = 8))






str(playstore_DF$Genre_main)                                                                         
table(playstore_DF$Genre_main)                                      
                             
                             
table(playstore_DF$Installs) #Installs is a Categorical Data

#Relevel Installs Variable

Installs_levels <- c("Free", "0", "0+", "1+", "5+", "10+", "50+", 
                  "100+", "500+", "1,000+", "5,000+", "10,000+", "50,000+",
                  "100,000+", "500,000+", "1,000,000+", "5,000,000+", "10,000,000+",
                  "50,000,000+", "100,000,000+", "500,000,000+", "1,000,000,000+")

str(playstore_DF$Installs)

playstore_DF$Installs <- playstore_DF$Installs <- factor (playstore_DF$Installs,levels = Installs_levels)

str(playstore_DF$Installs)

#Make new variable called "Popular" with cutoff stating at "1,000,000+"

popular_standard <- "1,000,000,000\\+|1,000,000\\+|10,000,000\\+|100,000,000\\+|5,000,000\\+|50,000,000\\+|500,000,000\\+"

playstore_DF <- playstore_DF %>% mutate (Popular = as.integer(grepl(popular_standard, Installs)))


#Last.updated conversion
Support <- "2017"
playstore_DF <- playstore_DF %>% mutate (Support = as.integer(grepl(Support, Last.Updated)))
#Created a new variable where if it contains the string "2017" spits out as true


#change size into a numerical value
# Create a seperate data frame storing all the values that are "Varies with device" 
# you might mix these back in if you are going to create a factor out of the bytes. 
playstore_varies_with_size_DF <- playstore_DF %>% filter(Size == "Varies with device")
# Create a data frame with the stuff you want to work with
playstore_has_size_DF <- playstore_DF %>% filter(Size != "Varies with device")

# Filter out the single row that does not have a size this would fit under data cleaning
# you have a row that is missing a column and all of the other ones are shifted one to the right.
playstore_has_size_DF<-playstore_has_size_DF %>% filter(grepl("[Mk]$", Size))

# This function will take a size that is either in the format xx.xM or xx.xk, 
# remove the trailing character and multiply it out so you get bytes 
.ConvertToBytes <- function(size)
{
  # Assign a multiplyer of 1,048,576 if we are looking at MB and 1,024 if KB
  multipliter = ifelse(grepl("M", size), 1024^2, 1024)
  # Drop the letter 
  number = as.numeric(str_replace(size,"[Mk]",""))
  # Return the calculated bytes
  return(multipliter*number)
}

# Add in a column that stores a numeric calculation for bytes
playstore_has_size_DF<-playstore_has_size_DF %>% mutate(bytes = .ConvertToBytes(Size))


# dropped variables LastUpdated, CurrentVer, AndroidVer


playstore_DF <- playstore_DF %>% select( -Category, -Last.Updated, -Current.Ver, -Android.Ver, -Genres,
                                         )

#---------------------------------------DATA CLEANING DONE-----------------------------------------------------------------

table(playstore_DF$Type) #Seperate Free and Paid into 2 Data Frames

free_DF <- playstore_DF %>% filter( Type == "Free")

paid_DF <- playstore_DF %>%  filter (Type == "Paid")

#Left 1 observation of NA out




#create train and test dataset for paid and free
# Using rule of thumb for 75% training and 25% testing datasets

all_train_idx <- sample(1:nrow(playstore_DF), size = 0.75 * floor(nrow(playstore_DF))) 
all_train <- playstore_DF %>% slice(all_train_idx)
all_test <- playstore_DF %>% slice(-all_train_idx)

size_train_idx <- sample(1:nrow(playstore_has_size_DF), size = 0.75 * floor(nrow(playstore_has_size_DF))) 
size_train <- playstore_has_size_DF %>% slice(size_train_idx)
size_test <- playstore_has_size_DF %>% slice(-size_train_idx)


free_train_idx <- sample(1:nrow(free_DF), size = 0.75 * floor(nrow(free_DF))) 
free_train <- free_DF %>% slice(free_train_idx)
free_test <- free_DF %>% slice(-free_train_idx)

paid_train_idx <- sample(1:nrow(paid_DF), size = 0.75 * floor(nrow(paid_DF))) 
paid_train <- paid_DF %>% slice(paid_train_idx)
paid_test <- paid_DF %>% slice(-paid_train_idx)


#visualizations

Size_Mb_list <- list("0"="0Mb","25000000"="25Mb","50000000"="50Mb","75000000"="75Mb","100000000"="100Mb")

ggplot(playstore_has_size_DF, aes( x = App, y = bytes, colour = Type)) + 
  geom_point() + scale_y_continuous(labels = Size_Mb_list) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + ggtitle("Apps and their Size") +
  labs(x = "Apps", y = "Size (in Mb)")
# Paid Apps seem to not have a pattern while Free Apps tend to be smaller in general



ggplot(playstore_DF, aes( x = Popular , y = Type)) + geom_jitter(alpha = 1/5) + 
  ggtitle("Popular versus Type") +
  labs(x = "Popular", y = "Type") + theme_clean()
#Cross tabulation of 2 categorical data, you can see that Paid Apps tend to not be "popular"
#Free apps seem to be evenly distributed between "popular" and not

ggplot(free_DF, aes( x = Popular , y = Content.Rating)) + geom_jitter() + 
  ggtitle("Popular versus Type with Paid Apps") +
  labs(x = "Popular", y = "Type") + theme_clean()
# Given that they're rated, the majority of our population data is rated teen


ggplot(playstore_DF, aes( x = Popular , y = Rating, colour = Type )) + geom_jitter(alpha = 1/2) + 
  ggtitle("Popular versus Rating") +
  labs(x = "Popular", y = "Rating") + theme_clean()+ ylim(0,5)
# 

ggplot(paid_DF, aes( x = Popular , y = Rating, colour = Type )) + geom_jitter(alpha = 1/2) + 
  ggtitle("Popular versus Rating") +
  labs(x = "Popular", y = "Rating") + theme_clean()+ ylim(0,5)
# Paid Apps tend to not be "popular"
ggplot(playstore_has_size_DF, aes( x = Installs , y = bytes, colour = Popular )) + geom_jitter(alpha = 1/2) + 
  ggtitle("Installs versus Size") + 
  scale_y_continuous( name = "size", limits = c(0, 100000000), labels = Size_Mb_list)+
  labs(x = "Installs", y = "Size", 
       caption = "Installs is Categorical data, it is not in scale") + 
  theme(axis.text.x = element_text(angle = 90 , hjust = 1))


#Installs vs size



ggplot(playstore_DF, aes( x = Installs , y = Numeric_Reviews, colour = Popular )) + geom_jitter(alpha = 1/2) + 
  ggtitle("Installs versus Reviews") +
  labs(x = "Installs", y = "Reviews") + theme(axis.text.x = element_text(angle = 90 , hjust = 1))
                                              
                                              
ggplot(playstore_has_size_DF, aes(x= Installs, y= bytes)) +
  theme(axis.text.x = element_text(angle = 45 , hjust =1))+
  geom_jitter() + 
  ggtitle("Installs vs Size") +
  facet_wrap(~Category_Main, scales = "free") +
  labs( x = "Installs", y = "bytes") +
  scale_y_continuous( name = "size", limits = c(0, 100000000), labels = Size_Mb_list)
#Game and Family Category Apps tend to have higher downloads and Size

ggplot(paid_DF, aes( x = Installs , y = Price, colour = Popular )) + geom_jitter(alpha = 1/2) + 
  ggtitle("Installs versus Price for Paid Apps") +
  labs(x = "Installs", y = "Price") + 
  theme(axis.text.x = element_text(angle = 90 , hjust = 1))+
  scale_y_continuous( name = "Price", limits = c(0, 50), labels = c("$0","$10", "$20","$30", "$40", "$50"))
#There are outliers for price, we cut the y lim to 50.


ggplot(playstore_DF, aes (x = Category_Main))+ geom_bar()+
  theme(axis.text.x = element_text(angle = 45 , hjust = 1))+ ggtitle("Count in Each Category Main")

ggplot(playstore_DF, aes (x = Content.Rating))+ geom_bar()+
  theme_clean(axis.text.x = element_text(angle = 45 , hjust = 1))+ ggtitle("Count in Each Content Rating")

ggplot(playstore_DF, aes (x= Rating)) +geom_density() + xlim( 0, 5)+ ggtitle("Rating Density Chart")

ggplot(playstore_DF, aes(x= Type)) +geom_bar()+ ggtitle ("Distribution of Free Versus Paid Apps")


ggplot(playstore_has_size_DF, aes( x= bytes)) + geom_density()+ scale_x_continuous(labels = Size_Mb_list)+
  ggtitle(" Density distribution of App Size in Bytes")
  
ggplot(paid_DF, aes (x= Support, y = Price)) + geom_jitter() + ylim(0,100)+
  ggtitle('Paid Apps vs Upates')

ggplot(playstore_DF, aes (x= Support, y = Rating)) + geom_jitter(alpha = 1/5)+ ylim(0,5) +
  ggtitle('All Apps vs Upates')

ggplot(playstore_DF, aes(x= Installs)) +geom_bar()+ ggtitle ("Distribution of Apps sorted by Installs")+
  theme(axis.text.x = element_text(angle = 45 , hjust = 1))

ggplot(playstore_DF, aes(x = Category_Main, y= Rating )) + geom_boxplot() +ylim(0,5)+
  theme(axis.text.x = element_text(angle = 90 , hjust = 1)) + ggtitle("Box Plot of Category_Main and Rating") 

ggplot(playstore_DF, aes (x = Genre_main)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90 , hjust = 1)) +ggtitle("Count of Apps in each Genre_main Category")

ggplot(playstore_DF, aes (x = Support, y = Installs)) + geom_jitter() + ggtitle(" Recently Supported vs Number of Downloads")
  
  


Top_Price_apps <- paid_DF %>%  arrange(desc(Price)) %>% slice (1:20)
#top 20 most expensive apps
Top_Price_apps

Most_Reviewed_apps <- playstore_DF %>% arrange(desc(Numeric_Reviews)) %>%  slice(1:20)

Most_Reviewed_apps


#Correlation Matrix
cormat <- cor(playstore_DF %>% select_if(is.numeric) %>% drop_na())
print(cormat[])



#------------------ MODEL------------------------------------------


#Logistic Regression
#Size Apps
#removed category main 
#remove content.rating

LogModSizeApps <- glm(Popular ~  Numeric_Reviews + Rating +
                 Price +  bytes + Type + Support + Genre_main , 
                 data = size_train)


summary(LogModSizeApps)

coef(LogModSizeApps)

size_logit_pred_train <- predict(LogModSizeApps, size_train )

size_logit_pred_test <- predict(LogModSizeApps,  size_test )

size_preds_test <- data.frame(scores = 
                                      predict (LogModSizeApps, newdata = size_test, type = "response"), 
                                      size_test)


size_preds_train <- data.frame(scores = 
                                      predict (LogModSizeApps, newdata = size_train, type = "response"), 
                                      size_train)

size_preds_train %<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

size_preds_test%<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

size_tab_train <- table( size_preds_train$Popular, size_preds_train$class_pred05)

size_tab_test <- table( size_preds_test$Popular, size_preds_test$class_pred05)

size_logit_diagnostics <- function(tab) {
  TP <- tab[2,2]
  TN <- tab[1,1]
  FP <- tab[1,2]
  FN <- tab[2,1]
  Ntrue <- tab [1,1] + tab[1,2]
  Ptrue <- tab [2,1] + tab[2,2]
  Accuracy <- (TP + TN)/(Ntrue +Ptrue)
  sens <- TP/Ptrue
  spec <- TN/Ntrue
  FPR <- FP/Ntrue
  cat(sprintf("Accuracy: %s\n", round(Accuracy,3)))
  cat(sprintf("TP: %s\n", round(TP,3)))
  cat(sprintf("TN: %s\n", round(TN,3)))
  cat(sprintf("Sensitivity : %s\n", round(sens,3)))
  cat(sprintf("Specificity: %s\n", round(spec,3)))
  cat(sprintf("False Pos Rate: %s\n", round(FPR,3)))
}

size_logit_diagnostics(size_tab_train)

size_logit_diagnostics(size_tab_test)

postResample(size_preds_train$class_pred05, size_preds_train$Popular)

postResample(size_preds_test$class_pred05, size_preds_test$Popular)

#Free Apps
LogModFreeApps <- glm(Popular ~  Numeric_Reviews + Rating  +
                      Support + Genre_main,
                     data = free_train)

summary(LogModFreeApps)

coef(LogModFreeApps)

free_logit_pred_train <- predict(LogModFreeApps, free_train )

free_logit_pred_test <- predict(LogModFreeApps,  free_test )

free_preds_test <- data.frame(scores = 
                                predict (LogModFreeApps, newdata = free_test, type = "response"), 
                              free_test)


free_preds_train <- data.frame(scores = 
                                 predict (LogModFreeApps, newdata = free_train, type = "response"), 
                               free_train)

free_preds_train %<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

free_preds_test%<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

free_tab_train <- table( free_preds_train$Popular, free_preds_train$class_pred05)

free_tab_test <- table( free_preds_test$Popular, free_preds_test$class_pred05)

free_logit_diagnostics <- function(tab) {
  TP <- tab[2,2]
  TN <- tab[1,1]
  FP <- tab[1,2]
  FN <- tab[2,1]
  Ntrue <- tab [1,1] + tab[1,2]
  Ptrue <- tab [2,1] + tab[2,2]
  Accuracy <- (TP + TN)/(Ntrue +Ptrue)
  sens <- TP/Ptrue
  spec <- TN/Ntrue
  FPR <- FP/Ntrue
  cat(sprintf("Accuracy: %s\n", round(Accuracy,3)))
  cat(sprintf("TP: %s\n", round(TP,3)))
  cat(sprintf("TN: %s\n", round(TN,3)))
  cat(sprintf("Sensitivity : %s\n", round(sens,3)))
  cat(sprintf("Specificity: %s\n", round(spec,3)))
  cat(sprintf("False Pos Rate: %s\n", round(FPR,3)))
}

free_logit_diagnostics(free_tab_train)

free_logit_diagnostics(free_tab_test)

postResample(free_preds_train$class_pred05, free_train$Popular)

postResample(free_preds_test$class_pred05, free_test$Popular)



#Paid Apps
#Category main genre main removed because of error
LogModPaidApps <- glm(Popular ~  Numeric_Reviews + Rating + Category_Main +
                       Price + Content.Rating  + Support ,
                     data = paid_train)

summary(LogModPaidApps)

coef(LogModPaidApps)

paid_logit_pred_train <- predict(LogModPaidApps, paid_train )

paid_logit_pred_test <- predict(LogModPaidApps,  paid_test )

paid_preds_test <- data.frame(scores = 
                                predict (LogModPaidApps, newdata = paid_test, type = "response"), 
                              paid_test)


paid_preds_train <- data.frame(scores = 
                                 predict (LogModPaidApps, newdata = paid_train, type = "response"), 
                               paid_train)

paid_preds_train %<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

paid_preds_test%<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

paid_tab_train <- table( paid_preds_train$Popular, paid_preds_train$class_pred05)

paid_tab_test <- table( paid_preds_test$Popular, paid_preds_test$class_pred05)

paid_logit_diagnostics <- function(tab) {
  TP <- tab[2,2]
  TN <- tab[1,1]
  FP <- tab[1,2]
  FN <- tab[2,1]
  Ntrue <- tab [1,1] + tab[1,2]
  Ptrue <- tab [2,1] + tab[2,2]
  Accuracy <- (TP + TN)/(Ntrue +Ptrue)
  sens <- TP/Ptrue
  spec <- TN/Ntrue
  FPR <- FP/Ntrue
  cat(sprintf("Accuracy: %s\n", round(Accuracy,3)))
  cat(sprintf("TP: %s\n", round(TP,3)))
  cat(sprintf("TN: %s\n", round(TN,3)))
  cat(sprintf("Sensitivity : %s\n", round(sens,3)))
  cat(sprintf("Specificity: %s\n", round(spec,3)))
  cat(sprintf("False Pos Rate: %s\n", round(FPR,3)))
}

paid_logit_diagnostics(paid_tab_train)

paid_logit_diagnostics(paid_tab_test)

postResample(paid_preds_train$class_pred05, paid_train$Popular)

postResample(paid_preds_test$class_pred05, paid_test$Popular)



#Forward Stepwise Model
#Size Apps
size_fit_fwd <- 
  regsubsets(Popular ~ Numeric_Reviews + Rating + Category_Main +
               Price + Content.Rating  + bytes + Type + Support,
             data = size_train,
             nvmax = 7,
             method = "forward")

summary(size_fit_fwd)

coef(size_fit_fwd,7)
#pick top 5
FwdLogModSizeApps <- glm(Popular ~  Rating+ Numeric_Reviews + Type + bytes +Support ,
                      data = size_train)

coef(FwdLogModSizeApps)

fwd_size_logit_pred_train <- predict(FwdLogModSizeApps, size_train )

fwd_size_logit_pred_test <- predict(FwdLogModSizeApps,  size_test )

fwd_size_preds_test <- data.frame(scores = 
                                predict (FwdLogModSizeApps, newdata = size_test, type = "response"), 
                              size_test)


fwd_size_preds_train <- data.frame(scores = 
                                 predict (FwdLogModSizeApps, newdata = size_train, type = "response"), 
                               size_train)

fwd_size_preds_train %<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

fwd_size_preds_test%<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

fwd_size_tab_train <- table( fwd_size_preds_train$Popular, fwd_size_preds_train$class_pred05)

fwd_size_tab_test <- table( fwd_size_preds_test$Popular, fwd_size_preds_test$class_pred05)

fwd_size_logit_diagnostics <- function(tab) {
  TP <- tab[2,2]
  TN <- tab[1,1]
  FP <- tab[1,2]
  FN <- tab[2,1]
  Ntrue <- tab [1,1] + tab[1,2]
  Ptrue <- tab [2,1] + tab[2,2]
  Accuracy <- (TP + TN)/(Ntrue +Ptrue)
  sens <- TP/Ptrue
  spec <- TN/Ntrue
  FPR <- FP/Ntrue
  cat(sprintf("Accuracy: %s\n", round(Accuracy,3)))
  cat(sprintf("TP: %s\n", round(TP,3)))
  cat(sprintf("TN: %s\n", round(TN,3)))
  cat(sprintf("Sensitivity : %s\n", round(sens,3)))
  cat(sprintf("Specificity: %s\n", round(spec,3)))
  cat(sprintf("False Pos Rate: %s\n", round(FPR,3)))
}

fwd_size_logit_diagnostics(fwd_size_tab_train)

fwd_size_logit_diagnostics(fwd_size_tab_test)

postResample(fwd_size_preds_train$class_pred05, fwd_size_preds_train$Popular)

postResample(fwd_size_preds_test$class_pred05, fwd_size_preds_test$Popular)


#Free Apps
free_fit_fwd <- 
  regsubsets(Popular ~ Numeric_Reviews + Rating + Category_Main +
               Content.Rating  + Support,
             data = free_train,
             nvmax = 5,
             method = "forward")

summary(free_fit_fwd)

coef(free_fit_fwd,5)

#run logistic regression
#drop content rating and support
FwdLogModFreeApps <- glm(Popular ~  Numeric_Reviews + Rating + Category_Main + Support,
                      data = free_train)

coef(FwdLogModFreeApps)

fwd_free_logit_pred_train <- predict(FwdLogModFreeApps, free_train )

fwd_free_logit_pred_test <- predict(FwdLogModFreeApps,  free_test )

fwd_free_preds_test <- data.frame(scores = 
                                predict (FwdLogModFreeApps, newdata = free_test, type = "response"), 
                              free_test)


fwd_free_preds_train <- data.frame(scores = 
                                 predict (FwdLogModFreeApps, newdata = free_train, type = "response"), 
                               free_train)

fwd_free_preds_train %<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

fwd_free_preds_test%<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

fwd_free_tab_train <- table( fwd_free_preds_train$Popular, fwd_free_preds_train$class_pred05)

fwd_free_tab_test <- table( fwd_free_preds_test$Popular, fwd_free_preds_test$class_pred05)

fwd_free_logit_diagnostics <- function(tab) {
  TP <- tab[2,2]
  TN <- tab[1,1]
  FP <- tab[1,2]
  FN <- tab[2,1]
  Ntrue <- tab [1,1] + tab[1,2]
  Ptrue <- tab [2,1] + tab[2,2]
  Accuracy <- (TP + TN)/(Ntrue +Ptrue)
  sens <- TP/Ptrue
  spec <- TN/Ntrue
  FPR <- FP/Ntrue
  cat(sprintf("Accuracy: %s\n", round(Accuracy,3)))
  cat(sprintf("TP: %s\n", round(TP,3)))
  cat(sprintf("TN: %s\n", round(TN,3)))
  cat(sprintf("Sensitivity : %s\n", round(sens,3)))
  cat(sprintf("Specificity: %s\n", round(spec,3)))
  cat(sprintf("False Pos Rate: %s\n", round(FPR,3)))
}

fwd_free_logit_diagnostics(fwd_free_tab_train)

fwd_free_logit_diagnostics(fwd_free_tab_test)

postResample(fwd_free_preds_train$class_pred05, fwd_free_preds_train$Popular)

postResample(fwd_free_preds_test$class_pred05, fwd_free_preds_test$Popular)

#Paid Apps
paid_fit_fwd <- 
  regsubsets(Popular ~ Numeric_Reviews + Rating + Category_Main+
               Price + Content.Rating  + Support,
             data = paid_train,
             nvmax = 7,
             method = "forward")

summary(paid_fit_fwd)

coef(paid_fit_fwd,7)



#fwd model
FwdLogModPaidApps <- glm(Popular ~  Numeric_Reviews  + Category_Main+ Support + Content.Rating +
                           Rating  ,
                      data = paid_train)

coef(FwdLogModPaidApps)

fwd_paid_logit_pred_train <- predict(FwdLogModPaidApps, paid_train )

fwd_paid_logit_pred_test <- predict(FwdLogModPaidApps,  paid_test )

fwd_paid_preds_test <- data.frame(scores = 
                                predict (FwdLogModPaidApps, newdata = paid_test, type = "response"), 
                              paid_test)


fwd_paid_preds_train <- data.frame(scores = 
                                 predict (FwdLogModPaidApps, newdata = paid_train, type = "response"), 
                               paid_train)

fwd_paid_preds_train %<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

fwd_paid_preds_test%<>% mutate(class_pred05 = ifelse (scores > 0.5,1,0))

fwd_paid_tab_train <- table( paid_preds_train$Popular, paid_preds_train$class_pred05)

fwd_paid_tab_test <- table( paid_preds_test$Popular, paid_preds_test$class_pred05)

fwd_paid_logit_diagnostics <- function(tab) {
  TP <- tab[2,2]
  TN <- tab[1,1]
  FP <- tab[1,2]
  FN <- tab[2,1]
  Ntrue <- tab [1,1] + tab[1,2]
  Ptrue <- tab [2,1] + tab[2,2]
  Accuracy <- (TP + TN)/(Ntrue +Ptrue)
  sens <- TP/Ptrue
  spec <- TN/Ntrue
  FPR <- FP/Ntrue
  cat(sprintf("Accuracy: %s\n", round(Accuracy,3)))
  cat(sprintf("TP: %s\n", round(TP,3)))
  cat(sprintf("TN: %s\n", round(TN,3)))
  cat(sprintf("Sensitivity : %s\n", round(sens,3)))
  cat(sprintf("Specificity: %s\n", round(spec,3)))
  cat(sprintf("False Pos Rate: %s\n", round(FPR,3)))
}

fwd_paid_logit_diagnostics(fwd_paid_tab_train)

fwd_paid_logit_diagnostics(fwd_paid_tab_test)

postResample(fwd_paid_preds_train$class_pred05, fwd_paid_preds_train$Popular)

postResample(fwd_paid_preds_test$class_pred05, fwd_paid_preds_test$Popular)

# Elastic Net
# All Apps
alpha_list <- seq(0,1,len = 5)
alpha_list

size_enet_fit <- cva.glmnet(Popular ~ Numeric_Reviews + Rating + Category_Main +
                              Price + Content.Rating + Genre_main + bytes + Type + Support,
                            data = size_train,
                       alpha = alpha_list)

size_enet_fit

minlossplot(size_enet_fit)

plot(size_enet_fit)


#---------------- Enet

# Free Apps
free_enet_fit <- cva.glmnet(Popular ~ Numeric_Reviews + Rating + Category_Main +
                              Content.Rating + Genre_main + Support,
                            data = free_train,
                           alpha = alpha_list)

free_enet_fit

minlossplot(free_enet_fit)

plot(free_enet_fit)


# Paid Apps
paid_enet_fit <- cva.glmnet(Popular ~ Numeric_Reviews + Rating + Category_Main+
                              Price + Content.Rating + Genre_main  + Support,
                            data = paid_train,
                            alpha = alpha_list)

paid_enet_fit

minlossplot(paid_enet_fit)

plot(paid_enet_fit)


#Random Forest
#Size apps

size_rf_fit <- randomForest(Popular ~ Numeric_Reviews + Rating + Category_Main +
                              Price + Content.Rating + Genre_main + bytes + Type + Support,
                            data = size_train,
                       type = classification,
                       mtry = 3,
                       ntree = 100,
                       importance = TRUE,
                       localImp = TRUE)

size_rf_fit
