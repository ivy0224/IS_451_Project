library(dplyr)
library(ggcorrplot)
library(tidyverse)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
options(scipen = 999, digits = 3)

## load data
booking <- read.csv("booking.csv")
str(booking)
summary(booking)

## data cleaning and pre-processing
# factor categorical data 
booking$type.of.meal <- factor(booking$type.of.meal)
booking$room.type <- factor(booking$room.type)
booking$market.segment.type <- factor(booking$market.segment.type)
booking$booking.status <- factor(booking$booking.status)
str(booking)

# factor numeric that should be categorical
# car.parking.space: 0=No, 1=Yes
booking$car.parking.space <- factor(booking$car.parking.space, levels = c(0, 1),
                                    labels = c("No_Parking", "Parking"))
booking$car.parking.space
# repeated: 0=No, 1=Yes
booking$repeated <- factor(booking$repeated, levels = c(0, 1),
                           labels = c("Not_Repeated", "Repeated"))
booking$repeated
str(booking)

# check levels after factor
levels(booking$type.of.meal)
levels(booking$room.type)
levels(booking$market.segment.type)
levels(booking$car.parking.space)
levels(booking$repeated)
levels(booking$booking.status)

# check missing values
na.rows <- booking %>% 
  filter(if_any(everything(), is.na))

# separate date --> month + day + year
# deal with format M/D/YYYY
mdy.df <- booking %>%
  separate(date.of.reservation, into = c("month", "day", "year"), 
           sep = "/", remove = FALSE)

# get 37 rows unmatched with M/D/YYYY but YYYY-M-D
ymd <- mdy.df %>%
  filter(if_any(everything(), is.na)) %>%
  pull(Booking_ID)

# split into 2 dataset
# get M/D/YYYY first
mdy.df <- mdy.df %>%
  filter(if_all(everything(), ~!is.na(.)))

# deal with YYYY-M-D then
ymd.df <- booking %>%
  filter(Booking_ID %in% ymd) %>%
  separate(date.of.reservation, into = c("year", "month", "day"),
           sep = "-", remove = FALSE)

# combine 2 dataset
booking <- bind_rows(mdy.df, ymd.df)

# transform from char to num
str(booking)
booking$month <- as.numeric(booking$month)
booking$day <- as.numeric(booking$day)
booking$year <- as.numeric(booking$year)

# drop rows whose market.segment.type = Complementary
# since it's free, no one canceled
booking <- booking %>%
  filter(market.segment.type != "Complementary")

# remove room type as we do not have detailed info
# of each room type
booking <- booking %>%
  select(-room.type)

# set base level
# type.of.meal to Not Selected
booking$type.of.meal <- relevel(booking$type.of.meal, ref = "Not Selected")
# booking.status to Not_Canceled
booking$booking.status <- relevel(booking$booking.status, ref = "Not_Canceled")


# partition: 70% train, 30% validation
set.seed(1234)
train.index <- sample(1:nrow(booking), nrow(booking) * 0.7)
train.df <- booking[train.index, ]
valid.df <- booking[-train.index, ]

## Model 1: logistic regression
## target: booking.status

# draw correlation matrix between each variable to target
# convert booking.status to numeric 
# (0 = Not_Canceled, 1 = Canceled)
booking.num <- booking
booking.num$booking.status <- as.numeric(booking.num$booking.status == "Canceled")
booking.num$repeated <- as.numeric(booking.num$repeated == "Repeated")
booking.num$type.of.meal <- as.numeric(booking.num$type.of.meal)
booking.num$car.parking.space <- as.numeric(booking.num$car.parking.space == "Parking")
booking.num$market.segment.type <- as.numeric(booking.num$market.segment.type)
booking.num <- booking.num[, -c(1, 15)]

# calculate correlations between each predictor and the target
corr.with.target <- sapply(booking.num[, -17], 
                           function(x) cor(x, booking.num$booking.status, 
                                           use = "complete.obs"))
corr.df <- data.frame(predictor = names(corr.with.target), 
                      correlation = corr.with.target)

# plot correlation
corr.plot <- ggplot(corr.df, aes(x = reorder(predictor, correlation), 
                                 y = correlation)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Correlation of Predictors with Booking Status",
       x = "Predictor", y = "Correlation with Booking Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
corr.plot

# logit 1-0: raw
# predictors: all variables exclude Booking_ID (key identifier)
#             and date.of.reservation (transformed into year, month, day)
# AIC = 21821
logit.reg.raw <- glm(booking.status~., data = train.df[, -c(1, 15)], 
                 family = "binomial")
summary(logit.reg.raw)

# check for multicollinearity among predictors
# no obvious multicollinearity
library(car)
vif(logit.reg.raw)

# logit 1-1
# predictors: all variables with great significance level (*** + **)
# AIC = 21817
selected.var.v1 <- c("number.of.weekend.nights", "number.of.week.nights",
                    "type.of.meal", "car.parking.space","lead.time", 
                    "market.segment.type", "repeated", "P.C",
                    "average.price", "special.requests", "month", 
                    "year", "booking.status")
logit.reg.v1 <- glm(booking.status~., data = train.df[, selected.var.v1], 
                     family = "binomial")
summary(logit.reg.v1)

# logit 1-2
# predictors: all variables with maximum significance level (***)
# AIC = 23442
selected.var.v2 <- c("number.of.weekend.nights","car.parking.space",
                     "lead.time", "repeated", "average.price", 
                     "special.requests", "month", "year", "booking.status")
logit.reg.v2 <- glm(booking.status~., data = train.df[, selected.var.v2], 
                    family = "binomial")
summary(logit.reg.v2)

# logit 1-3
# predictors: combination or ** and *** significance level 
#             based on corr.plot (top 6 correlations)
# AIC = 23582
selected.var.v3 <- c("lead.time", "special.requests", "year", 
                     "repeated" , "average.price" , "car.parking.space" ,
                     "booking.status")
logit.reg.v3 <- glm(booking.status~., data = train.df[, selected.var.v3], 
                    family = "binomial")
summary(logit.reg.v3)

# logit 1-4
# predictors: combination or ** and *** significance level 
#             based on SQL and visualization exploratory outcomes.
# AIC = 21962
selected.var.v4 <- c("car.parking.space", "lead.time", "repeated", 
                     "average.price", "special.requests", "month",
                     "market.segment.type", "booking.status")
logit.reg.v4 <- glm(booking.status~., data = train.df[, selected.var.v4], 
                    family = "binomial")
summary(logit.reg.v4)

# logit 1-5
# predictors: combination or ** and *** significance level 
#             further narrow down based on logit 1-4.
# AIC = 22172
selected.var.v5 <- c("lead.time", "average.price", "special.requests",
                     "month", "market.segment.type", "booking.status")
logit.reg.v5 <- glm(booking.status~., data = train.df[, selected.var.v5], 
                    family = "binomial")
summary(logit.reg.v5)

# logit 1-6
# predictors: combination or ** and *** significance level 
#             further narrow down based on logit 1-4.
# AIC = 22258
selected.var.v6 <- c("lead.time", "average.price", "special.requests",
                     "market.segment.type", "booking.status")
logit.reg.v6 <- glm(booking.status~., data = train.df[, selected.var.v6], 
                    family = "binomial")
summary(logit.reg.v6)

# logit 1-7
# predictors: combination or ** and *** significance level 
#             further narrow down based on logit 1-4.
# AIC = 22097
selected.var.v7 <- c("lead.time", "average.price", "special.requests",
                     "car.parking.space", "market.segment.type", "booking.status")
logit.reg.v7 <- glm(booking.status~., data = train.df[, selected.var.v7], 
                    family = "binomial")
summary(logit.reg.v7)

# logit 1-8
# predictors: combination or ** and *** significance level 
#             further narrow down based on logit 1-8.
# AIC = 22208
selected.var.v8 <- c("lead.time", "average.price", "special.requests",
                     "repeated", "market.segment.type", "booking.status")
logit.reg.v8 <- glm(booking.status~., data = train.df[, selected.var.v8], 
                    family = "binomial")
summary(logit.reg.v8)

# logit 1-9
# predictors: combination or ** and *** significance level 
#             further narrow down based on logit 1-8.
# AIC = 24089
selected.var.v9 <- c("lead.time", "average.price", "special.requests",
                     "repeated", "booking.status")
logit.reg.v9 <- glm(booking.status~., data = train.df[, selected.var.v9], 
                    family = "binomial")
summary(logit.reg.v9)

## logit.reg.raw: AIC = 21821, amount of variables = 16
## choose logit.reg.v6 with the AIC = 22258, but least amount of variables = 4
## reasoning: increase in AIC for around 400 based on 21821 is not significant,
## but decrease the amount of variables to collect and interpret by 12 saves a lot.

## predict based on valid.df, use cutoff value = 0.38
## aim at predicting "Canceled", so decrease cutoff to increase sensitivity
logit.reg.v6.pred <- predict(logit.reg.v6, valid.df, type = "response")
pred.v6 <- ifelse(logit.reg.v6.pred > 0.38, "Canceled", "Not_Canceled")

## measure accuracy of logistic regression
logit.cm.v6 <- confusionMatrix(factor(pred.v6, levels = c("Not_Canceled", "Canceled")), 
                               factor(valid.df$booking.status), 
                               positive = "Canceled")
logit.cm.v6

### Model 2: classification tree
## ct 2-1: default classification tree (5 nodes)
# target: booking_binary (Canceled/Not_Canceled)
# predictors: all other variables
default.ct <- rpart(booking.status ~., data = train.df[, -c(1, 15)], 
                    method = "class")
rpart.plot(default.ct, type = 1, extra = 1)

# predict based on default tree on valid.df
default.ct.pred <- predict(default.ct, valid.df, type = "class")
# measure accuracy of default tree
default.ct.cm <- confusionMatrix(default.ct.pred, factor(valid.df$booking.status), 
                                 positive = "Canceled")
default.ct.cm

## ct 2-2: control classification tree (5 nodes)
# target: booking_binary (Canceled/Not_Canceled)
# predictors: all other variables
control.ct <- rpart(booking.status ~., data = train.df[, -c(1, 15)], method = "class", 
                    control = rpart.control(maxdepth = 3, minbucket = 30))
rpart.plot(control.ct, type = 1, extra = 1)

# predict based on control tree on valid.df
control.ct.pred <- predict(control.ct, valid.df, type = "class")
# measure accuracy of control tree
control.ct.cm <- confusionMatrix(control.ct.pred, factor(valid.df$booking.status), 
                                 positive = "Canceled")
control.ct.cm

## compare Default and Control tree
# Accuracy: 0.828 vs 0.784
# Sensitivity : 0.692 vs 0.743 --> what we are looking for: "Canceled"
# Specificity : 0.894 vs 0.804
# Balanced Accuracy: 0.793 vs 0.774
## choose Control tree
# reasoning: 1. avoid problem of overfitting
#            2. though the overall accuracy is lower, but higher sensitivity (match our goal)
#            3. 4% decrease in accuracy but easier model to interpret, fewer variables needed

# Model 3: randomForest ensemble of decision tree
rf <- randomForest(booking.status~., data = train.df[, -c(1, 15)], 
                   ntree = 500, mtry = sqrt(ncol(train.df) - 2), 
                   importance = TRUE)
importance(rf)
varImpPlot(rf)

# predict based on valid.df
rf.pred <- predict(rf, valid.df, type = "prob")[,2]
head(rf.pred)

# set cutoff value = 0.5
rf.pred <- ifelse(rf.pred > 0.4, "Canceled", "Not_Canceled")

# measure accuracy of randomForest decision tree
rf.cm <- confusionMatrix(factor(rf.pred, levels = c("Not_Canceled", "Canceled")), 
                         valid.df$booking.status, 
                         positive = "Canceled")
rf.cm

### compare performance of three models
accuracy_comparison <- data.frame(
  Model = c("Logistic Regression", "Control Tree", "Random Forest"),
  Accuracy = c(logit.cm.v6$overall["Accuracy"], 
               control.ct.cm$overall["Accuracy"], 
               rf.cm$overall["Accuracy"]),
  Sensitivity = c(logit.cm.v6$byClass["Sensitivity"], 
                  control.ct.cm$byClass["Sensitivity"], 
                  rf.cm$byClass["Sensitivity"]),
  Specificity = c(logit.cm.v6$byClass["Specificity"], 
                  control.ct.cm$byClass["Specificity"], 
                  rf.cm$byClass["Specificity"]),
  Balanced_Accuracy = c(logit.cm.v6$byClass["Balanced Accuracy"], 
                        control.ct.cm$byClass["Balanced Accuracy"],
                        rf.cm$byClass["Balanced Accuracy"])
)

accuracy_comparison

### visualize comparison
# convert data to long format for ggplot
accuracy_long <- tidyr::pivot_longer(accuracy_comparison, cols = -Model, 
                                     names_to = "Metric", values_to = "Value")

# plot accuracy, sensitivity, specificity
ggplot(accuracy_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  ggtitle("Model Performance Comparison") +
  ylab("Performance Score") + 
  xlab("Model") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c(
    "Accuracy" = "#1f78b4", 
    "Sensitivity" = "#33a02c", 
    "Specificity" = "#e31a1c", 
    "Balanced_Accuracy" = "#ff7f00"
  ))
