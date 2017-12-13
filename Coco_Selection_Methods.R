
require(glmnet)
require(MASS)
require(pROC)
require(mosaic)
load("completeimputed50.Rda")
ment <- compdat50 
options(xtable.comment = FALSE)

ment$income <-  as.factor(ment$income)
ment$state <-  as.factor(ment$state)
ment$metro <-  as.factor(ment$metro)
ment$smoker <-  as.factor(ment$smoker)
ment$employ <-  as.factor(ment$employ)
ment$trans <-  as.factor(ment$trans)
ment$actlimit <-  as.factor(ment$actlimit)
ment$sexorient <- as.factor(ment$sexorient)
ment$race <- as.factor(ment$race)

require(xtable)
x <- subset(x = ment, select = -mental)
x<-data.matrix(x)
y<-data.matrix(ment$mental)


#LASSO
lasso <- glmnet(x, y, family = "binomial", alpha = 1)
#plot.lasso <- plot(lasso, xvar = "dev", main = "LASSO Regression")


cv.lasso <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", alpha = 1)
#plot(cv.lasso)
#coef(cv.lasso)


#RIDGE
ridge <- glmnet(x, y, family = "binomial", alpha=0)
#plot.ridge <- plot(ridge, xvar = "dev", main = "Ridge Regression")

cv.ridge <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", alpha = 0)
#plot(cv.ridge)
#coef(cv.ridge)

#StepAIC
ment <- mutate(ment, mental = as.factor(mental))
binom <- glm(mental ~ ., data = ment, family = "binomial")
step <- stepAIC(binom, direction= "both", trace = FALSE)
model.AIC <- glm(step$formula, family = "binomial", 
                 data = ment)

lasso.coef <- as.data.frame(as.matrix(coefficients(cv.lasso)))
colnames(lasso.coef) <- "LASSO"
lasso.coef <- mutate(lasso.coef, variable = rownames(lasso.coef))

ridge.coef <- as.data.frame(as.matrix(coefficients(cv.ridge)))
colnames(ridge.coef) <- "Ridge"
ridge.coef <- mutate(ridge.coef, variable = rownames(ridge.coef))

aic.coef <- as.data.frame(summary(model.AIC)$coefficients[,"Estimate"])
colnames(aic.coef) <- "Stepwise"
aic.coef <- mutate(aic.coef, variable = rownames(aic.coef))

coefficients <- merge(aic.coef, lasso.coef, by= "variable", all = TRUE)
coefficients <- merge(coefficients, ridge.coef, by = "variable", all = TRUE)


toshow <- filter(coefficients, variable %in% 
                   c("male", "binge", "bmi", "exer30", "smoker", "smoker1", "smoker2"))
toshow[5,2] <- 0
toshow[6:7, 3:4] <- 0
showing <- round(toshow[,2:4], 3)
rownames(showing) <- c("male", "binge", "bmi", "exer30", "smoker", "smoker1", "smoker2")



folds <- rep(1:10, each = 48630)
folds <- c(folds, 1:3)
folds <- sample(folds)
ment <- mutate(ment, fold = folds)

auc.test.lasso <- c()
auc.test.ridge <- c()
auc.test.aic <- c()
auc.test.aic2 <- c()
for (i in 1:10){
  data.training <- filter(ment, fold != i)
  data.test <- filter(ment, fold == i)
  x.training <- subset(x = data.training, select = -mental)
  x.training <- data.matrix(x.training)
  y.training <- data.matrix(data.training$mental)
  x.test <- subset(x = data.test, select = -mental)
  x.test <- data.matrix(x.test)
  y.test <- data.matrix(data.test$mental)
  #the models 
  cv.lasso <- cv.glmnet(x.training, y.training, family = "binomial", type.measure = "auc", alpha = 1)
  cv.ridge <- cv.glmnet(x.training, y.training, family = "binomial", type.measure = "auc", alpha = 0)
  binom <- glm(mental ~ ., data = data.training, family = "binomial")
  step <- stepAIC(binom, direction= "both", trace = FALSE)
  model.AIC <- glm(step$formula, family = "binomial", 
                   data = ment)
  #predictions
  preds.test.lasso <-  as.numeric(predict(cv.lasso, newx = x.test, type = "class"))
  preds.test.ridge <- as.numeric(predict(cv.ridge, newx = x.test, type = "class"))
  indexes <-  which(ment$fold == i)
  ors.test.aic <- predict(model.AIC, newx = subset(data.test, select = -mental), type = "response")[indexes]
  preds.test.aic <- ifelse(ors.test.aic < .5,
                           yes = 0, no = 1)
  preds.test.aic2 <- rbinom(n = length(preds.test.aic), size = 1, prob = ors.test.aic)
  #area under the curve
  auc.test.lasso[i] <- auc(roc(predictor = preds.test.lasso, response = data.test$mental))
  auc.test.ridge[i] <- auc(roc(predictor = preds.test.ridge, response = data.test$mental))
  auc.test.aic[i] <- auc(roc(predictor = preds.test.aic, response = data.test$mental))
  auc.test.aic2[i] <- auc(roc(predictor = preds.test.aic2, response = data.test$mental))
  
}

mean.auc.lasso <- mean(auc.test.lasso)
mean.auc.ridge <- mean(auc.test.ridge)
mean.auc.aic <- mean(auc.test.aic)
mean.auc.aic2 <- mean(auc.test.aic2)

compare <- data.frame(Mean.AUC = c(mean.auc.lasso, mean.auc.ridge, mean.auc.aic))
rownames(compare) <- c("LASSO", "Ridge Regression", "Step AIC")
xtable(compare)

simulated <- mutate(ment, index = 1:nrow(ment))
simulated <- subset(simulated, select = - c(mental))

auc10_lasso <- c()
auc10_ridge <- c()
auc10_aic <- c()
auc10_aic2 <- c()

for (i in 1:100){
data <- sample(simulated, size = 10000)
index <- data$index
ors <- predict(model.AIC, newx = data, type = "response")[index] 
data <- mutate(data, 
mental = as.factor(rbinom(n = nrow(data), size = 1, prob = ors)))

x <- subset(x = data, select = -c(mental, orig.id, index))
#x <- mutate(x, fold = rep(1:10, 10))
x <- data.matrix(x)
y <- data.matrix(data$mental)

fit.lasso <- cv.glmnet(x, as.factor(y), family = "binomial", type.measure = "auc", alpha = 1)
fit.ridge <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", alpha = 0)
binom <- glm(mental ~ ., data = subset(data, select = - c(orig.id, index)), family = "binomial")
step <- stepAIC(binom, direction= "both", trace = FALSE)
fit.AIC <- glm(step$formula, family = "binomial", 
data = data)
trial <- mutate(data,
y.lasso = as.numeric(predict(fit.lasso, newx = x, type = "class")), 
y.ridge = as.numeric(predict(fit.ridge, newx = x, type = "class")), 
y.aic = ifelse(predict(fit.AIC, newx = x, type = "response") < .5, 
yes = 0, no = 1),
y.aic2 = as.numeric(rbinom(n = nrow(data), size = 1, 
prob = predict(fit.AIC, newx = x, type = "response"))))
auc10_lasso[i] <- auc(roc(predictor = trial$y.lasso, response = data$mental))
auc10_ridge[i] <- auc(roc(predictor = trial$y.ridge, response = data$mental))
auc10_aic[i] <- auc(roc(predictor = trial$y.aic, response = data$mental))
auc10_aic2[i] <- auc(roc(predictor = trial$y.aic2, response = data$mental))
}


auc1000_lasso <- c()
auc1000_ridge <- c()
auc1000_aic <- c()
auc1000_aic2 <- c()

for (i in 1:100){
data <- sample(simulated, size = 100000)
index <- data$index
ors <- predict(model.AIC, newx = data, type = "response")[index] 
data <- mutate(data, 
mental = as.factor(rbinom(n = nrow(data), size = 1, prob = ors)))

x <- subset(x = data, select = -c(mental, orig.id, index))
#x <- mutate(x, fold = rep(1:10, 10))
x <- data.matrix(x)
y <- data.matrix(data$mental)

fit.lasso <- cv.glmnet(x, as.factor(y), family = "binomial", type.measure = "auc", alpha = 1)
fit.ridge <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", alpha = 0)
binom <- glm(mental ~ ., data = subset(data, select = - c(orig.id, index)), family = "binomial")
step <- stepAIC(binom, direction= "both", trace = FALSE)
fit.AIC <- glm(step$formula, family = "binomial", 
data = data)
trial <- mutate(data,
y.lasso = as.numeric(predict(fit.lasso, newx = x, type = "class")), 
y.ridge = as.numeric(predict(fit.ridge, newx = x, type = "class")), 
y.aic = ifelse(predict(fit.AIC, newx = x, type = "response") < .5, 
yes = 0, no = 1),
y.aic2 = as.numeric(rbinom(n = nrow(data), size = 1, 
prob = predict(fit.AIC, newx = x, type = "response"))))
auc1000_lasso[i] <- auc(roc(predictor = trial$y.lasso, response = data$mental))
auc1000_ridge[i] <- auc(roc(predictor = trial$y.ridge, response = data$mental))
auc1000_aic[i] <- auc(roc(predictor = trial$y.aic, response = data$mental))
auc1000_aic2[i] <- auc(roc(predictor = trial$y.aic2, response = data$mental))
}
save(auc.test.aic, auc.test.lasso, auc.test.ridge, auc10_ridge, auc10_aic, auc10_lasso, 
auc1000_aic, auc1000_lasso, auc1000_ridge, coefficients, lasso, ridge, file = "coco2.Rda")



