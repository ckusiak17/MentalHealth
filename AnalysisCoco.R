require(glmnet)
require(MASS)
require(pROC)
require(mosaic)
load("mental_final.Rda")


ment <- subset(mhfp, select = - c(trans, sexorient))
ment <- ment[complete.cases(ment), ]
x <- subset(x = ment, select = -mental)

x<-as.matrix(x)
y<-as.matrix(ment$mental)

#LASSO
lasso <- glmnet(x, y, family = "binomial", alpha = 1)
plot(lasso, xvar = "dev")


cv.lasso <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", alpha = 1)
plot(cv.lasso)
coef(cv.lasso)
preds.lasso <- as.numeric(predict(cv.lasso, newx = x, type = "class"))
tally(~preds.lasso)

#RIDGE
ridge <- glmnet(x, y, family = "binomial", alpha=0)
plot(ridge, xvar = "dev")


cv.ridge <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", alpha = 0)
plot(cv.ridge)
coef(cv.ridge)
preds.ridge <- as.numeric(predict(cv.ridge, newx = x, type = "class"))
tally(~preds.ridge)

#StepAIC
ment <- mutate(ment, mental = as.factor(mental))
binom <- glm(mental ~ ., data = ment, family = "binomial")
stepAIC(binom, direction= "both")
model.AIC <- glm(mental ~ state + sleep + sex + employ + actlimit + 
                   metro + exer30 + race + income + smoker + binge, family = "binomial", 
                 data = ment)
preds.AIC <- predict(model.AIC, newx = x, type = "response")


#Model Comparisons
ment <- mutate(ment, y.lasso = preds.lasso, y.ridge = preds.ridge)
rmse <- function(truths, predictions){
  error <- truths - predictions
  return(sqrt(mean(error^2)))
}
rmse.lasso <- rmse(truths = y, predictions = ment$y.lasso)
rmse.ridge <- rmse(truths = y, predictions = ment$y.ridge)
rmse.lasso
rmse.ridge

roc.lasso <- roc(predictor = ment$y.lasso, response = ment$mental)
roc.ridge <- roc(predictor = ment$y.ridge, response = ment$mental)
roc.aic <- roc(predictor = preds.AIC, response = ment$mental)
auc.lasso <- auc(roc.lasso)
auc.ridge <- auc(roc.ridge)
auc.aic <- auc(roc.aic)
paste0("Lasso has an AUC of ", round(auc.lasso,4))
paste0("Ridge has an AUC of ", round(auc.ridge, 4))
paste0("AIC has an AUC of ", round(auc.aic, 4))

#LASSO seems like the better of the two options
testing <- subset(ment, select = -c(y.ridge, mental, y.lasso))
testing <- mutate(testing, y = as.numeric(predict(cv.lasso, newx = x, type = "class")))

test.x<-as.matrix(subset(testing, select = - y))
test.y<-as.matrix(testing$y)


#Can LASSO find the true model?
cv.lasso.test <- cv.glmnet(test.x, test.y, family = "binomial", type.measure = "auc", alpha = 1)
plot(cv.lasso.test)
coef(cv.lasso.test)

#Can Ridge find the true model?
cv.ridge.test <- cv.glmnet(test.x, test.y, family = "binomial", type.measure = "auc", alpha = 0)
plot(cv.ridge.test)
coef(cv.ridge.test)

#Can stepAIC find the true model?
