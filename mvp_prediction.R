library(ggplot2)
library(foreach)
library(mosaic)
library(cluster)
library(tidyverse)
library(FNN)
library(glmnet)
library(knitr)

# import data
player_stats = read.csv('./data/nba_19-20_player_stats.csv', header=TRUE)
player_stats <- data.frame(player_stats)
player_stats <- player_stats[!(player_stats$Tm=="TOT"),]
player_stats <- player_stats[(player_stats$G>=25 & player_stats$MP>=1200),]


# rmse function
rmse = function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm=TRUE))
}

# variables that control how long the program takes to run
num_splits = 200
#k_limit = 20

#model 1: linear regression model (RMSE)
#80% training data, 20% test data
n = nrow(player_stats)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

#200 different random splits
lm_vals = do(num_splits)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = player_stats[train_cases, 6:27]
  on_test = player_stats[test_cases, 6:27]
  
  linearModel = lm(WS ~ (. - WS.48 - OWS - DWS), data=on_train)
  linearModelInteractions = lm(WS ~ (. - WS.48 - OWS - DWS)^2, data=on_train)
  
  # Predictions out of sample + convert to binary
  y_test = predict(linearModel, on_test)
  y_test_2 = predict(linearModelInteractions, on_test)
  
  c(rmse(y_test, on_test$WS),rmse(y_test_2, on_test$WS))
}
lm_avg = unname(colMeans(lm_vals))
lm_avg


"
#model 2: knn (RMSE)
k_vals = 2:k_limit
knn_vals = matrix(0, k_limit - 1, 2)

for (k_val in k_vals) {
  rmse_vals_iter = do(num_splits)*{
    
    # re-split into train and test cases with the same sample sizes
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    on_train = player_stats[train_cases, 6:27]
    on_test = player_stats[test_cases, 6:27]
    
    #create KNN model
    Xtrain_temp = model.matrix(WS ~ (. - WS.48 - OWS - DWS), data = on_train, na.action=na.pass)
    Xtest_temp = model.matrix(WS ~ (. - WS.48 - OWS - DWS), data = on_test, na.action=na.pass)
    
    ytrain = on_train$WS
    ytest = on_test$WS
    
    #standardize data
    scale_amount = apply(Xtrain_temp, 2, sd)
    Xtrain = scale(Xtrain_temp, scale=scale_amount)
    Xtest = scale(Xtest_temp, scale=scale_amount)
    
    #train k model
    knn_model = knn.reg(Xtrain, Xtest, ytrain, k=k_val)
    
    # Predictions out of sample + convert to binary
    yhat_test1_pred = knn_model$pred
    
    c(k_val,rmse(yhat_test1_pred, ytest))
  }
  rmse_vals_avg = colMeans(rmse_vals_iter)
  knn_vals[k_val - 1,] = rmse_vals_avg
}
knn_rmse = unname(knn_vals[which.min(knn_vals[,2]),])

knn_vals_rmse <- data.frame(knn_vals)
"
#ggplot(data=knn_vals_rmse) +
#  geom_line(aes(x = X1, y = X2), color='red') +
#  ggtitle("RMSE for Each Value of K") +
#  xlab("K") +
#  ylab("RMSE")


#model 3: lasso regression/ridge regression (RMSE)
vals_lr_rr = do(num_splits)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = player_stats[train_cases, 6:27]
  on_test = player_stats[test_cases, 6:27]
  
  temp_train = model.matrix.lm(WS ~ (. - WS.48 - OWS - DWS), data = on_train, na.action=na.pass)
  temp_test = model.matrix.lm(WS ~ (. - WS.48 - OWS - DWS), data = on_test, na.action=na.pass)
  x_train = temp_train[complete.cases(temp_train),]
  y_train = on_train$WS[complete.cases(temp_train)]
  x_test = temp_test[complete.cases(temp_test),]
  y_test = on_test$WS[complete.cases(temp_test)]
  
  # lasso regression
  cv_fit_l = cv.glmnet(x_train, y_train, family="gaussian", alpha = 1)
  # ridge regression
  cv_fit_r = cv.glmnet(x_train, y_train, family="gaussian", alpha = 0)
  
  opt_lambda_l = cv_fit_l$lambda.min
  opt_lambda_r = cv_fit_r$lambda.min
  
  y_pred_l = predict(cv_fit_l$glmnet.fit, s = opt_lambda_l, newx = x_test)
  y_pred_r = predict(cv_fit_r$glmnet.fit, s = opt_lambda_r, newx = x_test)
  
  c(rmse(y_pred_l, y_test), rmse(y_pred_r, y_test))
}
lr_model_avg = min(vals_lr_rr[,1])
rr_model_avg = min(vals_lr_rr[,2])
lr_model_avg
rr_model_avg


#model 4: logistic regression
vals_logm = do(num_splits)*{
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = player_stats[train_cases, 6:27]
  on_test = player_stats[test_cases, 6:27]
  
  logitModel = glm(WS ~ (. - WS.48 - OWS - DWS), data=on_train, family=gaussian, maxit = 100)
  logitModelInteractions = glm(WS ~ (. - WS.48 - OWS - DWS)^2, data=on_train, family=gaussian, maxit = 100)
  
  # Predictions out of sample + convert to binary
  y_test = predict(logitModel, on_test)
  y_test_2 = predict(logitModelInteractions, on_test)
  
  c(rmse(y_test, on_test$WS), rmse(y_test_2, on_test$WS))
}
logm_vals = unname(colMeans(vals_logm))





cat("MODEL SUCCESS:")
cat("1) LINEAR REGRESSION MODEL (without interactions) - RMSE:", lm_avg[1])
cat("1) LINEAR REGRESSION MODEL (with interactions) - RMSE:", lm_avg[2])
#cat("2) KNN ( k =",knn_rmse[1],") - RMSE:", knn_rmse[2])
cat("3) LASSO REGRESSION - RMSE:", lr_model_avg[1])
print("coefficients for lasso regression:")
print(coef(cv_fit_l$glmnet.fit,s = cv_fit_l$lambda.min))
cat("3) RIDGE REGRESSION - RMSE:", rr_model_avg[1])
#print("coefficients for ridge regression:")
#print(coef(cv_fit_r$glmnet.fit,s = cv_fit_r$lambda.min))
cat("5) LOGISTIC REGRESSION (without interactions) - RMSE:", logm_vals[1])
cat("5) LOGISTIC REGRESSION (with interactions) - RMSE:", logm_vals[2])





# Update player stats for end-of-season 2019-20

player_stats$AST.[player_stats$Pos=="PG"] <- player_stats$AST.[player_stats$Pos=="PG"] + 0.5
player_stats$AST.[player_stats$Pos=="SG"] <- player_stats$AST.[player_stats$Pos=="SG"] + 0.1
player_stats$AST.[player_stats$Pos=="SF"] <- player_stats$AST.[player_stats$Pos=="SF"] - 0.1
player_stats$AST.[player_stats$Pos=="PF"] <- player_stats$AST.[player_stats$Pos=="PF"] + 0.2
player_stats$AST.[player_stats$Pos=="C"] <- player_stats$AST.[player_stats$Pos=="C"] - 0.2

player_stats$ORB.[player_stats$Pos=="PG"] <- player_stats$ORB.[player_stats$Pos=="PG"] - 0.1
player_stats$ORB.[player_stats$Pos=="SG"] <- player_stats$ORB.[player_stats$Pos=="SG"] - 0.1
player_stats$ORB.[player_stats$Pos=="SF"] <- player_stats$ORB.[player_stats$Pos=="SF"] + 0.3
player_stats$ORB.[player_stats$Pos=="PF"] <- player_stats$ORB.[player_stats$Pos=="PF"] + 0.5
player_stats$ORB.[player_stats$Pos=="C"] <- player_stats$ORB.[player_stats$Pos=="C"] + 0.7

player_stats$DRB.[player_stats$Pos=="PG"] <- player_stats$DRB.[player_stats$Pos=="PG"] - 0.1
player_stats$DRB.[player_stats$Pos=="SG"] <- player_stats$DRB.[player_stats$Pos=="SG"] - 0.1
player_stats$DRB.[player_stats$Pos=="SF"] <- player_stats$DRB.[player_stats$Pos=="SF"] + 0.3
player_stats$DRB.[player_stats$Pos=="PF"] <- player_stats$DRB.[player_stats$Pos=="PF"] + 0.5
player_stats$DRB.[player_stats$Pos=="C"] <- player_stats$DRB.[player_stats$Pos=="C"] + 0.7

player_stats$TRB.[player_stats$Pos=="PG"] <- player_stats$TRB.[player_stats$Pos=="PG"] - 0.2
player_stats$TRB.[player_stats$Pos=="SG"] <- player_stats$TRB.[player_stats$Pos=="SG"] - 0.2
player_stats$TRB.[player_stats$Pos=="SF"] <- player_stats$TRB.[player_stats$Pos=="SF"] + 0.4
player_stats$TRB.[player_stats$Pos=="PF"] <- player_stats$TRB.[player_stats$Pos=="PF"] + 0.6
player_stats$TRB.[player_stats$Pos=="C"] <- player_stats$TRB.[player_stats$Pos=="C"] + 0.8

player_stats$BLK.[player_stats$Pos=="PG"] <- player_stats$BLK.[player_stats$Pos=="PG"] - 0.2
player_stats$BLK.[player_stats$Pos=="SG"] <- player_stats$BLK.[player_stats$Pos=="SG"] - 0.2
player_stats$BLK.[player_stats$Pos=="SF"] <- player_stats$BLK.[player_stats$Pos=="SF"] + 0.2
player_stats$BLK.[player_stats$Pos=="PF"] <- player_stats$BLK.[player_stats$Pos=="PF"] + 0.3
player_stats$BLK.[player_stats$Pos=="C"] <- player_stats$BLK.[player_stats$Pos=="C"] + 0.5

player_stats$TS.[player_stats$Pos=="PG"] <- player_stats$TS.[player_stats$Pos=="PG"] + 0.01
player_stats$TS.[player_stats$Pos=="SG"] <- player_stats$TS.[player_stats$Pos=="SG"] + 0.02
player_stats$TS.[player_stats$Pos=="SF"] <- player_stats$TS.[player_stats$Pos=="SF"] + 0.02
player_stats$TS.[player_stats$Pos=="PF"] <- player_stats$TS.[player_stats$Pos=="PF"] + 0.01
player_stats$TS.[player_stats$Pos=="C"] <- player_stats$TS.[player_stats$Pos=="C"] + 0.02


player_stats$WS[player_stats$Tm=="MIL"] <- player_stats$WS[player_stats$Tm=="MIL"] + 0.4
player_stats$WS[player_stats$Tm=="LAL"] <- player_stats$WS[player_stats$Tm=="LAL"] + 0.4
player_stats$WS[player_stats$Tm=="TOR"] <- player_stats$WS[player_stats$Tm=="TOR"] + 0.3
player_stats$WS[player_stats$Tm=="LAC"] <- player_stats$WS[player_stats$Tm=="LAC"] + 0.25
player_stats$WS[player_stats$Tm=="OKC"] <- player_stats$WS[player_stats$Tm=="OKC"] + 0.2


player_stats$WS[player_stats$USG.>=25.0] <- player_stats$WS[player_stats$USG.>=25.0] + 1.5
player_stats$WS.48[player_stats$USG.>=25.0] <- player_stats$WS.48[player_stats$USG.>=25.0] + 0.015



# Re-sample train/test splits
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
on_train = player_stats[train_cases, 6:27]
on_test = player_stats[test_cases, 6:27]



logitModelWS = glm(WS ~ (. - WS.48 - OWS - DWS), data=on_train, family=gaussian, maxit = 100)

predicted_WS = predict(logitModelWS, player_stats[,6:27])

predictions_WS <- data.frame(player_stats$Player, predicted_WS)
predictions_WS <- predictions_WS %>%
  arrange(-predicted_WS)
head(predictions_WS)

names(predictions_WS)[names(predictions_WS) == "player_stats.Player"] <- "Player"
names(predictions_WS)[names(predictions_WS) == "predicted_WS"] <- "Predicted WS"

knitr::kable(predictions_WS[1:5,], caption = "Predicted WS Leaders at the End of the Regular Season")

ggplot() +
  geom_point(data=player_stats[,6:27],aes(x = WS, y = predicted_WS ), color="black") + 
  geom_line(data=player_stats[,6:27],aes(x = WS, y = WS ), color="red") + 
  xlab("Actual WS") + 
  ylab("Predicted WS") + 
  ggtitle("Actual WS vs. Predicted WS")




logitModelVORP = glm(VORP ~ (. - WS - WS.48 - OWS - DWS), data=on_train, family=gaussian, maxit = 100)

predicted_VORP = predict(logitModelVORP, player_stats[,6:27])

predictions_VORP <- data.frame(player_stats$Player, predicted_VORP)
predictions_VORP <- predictions_VORP %>%
  arrange(-predicted_VORP)
head(predictions_VORP)

names(predictions_VORP)[names(predictions_VORP) == "player_stats.Player"] <- "Player"
names(predictions_VORP)[names(predictions_VORP) == "predicted_VORP"] <- "Predicted VORP"

knitr::kable(predictions_VORP[1:5,], caption = "Predicted VORP Leaders at the End of the Regular Season")

ggplot() +
  geom_point(data=player_stats[,6:27],aes(x = VORP, y = predicted_VORP ), color="black") + 
  geom_line(data=player_stats[,6:27],aes(x = VORP, y = VORP ), color="red") + 
  xlab("Actual VORP") + 
  ylab("Predicted VORP") + 
  ggtitle("Actual VORP vs. Predicted VORP")




logitModelPER = glm(PER ~ (. - WS - WS.48 - OWS - DWS), data=on_train, family=gaussian, maxit = 100)

predicted_PER = predict(logitModelPER, player_stats[,6:27])

predictions_PER <- data.frame(player_stats$Player, predicted_PER)
predictions_PER <- predictions_PER %>%
  arrange(-predicted_PER)
head(predictions_PER)

names(predictions_PER)[names(predictions_PER) == "player_stats.Player"] <- "Player"
names(predictions_PER)[names(predictions_PER) == "predicted_PER"] <- "Predicted PER"

knitr::kable(predictions_PER[1:5,], caption = "Predicted PER Leaders at the End of the Regular Season")

ggplot() +
  geom_point(data=player_stats[,6:27],aes(x = PER, y = predicted_PER ), color="black") + 
  geom_line(data=player_stats[,6:27],aes(x = PER, y = PER ), color="red") + 
  xlab("Actual PER") + 
  ylab("Predicted PER") + 
  ggtitle("Actual PER vs. Predicted PER")





logitModelUSG = glm(USG. ~ (. - WS - WS.48 - OWS - DWS), data=on_train, family=gaussian, maxit = 100)

predicted_USG = predict(logitModelUSG, player_stats[,6:27])

predictions_USG <- data.frame(player_stats$Player, predicted_USG)
predictions_USG <- predictions_USG %>%
  arrange(-predicted_USG)
head(predictions_USG)

names(predictions_USG)[names(predictions_USG) == "player_stats.Player"] <- "Player"
names(predictions_USG)[names(predictions_USG) == "predicted_USG"] <- "Predicted USG%"

knitr::kable(predictions_USG[1:5,], caption = "Predicted USG% Leaders at the End of the Regular Season")

ggplot() +
  geom_point(data=player_stats[,6:27],aes(x = USG., y = predicted_USG ), color="black") + 
  geom_line(data=player_stats[,6:27],aes(x = USG., y = USG. ), color="red") + 
  xlab("Actual USG%") + 
  ylab("Predicted USG%") + 
  ggtitle("Actual USG% vs. Predicted USG%")





logitModelOBPM = glm(OBPM ~ (. - WS - WS.48 - OWS - DWS - BPM), data=on_train, family=gaussian, maxit = 100)

predicted_OBPM = predict(logitModelOBPM, player_stats[,6:27])

predictions_OBPM <- data.frame(player_stats$Player, predicted_OBPM)
predictions_OBPM <- predictions_OBPM %>%
  arrange(-predicted_OBPM)
head(predictions_OBPM)

names(predictions_OBPM)[names(predictions_OBPM) == "player_stats.Player"] <- "Player"
names(predictions_OBPM)[names(predictions_OBPM) == "predicted_OBPM"] <- "Predicted OBPM"

knitr::kable(predictions_OBPM[1:5,], caption = "Predicted OBPM Leaders at the End of the Regular Season")

ggplot() +
  geom_point(data=player_stats[,6:27],aes(x = OBPM, y = predicted_OBPM ), color="black") + 
  geom_line(data=player_stats[,6:27],aes(x = OBPM, y = OBPM ), color="red") + 
  xlab("Actual OBPM") + 
  ylab("Predicted OBPM") + 
  ggtitle("Actual OBPM vs. Predicted OBPM")





logitModelDBPM = glm(DBPM ~ (. - WS - WS.48 - OWS - DWS - BPM), data=on_train, family=gaussian, maxit = 100)

predicted_DBPM = predict(logitModelDBPM, player_stats[,6:27])

predictions_DBPM <- data.frame(player_stats$Player, predicted_DBPM)
predictions_DBPM <- predictions_DBPM %>%
  arrange(-predicted_DBPM)
head(predictions_DBPM)

names(predictions_DBPM)[names(predictions_DBPM) == "player_stats.Player"] <- "Player"
names(predictions_DBPM)[names(predictions_DBPM) == "predicted_DBPM"] <- "Predicted DBPM"

knitr::kable(predictions_DBPM[1:5,], caption = "Predicted DBPM Leaders at the End of the Regular Season")

ggplot() +
  geom_point(data=player_stats[,6:27],aes(x = DBPM, y = predicted_DBPM ), color="black") + 
  geom_line(data=player_stats[,6:27],aes(x = DBPM, y = DBPM ), color="red") + 
  xlab("Actual DBPM") + 
  ylab("Predicted DBPM") + 
  ggtitle("Actual DBPM vs. Predicted DBPM")



recorded_stats <- data.frame(colnames(player_stats[,6:27]))
names(recorded_stats)[1] <- "Statistics"
recorded_stats["Meaning"] <- c("Num. games played", 
                               "Num. minutes played", 
                               "Measure of per-minute production", 
                               "Overall shooting efficiency", 
                               "% of field goal attempts from 3-point range", 
                               "Num. free throw attempts per field goal attempt", 
                               "% of available offensive rebounds that a player grabbed", 
                               "% of available defensive rebounds that a player grabbed", 
                               "% of available total rebounds that a player grabbed", 
                               "% of teammate field goals that a player assisted", 
                               "% of opponent possessions that were stolen by a player", 
                               "% of opponent field goals attempts that were blocked by a player", 
                               "Num. turnovers committed per 100 plays", 
                               "% of team plays used by a player", 
                               "Num. wins contributed by a player from his offense", 
                               "Num. wins contributed by a player from his defense", 
                               "Num. wins contributed by a player", 
                               "Num. wins contributed by a player per 48 minutes", 
                               "Offensive points per 100 possessions above a league-average player", 
                               "Defensive points per 100 possessions above a league-average player", 
                               "Total points per 100 possessions above a league-average player", 
                               "Points per 100 team possessions contributed by a player above a replacement-level player")
knitr::kable(recorded_stats)

