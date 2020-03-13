install.packages("grf")
install.packages("https://raw.github.com/grf-labs/grf/master/releases/grf_0.10.2.tar.gz", repos = NULL, type = "source")
library(data.table)
library(dplyr)
causal_forest<- fread("customerdata(finalreport).csv")



install.packages("devtools")
library(devtools) 
install_github("susanathey/causalTree")
install.packages("onehot")
library(onehot)



survey <- causal_forest[,c(2,3,6,8,11:78)]

## making random covariate

survey$ran <- sample(1000, size = nrow(survey), replace = TRUE)

control.treat1 <-survey%>% mutate(c.treat = ifelse(coupon_type_num>=1,1,0))
count(control.treat1$c.treat)

n <- nrow(control.treat1)

count(control.treat1,group)
control.treat1 <- control.treat1[c('ran','group','d_purchase17','c.treat','tenure','android','gender','age','history_purchase_count','d_his_coupon_use','d_his_pro','his_session','VisitMypage','WriteQnA','MakePurchaseDecision','VisitExhibitionTab','WriteReview','SeeProductDetailPage','RemoveProductFavorite','AddProductToFavorite','SearchInStore','RequestRefund','DownloadCoupon','Order','CancelOrder')]
control.treat1.A <-control.treat1 %>% filter(group == 'B_FavoriteGroup') 
write.csv(control.treat1,"causal_forest_my.csv")
encoder <-(onehot(control.treat1.A, stringsAsFactors=TRUE))
control.treat1.A <- predict(encoder, control.treat1.A)

control.treat1<- as.data.frame(control.treat1.A)

trIdx <- which(control.treat1$c.treat == 1)
conIdx <- which(control.treat1$c.treat == 0)
set.seed(12)

train_idx <- c(sample(trIdx, length(trIdx)/2),
               sample(conIdx, length(conIdx)/2)
)
train_data <- control.treat1[train_idx,]
est_data <- control.treat1[-train_idx,]

########################coupon use ####
X <- train_data[c('ran','tenure','android','gender','age','history_purchase_count','d_his_coupon_use','d_his_pro','his_session','VisitMypage','WriteQnA','MakePurchaseDecision','VisitExhibitionTab','WriteReview','SeeProductDetailPage','RemoveProductFavorite','AddProductToFavorite','SearchInStore','RequestRefund','DownloadCoupon','Order','CancelOrder')]
Y <- train_data[c('d_purchase17')]
W <- train_data[c('c.treat')]


library(grf)
X[is.na(X)] <- 0
Y[is.na(Y)] <- 0
W[is.na(W)] <- 0
X<- as.vector(X)
Y<- as.vector(Y)
W<- as.vector(W)
Y<- as.matrix(Y)
W<- as.matrix(W)



c.forest <- causal_forest(X,Y,W)

# Predict using the forest.

c.pred = predict(c.forest)
c.forest = causal_forest(X,Y,W, num.trees = 1000)

est_data<- est_data[c('ran','tenure','android','gender','age','history_purchase_count','d_his_coupon_use','d_his_pro','his_session','VisitMypage','WriteQnA','MakePurchaseDecision','VisitExhibitionTab','WriteReview','SeeProductDetailPage','RemoveProductFavorite','AddProductToFavorite','SearchInStore','RequestRefund','DownloadCoupon','Order','CancelOrder')]
c.pred = predict(c.forest, est_data, estimate.variance = TRUE)


forest.W = regression_forest(X,W, tune.parameters = TRUE)
W.hat = predict(forest.W)$predictions
forest.Y = regression_forest(X,Y, tune.parameters = TRUE)
Y.hat = predict(forest.Y)$predictions
forest.Y.varimp = variable_importance(forest.Y)
forest.Y.varimp
set.seed(4)
tau.forest = causal_forest(X, Y,W,
                           W.hat = W.hat, Y.hat = Y.hat,
                           tune.parameters = TRUE)

tau.hat = predict(tau.forest)$predictions

tau.hat2 = predict(tau.forest, est_data, estimate.variance = TRUE)
sigma.hat = sqrt(tau.hat2$variance.estimates)

test_calibration(tau.forest)

est_data <- est_data %>% cbind(as.data.frame(tau.hat2))

print(tau.forest, decay.exponent = 2, max.depth = 3)
split_frequencies(tau.forest, max.depth = 3)
variable_importance <- as.data.frame(variable_importance(tau.forest, decay.exponent = 2, max.depth = 3))

write.csv(variable_importance, "variable_importance.csv")
write.csv(est_data,"est_data.csv")

est_data<- est_data[c('d_purchase17','c.treat','ran','tenure','android','gender','age','history_purchase_count','d_his_coupon_use','d_his_pro','his_session','VisitMypage','WriteQnA','MakePurchaseDecision','VisitExhibitionTab','WriteReview','SeeProductDetailPage','RemoveProductFavorite','AddProductToFavorite','SearchInStore','RequestRefund','DownloadCoupon','Order','CancelOrder')]
colnames(est_data)[4] <- 'A_CART'
colnames(est_data)[5] <- 'B_FA'
colnames(est_data)[6] <- 'C_S'
colnames(est_data)[7] <- 'D_N'

colnames(train_data)[2] <- 'A_CART'
colnames(train_data)[3] <- 'B_FA'
colnames(train_data)[4] <- 'C_S'
colnames(train_data)[5] <- 'D_N'

summary(est_data$tenure)
est_data <- est_data %>% mutate(tenure = ifelse(tenure <30, '<30',ifelse(tenure<40,'<40','>=40')))
train_data <-train_data %>% mutate(tenure = ifelse(tenure <30, '<30',ifelse(tenure<40,'<40','>=40')))

est_data <- est_data %>% mutate(age = ifelse(age <20, '<20',ifelse(age<30,'<30',ifelse(age<40,'<40','>=40'))))
train_data <-train_data %>% mutate(age = ifelse(age <20, '<20',ifelse(age<30,'<30',ifelse(age<40,'<40','>=40'))))


set.seed(1)

colnames(est_data)[17] <- 'product_detail'
colnames(train_data)[17] <- 'product_detail'

honestTree <- honest.causalTree(d_purchase17 ~product_detail+AddProductToFavorite+RemoveProductFavorite+ history_purchase_count+VisitMypage+ tenure+age+his_session+product_detail+d_his_coupon_use+d_his_pro,data=train_data, treatment=train_data$c.treat,est_data = est_data, est_treatment = est_data$c.treat,split.Rule = "CT",split.Honest = T,HonestSampleSize = nrow(est_data),split.Bucket = T, cv.option = "CT",cv.Honest = T)
rpart.plot(honestTree,uniform=TRUE, cex = 0.711)
opcp <- honestTree$cptable[,1][which.min(honestTree$cptable[,4])]
opTree <- prune(honestTree,cp = opcp, max.depth = 4)
rpart.plot(opTree, cex = 0.711)



#############################

treat1.treat2 <- survey %>% filter(group!=1)
treat1.treat2 <- treat1.treat2 %>% mutate(feedback.treat = ifelse(group==3,1,0))
count(treat1.treat2,feedback.treat)

n <- nrow(treat1.treat2)
trIdx <- which(treat1.treat2$feedback.treat == 1)
conIdx <- which(treat1.treat2$feedback.treat == 0)
set.seed(1)
train_idx <- c(sample(trIdx, length(trIdx)/2),
               sample(conIdx, length(conIdx)/2)
               )
train_data <- treat1.treat2[train_idx,]
est_data <-treat1.treat2[-train_idx,]

########################

########################
X <- train_data[,c(3:32)]
Y <- train_data[,c(2)]
W <- train_data[,c(33)]

c.forest <- causal_forest(train_data[,c(3:32)],train_data[,c(2)],train_data[,c(33)])
# Predict using the forest.
c.pred = predict(c.forest,est_data )
c.pred = predict(c.forest)
c.forest = causal_forest(train_data[,c(3:32)],train_data[,c(2)],train_data[,c(33)], num.trees = 7000)

c.pred = predict(c.forest, est_data, estimate.variance = TRUE)

forest.W = regression_forest(train_data[,c(3:32)],train_data[,c(33)], tune.parameters = TRUE)

W.hat = predict(forest.W)$predictions
forest.Y = regression_forest(train_data[,c(3:32)],train_data[,c(2)], tune.parameters = TRUE)
Y.hat = predict(forest.Y)$predictions
forest.Y.varimp = variable_importance(forest.Y)
forest.Y.varimp
write.csv(forest.Y.varimp, "varimp_feedbackeffect.csv")
set.seed(1)
tau.forest = causal_forest(X, train_data[,c(2)], train_data[,c(33)],
                           W.hat = W.hat, Y.hat = Y.hat,
                           tune.parameters = TRUE)

tau.hat = predict(tau.forest)$predictions

tau.hat2 = predict(tau.forest, est_data, estimate.variance = TRUE)
sigma.hat = sqrt(tau.hat2$variance.estimates)

test_calibration(tau.forest)

est_data <- est_data %>% cbind(as.data.frame(tau.hat2))

print(tau.forest, decay.exponent = 2, max.depth = 3)
split_frequencies(tau.forest, max.depth = 3)
variable_importance <- as.data.frame(variable_importance(tau.forest, decay.exponent = 2, max.depth = 3))

write.csv(variable_importance, "variable_importance_feedbackeffect_02.csv")
write.csv(est_data,"est_data_feedbackeffect02.csv")


#####################

control.treat2 <- survey %>% filter(group!=2)
control.treat2 <- control.treat2 %>% mutate(gfeedback.treat = ifelse(group==3,1,0))
count(control.treat2,gfeedback.treat)

n <- nrow(control.treat2)
trIdx <- which(control.treat2$gfeedback.treat == 1)
conIdx <- which(control.treat2$gfeedback.treat == 0)
set.seed(1)
train_idx <- c(sample(trIdx, length(trIdx)/2),
               sample(conIdx, length(conIdx)/2)
)
train_data <- control.treat2[train_idx,]
est_data <-control.treat2[-train_idx,]


######################

########################
X <- train_data[,c(3:32)]
Y <- train_data[,c(2)]
W <- train_data[,c(33)]

c.forest <- causal_forest(train_data[,c(3:32)],train_data[,c(2)],train_data[,c(33)])
# Predict using the forest.
c.pred = predict(c.forest,est_data )
c.pred = predict(c.forest)
c.forest = causal_forest(train_data[,c(3:32)],train_data[,c(2)],train_data[,c(33)], num.trees = 7000)

c.pred = predict(c.forest, est_data, estimate.variance = TRUE)

forest.W = regression_forest(train_data[,c(3:32)],train_data[,c(33)], tune.parameters = TRUE)

W.hat = predict(forest.W)$predictions
forest.Y = regression_forest(train_data[,c(3:32)],train_data[,c(2)], tune.parameters = TRUE)
Y.hat = predict(forest.Y)$predictions
forest.Y.varimp = variable_importance(forest.Y)
forest.Y.varimp
write.csv(forest.Y.varimp, "varimp_gfeedbackeffect.csv")
set.seed(1)
tau.forest = causal_forest(X, train_data[,c(2)], train_data[,c(33)],
                           W.hat = W.hat, Y.hat = Y.hat,
                           tune.parameters = TRUE)

tau.hat = predict(tau.forest)$predictions

tau.hat2 = predict(tau.forest, est_data, estimate.variance = TRUE)
sigma.hat = sqrt(tau.hat2$variance.estimates)

test_calibration(tau.forest)

est_data <- est_data %>% cbind(as.data.frame(tau.hat2))

print(tau.forest, decay.exponent = 2, max.depth = 3)
split_frequencies(tau.forest, max.depth = 3)
variable_importance <- as.data.frame(variable_importance(tau.forest, decay.exponent = 2, max.depth = 3))

write.csv(variable_importance, "variable_importance_gfeedbackeffect.csv")
write.csv(est_data,"est_data_gfeedbackeffect.csv")










##################forest

library(grf)


c.forest <- causal_forest(train_data[,c(3:49)],train_data[,c(2)],train_data[,c(50)])
# Predict using the forest.
c.pred = predict(c.forest,est_data )
c.pred = predict(c.forest)
c.forest = causal_forest(train_data[,c(3:49)],train_data[,c(2)],train_data[,c(50)], num.trees = 4000)
c.pred = predict(c.forest, est_data, estimate.variance = TRUE)

forest.W = regression_forest(train_data[,c(3:49)],train_data[,c(50)], tune.parameters = TRUE)

W.hat = predict(forest.W)$predictions
forest.Y = regression_forest(train_data[,c(3:49)],train_data[,c(2)], tune.parameters = TRUE)
Y.hat = predict(forest.Y)$predictions
forest.Y.varimp = variable_importance(forest.Y)
forest.Y.varimp

selected.vars = which(forest.Y.varimp / mean(forest.Y.varimp) > 0.03)
tau.forest = causal_forest(train_data[,selected.vars], train_data[,c(2)], train_data[,c(50)],
                           W.hat = W.hat, Y.hat = Y.hat,
                           tune.parameters = TRUE)
tau.hat = predict(tau.forest)$predictions

tau.hat2 = predict(tau.forest, est_data, estimate.variance = TRUE)
sigma.hat = sqrt(tau.hat2$variance.estimates)

test_calibration(tau.forest)

est_data <- est_data %>% cbind(as.data.frame(tau.hat))

print(tau.forest, decay.exponent = 2, max.depth = 3)
split_frequencies(tau.forest, max.depth = 3)
variable_importance(tau.forest, decay.exponent = 2, max.depth = 3)


