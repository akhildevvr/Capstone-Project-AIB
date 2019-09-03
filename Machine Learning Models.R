#Splitting the dataset into training and test dataset
library(caret)
training1 <- createDataPartition(RPZdata$Price, p=0.60,list=FALSE)
train<-RPZdata[training1,]
test<-RPZdata[-training1,]




library(h2o)

#Initiliasing the package H2O
h2o.init()

#Converting train dataset into H2o dataset 
train1<-as.h2o(train)
model1<-h2o.glm(y = "Price", x = c( "No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude", "dateyear","month","day"), training_frame = train1, family = "gaussian",
                nfolds = 5, alpha = 0.1, lambda_search = FALSE)
#Converting test dataset into H2o dataset 
test1<-as.h2o(test)

#Model performance on test dataset
perf1<-h2o.performance(model1,newdata=test1)
perf2<-h2o.performance(model1,newdata=test1)

#Predictio on the test dataset
new1<- as.data.frame(h2o.predict(object=model1,newdata=test1))

#Finding correlation between predicted values and actual values
cor(new1, test$Price)

#Adding predicted values to test dataset
test$LR_Pred<-new1$predict

#Finding variable importance
v<-h2o.varimp(model1)

#Variable importance plot
h2o.varimp_plot(model1)


#Plot of actual vs predicted values over time
p<-ggplot(data = test, aes(x = Date.of.Purchase))+
  geom_smooth(aes( y = Price_Log,color = "Actual"), size = 0.5) + geom_smooth(aes(y= new4$predict,color = "Predicted")) +
  ggtitle("Neural Net") + labs(y = "Price", x = "Date of Purchase")

#Gradient Boosting Machine
model2<-h2o.gbm(y= "Price", x = c( "No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude","Rent.Pressure.Zones", "dateyear","month","day"), training_frame = train1, ntrees = 10,
                max_depth = 3,min_rows = 2, learn_rate = 0.01,nfolds = 5,sample_rate = 0.6,
                distribution= "gaussian")
h2o.performance(model2,newdata=test1)
new2<- as.data.frame(h2o.predict(object=model2,newdata=test1))
cor(new2, test$Price)
test$GBM_Pred<-new2$predict
v<-h2o.varimp(model2)
h2o.varimp_plot(model2)


#Variable Importance Plots
ggplot(imp1, aes(x=reorder(variable, percentage), weight=percentage,fill = percentage )) + 
  geom_bar() +
  ylab("Percentage") +
  xlab("Variable Name")

ggplot(imp2, aes(x=reorder(variable, percentage), weight=percentage,fill = percentage )) + 
  geom_bar() +
  ylab("Percentage") +
  xlab("Variable Name")

ggplot(imp3, aes(x=reorder(variable, percentage), weight=percentage,fill = percentage )) + 
  geom_bar() +
  ylab("Percentage") +
  xlab("Variable Name")

#Random Forest Regression
model3 <- h2o.randomForest(y= "Price", x = c( "No.of.Bedrooms","Number.of.Bathrooms", "County","latitude","longitude","Rent.Pressure.Zones","month","dateyear","day"), #targets
                           training_frame = train1, #training data
                           #validation data 
                           ntrees = 100, #default trees is 50
                           max_depth = 20, #default is 20,
                           score_each_iteration = TRUE,## Predict against training and validation for each tree
                           fold_assignment = 'Random', #startified sampling
                           nfolds=2,
                           balance_classes = TRUE
                           #balance class
)

test1<-as.h2o(test)
h2o.performance(model3, newdata=test1)
new3<- as.data.frame(h2o.predict(object=model3,newdata=test1))
cor(new3, test$Price)
test$RF_Pred<-new3$predict
v<-h2o.varimp(model3)
h2o.varimp_plot(model3)



#Neural Network
n_model <- h2o.deeplearning( # Neural Network Model
  model_id="dl_model_nn",     # Model Name
  training_frame=train1,       # training data
  #validation_frame = valid,                            # validation data 
  y= "Price", x = c("No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude","Rent.Pressure.Zones","month","dateyear","day"),                   # dependent variable
  hidden=c(30),               # No of hidden layers and hidden nodes
  epochs=10,
  variable_importances = TRUE,
  score_validation_samples=10000,
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  max_w2=10                       # number of runs
)

nn_train_perf<-h2o.performance(h2o.getModel('dl_model_nn'))
# Validation data performance
nn_vali_perf<-n_model@model$validation_metrics 
# Test data performance
nn_test_perf<-h2o.performance(h2o.getModel('dl_model_nn'), newdata = test1)
# Model Summary
nn_sum<-summary(n_model) 
# Model prediction
nn_pred<-h2o.predict(n_model, test1)
new4<-as.data.frame(nn_pred)
cor(new4, test$Price)
test$NN_Pred<-new4$predict
v<-h2o.varimp(n_model)
h2o.varimp_plot(n_model)

#Neural Network Hyperparameter tuning using Grid Search
hidden_opt <- list(c(32,32), c(32,16,8), c(100)) 
l1_opt <- c(1e-4, 1e-3) 
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt) 
Model_Grid1 <- h2o.grid("deeplearning", grid_id = "mygrid",training_frame = train1, y= "Price", x = c( "Date.of.Purchase", "No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude","Rent.Pressure.Zones"),
                        hyper_params = hyper_params)
summary(Model_Grid1)
model_ids <- Model_Grid1@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})
nn<-h2o.getModel('mygrid_model_5')
nn_pred_1<-h2o.predict(nn, test1)
new5<-as.data.frame(nn_pred_1)
cor(new4, test$Price)

