#pre processing data #displays structures of R objects = class
hh<-df
which(apply(hh[,1:12],2,var)==0)
hh[,1:12]<- hh[,1:12][,which(apply(hh[,1:12],2,var)!=0)]
sum(is.na(hh))
sapply(hh, class)
# Select only character columns
char_cols <- sapply(hh, is.character)
# Convert character columns to numeric
hh[, char_cols] <- apply(hh[, char_cols], 2, function(x) as.numeric(as.character(x)))
hh$Label<-factor(Label)
hh$Label <- factor(hh$Label)
str(hh)	
row.names(hh) <- samplename
#check if there are missing data? or the class of hh
sum(is.na(hh))
sapply(hh, class)
hh2<-hh[,c(5,7,8,9,13)]
#create training_indices
training_indices <- createDataPartition(y = hh$Label, p = 0.2, list = FALSE)
set.seed(123)
hTrain <- hh[training_indices,]
hTest <- hh[-training_indices,]
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                        summaryFunction = multiClassSummary, classProbs = TRUE, savePredictions = TRUE)
metric <- "Accuracy"
#Random Forest (RF)
set.seed(123)
fit.rf <- train(x=hTrain[,1:12], y=hTrain$Label, method = "rf", metric = metric, trControl = control,
                preProcess = c("center", "scale"))
#Artificial neuronal network (ANN)
set.seed(123)
fit.nnet <- train(x=hTrain[,1:12], y=hTrain$Label, method = "nnet", metric = metric,
                  trControl = control, preProcess = c("center", "scale"))
#Linear discriminant analysis (LDA)
set.seed(123)
fit.lda <- train(x=hTrain[,1:12], y=hTrain$Label, method = "lda", metric = metric, trControl =
                   control, preProcess = c("center", "scale"))
#SVM
set.seed(123)
fit.svm_Lin <- train(x=hTrain24[,1:12], y=hTrain24$Lable, method = "svmLinear", metric = metric,
trControl = control, preProcess = c("center", "scale"))
#Summarize ML performance
results <- resamples(list(lda=fit.lda, rf=fit.rf, nnet=fit.nnet))
summary(results24)
#Plot comparison of models
scales <- list(x=list(relation="free"), y=list(relation="free"))
png(filename = "performance.png", width = 20, height = 20, units = "cm", res = 300)
dotplot(results, scales=scales, par.strip.text=list(cex=0.76), par.settings = list(par.xlab.text = list(cex = 0)))
dev.off()
#Predicting outcome for testing data set
predictions <- predict(fit.nnet, hTest[,1:12])
#Compare predicted outcome and true outcome
confusionMatrix(predictions, hTest$Label)
#Plot important features 
gbmImp <- varImp(fit.nnet, scale = T)
png(filename = "ANN_F.png", width = 11, height = 16, units = "cm", res = 600)
ggplot(gbmImp, aes(x = variable, y = importance)) +
  geom_col() +
  ggtitle("ANN_F") +
  xlab("features") +
  ylab("Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")
dev.off()
#feature selection
set.seed(123)
ctrl <- nnetControl(method = "cv", number = 10)
rfProfile <- nnet(x = hTrain[,1:12], y = hTrain$Label, sizes = c(1:12), nnetControl = ctrl, method = "nnet")
print(rfProfile)
rfProfile[["bestSubset"]]
#optimizing data_set based on nnet for runing machine
df <- df[, c(rfProfile[["24Variables"]])]
df<- cbind(df,Label)
df<- df[,-6]
