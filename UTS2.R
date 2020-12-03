#UTS 
# ubah format file menjadi csv
TAE.data <- read.csv(file.choose(), header = F)
View(TAE.data)
head(TAE.data)

# ubah column names
colnames(TAE.data)
names(TAE.data)[names(TAE.data) == "V1"] <- "native"
names(TAE.data)[names(TAE.data) == "V2"] <- "CourseIns"
names(TAE.data)[names(TAE.data) == "V3"] <- "Course"
names(TAE.data)[names(TAE.data) == "V4"] <- "sumr_reg"
names(TAE.data)[names(TAE.data) == "V5"] <- "classSize"
names(TAE.data)[names(TAE.data) == "V6"] <- "classAttr"

TAE.data$native = as.factor(TAE.data$native)
TAE.data$CourseIns = as.numeric(TAE.data$CourseIns)
TAE.data$Course = as.numeric(TAE.data$Course)
TAE.data$sumr_reg = as.numeric(TAE.data$sumr_reg)
TAE.data$classSize = as.numeric(TAE.data$classSize)
TAE.data$classAttr = as.numeric(TAE.data$classAttr)

set.seed(123)
indx <- sample(2, replace=TRUE,nrow(TAE.data),prob=c(0.7,0.3))
View(indx)

# Metode yang digunakan adalah SVM (Support Vactor Machines)


# 1. Split data menjadi training data dan test data
# Training Data
trainData <- TAE.data[indx==1,]
View(trainData)
head(trainData)

# Test Data
testData <- TAE.data[indx==2,]
View(testData)
head(testData)

# structure dataset
str(TAE.data)
str(trainData)
str(testData)

# 2. Feature Scaling
trainData[-1] <- scale(trainData[-1]) 
testData[-1] <- scale(testData[-1])
# Scale train dataset
View(trainData)
head(trainData)
# scale test dataset
View(testData)
head(testData)

# 3. Fitting SVM ke dalam Training dataset
library("e1071") 
classifier <- svm(formula = native ~ ., 
                  data = trainData, 
                  type = 'C-classification', 
                  kernel = 'linear') 
View(classifier)
classifier

# 4. Predicting test dataSet Results
y_prediction <- predict(classifier, newdata = testData[-1])
print(y_prediction)
table(y_prediction)
table(testData$native)

# cek
y_prediction <- predict(classifier, newdata = trainData[-1])
print(y_prediction)
table(y_prediction)
table(trainData$native)

# 5. membuat Confusion Matrix
cm <- table(testData[, 1], y_prediction)
cm
#
cm <- table(trainData[, 1], y_prediction)
cm
#

# 6. Memvisualisasi training data set results
set <- testData
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
set[, 2]
head(set)
