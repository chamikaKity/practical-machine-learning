Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement - a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it. In this project, your goal
will be to use data from accelerometers on the belt, forearm, arm, and
dumbell of 6 participants. They were asked to perform barbell lifts
correctly and incorrectly in 5 different ways. More information is
available from the website here:
<http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight
Lifting Exercise Dataset).

Data
----

The training data:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source:
<http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>

Analysis
--------

#### Load the data

    trainData <- read.csv(file.path("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
    testData <- read.csv(file.path("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

loading packages

    library(lattice)
    library(caret)
    library(randomForest)
    library(ggplot2)
    library(rpart)
    library(rattle)

Training and testing data

    training <- subset(trainData, select = -c(X))
    testing <- subset(testData, select = -c(X))

#### Clean the data

Remove the near zero variance predictors. They are not only
non-informative, they are problematic to fit models.

    nzv <- nearZeroVar(training, saveMetrics = TRUE)
    training <- training[,nzv$nzv==FALSE]

Remove all the variables with more than 75% NA

    rows <- dim(training)[1]
    variables <- dim(training)[2]

    cleanNA <- training
    for(i in 1:variables){
            if( sum(is.na(training[,i]))/rows >= 0.75){
                    for(j in 1:dim(cleanNA)[2]){
                            if(length( grep(names(training[i]), names(cleanNA)[j]))==1){
                                    cleanNA <- cleanNA[,-j]
                            }
                    }
            }
    }

    training <- cleanNA

    predictors <- colnames(training[,-58])
    testing <- testing[predictors]

Coerce the training and testing data into a same type and get the same
class in all the variables.

    for (i in 1:dim(testing)[2] ) {
        for(j in 1:dim(training)[2]) {
            if( length( grep(names(training[i]), names(testing)[j]) ) == 1)  {
                class(testing[j]) <- class(training[i])
            }      
        }      
    }

    testing <- rbind(training[1, -58] , testing)
    testing <- testing[-1,]

#### Partition data

partitioning data to train and test the model

    inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
    myTraining <- training[inTrain,]
    myTesting <- training[-inTrain,]

#### Model fitting

1.  Random Forest Method

<!-- -->

    set.seed(333)
    mod1 <- randomForest(classe ~ . , data=myTraining)
    pred1 <- predict(mod1, myTesting)
    accuracy1 <- confusionMatrix(pred1, myTesting$classe)$overall[1]
    accuracy1

    ##  Accuracy 
    ## 0.9994902

    plot(mod1)

![](machine_learning_project_files/figure-markdown_strict/random%20forest-1.png)

2.  Linear Discriminant Analysis

<!-- -->

    set.seed(333)
    mod2 <- train(classe~. , method="lda", data=myTraining)
    pred2 <- predict(mod2, myTesting)
    accuracy2 <- confusionMatrix(pred2, myTesting$classe)$overall[1]
    accuracy2

    ##  Accuracy 
    ## 0.8513169

3.  Regression Trees

<!-- -->

    set.seed(333)
    mod3 <- rpart(classe~., data = myTraining, method = "class")
    pred3 <- predict(mod3,myTesting, type="class")
    accuracy3 <- confusionMatrix(pred3, myTesting$classe)$overall[1]
    accuracy3

    ##  Accuracy 
    ## 0.8769754

    fancyRpartPlot(mod3)

![](machine_learning_project_files/figure-markdown_strict/regression%20tree-1.png)

#### Final Model

    model <- c("rf","lda","rpart")
    accuracy <- c(accuracy1,accuracy2,accuracy3)
    data.frame(model,accuracy)

    ##   model  accuracy
    ## 1    rf 0.9994902
    ## 2   lda 0.8513169
    ## 3 rpart 0.8769754

According to the above table random forest method has the best best
accuracy 99.91% for myTesting data. The expected out of sample error is
0.09%. Therefore random forest prediction model is the best model to
predict the 20 different test cases in testing data.

#### Predicting Results

Predicting results for testing data

    set.seed(333)
    pred <-predict(mod1, testing)
    pred

    ##  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

#### Write the results to a text file for submission

    pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
    }

    pml_write_files(pred)
