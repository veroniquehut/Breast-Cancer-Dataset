# Breast-Cancer-Dataset
Fall 2014 Data 101

Blue = comments 
Black = code 
Purple = description  

This code is used to predict malignant vs. benign cancer
df=read.csv("breast-cancer wisconsin.data.txt",header=F,na.strings="?",col.names=c("ID","Thickness","Size","Shape","adhesion","singlesize","nuclei","chromatin","nucleoli","mitoses","classes"))
#this function reads the csv file and then renames the columns of the data set into something simpler  

These are the original columns that were renamed 
1. Sample code number            id number
2. Clump Thickness               1 - 10
3. Uniformity of Cell Size       1 - 10
4. Uniformity of Cell Shape      1 - 10
5. Marginal Adhesion             1 - 10
6. Single Epithelial Cell Size   1 - 10
7. Bare Nuclei                   1 - 10
8. Bland Chromatin               1 - 10
9. Normal Nucleoli               1 - 10
10. Mitoses                       1 - 10
11. Class:                        (2 for benign, 4 for malignant)


This function splits the data set into a training data set and testing data set where models of subsets of the data can be tested on other subsets of the data- this is used for the methods and algorithms used 
train.data = df[1:400,]
test.data = df[-(1:400),]

#1 --> SUMMARY OF DATA SET, used to view the data in the whole file 
summary(df)

#use this function to find the correlation between the variables
-highest correlation of .9 was between size and shape of cells
-purpose is to see which variables are important and which are not  
cor(df[,c(-1,-11)])
plot(cor(df[,c(-1,-11)]) #size and thickness

hist(df$Thickness) 
-histogram of thickness 
# only one variable, shows a plot 

#this changes the data frame name and uses the subset matrix where all malignant tumors in the dataset = 4, all benign tumors = 2... allows us to distinguish the two types of tumors in the data set 
df.M=df[df$class==4,]
df.B=df[df$class==2,]

#forms histograms of just the thickness variable for the two types of tumors and changes the title and axis 
hist(df.M$Thickness,xlab='Thickness', main= 'Malignant Thickness')
hist(df.B$Thickness,xlab='Thickness', main= 'Benign Thickness')

#Double Histogram of Thickness to show malignant vs benign 
plot(density(df.B$Thickness),xlab='Thickness',xlim=c(1,10),main='Density of Thickness',col='blue')
lines(density(df.M$Thickness),col='green')
legend('topleft',c('Benign','Malignant'),col=c('blue','green'),lty=c(1,1))

#Double Histogram of Uniformity of Cell Size  
plot(density(df.B$Size),xlab='size',xlim=c(1,10),main='Density of the Uniformity of Cell Size',col='blue')
lines(density(df.M$Size),col='green')
legend('topleft',c('Benign','Malignant'),col=c('blue','green'),lty=c(1,1))


#2 Data Framework - Clustering --> KMEANS & HIER
#Hierarchical Clustering 
#only 7th column is a problem, need to fill in NAs
-makes “?” all equal 5 
df$nuclei[is.na(df$nuclei)]=5 

dendogram of all except ID’s and class 
makes it viewable to the client- gives a visual 
df=df[,-1]
r=hclust(dist(df[,-10]))
dend = as.dendrogram(r)

#install.packages('dendextend')
# loading the package
library(dendextend)
# Assigning the labels of dendrogram object with new colors:
labels_colors(dend) <- df$class
# Plotting the new dendrogram
plot(dend)

#kmeans- forms clusters using 3,4,6 
-this is the number of clusters we are telling the computer to form, informs the computer where to place all the variables 
-used to organize the data and see if there is any correlation and forms “natural groups”

rk = kmeans(df[,-10],3)
plot(df[,-10], col = rk$cluster)

rk = kmeans(df[,-10],4)
plot(df[,-10], col = rk$cluster)

rk = kmeans(df[,-10],6)
plot(df[,-10], col = rk$cluster)

#3  Implementation of Methods 

library(rpart)

#DECISION TREE METHOD 
#needed to create our own training set since there was not one, out of 699 variables ^^ done before hand above 
#cross validation .. you need to create your own training data set to compare it to our data set 
-decision tree splits the data set into 2 parts: train and test and uses one against the other to predict the type of tumor with what accuracy 

decision.tree = rpart(classes ~ Thickness + Size + Shape + adhesion +singlesize + nuclei + chromatin + nucleoli + mitoses,data=train.data,method='class')
predictions = predict(decision.tree,test.data,type="class")
accuracy = sum(predictions==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.976667

plot(decision.tree)
text(decision.tree) #adds labels on the plot 

#RANDOM FORESTS METHOD 
library(randomForest) #installs the package 
rf = randomForest(factor(classes) ~ Thickness + Size + Shape + adhesion +singlesize + nuclei + chromatin + nucleoli + mitoses, ntree=10,data=train.data,type='classification')
#need to change the number of ntrees, see what the difference is and if more trees or less trees have higher accuracy (cross validation) 
predictions = predict(rf,test.data,type="class")
accuracy = sum(predictions==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.9765886

rf = randomForest(factor(classes) ~ Thickness + Size + Shape + adhesion +singlesize + nuclei + chromatin + nucleoli + mitoses, ntree=500,data=train.data,type='classification')
#need to change the number of ntrees, see what the difference is and if more trees or less trees have higher accuracy 
predictions = predict(rf,test.data,type="class")
accuracy = sum(predictions==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.986621

#K NEAREST NEIGHBOR METHOD 
#forms clusters when you already have hypotheses regarding the number of clusters in your variables
#you change the K (which is the number of clusters you are telling the computer to form) to see if the number of clusters affects the accuracy 

library(class)
knn.results = knn(train.data[,1:9], test.data[,1:9], train.data$classes, k = 20, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.9866221

knn.results = knn(train.data[,1:9], test.data[,1:9], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.989966 ***********

knn.results = knn(train.data[,1:9], test.data[,1:9], train.data$classes, k = 30, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.9866221

knn.results = knn(train.data[,1:9], test.data[,1:9], train.data$classes, k = 5, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.98337

knn.results = knn(train.data[,1:9], test.data[,1:9], train.data$classes, k = 3, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy is 0.9799331

#k=10 nearest neighbors gives the best accuracy 

# Check the variables → we slowly remove each of the variables to see how it affects the accuracy

#Take a variable out to see if the variables are important or not
#ex: df[-2] takes out a column or in other words takes out a variable 
#if the accuracy changes, or there is a drastic change then the variable is important, if there is not then they are all similarily important 
library(class)
knn.results = knn(train.data[,-2], test.data[,-2], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy = 0.986621

knn.results = knn(train.data[,-3], test.data[,-3], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#accuracy = 0.986621

knn.results = knn(train.data[,-4], test.data[,-4], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#no change 

knn.results = knn(train.data[,-5], test.data[,-5], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#no change 

knn.results = knn(train.data[,-6], test.data[,-6], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#no change 

knn.results = knn(train.data[,-7], test.data[,-7], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#no change 

knn.results = knn(train.data[,-8], test.data[,-8], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#0.9933 !!!!! ---> important, does not change dramatically but there is a difference: column 8 is the chromatin  

knn.results = knn(train.data[,-9], test.data[,-9], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#no change 

knn.results = knn(train.data[,-10], test.data[,-10], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#no change

#the mimimum subset of variables vs entire data set 
library(class)
knn.results = knn(train.data[,c("Size","Shape","Thickness","nuclei")], test.data[,c("Size","Shape","Thickness","nuclei")], train.data$classes, k = 10, prob=TRUE)
accuracy = sum(knn.results==test.data$classes)/nrow(test.data)
print(accuracy)
#0.98996
→ just as efficient as the entire data set 

#find the results
knn.results 
plot(knn.results)
#the actual results (2= benign, 4= malignant)
  [1] 4 2 4 2 2 2 2 2 2 2 2 2 4 2 4 2 4 2 2 2 2 4 2 2 2 4 4 4 2 2 2 2 2 2 4 4 4 2 2 2 4 2 2 2 2 2 2 2 2 4 2 2 2 4 2
 [56] 4 4 4 2 2 2 2 2 2 2 4 4 4 2 2 2 2 2 2 2 2 2 2 2 4 2 2 4 4 2 2 2 4 4 2 2 4 2 4 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2
[111] 2 2 2 2 4 4 2 2 2 4 2 2 4 4 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
[166] 4 2 2 4 4 4 4 2 2 4 2 2 2 2 2 2 4 4 2 2 2 4 2 4 2 4 4 4 2 4 2 2 2 2 4 2 2 2 4 2 4 2 2 4 2 4 4 4 2 2 2 2 2 2 2
[221] 2 4 2 2 2 2 4 2 2 2 4 2 2 4 2 2 4 4 2 2 2 2 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 4 4 2 2 2 2 2 2 2 2 2 4 4 4 2 2 2 2
[276] 2 2 2 2 2 4 4 2 2 2 2 2 2 2 2 2 4 2 2 2 2 4 4 4
attr(,"prob")


 


