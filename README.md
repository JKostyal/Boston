# Boston -Predicting whether or not a given Boston suburb has a crime rate above/below the median.

# function for fun

Libraryy=function(){
library(ISLR)
library(class)
library(MASS)
}
Libraryy()

# exploratory data analysis

fix(Boston)
attach(Boston)
plot(Boston)
cor(Boston)
crimemed=rep(0,length(crim))
crimemed[crim>median(crim)]=1
Boston=data.frame(Boston,crimemed)
train= 1:(dim(Boston)[1]/2)
test= (dim(Boston)[1]/2+1):dim(Boston)[1]
Boston.train=Boston[train,]
Boston.test=Boston[test,]
crime.test=crimemed[test]

##Logistic Regression

glm.fit=glm(crimemed ~ . - crimemed - crim -chas - tax ,data=Boston,
            family=binomial,subset=train,maxit=100)
summary(glm.fit)
glm.prob=predict(glm.fit,Boston.test,type="response")
glm.pred=rep(0,length(glm.prob))
glm.pred[glm.prob > 0.5]= 1
table(glm.pred,crime.test)
mean(glm.pred==crime.test)
mean(glm.pred!=crime.test)

##LDA

lda.fit=lda(crimemed~. - crimemed-crim-chas-tax,data=Boston,subset=train,maxit=100)
lda.pred=predict(lda.fit,Boston.test,type="response")
lda.class=lda.pred$class
table(lda.pred$class,crime.test)
mean(lda.pred$class==crime.test)
mean(lda.pred$class!=crime.test)

##KNN

?Boston
set.seed(1)
train.x=cbind(zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv)[train,]
test.x=cbind(zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv)[test,]
crim.x=crimemed[train]
knn.pred=knn(train.x,test.x,crim.x,k=10)
mean(knn.pred==crime.test)
mean(knn.pred!=crime.test)

