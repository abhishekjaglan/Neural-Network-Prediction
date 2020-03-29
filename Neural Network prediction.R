# Neural Network

getwd()
data <- read.csv("~/Desktop/binary.csv", header = TRUE)
str(data)

# min max normalization (converting every variable to value between 1-0)
hist(data$gre)
data$gre <- (data$gre - min(data$gre))/(max(data$gre) - min(data$gre))
hist(data$gre)
hist(data$gpa)
data$gpa <- (data$gpa - min(data$gpa))/ (max(data$gpa) - min(data$gpa))
hist(data$gpa)
hist(data$rank)
data$rank <- (data$rank - min(data$rank))/ (max(data$rank) - min(data$rank))
hist(data$rank)

#data partition

set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
training <- data[ind == 1,]
testing <- data[ind == 2,]

#neural network model
library(neuralnet)
set.seed(333)
n <- neuralnet(admit ~ gre+gpa+rank,
               data = training,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 5)

plot(n, rep = 3)

#prediction
#output <- compute(n,training[,-1]) #exclusing the output variable
#head(output$net.result)
#head(training[1,])

#confusion matrix & misclassification error
output1 <- compute(n, training[,-1], rep = 3)
p1 <- output1$net.result
pred1 <- ifelse(p1 > 0.5,1 , 0)
tab1 <- table(pred1, training$admit)
tab1
accuracy1 <- sum(diag(tab1))/sum(tab1)
1-accuracy1
output2 <- compute(n, testing[,-1], rep = 3)
p2 <- output2$net.result
pred2 <- ifelse(p2 > 0.5,1 , 0)
tab2 <- table(pred2, testing$admit)
tab2
accuracy2 <- sum(diag(tab2))/sum(tab2)
1-accuracy2