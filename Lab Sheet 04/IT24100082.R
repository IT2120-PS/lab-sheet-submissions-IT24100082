getwd()
setwd("C://Users//IT24100082//Desktop//IT24100082")
branch_data <- read.csv("Exercise.txt", header=TRUE)
branch_data
boxplot(branch_data$Sales_X1, main="Boxplot for Sales", ylab="Sales")
fivenum(branch_data$Advertising_X2)
IQR(branch_data$Advertising_X2)
get.outliers <- function(z){
  q1 <- quantile(z, 0.25)
  q3 <- quantile(z, 0.75)
  iqr <- q3 - q1
  
  ub <- q3 + 1.5 * iqr
  lb <- q1 - 1.5 * iqr
  
  outliers <- z[z < lb | z > ub]
  return(list("Q1"=q1, "Q3"=q3, "IQR"=iqr, "Lower Bound"=lb, "Upper Bound"=ub, "Outliers"=outliers))
}

get.outliers(branch_data$Years_X3)