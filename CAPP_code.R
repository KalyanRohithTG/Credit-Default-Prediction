#Reading the data
#========================================
cred = read.csv('C:\\Users\\Kalyan Rohith T G\\Downloads\\Credit Bureau data.csv')
dim(cred)
head(cred)
str(cred)

cred = cred[,-2] #dropping the application id variable as it is not an important variable for our model
table(cred$Defaulter)
View(cred)

prop.table(table(cred$Defaulter))*100 #showing the class imbalance in our target variable defaulter 



#Data visualisation


#Univariate analysis

#Histogram and barplots for variables

d = 0
e = 0
for(i in 1:ncol(cred))
{
  if(is.numeric(cred[,i]))
  {
    if(length(unique(cred[,i])) > 10)
    {
      hist(cred[,i], main = names(cred)[i], xlab = names(cred)[i])
      d = d+1
    }
    
    else if(length(unique(cred[,i])) < 10)
    {
      barplot(table(cred[,i]), main=names(cred)[i], xlab = names(cred)[i])
      e = e + 1
    }
  }
}
e 
d

#Barplots for categorical varibales
c = 0
for(i in 1:ncol(cred))

{
  if(is.factor(cred[,i]))
  {
    barplot(table(cred[,i]), main=names(cred)[i], xlab = names(cred)[i])
    c = c + 1
  }
}
print(c)


#B. BIVARIATE ANALYSIS

#Side-by-side Boxplots for numerical variables
for(i in 2:ncol(cred))
{
  if(is.numeric(cred[,i]))
  {
    if(length(unique(cred[,i])) > 10)
    {
      boxplot(cred[,i] ~ cred$Defaulter, main = names(cred)[i], ylab = names(cred)[i])
    }
    
    else if(length(unique(cred[,i])) < 10)
    {
      barplot(table(cred[,i], cred$Defaulter), main=names(cred)[i], 
              xlab = names(cred)[i], beside = T, legend = rownames(table(cred[,i])))
    }
  }
}
#checking missing values
for(i in 1:ncol(cred))
{
  
  print(sum(is.na(cred[,i])))
}

#Dependent variable 'Defaulter' have 1425 missing values




#checking the complete cases 
cd1 = cred[complete.cases(cred),]
dim(cd1)







sum(is.na(cred$Age))
mean(cred$Defaulter)
median(cred$Defaulter)

#for(i in 1:ncol(cred))
#{
#  print(summary(cred$i))
#}



#Dependent variable 'Defaulter' have 1425 missing values


#mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#mode for categorical variables


#categorical
getmode(cred$Avg.CC.12)

#categorical
getmode(cred$Trades.6)

#categorical
getmode(cred$HL)


#checking the distribution of Outstanding Balance (Bal)
hist(cred[,"Bal"], main = names(cred)["Bal"], xlab = names(cred)["Bal"])
#Bal is right skewed


#missing values imputation


library(Hmisc)
#is.na(creditb[,"Defaulter"]) <- with(creditb, impute(Defaulter, 0))

summary(cred)

#Dropping the missing value observations in Target variable Defaulter (1425 records)

cred <- cred[!is.na(cred$Defaulter), ]
dim(cred)
summary(cred)

#imputing Avg.CC.12 with mode
table(cred$Defaulter~cred$Avg.CC.12)
cred$Avg.CC.12 <- with(cred, impute(Avg.CC.12, getmode(Avg.CC.12)))
summary(cred)

#imputing Trades.6 with mode
cred$Trades.6 <- with(cred, impute(Trades.6, getmode(Trades.6)))
summary(cred)

#imputing HL with mode
cred$HL <- with(cred, impute(HL, getmode(HL)))
summary(cred)

# median
cred
cred$Bal[is.na(cred$Bal)] <- median(cred$Bal, na.rm = T)
summary(cred)

#verifying the imputation
sum(complete.cases(cred))



