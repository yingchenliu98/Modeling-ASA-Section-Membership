library(dplyr)
library(caret)
library(ggplot2)

# Import the data
setwd('C:/Users/Blake Bullwinkel/Documents/Harvard/Spring 2021/STAT149/stat149_final_project/data')
member = read.csv('member-data-2020-stat149.csv')

## MISSING VALUES

# Convert yes/no vars with no missing values to 1/0
member$AnySection = as.numeric(as.factor(member$AnySection))-1
member$DontPublish = as.numeric(as.factor(member$DontPublish))-1
member$MEMTYPE = as.numeric(as.factor(member$MEMTYPE))-1
member$InChapter = as.numeric(as.factor(member$InChapter))-1
member$P.SEC.BE = as.numeric(as.factor(member$P.SEC.BE))-1
member$P.SEC.BIOM = as.numeric(as.factor(member$P.SEC.BIOM))-1
member$P.SEC.BIOP = as.numeric(as.factor(member$P.SEC.BIOP))-1
member$P.SEC.CNSL = as.numeric(as.factor(member$P.SEC.CNSL))-1
member$P.SEC.COMP = as.numeric(as.factor(member$P.SEC.COMP))-1
member$P.SEC.EDUC = as.numeric(as.factor(member$P.SEC.EDUC))-1
member$P.SEC.ENVR = as.numeric(as.factor(member$P.SEC.ENVR))-1
member$P.SEC.EPI = as.numeric(as.factor(member$P.SEC.EPI))-1
member$P.SEC.GOVT = as.numeric(as.factor(member$P.SEC.GOVT))-1
member$P.SEC.GRPH = as.numeric(as.factor(member$P.SEC.GRPH))-1
member$P.SEC.HPSS = as.numeric(as.factor(member$P.SEC.HPSS))-1
member$P.SEC.MDD = as.numeric(as.factor(member$P.SEC.MDD))-1
member$P.SEC.MHS = as.numeric(as.factor(member$P.SEC.MHS))-1
member$P.SEC.MKTG = as.numeric(as.factor(member$P.SEC.MKTG))-1
member$P.SEC.NPAR = as.numeric(as.factor(member$P.SEC.NPAR))-1
member$P.SEC.QP = as.numeric(as.factor(member$P.SEC.QP))-1
member$P.SEC.SBSS = as.numeric(as.factor(member$P.SEC.SBSS))-1
member$P.SEC.SDNS = as.numeric(as.factor(member$P.SEC.SDNS))-1
member$P.SEC.SGG = as.numeric(as.factor(member$P.SEC.SGG))-1
member$P.SEC.SI = as.numeric(as.factor(member$P.SEC.SI))-1
member$P.SEC.SIS = as.numeric(as.factor(member$P.SEC.SIS))-1
member$P.SEC.SLDM = as.numeric(as.factor(member$P.SEC.SLDM))-1
member$P.SEC.SOC = as.numeric(as.factor(member$P.SEC.SOC))-1
member$P.SEC.SPES = as.numeric(as.factor(member$P.SEC.SPES))-1
member$P.SEC.SRMS = as.numeric(as.factor(member$P.SEC.SRMS))-1
member$P.SEC.SSPA = as.numeric(as.factor(member$P.SEC.SSPA))-1
member$P.SEC.TSHS = as.numeric(as.factor(member$P.SEC.TSHS))-1

# Impute missing values of USA.CAN with the mean (most frequent binary obs)
table(member$USA.CAN)
member$USA.CAN = as.numeric(as.factor(member$USA.CAN))-1
member$USA.CAN[is.na(member$USA.CAN )] = 1

# Convert Gender and EmploymentCategory to factors with NA level
setwd('C:/Users/Blake Bullwinkel/Documents/Harvard/Spring 2021/STAT149/stat149_final_project')
source("na-convert.R")
member$Gender = as.factor(member$Gender)
member$EmploymentCategory = as.factor(member$EmploymentCategory)
member.na = na.convert.mean(member)

## EDA PLOTS

# Univariate plots
age.hist = ggplot(member.na, aes(x=Age)) + 
  geom_histogram(color='black', fill='white')
age.joined.hist = ggplot(member.na, aes(x=AgeJoinedASA)) + 
  geom_histogram(color='black', fill='white')
employment.bar = ggplot(member.na, aes(x=EmploymentCategory)) + 
  geom_bar(color='black', fill='white') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
memtype.hist = ggplot(member.na, aes(x=MEMTYPE)) + 
  geom_histogram(color='black', fill='white')

# Bivariate plots
age.anysection = ggplot(member.na, aes(x=Age, y=AnySection)) + geom_point()
agejoined.anysection = ggplot(member.na, aes(x=AgeJoinedASA, y=AnySection)) + geom_point()

## PRELIMINARY MODELING

# Split the data into train and test
test_ind = sample(seq_len(nrow(member.na)), size=3519)
member_test = member.na[test_ind, ]
member_train = member.na[-test_ind, ]

# Fit a binary classification tree with cp=0.001
member.tree1 = rpart(AnySection ~ ., cp=0.001, 
                   method='class', data=member_train)

# Use plotcp to see the CV results graphically
png(file="membertree1-plotcp.png",
    width=11, height=8.5, units="in", res=300)
plotcp(member.tree1)
dev.off()

# Prune the tree based on cp=0.0043
# Most important predictors: InChapter=0, AgeJoinedASA.na, JSMtot, AgeJoinedASA
member.tree2 = prune(member.tree1, cp=0.0043)
png(file="member.tree2.png",
    width=11, height=8.5, units="in", res=300)
prp(member.tree2, extra=1,
    main="Tree representation of pruned model")
dev.off()

# Predict on the test set and get confusion matrix (test accuracy: 0.6911)
member.tree2.preds = predict(member.tree2, member_test)[,2]
confusionMatrix(data=factor(as.numeric(member.tree2.preds>=0.5)), 
                reference=factor(member_test$AnySection), positive="1")

# Fit an ordinary GLM with a logit link
# residual deviance / residual df = 1.142
member.glm = glm(AnySection ~ ., family=binomial, data=member_train)
summary(member.glm)

## QUESTIONS

# 1. How to deal with outliers e.g. Age + categorical predictors
# 2. How should we think about the classification tree
# 3. What should our main diagnostics be?
# 4. Should we use anything beyond binary response prediction?

## TODO
# 1. MEMTYPE shouldn't be a numerical variable, right?
# 2. Group together membership types and encode as factors