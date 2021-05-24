library(regclass)
library (mgcv)

# Read in the train and test data
setwd('C:/Users/Blake Bullwinkel/Documents/Harvard/Spring 2021/STAT149/stat149_final_project/data')
member = read.csv('member-data-2020-stat149.csv')

## DATA PRE-PROCESSING

# Convert yes/no vars with no missing values to 1/0
member$AnySection = as.numeric(as.factor(member$AnySection))-1
member$DontPublish = as.numeric(as.factor(member$DontPublish))-1
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

# Convert MEMTYPE to a factor variable
member$MEMTYPE = as.factor(member$MEMTYPE)

# Impute missing values of USA.CAN with the mean (most frequent binary obs)
table(member$USA.CAN)
member$USA.CAN = as.numeric(as.factor(member$USA.CAN))-1
member$USA.CAN[is.na(member$USA.CAN )] = 1

# Convert Gender and EmploymentCategory to factors with NA level
setwd('C:/Users/Blake Bullwinkel/Documents/Harvard/Spring 2021/STAT149/stat149_final_project')
source("na-convert.R")
member$Gender = as.factor(member$Gender)
member$EmploymentCategory = as.factor(member$EmploymentCategory)

# Handling other missing variables via na.convert.mean
member = na.convert.mean(member)

# Handle Outliers of Age
member = member[member$Age<101,]
member = member[member$Age>15,]
member = member[member$AgeJoinedASA<90,]
member = member[member$AgeJoinedASA>15,]

# Get suggested level for MEMTYPE
SL = suggest_levels(AnySection ~ MEMTYPE,data=member,target=4,recode=TRUE)
SL$Conversion
member$MEMTYPE_new = SL$newlevels

# Set training set size to 75%
smp_size = floor(0.75 * nrow(member))

# Set seed to make partition reproducible
set.seed(149)
train_ind = sample(seq_len(nrow(member)), size = smp_size)
member_train = member[train_ind, ]
member_test = member[-train_ind, ]

## Binary response GLM

# Model with all predictors
member.glm1 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                  +P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS
                  +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM+P.SEC.SOC+P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA+P.SEC.TSHS+Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
summary(member.glm1)

# Perform sequential likelihood ratio tests to obtain a smaller model
member.glm2 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                  +P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS
                  +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA+P.SEC.TSHS+Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm2, member.glm1, test='Chi')
member.glm3 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                  +P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP#+P.SEC.SBSS
                  +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA+P.SEC.TSHS+Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm3, member.glm2, test='Chi')
member.glm4 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                  #+P.SEC.GRPH
                  +P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP#+P.SEC.SBSS
                  +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA+P.SEC.TSHS+Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm4, member.glm3, test='Chi')
member.glm5 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                  #+P.SEC.GRPH
                  +P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS
                  +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA#+P.SEC.TSHS
                  +Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm5, member.glm4, test='Chi')
member.glm6 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                  #+P.SEC.GRPH+P.SEC.HPSS
                  +P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS
                  +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA#+P.SEC.TSHS
                  +Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm6, member.glm5, test='Chi')
member.glm7 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                  #+P.SEC.GRPH+P.SEC.HPSS
                  +P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                  +P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA#+P.SEC.TSHS
                  +Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm7, member.glm6, test='Chi')
member.glm8 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC+P.SEC.ENVR#+P.SEC.EPI
                  +P.SEC.GOVT
                  #+P.SEC.GRPH+P.SEC.HPSS
                  +P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                  +P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA#+P.SEC.TSHS
                  +Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm8, member.glm7, test='Chi')
member.glm9 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC#+P.SEC.ENVR+P.SEC.EPI
                  +P.SEC.GOVT
                  #+P.SEC.GRPH+P.SEC.HPSS
                  +P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                  +P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA#+P.SEC.TSHS
                  +Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm9, member.glm8, test='Chi')
member.glm10 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                  +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                  +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                  +P.SEC.EDUC#+P.SEC.ENVR+P.SEC.EPI
                  +P.SEC.GOVT
                  #+P.SEC.GRPH+P.SEC.HPSS
                  +P.SEC.MDD+P.SEC.MHS
                  +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                  +P.SEC.SGG#+P.SEC.SI
                  +P.SEC.SIS
                  +P.SEC.SLDM#+P.SEC.SOC
                  +P.SEC.SPES+P.SEC.SRMS
                  +P.SEC.SSPA#+P.SEC.TSHS
                  +Age.na+AgeJoinedASA.na
                  +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm10, member.glm9, test='Chi')
member.glm11 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   +P.SEC.EDUC#+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS
                   +P.SEC.MDD+P.SEC.MHS
                   +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   +P.SEC.SGG#+P.SEC.SI
                   +P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm11, member.glm10, test='Chi')
member.glm12 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS
                   +P.SEC.MDD+P.SEC.MHS
                   +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   +P.SEC.SGG#+P.SEC.SI
                   +P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm12, member.glm11, test='Chi')
member.glm13 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS
                   +P.SEC.MDD+P.SEC.MHS
                   +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   +P.SEC.SGG#+P.SEC.SI
                   +P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm13, member.glm12, test='Chi')
member.glm14 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS
                   +P.SEC.MDD+P.SEC.MHS
                   +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   +P.SEC.SGG#+P.SEC.SI
                   +P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm14, member.glm13, test='Chi')
member.glm15 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS
                   +P.SEC.MDD+P.SEC.MHS
                   +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   +P.SEC.SGG#+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm15, member.glm14, test='Chi')
member.glm16 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS
                   +P.SEC.MDD+P.SEC.MHS
                   +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm16, member.glm15, test='Chi')
member.glm17 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD
                   +P.SEC.MHS
                   +P.SEC.MKTG+P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm17, member.glm16, test='Chi')
member.glm18 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
                   +P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD
                   +P.SEC.MHS
                   #+P.SEC.MKTG
                   +P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm18, member.glm17, test='Chi')
member.glm19 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD
                   +P.SEC.MHS
                   #+P.SEC.MKTG
                   +P.SEC.NPAR#+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm19, member.glm18, test='Chi')
member.glm20 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD
                   +P.SEC.MHS
                   #+P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm20, member.glm19, test='Chi')
member.glm21 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+DontPublish+InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP#+P.SEC.CNSL
                   +P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD
                   +P.SEC.MHS
                   #+P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm21, member.glm20, test='Chi')
member.glm22 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN#+DontPublish
                   +InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP#+P.SEC.CNSL
                   +P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD
                   +P.SEC.MHS
                   #+P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
                   +P.SEC.SLDM#+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm22, member.glm21, test='Chi')
member.glm23 = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN#+DontPublish
                   +InChapter#+P.SEC.BE
                   +P.SEC.BIOM+P.SEC.BIOP#+P.SEC.CNSL
                   +P.SEC.COMP
                   #+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
                   #+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD
                   +P.SEC.MHS
                   #+P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS
                   #+P.SEC.SGG+P.SEC.SI+P.SEC.SIS+P.SEC.SLDM+P.SEC.SOC
                   +P.SEC.SPES+P.SEC.SRMS
                   #+P.SEC.SSPA+P.SEC.TSHS
                   +Age.na#+AgeJoinedASA.na
                   +MEMTYPE_new, family=binomial, data=member_train)
anova(member.glm23, member.glm22, test='Chi')
member.reducedglm = glm(AnySection ~ Age+AgeJoinedASA+Gender+EmploymentCategory
                   +JSMtot+USA.CAN+InChapter+P.SEC.BIOM+P.SEC.BIOP+P.SEC.COMP
                   +P.SEC.MHS+P.SEC.SPES+P.SEC.SRMS+Age.na
                   +MEMTYPE_new, family=binomial, data=member_train)
# Try adding pairwise interactions
member.iteracglm = glm(AnySection ~ AgeJoinedASA+Gender+Age+EmploymentCategory
                      +JSMtot+USA.CAN+InChapter+P.SEC.BIOM+P.SEC.BIOP+P.SEC.COMP
                      +P.SEC.MHS+P.SEC.SPES+P.SEC.SRMS+Age.na
                      +MEMTYPE_new+Age:EmploymentCategory+Age:JSMtot, family=binomial, data=member_train)
anova(member.reducedglm, member.iteracglm, test='Chi')

# Sequential LRTs is a hacky method! Instead we pre-specify a few models and then use LRTs
glm1 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender, family=binomial, data=member_train)
glm2 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender+EmploymentCategory 
           +JSMtot+USA.CAN, family=binomial, data=member_train)
glm3 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter
           +MEMTYPE_new, family=binomial, data=member_train)
glm4 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter+MEMTYPE_new
           +P.SEC.BIOM+P.SEC.BIOP+P.SEC.EPI
           +P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
           +P.SEC.SGG+P.SEC.TSHS, family=binomial, data=member_train)
glm5 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter+MEMTYPE_new
           +P.SEC.COMP+P.SEC.GRPH+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS
           +P.SEC.SI+P.SEC.SLDM+P.SEC.SRMS+P.SEC.SSPA, 
           family=binomial, data=member_train)
glm6 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter+MEMTYPE_new
           +P.SEC.BE+P.SEC.CNSL+P.SEC.EDUC+P.SEC.ENVR+P.SEC.GOVT
           +P.SEC.MKTG+P.SEC.SBSS+P.SEC.SDNS+P.SEC.SIS+P.SEC.SOC
           +P.SEC.SPES, family=binomial, data=member_train)
glm7 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter+MEMTYPE_new
           +P.SEC.BE+P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
           +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
           +P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
           +P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS
           +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
           +P.SEC.SLDM+P.SEC.SOC+P.SEC.SPES+P.SEC.SRMS
           +P.SEC.SSPA+P.SEC.TSHS, family=binomial, data=member_train)
glm8 = glm(AnySection ~ Age+AgeJoinedASA+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter+MEMTYPE_new
           +P.SEC.BE+P.SEC.BIOM+P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP
           +P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI+P.SEC.GOVT
           +P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS
           +P.SEC.MKTG+P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS
           +P.SEC.SDNS+P.SEC.SGG+P.SEC.SI+P.SEC.SIS
           +P.SEC.SLDM+P.SEC.SOC+P.SEC.SPES+P.SEC.SRMS
           +P.SEC.SSPA+P.SEC.TSHS+Age:EmploymentCategory
           +Age:JSMtot, family=binomial, data=member_train)

# Check for collinearity using vif
library(car)
member.interacglm.lm = lm(AnySection ~ AgeJoinedASA+Gender+Age*EmploymentCategory
                        +JSMtot+USA.CAN+InChapter+P.SEC.BIOM+P.SEC.BIOP+P.SEC.COMP
                        +P.SEC.MHS+P.SEC.SPES+P.SEC.SRMS+Age.na
                        +MEMTYPE_new, data=member_train)
vif(member.interacglm.lm)

# Hosmer-Lemeshow function
hosmerlem = function (y, yhat, g = 10) {
  cutyhat = cut(yhat, breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest = T)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}

# Hosmer-Lemeshow test
hosmerlem(glm8$y, fitted(glm8), g=10)

# Model diagnostics

# fitted probabilities
member.fitted = fitted(glm8)
# deviance residuals
member.devresid = residuals(glm8,type="deviance")
# approximate studentized/jackknifed residuals
member.jresid = rstudent(glm8)
# approximate Cooks distances
member.cooks = cooks.distance(glm8)

# Stripey plots - deviance residuals and jackknifed residuals
png(file="member-resid1.png",
    width=11, height=8.5, units="in", res=300)
plot(member.fitted, member.devresid,
     xlab="Fitted probabilities",
     ylab="Deviance residuals",
     pch=19, col="red", cex=1.5,
     main="Fitted vs deviance residual plot
for preferred binary GLM")
abline(h=0,lty=2,col="green")
dev.off()

png(file="member-resid2.png",
    width=11, height=8.5, units="in", res=300)
plot(member.fitted, member.jresid,
     xlab="Fitted probabilities",
     ylab="Jackknifed residuals",
     pch=19, col="red", cex=1.5,
     main="Fitted vs jackknifed residual plot
for preferred binary GLM")
abline(h=0,lty=2,col="green")
dev.off()

library(arm)
# Plot average residuals 
png(file="member-resid3.png",
    width=6, height=5, units="in", res=300)
binnedplot(member.fitted, residuals(glm8,type="response"),
           xlab="Averaged fitted probabilities",
           ylab="Averaged residuals",
           pch=19, col.pts="red", cex.pts=1.5,
           main="Fitted vs residual plot
for preferred binary GLM")
abline(h=0,lty=2,col="green")
dev.off()

# Cook's distances
png(file="member-cooks.png",
    width=6, height=5, units="in", res=300)
plot(member.cooks, type="h", lwd=2,
     xlab="Observation index",
     ylab="Cook's distances",
     main="Cook's distances for preferred binary GLM")
dev.off()

# Find confidence intervals for final model coefficients
confint(glm8)

# Fit a GAM with the same predictor set as glm8

# Train on full data set
gam2 = gam(AnySection ~ s(Age)+s(AgeJoinedASA)+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter+MEMTYPE_new+P.SEC.BE+P.SEC.BIOM
           +P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
           +P.SEC.GOVT+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS+P.SEC.MKTG
           +P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS+P.SEC.SGG+P.SEC.SI
           +P.SEC.SIS+P.SEC.SLDM+P.SEC.SOC+P.SEC.SPES+P.SEC.SRMS+P.SEC.SSPA
           +P.SEC.TSHS+ ti(Age, EmploymentCategory, bs='fs') 
           + ti(Age, JSMtot, bs='fs'), family=binomial, data=member)

# Train on only the training data
gam3 = gam(AnySection ~ s(Age)+s(AgeJoinedASA)+Age.na+Gender+EmploymentCategory
           +JSMtot+USA.CAN+DontPublish+InChapter+MEMTYPE_new+P.SEC.BE+P.SEC.BIOM
           +P.SEC.BIOP+P.SEC.CNSL+P.SEC.COMP+P.SEC.EDUC+P.SEC.ENVR+P.SEC.EPI
           +P.SEC.GOVT+P.SEC.GRPH+P.SEC.HPSS+P.SEC.MDD+P.SEC.MHS+P.SEC.MKTG
           +P.SEC.NPAR+P.SEC.QP+P.SEC.SBSS+P.SEC.SDNS+P.SEC.SGG+P.SEC.SI
           +P.SEC.SIS+P.SEC.SLDM+P.SEC.SOC+P.SEC.SPES+P.SEC.SRMS+P.SEC.SSPA
           +P.SEC.TSHS+ ti(Age, EmploymentCategory, bs='fs') 
           + ti(Age, JSMtot, bs='fs'), family=binomial, data=member_train)

# Model summary for final GAM:
"
Parametric coefficients:
                                                       Estimate Std. Error z value Pr(>|z|)    
(Intercept)                                           -2.246577   0.539706  -4.163 3.15e-05 ***
Age.na                                                -0.263024   0.087327  -3.012 0.002596 ** 
GenderM                                               -0.070865   0.039950  -1.774 0.076087 .  
GenderNA                                              -0.329112   0.086584  -3.801 0.000144 ***
EmploymentCategoryBusiness and Industry                0.086867   0.048095   1.806 0.070896 .  
EmploymentCategoryFederal/National Government         -0.008037   0.078104  -0.103 0.918045    
EmploymentCategoryNA                                  -0.407156   0.074941  -5.433 5.54e-08 ***
EmploymentCategoryOther                                0.022066   0.083454   0.264 0.791461    
EmploymentCategoryPrivate Consultant/Self Employed     0.267798   0.101295   2.644 0.008200 ** 
EmploymentCategoryState, Provincial, Local Government -0.011736   0.141208  -0.083 0.933762    
JSMtot                                                 0.270473   0.014590  18.538  < 2e-16 ***
USA.CAN                                               -0.269627   0.062358  -4.324 1.53e-05 ***
DontPublish                                           -0.062417   0.057405  -1.087 0.276897    
InChapter                                              1.582661   0.037696  41.985  < 2e-16 ***
MEMTYPE_newB                                           1.653962   0.553923   2.986 0.002827 ** 
MEMTYPE_newC                                           1.985130   0.537721   3.692 0.000223 ***
MEMTYPE_newD                                           1.913849   0.536576   3.567 0.000361 ***
P.SEC.BE                                              -0.018872   0.238785  -0.079 0.937007    
P.SEC.BIOM                                            -0.561561   0.195970  -2.866 0.004163 ** 
P.SEC.BIOP                                            -1.046066   0.231568  -4.517 6.26e-06 ***
P.SEC.CNSL                                            -0.471980   0.201331  -2.344 0.019063 *  
P.SEC.COMP                                             0.370339   0.200881   1.844 0.065246 .  
P.SEC.EDUC                                            -0.077697   0.238968  -0.325 0.745079    
P.SEC.ENVR                                             0.135345   0.287547   0.471 0.637864    
P.SEC.EPI                                              0.077247   0.216198   0.357 0.720869    
P.SEC.GOVT                                             0.231415   0.288236   0.803 0.422052    
P.SEC.GRPH                                             0.032584   0.221775   0.147 0.883193    
P.SEC.HPSS                                             0.360256   0.296177   1.216 0.223850    
P.SEC.MDD                                              0.580030   0.506498   1.145 0.252136    
P.SEC.MHS                                              1.153256   0.490468   2.351 0.018706 *  
P.SEC.MKTG                                            -0.590745   0.302734  -1.951 0.051013 .  
P.SEC.NPAR                                             0.729153   0.324378   2.248 0.024586 *  
P.SEC.QP                                              -0.115597   0.309301  -0.374 0.708600    
P.SEC.SBSS                                            -0.120101   0.214900  -0.559 0.576251    
P.SEC.SDNS                                             0.411231   0.387262   1.062 0.288284    
P.SEC.SGG                                             -0.242003   0.457082  -0.529 0.596492    
P.SEC.SI                                              -0.369648   0.516743  -0.715 0.474398    
P.SEC.SIS                                              0.105853   0.287220   0.369 0.712467    
P.SEC.SLDM                                             0.410326   0.216306   1.897 0.057831 .  
P.SEC.SOC                                              0.171835   0.302704   0.568 0.570261    
P.SEC.SPES                                            -0.620970   0.315614  -1.967 0.049126 *  
P.SEC.SRMS                                            -0.567841   0.228838  -2.481 0.013086 *  
P.SEC.SSPA                                            -0.553829   0.286224  -1.935 0.052997 .  
P.SEC.TSHS                                            -0.022986   0.269733  -0.085 0.932088    
"