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
(Intercept)                                           -2.094851   0.560227  -3.739 0.000185 ***
Age.na                                                -0.308783   0.103953  -2.970 0.002974 ** 
GenderM                                               -0.066652   0.046073  -1.447 0.147992    
GenderNA                                              -0.338242   0.098934  -3.419 0.000629 ***
EmploymentCategoryBusiness and Industry                0.049200   0.055475   0.887 0.375137    
EmploymentCategoryFederal/National Government         -0.022765   0.091034  -0.250 0.802531    
EmploymentCategoryNA                                  -0.404504   0.087218  -4.638 3.52e-06 ***
EmploymentCategoryOther                                0.008108   0.095384   0.085 0.932262    
EmploymentCategoryPrivate Consultant/Self Employed     0.356218   0.120377   2.959 0.003084 ** 
EmploymentCategoryState, Provincial, Local Government -0.013114   0.156507  -0.084 0.933222    
JSMtot                                                 0.280209   0.016982  16.500  < 2e-16 ***
USA.CAN                                               -0.265099   0.071462  -3.710 0.000208 ***
DontPublish                                           -0.119500   0.066388  -1.800 0.071854 .  
InChapter                                              1.592618   0.043798  36.363  < 2e-16 ***
MEMTYPE_newB                                           1.434331   0.579458   2.475 0.013313 *  
MEMTYPE_newC                                           1.818265   0.557595   3.261 0.001111 ** 
MEMTYPE_newD                                           1.764589   0.556182   3.173 0.001510 ** 
P.SEC.BE                                               0.179972   0.281241   0.640 0.522224    
P.SEC.BIOM                                            -0.483444   0.226289  -2.136 0.032647 *  
P.SEC.BIOP                                            -0.825842   0.261210  -3.162 0.001569 ** 
P.SEC.CNSL                                            -0.407857   0.244950  -1.665 0.095900 .  
P.SEC.COMP                                             0.375532   0.233213   1.610 0.107342    
P.SEC.EDUC                                            -0.201447   0.272982  -0.738 0.460545    
P.SEC.ENVR                                             0.112345   0.355205   0.316 0.751789    
P.SEC.EPI                                             -0.114094   0.254779  -0.448 0.654285    
P.SEC.GOVT                                             0.386580   0.338328   1.143 0.253197    
P.SEC.GRPH                                             0.029109   0.256171   0.114 0.909531    
P.SEC.HPSS                                             0.046425   0.340875   0.136 0.891668    
P.SEC.MDD                                              0.686359   0.560533   1.224 0.220773    
P.SEC.MHS                                              2.151081   0.749196   2.871 0.004089 ** 
P.SEC.MKTG                                            -0.572978   0.347123  -1.651 0.098811 .  
P.SEC.NPAR                                             0.674483   0.368857   1.829 0.067463 .  
P.SEC.QP                                              -0.129295   0.354968  -0.364 0.715676    
P.SEC.SBSS                                            -0.064198   0.253266  -0.253 0.799897    
P.SEC.SDNS                                             0.107476   0.470380   0.228 0.819267    
P.SEC.SGG                                             -0.723454   0.500457  -1.446 0.148293    
P.SEC.SI                                              -0.403335   0.560478  -0.720 0.471755    
P.SEC.SIS                                              0.406254   0.330996   1.227 0.219684    
P.SEC.SLDM                                             0.459206   0.254316   1.806 0.070973 .  
P.SEC.SOC                                              0.092006   0.343228   0.268 0.788653    
P.SEC.SPES                                            -0.780314   0.353983  -2.204 0.027497 *  
P.SEC.SRMS                                            -0.662015   0.262422  -2.523 0.011645 *  
P.SEC.SSPA                                            -0.247586   0.342965  -0.722 0.470358    
P.SEC.TSHS                                             0.060574   0.302020   0.201 0.841040   
"