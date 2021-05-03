library(regclass)

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
member.finalglm = glm(AnySection ~ AgeJoinedASA+Gender+Age*EmploymentCategory
                      +JSMtot+USA.CAN+InChapter+P.SEC.BIOM+P.SEC.BIOP+P.SEC.COMP
                      +P.SEC.MHS+P.SEC.SPES+P.SEC.SRMS+Age.na
                      +MEMTYPE_new, family=binomial, data=member_train)
anova(member.reducedglm, member.finalglm, test='Chi')

# Check for collinearity using vif
library(car)
member.finalglm.lm = lm(AnySection ~ AgeJoinedASA+Gender+Age*EmploymentCategory
                        +JSMtot+USA.CAN+InChapter+P.SEC.BIOM+P.SEC.BIOP+P.SEC.COMP
                        +P.SEC.MHS+P.SEC.SPES+P.SEC.SRMS+Age.na
                        +MEMTYPE_new, data=member_train)
vif(member.finalglm.lm)

# Hosmer-Lemeshow function
hosmerlem = function (y, yhat, g = 10) {
  cutyhat = cut(yhat, breaks = quantile(yhat, probs = seq(0,
                                                          1, 1/g)), include.lowest = T)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}

# Hosmer-Lemeshow test
hosmerlem(member.reducedglm$y, fitted(member.reducedglm), g=10)

# Model diagnostics

# fitted probabilities
member.fitted = fitted(member.finalglm)
# deviance residuals
member.devresid = residuals(member.finalglm,type="deviance")
# approximate studentized/jackknifed residuals
member.jresid = rstudent(member.finalglm)
# approximate Cooks distances
member.cooks = cooks.distance(member.finalglm)

# Stripey plots - deviance residuals and jackknifed residuals
png(file="member-resid1.png",
    width=11, height=8.5, units="in", res=300)
plot(member.fitted, member.devresid,
     xlab="Fitted probabilities",
     ylab="Deviance residuals",
     pch=19, col="red", cex=1.5,
     main="Fitted vs deviance residual plot
for final binary GLM")
abline(h=0,lty=2,col="green")
dev.off()

png(file="member-resid2.png",
    width=11, height=8.5, units="in", res=300)
plot(member.fitted, member.jresid,
     xlab="Fitted probabilities",
     ylab="Jackknifed residuals",
     pch=19, col="red", cex=1.5,
     main="Fitted vs jackknifed residual plot
for final binary GLM")
abline(h=0,lty=2,col="green")
dev.off()

library(arm)
# Plot average residuals 
png(file="member-resid3.png",
    width=11, height=8.5, units="in", res=300)
binnedplot(member.fitted, residuals(member.finalglm,type="response"),
           xlab="Averaged fitted probabilities",
           ylab="Averaged residuals",
           pch=19, col.pts="red", cex.pts=1.5,
           main="Fitted vs residual plot
for final binary GLM")
abline(h=0,lty=2,col="green")

# Cook's distances
png(file="lowbwt-cooks.png",
    width=11, height=8.5, units="in", res=300)
plot(lowbwt.cooks, type="h", lwd=2,
     xlab="Observation index",
     ylab="Cook's distances",
     main="Cook's distances for low birth weight")

# Find confidence intervals for final model coefficients
confint(member.glm1)
