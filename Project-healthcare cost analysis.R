
# PROJECT - Healthcare cost analysis

# load the data

hospital_cost=read.csv(file.choose(),header=TRUE)

#check column names
names(hospital_cost)

nrow(hospital_cost)
ncol(hospital_cost)
summary(hospital_cost)

sum(is.na(hospital_cost$AGE))





#Attribute	Description
#Age -	Age of the patient discharged
#Female -	A binary variable that indicates if the patient is female
#Los-	Length of stay in days
#Race -	Race of the patient (specified numerically)
#Totchg-	Hospital discharge costs
#Aprdrg-	All Patient Refined Diagnosis Related Groups

#Analysis to be done: 

#1. To record the patient statistics, the agency wants to
#   find the age category of
#   people who frequently visit the hospital and has the maximum expenditure.

hist(hospital_cost$AGE) #------graphical representation

class(hospital_cost$AGE)
summary(as.factor(hospital_cost$AGE))

# conclusion----infant category has max visit
# 0     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 307  10   1   3   2   2   2   3   2   2   4   8  15  18  25  29  29  38 

names(hospital_cost)

aggregate(TOTCHG~AGE,FUN=sum,data=hospital_cost)
max(aggregate(TOTCHG~AGE,FUN=sum,data=hospital_cost))

#   AGE  TOTCHG
#    0   678118 ---------infant category has max expenditure




# 2.-----In order of severity of the diagnosis and treatments and 
#   to find out the expensive treatments, the agency wants to find the 
#   diagnosis-related group that has maximum hospitalization and expenditure.


hist(hospital_cost$APRDRG)
class(hospital_cost$APRDRG)

summary(as.factor(hospital_cost$APRDRG))

max(summary(as.factor(hospital_cost$APRDRG)))
which.max(summary(as.factor(hospital_cost$APRDRG)))

df=aggregate(TOTCHG~APRDRG,FUN=sum,data=hospital_cost)
which.max(df$TOTCHG)
df[which.max(df$TOTCHG),]


# 3.-- To make sure that there is no malpractice, the agency needs to analyze 
#  if the race of the patient is related to the hospitalization costs.

hospital_cost$RACE
sum(is.na(hospital_cost$RACE))
table(hospital_cost$RACE)
# remove na value
nrow(hospital_cost)
hospital_cost=na.omit(hospital_cost)
nrow(hospital_cost)

class(hospital_cost$RACE)

summary(as.factor(hospital_cost$RACE))

hospital_cost$RACE=as.factor(hospital_cost$RACE)

model=aov(hospital_cost$TOTCHG~hospital_cost$RACE)
summary(model)
model

# f value is small and p value is high , it means there is no relationship
# between cost and race
# but also,the data is skewed
#    1     2   3   4   5   6 
#    484   6   1   3   3   2 (out of 500, race 1 is 484) skewed data


# 4.------- To properly utilize the costs, the agency has to 
#   analyze the severity of the hospital costs by age and gender 
#   for the proper allocation of resources.

class(hospital_cost$FEMALE)
table(hospital_cost$FEMALE)
hospital_cost$FEMALE=as.factor(hospital_cost$FEMALE)

#create linear model

model1=lm(TOTCHG~AGE+FEMALE,data=hospital_cost)
model1
summary(model1)



# 5.---- Since the length of stay is the crucial factor for inpatients,
#       the agency wants to find if the 
#       length of stay can be predicted from age, gender, and race.


class(hospital_cost$FEMALE)
class(hospital_cost$RACE)
names(hospital_cost)

model2=lm(LOS~AGE+FEMALE+RACE,data=hospital_cost)
summary(model2)
# p value is very high for all,so we cant conclude length of stay from
# age,gender and race.




# 6. To perform a complete analysis, the agency wants to 
#    find the variable that mainly affects hospital costs.

model3=lm(TOTCHG~.,data = hospital_cost)
summary(model3)

# Age and length of stay affect the cost.
# length of stay is positively related to the cost. Increase in one day
#     cause 742 rupees change in cost






















































