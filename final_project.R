print("parth shah")
pacman::p_load(rio, tidyverse, ggplot2, party, dplyr, plyr, webr, plotrix, vcd)
r=getOption("repos")
r["CRAN"]="https://cran.r-project.org/"
options(repos=r)

library(readxl)
creditscore <- read_excel("Churn_Modelling.xlsx")
View(creditscore)

#Retained and Churn as per has credit card
hascrcard <- table(creditscore$HasCrCard, creditscore$Exited)
hascrcard
colorhascrcard <- c("darkorange", "gray")
barplot(hascrcard,
     col = colorhascrcard,
     beside = TRUE,
     main = "Retained and Churn as per has Credit card",
     xlab = "Has credit card",
     ylab = "count",
     ylim = c(0,3500)
     )    
legend("topleft", c("Credit Card", "No Credit Card"), cex = 0.6, fill = colorhascrcard)

#Retained and Churn as per active member
activemember <- table(creditscore$IsActiveMember, creditscore$Exited)
activemember
coloractivemember <- c("darkorange", "gray")
barplot(activemember,
        col = coloractivemember,
        beside = TRUE,
        main = "Retained and Churn as per active member",
        xlab = "Active Member",
        ylab = "Count",
        ylim = c(0,3000)
        )
legend("topleft", c("Active", "Inactive"), cex = 0.6, fill = coloractivemember)

#boxplot

#Exited/Retained as per credit score
ggplot(creditscore, aes(x=creditscore$Exited, y=creditscore$CreditScore)) + 
  geom_boxplot(fill="darkorange") + 
  xlab("Exited and Retained")+ ylab("Credit Score")+
  labs(title = 'Exited/Retained as per credit score')

#exited/Retained as per age
ggplot(creditscore, aes(x=creditscore$Exited, y=creditscore$Age)) + 
  geom_boxplot(fill="pink") + 
  xlab("Exited and Retained")+ ylab("Age")+
  labs(title = 'Exited/Retained as per age')

#exited/Retained as per Tenure
ggplot(creditscore, aes(x=creditscore$Exited, y=creditscore$Tenure)) + 
  geom_boxplot(fill="lightgreen") + 
  xlab("Exited and Retained")+ ylab("Tenure")+
  labs(title = 'Exited/Retained as per Tenure')

#exited/Retained as per Balance
ggplot(creditscore, aes(x=creditscore$Exited, y=creditscore$Balance)) + 
  geom_boxplot(fill="lightBlue") + 
  xlab("Exited and Retained")+ ylab("Balance")+
  labs(title = 'Exited/Retained as per Balance')

#exited/Retained as per Number of Products
ggplot(creditscore, aes(x=creditscore$Exited, y=creditscore$NumOfProducts)) + 
  geom_boxplot(fill="gray") + 
  xlab("Exited and Retained")+ ylab("Number of Products")+
  labs(title = 'Exited/Retained as per Number of Products')

#Exited/Retained as per estimated salary
ggplot(creditscore, aes(x=creditscore$Exited, y=creditscore$EstimatedSalary)) + 
  geom_boxplot(fill="cyan4") + 
  xlab("Exited and Retained")+ ylab("Estimated Salary")+
  labs(title = 'Exited/Retained as per estimated salary')

#Exited/Retained as per estimated balance-salary ratio
balancesalaryratio <- table(creditscore$Balance/creditscore$EstimatedSalary)
balancesalaryratio

#Overall Retained-Exited ratio
PieDonut(creditscore, aes(Exited)) + 
           labs(title = 'Overall Retained-Exited ratio') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())

#Cities with Male-Female ratio
PieDonut(creditscore, aes(Geography, Gender),title = "Cities with Male-Female ratio")

#Cities with Exit-Retain ratio
PieDonut(creditscore, aes(Geography, Exited), title = "Cities with Exit-Retain ratio")

#Male-Female with Exit-Retain ratio
PieDonut(creditscore, aes(Gender, Exited), title = "Male-Female with Exit-Retain ratio")


