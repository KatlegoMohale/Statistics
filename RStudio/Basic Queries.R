# PRACTICAL 3

# Name: Katlego Mohale

# Student Number: 19194588

library(readr)
library(sqldf)
library(RJDBC)
library(RH2)
library(lubridate)


orion_employee_payroll <- read_csv("orion_employee_payroll.csv")
orion_employee_addresses <- read_csv("orion_employee_addresses.csv")
orion_customer <- read_csv("orion_customer.csv")
music_customer <- read_csv("music_customer.csv")

# Question 1
q1 <- sqldf("SELECT City, COUNT(*) AS EmployeeCount
              FROM orion_employee_addresses
              GROUP BY City
            ")
#count all the different rows in the column specified, only the rows in the column


# Question 2
q2 <- sqldf("SELECT  Employee_ID, Birth_Date, Employee_Hire_Date,
              (Employee_Hire_Date - Birth_Date)/365 AS Age
              FROM orion_employee_payroll
              ORDER BY Age DESC")

q2 <- sqldf("SELECT  Employee_ID, Birth_Date, Employee_Hire_Date,
              floor((Employee_Hire_Date - Birth_Date)/365.25) AS Age
              FROM orion_employee_payroll
              ORDER BY Age DESC")
#divide by 365.25 to account for the leap year
#the floor function removes decimal places without rounding up/down, 
#floor goes to the lowers whole number, every time never rounds up

# Question 3
Males <- orion_customer[orion_customer$Customer_Gender == 'M',]
q3males <- sqldf("SELECT Customer_Country,count(*) As Male_Customers
              FROM Males
              GROUP BY Customer_Country
              ")
Females <- orion_customer[orion_customer$Customer_Gender == 'F',]
q3females <- sqldf("SELECT Customer_Country,count(*) As Female_Customers
              FROM Females
              GROUP BY Customer_Country
              ")

q3 <- sqldf("SELECT a.*, b.Female_Customers
              FROM q3males AS a
              LEFT JOIN q3females AS b
            on a.Customer_Country = b.Customer_Country
              ORDER BY (a.Male_Customers + b.Female_Customers) DESC")


q3 <- sqldf("SELECT Customer_Country, count(Customer_ID) As Customers,
              SUM(Customer_Gender = 'M') AS Men,
              SUM(Customer_Gender = 'F') AS Women
              FROM orion_customer
              GROUP BY Customer_Country
              ORDER BY Customers DESC")
# == checks for equality in r, in sql = checks for equality
#= assign values in r, in sql you never assign values so its only use is to check
#in sql = check and return 1 if equal and 0 if not, sum the 1 and 0s gives total males/females


# Question 4
q4 <- sqldf("SELECT a.*, b.Female_Customers
              FROM q3males AS a
              LEFT JOIN q3females AS b
            on a.Customer_Country = b.Customer_Country
              WHERE (b.Female_Customers > a.Male_Customers)
              ORDER BY (a.Male_Customers + b.Female_Customers) DESC
             ")


q4 <- sqldf("SELECT Customer_Country AS Country,
              SUM(Customer_Gender='M') AS Male_Customers,
              SUM(Customer_Gender='F') AS Female_Customers
              FROM orion_customer
              GROUP BY Country
              HAVING SUM(Customer_Gender='F') > SUM(Customer_Gender='M')
              ORDER BY Female_Customers DESC
             ")
#cant have a calculated column inside a where clause

# Question 5
JHB <- music_customer[music_customer$Customer_City == 'Johannesburg',]
q5JHB <- sqldf("SELECT *
              FROM JHB
              WHERE Customer_Email like '%yahoo%'
              ORDER BY Customer_Name 
               ")
PTA <- music_customer[music_customer$Customer_City == 'Pretoria',]
q5PTA <- sqldf("SELECT *
              FROM PTA
              WHERE Customer_Email like '%yahoo%'
              ORDER BY Customer_Name ")
q5 <- rbind(q5JHB, q5PTA)


q5 <- sqldf("SELECT Customer_Name,Customer_Surname,Customer_Phone,Customer_Address,Customer_Email
            From music_customer
            WHERE (Customer_City in('Johannesburg','Pretoria')) AND (Customer_Email like '%yahoo%')
            ORDER BY Customer_Name
            ")







