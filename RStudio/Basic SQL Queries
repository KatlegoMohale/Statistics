# Name: Katlego Mohale

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
# Consider the Orion database. Create a report that displays the number of employees residing in each city. Name this report q1.

q1 <- sqldf("SELECT City, COUNT(*) AS EmployeeCount
              FROM orion_employee_addresses
              GROUP BY City
            ")

# Question 2
# Consider the Orion database. Create a report that includes each employee’s age at time of employment. 
# The report should contain the columns Employee_ID, Birth_Date, Employee_Hire_Date and Age. Name it q2.

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
# Consider the Orion database. Using data contained in the customer table, create a report with the name q3, that shows the following statistics for each country:
# a) total number of customers
# b) total number of male customers
# c) total number of female customers
# d) Arrange the report by total number of customers so that the country with the highest value is listed first, with the remaining countries following in descending order.

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
# Consider the Orion database. Use the customer table to determine the number of Orion customers of each gender in each country. Display columns named Country, 
# Male Customers, and Female Customers. Display only those countries that have more female customers than male customers. Order the report by descending female 
# customers. Name it q4.

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
# Consider the Musico databse. The marketing division at Musico would like a list of customers who reside in either Johannesburg or Pretoria and whose email address is 
# in the yahoo domain. Write a SQL query that will provide the marketing division with the relevant information so that they can contact these customers. Make sure to 
# alphabetize by the customers names. Name it q5

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
