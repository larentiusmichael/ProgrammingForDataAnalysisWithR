#LAURENTIUS MICHAEL
#TP061310



#install package
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("crayon")
install.packages("rgl")
install.packages("plot3D")
install.packages("plotly")
install.packages("plotrix")

#load the package
library(janitor)
library(dplyr)
library(ggplot2)
library(crayon)
library(rgl)
library(plot3D)
library(plotly)
library(plotrix)


#DATA IMPORT
data <- read.csv("C:\\Users\\ASUS\\OneDrive\\Dokumen\\lminspiration.com\\APU Materials\\Year 2 Semester 1\\Programming for Data Analysis\\Assignment\\employee_attrition.csv", header = TRUE)


#DATA CLEANING
#remove empty columns and rows
data <- remove_empty(data, whic = c("rows", "cols"), quiet = FALSE)


#DATA PRE-PROCESSING
#assigning headers to each column
names(data) = c("EMPLOYEE_ID", "RECORD_DATE", "BIRTH_DATE", "HIRE_DATE", "TERMINATION_DATE", "AGE", "LENGTH_OF_SERVICE", "CITY_NAME", "DEPARTMENT_NAME", "JOB_TITLE", "STORE_CODE", "GENDER_SHORT", "GENDER_FULL", "TERMINATION_REASON", "TERMINATION_TYPE", "STATUS_YEAR", "STATUS", "BUSINESS_UNIT")

#viewing the dataset in the console page
data

#viewing the dataset in table form
View(data)

#DATA EXPLORATION
#checking number of columns
ncol(data)

#checking number of rows
nrow(data)

#viewing first 12 lines
head(data, 12)

#viewing last 12 lines
tail(data, 12)

#viewing summary
summary(data)


#QUESTION
# Question 1: What is the real number of employees that are currently working, 
#             have worked, and total of both have worked and currently working in the company?

# Analysis 1.1: Find rows that contain the updated values based on employee ID.
updated_rows <- data %>% group_by(EMPLOYEE_ID) %>% top_n(1, STATUS_YEAR)

# Analysis 1.2: Create a new file which only stores rows that contain the updated values.
write.csv(updated_rows, "C:\\Users\\ASUS\\OneDrive\\Dokumen\\lminspiration.com\\APU Materials\\Year 2 Semester 1\\Programming for Data Analysis\\Assignment\\employee_attrition_update.csv")

# Analysis 1.3: Import, view, and check the file which has just created.
newdata <- read.csv("C:\\Users\\ASUS\\OneDrive\\Dokumen\\lminspiration.com\\APU Materials\\Year 2 Semester 1\\Programming for Data Analysis\\Assignment\\employee_attrition_update.csv", header = TRUE)
names(newdata) = c("ROW_NO", "EMPLOYEE_ID", "RECORD_DATE", "BIRTH_DATE", "HIRE_DATE", "TERMINATION_DATE", "AGE", "LENGTH_OF_SERVICE", "CITY_NAME", "DEPARTMENT_NAME", "JOB_TITLE", "STORE_CODE", "GENDER_SHORT", "GENDER_FULL", "TERMINATION_REASON", "TERMINATION_TYPE", "STATUS_YEAR", "STATUS", "BUSINESS_UNIT")
newdata$ROW_NO = NULL
newdata
View(newdata)
duplicateddata <- newdata[duplicated(newdata$EMPLOYEE_ID), ]  #to check is there any duplicate data based on EMPLOYEE_ID
duplicateddata

# Analysis 1.4: Remove the invalid rows.
#(ONLY CAN BE EXECUTED ONCE)
subset(newdata, EMPLOYEE_ID == 3008)  #viewing the rows based on EMPLOYEE_ID
which(newdata$EMPLOYEE_ID == 3008)  #check the row no based on EMPLOYEE_ID
newdata <- newdata[-784, ]  #removing the row
which(newdata$EMPLOYEE_ID == 3008)
newdata <- newdata[-5216, ]
which(data$EMPLOYEE_ID == 3008)
data <- data[-9679, ]
which(data$EMPLOYEE_ID == 3008)
data <- data[-9679, ]
which(data$EMPLOYEE_ID == 3008)
data <- data[-48579, ]
subset(newdata, EMPLOYEE_ID == 3401)
which(newdata$EMPLOYEE_ID == 3401)
newdata <- newdata[-1013, ]
which(newdata$EMPLOYEE_ID == 3401)
newdata <- newdata[-5295, ]
which(data$EMPLOYEE_ID == 3401)
data <- data[-12164, ]
which(data$EMPLOYEE_ID == 3401)
data <- data[-12164, ]
which(data$EMPLOYEE_ID == 3401)
data <- data[-12164, ]
which(data$EMPLOYEE_ID == 3401)
data <- data[-48656, ]
subset(newdata, EMPLOYEE_ID == 7007)
which(newdata$EMPLOYEE_ID == 7007)
newdata <- newdata[-3718, ]
which(newdata$EMPLOYEE_ID == 7007)
newdata <- newdata[-6037, ]
which(data$EMPLOYEE_ID == 7007)
data <- data[-41838, ]
which(data$EMPLOYEE_ID == 7007)
data <- data[-41838, ]
which(data$EMPLOYEE_ID == 7007)
data <- data[-49397, ]
subset(newdata, EMPLOYEE_ID == 7023)
which(newdata$EMPLOYEE_ID == 7023)
newdata <- newdata[-3730, ]
which(newdata$EMPLOYEE_ID == 7023)
newdata <- newdata[-6039, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-41954, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-41954, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-41954, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-41954, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-41954, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-41954, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-41954, ]
which(data$EMPLOYEE_ID == 7023)
data <- data[-49393, ]
subset(newdata, EMPLOYEE_ID == 8296)
which(newdata$EMPLOYEE_ID == 8296)
newdata <- newdata[-4761, ]
which(newdata$EMPLOYEE_ID == 8296)
newdata <- newdata[-6279, ]
which(data$EMPLOYEE_ID == 8296)
data <- data[-48036, ]
which(data$EMPLOYEE_ID == 8296)
data <- data[-49633, ]

# Analysis 1.5: Find the total number of employees (both currently working and have worked).
g = factor(data$EMPLOYEE_ID)  #from the old file
nlevels(g)
nrow(newdata)  #from the new file

# Analysis 1.6: Find the number of employees which are currently working in the company.
active_emp <- filter(newdata, STATUS == "ACTIVE")
employee_act <- nrow(active_emp)
employee_act

# Analysis 1.7: Find the number of employees which have worked in the company.
terminated_emp <- subset(newdata, STATUS == "TERMINATED")
employee_term <- nrow(terminated_emp)
employee_term

# Conclusion:
a = c(employee_act, employee_term)
b = c("ACTIVE", "TERMINATED")
pct = round(a / sum(a) * 100)
new_labels = paste(b, "\n", pct, "%", sep = "")
pie(a, new_labels, radius = 1, main = "NUMBER OF EMPLOYEES", col = c("blue", "red"))


# Question 2: What is the total number of employees 
#             that are currently working based on department name?

# Analysis 2.1: Find how many and list the department names in the company.
departmentname_list <- unique(newdata$DEPARTMENT_NAME)
departmentname_list
h = factor(newdata$DEPARTMENT_NAME)
number = nlevels(h)
number

# Analysis 2.2: Find the total number of employees that are currently working based on department name.
for (x in departmentname_list)
{
  emp_in_dept <- filter(active_emp, DEPARTMENT_NAME == x)
  y <- nrow(emp_in_dept)
  message(red$bold("Department Name\t\t\t: "), black$bold(x))
  message(red$bold("Total Number of Employees\t: "), black$bold(y))
  print("\n")
}

# Conclusion:
vec_num <- numeric()
for (x in 1 : number)
{
  emp_in_dept <- filter(active_emp, DEPARTMENT_NAME == departmentname_list[x])
  vec_num[x] <- nrow(emp_in_dept)
}
z <- data.frame(departmentname_list, vec_num)
ggplot(z, aes(x = vec_num, y = departmentname_list)) + geom_bar(stat = "identity", width = 0.5, color = "black", fill = "pink") + geom_text(aes(label = vec_num))


# Question 3: Add new columns called base salary and bonus to every employee 
#             who are still working, then assign the values based on 
#             department name (for base salary) and length of service (for bonus)! 
#             What will be the smallest, biggest, and average amount of total salaries 
#             that employees get based on the whole company and department name?

# Analysis 3.1: Find the department names which have active employees.
active_dept <- subset(newdata, STATUS == "ACTIVE", select = DEPARTMENT_NAME)
active_dept <- unique(active_dept)
active_dept

# Analysis 3.2: Assign the base salary value based on the department.
dept_name_list = select(newdata, "DEPARTMENT_NAME")
status_list = select(newdata, "STATUS")
no <- nrow(dept_name_list)
no
no <- nrow(status_list)
no
base_salary_list <- data.frame(BASE_SALARY = numeric())
for (x in 1 : no) 
{
  if (status_list[x, 1] == "ACTIVE")
  {
    if (dept_name_list[x, 1] == "Executive")
    {
      base_salary_list[x, 1] = 2500
    } else if (dept_name_list[x, 1] == "Store Management")
    {
      base_salary_list[x, 1] = 1500
    } else if (dept_name_list[x, 1] == "Meats")
    {
      base_salary_list[x, 1] = 200
    } else if (dept_name_list[x, 1] == "Recruitment")
    {
      base_salary_list[x, 1] = 1300
    } else if (dept_name_list[x, 1] == "Customer Service")
    {
      base_salary_list[x, 1] = 300
    } else if (dept_name_list[x, 1] == "Produce")
    {
      base_salary_list[x, 1] = 200
    } else if (dept_name_list[x, 1] == "Bakery")
    {
      base_salary_list[x, 1] = 250
    } else if (dept_name_list[x, 1] == "Dairy")
    {
      base_salary_list[x, 1] = 200
    } else if (dept_name_list[x, 1] == "Processed Foods")
    {
      base_salary_list[x, 1] = 200
    } else
    {
      base_salary_list[x, 1] = 0
    }
  } else
  {
    base_salary_list[x, 1] = 0
  }
}
newdata <- cbind(newdata, base_salary_list)

# Analysis 3.3: Assign bonus value based on length of service.
newdata <- mutate(newdata, BONUS = LENGTH_OF_SERVICE * 10)
bonus_col_index <- which(colnames(newdata) == "BONUS")
for (x in 1 : no) 
{
  if (status_list[x, 1] == "TERMINATED")
  {
    newdata[x, bonus_col_index] = 0
  }
}

# Analysis 3.4: Sum the base salary and bonus to get the total salary.
total_salary <- data.frame(TOTAL_SALARY = newdata$BASE_SALARY + newdata$BONUS)
total_salary2 <- cbind(total_salary, dept_name_list, status_list)
View(total_salary)

# Analysis 3.5: Find the smallest, biggest, and average amount of total salary in the whole company.
total_salary2 <- filter(total_salary2, STATUS == "ACTIVE")
max(total_salary2$TOTAL_SALARY)
min(total_salary2$TOTAL_SALARY)
mean(total_salary2$TOTAL_SALARY)

# Analysis 3.6: Find the smallest, biggest, and average amount of total salary based on department.
no <- nrow(active_dept)
for (x in 1 : no) 
{
  salary_based_dept <- subset(total_salary2, DEPARTMENT_NAME == active_dept[x, 1])
  l = max(salary_based_dept$TOTAL_SALARY)
  m = min(salary_based_dept$TOTAL_SALARY)
  n = mean(salary_based_dept$TOTAL_SALARY)
  message(black$bold$underline("Department Name\t: "), black$bold$underline(active_dept[x, 1]))
  message(blue$bold("Biggest Salary\t: "), green$bold(l))
  message(blue$bold("Smallest Salary\t: "), red$bold(m))
  message(blue$bold("Average\t\t: "), yellow$bold(n))
}

# Conclusion:
temp_data <- filter(newdata, STATUS == "ACTIVE")
ggplot(temp_data, aes(y = BONUS, x = BASE_SALARY)) + geom_point(aes(color = factor(DEPARTMENT_NAME))) + ggtitle("BASE SALARY VS BONUS based on Department")
ggplot(total_salary2, aes(x = DEPARTMENT_NAME, y = TOTAL_SALARY)) + geom_boxplot(aes(color = factor(DEPARTMENT_NAME)))


# Question 4: Is there any common reason that causes employees based on
#             particular range of age tend to work in a certain department?

# Analysis 4.1: Find the department names which have active employees.
active_dept <- subset(newdata, STATUS == "ACTIVE", select = DEPARTMENT_NAME)
active_dept <- unique(active_dept)
active_dept

# Analysis 4.2: Find the minimum, maximum, and average age of employees that are currently working in the company.
age_list <- filter(newdata, STATUS == "ACTIVE")
min(age_list$AGE)
max(age_list$AGE)
mean(age_list$AGE)

# Analysis 4.3: Find the range of age of employees that are currently working based on department.
no <- nrow(active_dept)
for (x in 1 : no) 
{
  age_based_dept <- subset(newdata, STATUS == "ACTIVE" & DEPARTMENT_NAME == active_dept[x, 1], 
                           select = c(AGE, DEPARTMENT_NAME))
  l = max(age_based_dept$AGE)
  m = min(age_based_dept$AGE)
  n = round(mean(age_based_dept$AGE))
  message(black$bold$underline("Department Name\t: "), black$bold$underline(active_dept[x, 1]))
  message(blue$bold("Max. Range of Age\t: "), green$bold(l))
  message(blue$bold("Min. Range of Age\t: "), red$bold(m))
  message(blue$bold("Average\t\t\t: "), yellow$bold(n))
  print("\n")
}

# Analysis 4.4: Find the range of length of service of employees that are currently working based on department.
no <- nrow(active_dept)
for (x in 1 : no) 
{
  service_based_dept <- subset(newdata, STATUS == "ACTIVE" & DEPARTMENT_NAME == active_dept[x, 1], select = c(LENGTH_OF_SERVICE, DEPARTMENT_NAME))
  l = max(service_based_dept$LENGTH_OF_SERVICE)
  m = min(service_based_dept$LENGTH_OF_SERVICE)
  n = round(mean(service_based_dept$LENGTH_OF_SERVICE), digits = 2)
  message(black$bold$underline("Department Name\t: "), black$bold$underline(active_dept[x, 1]))
  message(blue$bold("Longest Length of Service\t: "), green$bold(l))
  message(blue$bold("Shortest Length of Service\t: "), red$bold(m))
  message(blue$bold("Average Length of Service\t: "), yellow$bold(n))
  print("\n")
}

# Conclusion:
temp_data <- filter(newdata, STATUS == "ACTIVE")
result <- ggplot(temp_data, aes(y = AGE, x = DEPARTMENT_NAME)) + geom_jitter(aes(color = factor(DEPARTMENT_NAME))) + ggtitle("The Relation between Age and Department") + stat_smooth(method = lm)
result <- result + theme_classic()
result <- result + stat_summary(aes(y = AGE, x = DEPARTMENT_NAME), fun = mean, color = "black")
result
result <- ggplot(temp_data, aes(y = LENGTH_OF_SERVICE, x = DEPARTMENT_NAME)) + geom_jitter(aes(color = factor(DEPARTMENT_NAME))) + ggtitle("The Relation between Length of Service and Department") + stat_smooth(method = lm)
result <- result + theme_classic()
result <- result + stat_summary(aes(y = LENGTH_OF_SERVICE, x = DEPARTMENT_NAME), fun = mean, color = "black")
result


# Question 5: Is there any common reason that causes employees 
#             based on particular range of age tend to work in a certain job title?

# Analysis 5.1: Find what are the job titles of active employees.
active_job <- subset(newdata, STATUS == "ACTIVE", select = JOB_TITLE)
active_job <- unique(active_job)
active_job

# Analysis 5.2: Find the average age of employees that are currently working in the company.
age_list <- filter(newdata, STATUS == "ACTIVE")
min(age_list$AGE)
max(age_list$AGE)
mean(age_list$AGE)

# Analysis 5.3: Find the range of age of employees that are currently working based on job title.
no <- nrow(active_job)
for (x in 1 : no) 
{
  age_based_job <- subset(newdata, STATUS == "ACTIVE" & JOB_TITLE == active_job[x, 1], select = c(AGE, JOB_TITLE))
  l = max(age_based_job$AGE)
  m = min(age_based_job$AGE)
  n = round(mean(age_based_job$AGE))
  message(black$bold$underline("Job Title\t\t: "), black$bold$underline(active_job[x, 1]))
  message(blue$bold("Max. Range of Age\t: "), green$bold(l))
  message(blue$bold("Min. Range of Age\t: "), red$bold(m))
  message(blue$bold("Average\t\t\t: "), yellow$bold(n))
  print("\n")
}

# Analysis 5.4: Find the range of length of service of employees that are currently working based on job title.
no <- nrow(active_job)
for (x in 1 : no) 
{
  service_based_job <- subset(newdata, STATUS == "ACTIVE" & JOB_TITLE == active_job[x, 1], select = c(LENGTH_OF_SERVICE, JOB_TITLE))
  l = max(service_based_job$LENGTH_OF_SERVICE)
  m = min(service_based_job$LENGTH_OF_SERVICE)
  n = round(mean(service_based_job$LENGTH_OF_SERVICE), digits = 2)
  message(black$bold$underline("JOb Title Name\t: "), black$bold$underline(active_job[x, 1]))
  message(blue$bold("Longest Length of Service\t: "), green$bold(l))
  message(blue$bold("Shortest Length of Service\t: "), red$bold(m))
  message(blue$bold("Average Length of Service\t: "), yellow$bold(n))
  print("\n")
}

# Conclusion:
temp_data <- filter(newdata, STATUS == "ACTIVE")
ggplot(temp_data, aes(x = JOB_TITLE, y = AGE)) + geom_point(aes(color = JOB_TITLE)) + facet_wrap(~JOB_TITLE)
ggplot(temp_data, aes(x = JOB_TITLE, y = LENGTH_OF_SERVICE)) + geom_point(aes(color = JOB_TITLE)) + facet_wrap(~JOB_TITLE)


# Question 6: What is the trend of termination and joining the company year by year? 
#             Is it increasing or decreasing?

# Analysis 6.1: Find the years when there is at least one employee got terminated.
temporary <- subset(newdata, select = c(STATUS, HIRE_DATE, TERMINATION_DATE))
class(newdata$TERMINATION_DATE)
termination_date_list = c(temporary$TERMINATION_DATE)
no <- nrow(temporary)
temp_vec <- numeric()
termination_year_list <- data.frame(TERMINATION_YEAR = numeric())
for (x in 1 : no) 
{
  date_temp = termination_date_list[x]
  save_date = as.POSIXct(date_temp, format = "%m/%d/%Y")
  save_year = format(save_date, format = "%Y")
  temp_vec = c(temp_vec, save_year)
  termination_year_list[x, 1] = temp_vec[x]
}
temporary <- cbind(temporary, termination_year_list)
termination_year_list <- unique(termination_year_list)
termination_year_list <- arrange(termination_year_list, TERMINATION_YEAR)
no <- nrow(termination_year_list)
data_a_moment <- numeric()
for (x in 2 : no)
{
  data_moment = termination_year_list[x, 1]
  data_a_moment = c(data_a_moment, data_moment)
}
data_a_moment

# Analysis 6.2: Find the years when there is at least one employee got hired.
class(newdata$HIRE_DATE)
hire_date_list = c(temporary$HIRE_DATE)
no <- nrow(temporary)
temp_vec <- numeric()
hire_year_list <- data.frame(HIRE_YEAR = numeric())
for (x in 1 : no) 
{
  date_temp = hire_date_list[x]
  save_date = as.POSIXct(date_temp, format = "%m/%d/%Y")
  save_year = format(save_date, format = "%Y")
  temp_vec = c(temp_vec, save_year)
  hire_year_list[x, 1] = temp_vec[x]
}
temporary <- cbind(temporary, hire_year_list)
hire_year_list <- unique(hire_year_list)
hire_year_list <- arrange(hire_year_list, HIRE_YEAR)
hire_year_list

# Analysis 6.3: Find the number of terminations based on year.
year_of_termination <- numeric()
value_of_termination <- numeric()
no <- nrow(termination_year_list)
for (x in 2 : no) 
{
  temporary_list <- subset(temporary, TERMINATION_YEAR == termination_year_list[x, 1])
  l = nrow(temporary_list)
  message(black$bold$underline("YEAR\t\t\t: "), black$bold$underline(termination_year_list[x, 1]))
  message(blue$bold("Number of Termination\t: "), red$bold(l))
  print("\n")
  year_of_termination = c(year_of_termination, termination_year_list[x, 1])
  value_of_termination = c(value_of_termination, l)
}

# Analysis 6.4: Find the number of hires based on year.
year_of_hire <- numeric()
value_of_hire <- numeric()
no <- nrow(hire_year_list)
for (x in 1 : no) 
{
  temporary_list <- subset(temporary, HIRE_YEAR == hire_year_list[x, 1])
  l = nrow(temporary_list)
  message(black$bold$underline("YEAR\t\t\t: "), black$bold$underline(hire_year_list[x, 1]))
  message(blue$bold("Number of Hire\t\t: "), red$bold(l))
  print("\n")
  year_of_hire = c(year_of_hire, hire_year_list[x, 1])
  value_of_hire = c(value_of_hire, l)
}

# Conclusion:
plot(x = year_of_termination, y = value_of_termination, type = "o", xlab = "YEAR", ylab = "VALUE", col = "red", main = "TERMINATION TREND")
plot(x = year_of_hire, y = value_of_hire, type = "o", xlab = "YEAR", ylab = "VALUE", col = "blue", main = "HIRE TREND")


# Question 7: What is the trend of the number of active employees year by year? 
#             Is it increasing or decreasing?

# Analysis 7.1: Find the oldest hire year and the newest both hire and termination year.
min(temporary$HIRE_YEAR)
max(temporary$HIRE_YEAR)
max(temporary$TERMINATION_YEAR)
min_range <- min(temporary$HIRE_YEAR)
max_range <- max(temporary$TERMINATION_YEAR)

# Analysis 7.2: Find the number of active employees year by year.
total_active_emp <- data.frame(YEAR = numeric(), TOTAL_ACTIVE_EMP = numeric())
n = 0
y = 1
for (x in min_range : max_range) 
{
  temporary_list <- subset(temporary, HIRE_YEAR == x)
  l = nrow(temporary_list)
  temporary_list <- subset(temporary, TERMINATION_YEAR == x)
  m = nrow(temporary_list)
  n = n + l
  n = n - m
  message(black$bold$underline("YEAR\t\t\t\t: "), black$bold$underline(x))
  message(blue$bold("Number of Active Employees\t: "), red$bold(n))
  print("\n")
  total_active_emp[y, 1] = x
  total_active_emp[y, 2] = n
  y = y + 1
}

# Conclusion:
ggplot(total_active_emp, aes(x = YEAR, y = TOTAL_ACTIVE_EMP)) + geom_line(col = "purple") + ggtitle("TREND OF ACTIVE EMPLOYEE")


# Question 8: What is the most common reason that makes employees have a long length of service?

# Analysis 8.1: Find the average length of service from the whole company.
newdata2 <- cbind(newdata, total_salary)
avg_los <- subset(newdata2, STATUS == "ACTIVE")
value_avg_los <- round(mean(avg_los$LENGTH_OF_SERVICE), digits = 2)
value_avg_los 

# Analysis 8.2: Find the average age from the whole company.
avg_age <- subset(newdata2, STATUS == "ACTIVE")
value_avg_age <- round(mean(avg_age$AGE), digits = 2)
value_avg_age 

# Analysis 8.3: Find the average total salary from the whole company.
avg_ts <- subset(newdata2, STATUS == "ACTIVE")
value_avg_ts <- round(mean(avg_ts$TOTAL_SALARY), digits = 2)
value_avg_ts

# Analysis 8.4: Find the age of employees that have length of service longer than the average.
temporary_list <- subset(newdata2, STATUS == "ACTIVE" & LENGTH_OF_SERVICE > value_avg_los, select = c(AGE))
temporary_list <- unique(temporary_list)
arrange(temporary_list, AGE)

# Analysis 8.5: Find the total salary of employees that have length of service longer than the average.
temporary_list <- subset(newdata2, STATUS == "ACTIVE" & LENGTH_OF_SERVICE > value_avg_los, select = c(TOTAL_SALARY))
temporary_list <- unique(temporary_list)
arrange(temporary_list, TOTAL_SALARY)

# Conclusion:
newdata3 <- subset(newdata2, STATUS == "ACTIVE")
rgl.open()
rgl.bg(color = "white")
length_of_service <- newdata3$LENGTH_OF_SERVICE
age <- newdata3$AGE
total_salary <- newdata3$TOTAL_SALARY
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}
rgl_init
plot3d(length_of_service, age, total_salary)
rgl.spheres(length_of_service, age, total_salary, r = 5, color = "yellow", size = 5)
rgl.bbox(color = "#333377")


# Question 9: What is the most common reason that makes employees have a short length of service?

# Analysis 9.1: Find the average length of service from the whole company.
avg_los <- subset(newdata2, STATUS == "ACTIVE")
value_avg_los <- round(mean(avg_los$LENGTH_OF_SERVICE), digits = 2)
value_avg_los 

# Analysis 9.2: Find the average age from the whole company.
avg_age <- subset(newdata2, STATUS == "ACTIVE")
value_avg_age <- round(mean(avg_age$AGE), digits = 2)
value_avg_age 

# Analysis 9.3: Find the average total salary from the whole company.
avg_ts <- subset(newdata2, STATUS == "ACTIVE")
value_avg_ts <- round(mean(avg_ts$TOTAL_SALARY), digits = 2)
value_avg_ts

# Analysis 9.4: Find the age of employees that have length of service shorter than the average.
temporary_list <- subset(newdata2, STATUS == "ACTIVE" & LENGTH_OF_SERVICE < value_avg_los, select = c(AGE))
temporary_list <- unique(temporary_list)
arrange(temporary_list, AGE)

# Analysis 9.5: Find the total salary of employees that have length of service shorter than the average.
temporary_list <- subset(newdata2, STATUS == "ACTIVE" & LENGTH_OF_SERVICE < value_avg_los, select = c(TOTAL_SALARY))
temporary_list <- unique(temporary_list)
arrange(temporary_list, TOTAL_SALARY)

# Conclusion:
newdata3 <- subset(newdata2, STATUS == "ACTIVE")
scatter3D(x = newdata3$LENGTH_OF_SERVICE, y = newdata3$AGE, z = newdata3$TOTAL_SALARY, bty = "g", pch = 20, cex = 2, ticktype = "detailed", clab="Range of Total Salary", xlab = "Length of Service", ylab = "Age", zlab = "Total Salary" )


# Question 10: How about the distribution of departments in the company based on cities?

# Analysis 10.1: Find what are the departments.
departments <- subset(newdata, select = DEPARTMENT_NAME)
departments <- unique(departments)
departments

# Analysis 10.2: Find what are the cities.
cities <- subset(newdata, select = CITY_NAME)
cities <- unique(cities)
cities

# Analysis 10.3: Find the departments based on cities.
dept_based_city <- data.frame(CITY = character(), NO_OF_DEPT = numeric())
no <- nrow(cities)
for (x in 1 : 40) 
{
  city_loc = cities[x, 1]
  temporary_list <- subset(newdata, CITY_NAME == city_loc, select = c(DEPARTMENT_NAME))
  temporary_list <- unique(temporary_list)
  l = nrow(temporary_list)
  message(black$bold("CITY NAME\t: "), black$bold(city_loc))
  message(blue$bold("Departments\t: "))
  for (y in 1 : l)
  {
    print(temporary_list[y, 1])
  }
  dept_based_city[x, 1] = city_loc
  dept_based_city[x, 2] = l
  print("\n")
}

# Conclusion:
plot_ly(data = dept_based_city, labels = dept_based_city$CITY, values = dept_based_city$NO_OF_DEPT, type = 'pie', sort = FALSE)


# Question 11: What is the percentage and total number of employees got terminated 
#              based on termination type and termination reason?

# Analysis 11.1: Find what are the termination types.
type_termination <- subset(newdata, STATUS == "TERMINATED", select = TERMINATION_TYPE)
type_termination <- unique(type_termination)
type_termination

# Analysis 11.2: Find what are the termination reasons.
reason_termination <- subset(newdata, STATUS == "TERMINATED", select = TERMINATION_REASON)
reason_termination <- unique(reason_termination)
reason_termination

# Analysis 11.3: Find the total number of inactive employees in the company.
terminated_emp <- subset(newdata, STATUS == "TERMINATED")
employee_term <- nrow(terminated_emp)
employee_term

# Analysis 11.4: Find the percentage and total number of inactive employees based on termination type.
term_type_value <- numeric()
term_type_label <- numeric()
term_type_percent <- numeric()
no <- nrow(type_termination)
for (x in 1 : no) 
{
  type_of_term = type_termination[x, 1]
  temporary_list <- subset(newdata, TERMINATION_TYPE == type_of_term)
  l = nrow(temporary_list)
  m = round(((l / employee_term) * 100), digits = 2)
  message(black$bold("TERMINATION TYPE\t: "), black$bold(type_of_term))
  message(blue$bold("Number of Employees\t: "), green$bold(l))
  message(blue$bold("Percentage\t\t: "), red$bold(m))
  print("\n")
  term_type_value[x] <- l
  term_type_label[x] <- type_of_term
  term_type_percent[x] <- m
}

# Analysis 11.5: Find the percentage and total number of inactive employees based on termination reason.
term_reason_value <- numeric()
term_reason_label <- numeric()
term_reason_percent <- numeric()
no <- nrow(reason_termination)
for (x in 1 : no) 
{
  reason_of_term = reason_termination[x, 1]
  temporary_list <- subset(newdata, TERMINATION_REASON == reason_of_term)
  l = nrow(temporary_list)
  m = round(((l / employee_term) * 100), digits = 2)
  message(black$bold("TERMINATION REASON\t: "), black$bold(reason_of_term))
  message(blue$bold("Number of Employees\t: "), green$bold(l))
  message(blue$bold("Percentage\t\t: "), red$bold(m))
  print("\n")
  term_reason_value[x] <- l
  term_reason_label[x] <- reason_of_term
  term_reason_percent[x] <- m
}

# Conclusion:
new_labels = paste(term_type_label, "\n", term_type_percent, "%", " - ", term_type_value, " people", sep = "")
pie3D(term_type_value, labels = new_labels, main = "TERMINATION BASED ON TERMINATION TYPE")
new_labels = paste(term_reason_label, "\n", term_reason_percent, "%", " - ", term_reason_value, " people", sep = "")
pie3D(term_reason_value, labels = new_labels, main = "TERMINATION BASED ON TERMINATION REASON")


# Question 12: Which store codes that are currently active based on cities?

# Analysis 12.1: Find what are the active store codes.
code_store <- subset(newdata, STATUS == "ACTIVE", select = STORE_CODE)
code_store <- unique(code_store)
code_store

# Analysis 12.2: Find what are the active cities.
cities <- subset(newdata, STATUS == "ACTIVE", select = CITY_NAME)
cities <- unique(cities)
cities

# Analysis 12.3: Find the active store codes based on cities.
code_based_city <- data.frame(CITY = character(), TOTAL_ACTIVE_STORE_CODE = numeric())
no <- nrow(cities)
for (x in 1 : no) 
{
  city_loc = cities[x, 1]
  temporary_list <- subset(newdata, CITY_NAME == city_loc & STATUS == "ACTIVE", 
                           select = c(STORE_CODE))
  temporary_list <- unique(temporary_list)
  l = nrow(temporary_list)
  message(black$bold("CITY NAME\t\t: "), black$bold(city_loc))
  message(blue$bold("Active Store Code\t: "))
  for (y in 1 : l)
  {
    print(temporary_list[y, 1])
  }
  code_based_city[x, 1] = city_loc
  code_based_city[x, 2] = l
  print("\n")
}

# Conclusion:
plot_ly(data = code_based_city, labels = code_based_city$CITY, values = code_based_city$TOTAL_ACTIVE_STORE_CODE, type = 'pie', sort = FALSE)


# Question 13: Which gender got the most layoff?

# Analysis 13.1: Find what are the genders.
emp_gender <- subset(newdata, STATUS == "TERMINATED", select = GENDER_FULL)
emp_gender <- unique(emp_gender)
emp_gender

# Analysis 13.2: Find how many layoffs that each gender has
gender_layoff <- data.frame(GENDER = character(), NO_OF_LAYOFF = numeric())
no <- nrow(emp_gender)
for (x in 1 : no) 
{
  gender_of_emp = emp_gender[x, 1]
  temporary_list <- subset(newdata, TERMINATION_REASON == "Layoff" & GENDER_FULL == gender_of_emp)
  l = nrow(temporary_list)
  message(black$bold("GENDER\t\t\t: "), black$bold(gender_of_emp))
  message(blue$bold("Number of LAYOFF\t: "), green$bold(l))
  print("\n")
  gender_layoff[x, 1] = gender_of_emp
  gender_layoff[x, 2] = l
}

# Conclusion:
colors = c("blue", "pink")
ggplot(gender_layoff, aes(x = GENDER, y = NO_OF_LAYOFF)) + geom_bar(stat = "identity", fill = factor(colors))


# Question 14: What is the most common amount of total salary that employees get?

# Analysis 14.1: Find what are the amount of total salaries that employees get.
newdata2 <- cbind(newdata, total_salary)
sum_salary <- subset(newdata2, STATUS == "ACTIVE")
sum_salary2 <- subset(sum_salary, select = c(TOTAL_SALARY))
sum_salary2 <- unique(sum_salary2)
sum_salary2

# Analysis 14.2: Find how many occurrences of each amount of total salaries.
no <- nrow(sum_salary2)
for (x in 1 : no) 
{
  temp_total_salary = sum_salary2[x, 1]
  temporary_list <- subset(sum_salary, TOTAL_SALARY == temp_total_salary)
  l = nrow(temporary_list)
  message(black$bold("TOTAL SALARY\t\t: "), black$bold(temp_total_salary))
  message(blue$bold("Number of Occurrence\t: "), green$bold(l))
  print("\n")
}

# Conclusion:
sum_salary %>% ggplot(aes(x = TOTAL_SALARY)) + geom_density(color = "midnightblue", fill = "skyblue") 
  + ggtitle("Number of Occurrence of Unique Total Salary Value")


# Question 15: What is the most common number of lengths of service?

# Analysis 15.1: Find what are the unique number of lengths of service.
unique_los <- subset(newdata, select = LENGTH_OF_SERVICE)
unique_los <- unique(unique_los)
unique_los

# Analysis 15.2: Find how many occurrences of each number of lengths of service.
no <- nrow(unique_los)
for (x in 1 : no) 
{
  temp_length_service = unique_los[x, 1]
  temporary_list <- subset(newdata, LENGTH_OF_SERVICE == temp_length_service)
  l = nrow(temporary_list)
  message(black$bold("LENGTH OF SERVICE\t: "), black$bold(temp_length_service))
  message(blue$bold("Number of Occurrence\t: "), green$bold(l))
  print("\n")
}

# Conclusion:
ggplot(newdata, aes(x = LENGTH_OF_SERVICE)) + geom_histogram(color = "black", aes(fill = ..count..)) 
+ scale_fill_gradient("Count", low = "red", high = "green") + ggtitle("Number of Occurrence of Length of Service")



