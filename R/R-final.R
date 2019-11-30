
###### On Linux server:
##library(DBI)
##con <- dbConnect(odbc::odbc(), .connection_string = "Driver={ODBC Driver 17 for SQL Server};server=192.168.1.1;
##database=COLLEGE;uid=dsuser02;
##pwd=DSuser02!", timeout = 10)



##### On Windows:
library(DBI)
con <- dbConnect(odbc::odbc(),DSN = "collage;Trusted_Connection=yes;", timeout = 10)
sql1 <- "SELECT * FROM collage.dbo.Classrooms"
sql2 <- "SELECT * FROM collage.dbo.Courses"
sql3 <- "SELECT * FROM collage.dbo.Departments"
sql4 <- "SELECT * FROM collage.dbo.Students"
sql5 <- "SELECT * FROM collage.dbo.Teachers"

Classrooms <- dbGetQuery(con,sql1)
Courses <- dbGetQuery(con,sql2)
Departments <- dbGetQuery(con,sql3)
Students <- dbGetQuery(con,sql4)
Teachers <- dbGetQuery(con,sql5)

dbDisconnect(con)


#fixing the names:

Students <- Students %>% rename(StudentId = StudentId, student_FirstName = FirstName ,Student_LastName = LastName, student_Gender = Gender)
Teachers <- Teachers %>% rename(TeacherId = TeacherId, Teacher_FirstName = FirstName ,Teacher_LastName = LastName, Teacher_Gender = Gender)


### Get the whole table:
#df <- dbReadTable(con, "Classrooms")    ?????  למה לא עובד 

library(dplyr)

#### Questions
# Creating union data frame:
union1 <- full_join(Departments,Courses,by = "DepartmentID")
union2 <- full_join(union1,Classrooms,by = "CourseId")
union3 <- full_join(union2,Students, by = "StudentId")
union4 <- full_join(union3,Teachers, by = "TeacherId")


###############
### Q1. Count the number of students on each departmentֲ¶
###############

union4 %>% group_by(DepartmentName) %>% select(StudentId) %>% unique() %>% tally() 



###############
### Q2. How many students have each course of the English department and the 
###     total number of students in the department?
###############

# summarize by course name:
english_students_by_course <- union4 %>% filter(DepartmentName %in% "English") %>% 
  group_by(DepartmentName,CourseName) %>% select(StudentId) %>% unique() %>% summarise(cnt = n ()) 

#summarize by english department:
english_students_by_department <- union4 %>% filter(DepartmentName %in% "English") %>%
  group_by(DepartmentName) %>% select(StudentId) %>% unique() %>% summarise(cnt = n ()) 
 
# merge the english_students_by_department in to english_students_by_department:

Q2 <- english_students_by_course %>% 
  mutate(all_english_students = english_students_by_department$cnt)
Q2


###############
### Q3. How many small (<22 students) and large (22+ students) classrooms are 
###     needed for the Science department?
###############

# filter by Science department:

Science_students <- union4 %>% filter(DepartmentName %in% "Science") %>%
  group_by(DepartmentName,CourseName) %>% select(StudentId) %>% unique() %>% 
  summarise(cnt = n (), na.rm = T)

# classify the classes to big or small:

Science_students1 <- Science_students %>% mutate(Class_size = ifelse(Science_students$cnt < 22, "Small","Big"))

#counting the number of big and small classes:

Science_students1 %>% group_by(Class_size) %>% summarise(cnt = n ())
       

                                         
                                                
###############
### Q4. A feminist student claims that there are more male than female in the 
###     College. Justify if the argument is correct
###############

# למה לא עובד כשאני עובד על טבלה מאוחדת?????????????????????????????????????????????????

Teachears_gender <- Teachers %>% group_by(Teacher_Gender) %>% summarise(cnt = n(), na.rm = T)
Students_gender <- Students %>% group_by(student_Gender) %>% summarise(cnt = n(), na.rm = T)

Gender_ratio <- left_join(Students_gender,Teachears_gender)
Gender_ratio[,-3]



###############
### Q5. For which courses the percentage of male/female students is over 70%?
###############

corse_by_gender <- union4 %>% group_by(CourseName,student_Gender) %>% summarise(sum_by_gender = n (), na.rm = T) 
corse_all_student <- union4 %>% group_by(CourseName) %>% summarise(sum_all = n (), na.rm = T)

Q5 <- full_join(corse_by_gender,corse_all_student, by = "CourseName" ) %>%
  mutate(precentege = (sum_by_gender/sum_all)*100) %>% filter(precentege > 70) 

 
############################################################### second try:

gender_class <- (Courses %>% left_join(Classrooms, by = "CourseId") %>%
  left_join(Students, by = "StudentId")) %>% group_by(CourseName,student_Gender) %>%
  summarise(sum_gender = n(), na.rm = T) %>% mutate(precentege = (sum_gender/sum(sum_gender))*100) %>%
  filter(precentege > 70)
View(gender_class)



###############
### Q6. For each department, how many students passed with a grades over 80?
###############


Q6 <- union4 %>%  filter(degree > 80 ) 
Q6 <- Q6 %>% group_by(DepartmentName ) %>% select(StudentId) %>% unique() %>% summarise(cnt = n (), na.rm = T)
Q6 %>% rename(DepartmentName = DepartmentName, degree_over_80 = cnt)



###############
### Q7. For each department, how many students passed with a grades under 60?
###############

Q7 <- union4 %>% filter(degree < 60 )

Q7 <- Q7 %>% group_by(DepartmentName ) %>% select(StudentId) %>% unique() %>% 
  summarise(cnt = n (), na.rm = T)

Q7 %>% rename(DepartmentName = DepartmentName, degree_over_60 = cnt)



###############
### Q8. Rate the teachers by their average student's grades (in descending order).
###############


Q8 <- union4 %>% group_by(TeacherId,Teacher_FirstName,Teacher_LastName) %>%
  summarise(degree_mean = mean(degree, na.rm = T)) %>% arrange(desc(degree_mean))



###############
### Q9. Create a dataframe showing the courses, departments they are associated with, 
###     the teacher in each course, and the number of students enrolled in the course 
###     (for each course, department and teacher show the names).
###############


union4 %>% group_by(DepartmentName,CourseName,Teacher_FirstName,Teacher_LastName) %>%
  summarise(cnt = n (), na.rm = T)



###############
### Q10. Create a dataframe showing the students, the number of courses they take, 
###      the average of the grades per class, and their overall average (for each student 
###      show the student name).
###############


# finding the number of courses any student take:

Student_courses <- union4 %>%
  group_by(StudentId,student_FirstName,Student_LastName,CourseName) %>%
  summarise(Number_of_courses_they_take = n (), na.rm = T)

# finding every studets overall average:

overall_student_degree <- union4 %>%
  group_by(StudentId) %>% summarise(overall_degree_mean = mean(degree), na.rm = T)


# mean degree for any studet by  Department:

student_degree_by_Department <- union4 %>% group_by(StudentId,DepartmentName) %>%
  summarise(degree_mean = mean(degree))

student_degree_by_Department <- student_degree_by_Department %>%
  rename(StudentId = StudentId,DepartmentName = DepartmentName,student_department_degree = degree_mean)

# merge:

Q10 <- left_join(student_degree_by_Department,overall_student_degree, by = "StudentId") %>%
  left_join(Student_courses,degreeses, by = "StudentId")

view(Q10)







