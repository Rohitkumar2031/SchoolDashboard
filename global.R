library(shiny)
library(DT)
library(RMySQL)
library(DBI)
library(dplyr)


###################
get_data <- function(query, class, term, date) {
  conn <- dbConnect(RMySQL::MySQL(),
                    dbname="schoolcb",
                    user="root",
                    password="root",
                    host="localhost",
                    port=3306)
  
  if ( !missing('term')){
    stdd = print(term)
    query = gsub("TERM",stdd,query)
    sch1 <- data.frame(dbGetQuery(conn,paste0(query)))
    on.exit(dbDisconnect(conn))
    return (sch1)
  }
  
  else if ( !missing('class')){
    stdd = print(class)
    query = gsub("SC",stdd,query)
    sch1 <- data.frame(dbGetQuery(conn,paste0(query)))
    on.exit(dbDisconnect(conn))
    return (sch1)
  }
  
  else if ( !missing('date')){
    stdd = print(date)
    query = gsub("SD",stdd,query)
    sch1 <- data.frame(dbGetQuery(conn,paste0(query)))
    on.exit(dbDisconnect(conn))
    return (sch1)
  }
  
  else{
    a <- data.frame(dbGetQuery(conn, paste0(query)))
    
    on.exit(dbDisconnect(conn))
    return(a)
  }
}
##################
#query for dropdown & render table

queryDD <- "SELECT  t.studentClass studentclass from student_fee_order t inner join  studentclasses s on s.className = t.studentClass  GROUP BY studentClass order by classInNumeric "

queryuniqueterm <- "SELECT DISTINCT(termName) FROM marks_percentage"

queryTSD <- "SELECT DATE(MAX(attendanceDate))  date from teacher_attendance"

querySSD <- "SELECT DATE(MAX(attendanceDate)) date from studentattendance"

pendingrendertable <- "SELECT s.student_studentId Id ,st.studentName Name , concat(sc.classInNumeric,'th') Class,
s.balanceAmount Balance_Amount, s.totalPaidCurrentSession Total_Paid , s.totalAmount Total_Amount FROM student_fee_order s
INNER JOIN student st on s.student_studentId = st.studentId
INNER JOIN studentclasses sc on sc.className = s.studentClass
WHERE feePendingStatus = 'Pending'"

querystaff <- "SELECT concat(FIRST_NAME, MIDDLE_NAME, LAST_NAME) as Full_Name, t2.STAFF_PROFILE_TYPE_NAME Profile_Type ,  t1.MOB_NO Contact_no, t1.EMAIL_ID, t1.`status` FROM staffprofile t1 INNER JOIN staffprofiletype t2 on t1.STAFF_PROFILE_TYPE_ID = t2.STAFF_PROFILE_TYPE_ID WHERE t1.`status` = 'ACTIVE' AND t2.STAFF_PROFILE_TYPE_NAME != 'Parents'"

male <-"SELECT gender from student where `status`= 'ACTIVE'"
feemonth <- "SELECT COALESCE(SUM(transactionDetailAmt), 0) as total_amt from stu_fee_transaction_details
where MONTH(transactionDate) = MONTH(SYSDATE())"
studetail <- "SELECT studentId,studentName,gender,regYear,lastSchoolName,erpFatherName as FatherName,erpMotherName as MotherName from student where `status`= 'ACTIVE'"
cwise <- "SELECT T1.`status`, `year`, sectionName,section_sectionId,stuClass_classid, student_studentId,concat( T2.classInNumeric ,'th',' ',T1.sectionName) class, count(*) count FROM studentrollno T1
INNER JOIN studentclasses T2 ON T1.stuClass_classid  = T2.classid
WHERE T1.`status` = 'ACTIVE' 
GROUP BY T2.className"
feeday <- "SELECT COALESCE(SUM(transactionDetailAmt), 0) as total_amt from stu_fee_transaction_details where MONTH(transactionDate) = SYSDATE()"
swise <- "SELECT sum(paidCurrentSession) current_ssn_sum FROM stu_fee_transaction_details" 
lastwise <- "SELECT DATE(transactionDate) last_seven_date , sum(transactionDetailAmt) last_seven_days_collection from stu_fee_transaction_details WHERE DAYOFYEAR(transactionDate) BETWEEN (SELECT max(DAYOFYEAR(transactionDate))-7 from stu_fee_transaction_details) AND (SELECT max(DAYOFYEAR(transactionDate)) from stu_fee_transaction_details)GROUP BY DAYOFYEAR(transactionDate)"
cureg <- "SELECT COUNT(*) today_reg FROM registration
WHERE date = CURDATE()"
cureg1 <- "SELECT count(*) curent_month_reg from registration
WHERE MONTH(date) = MONTH(CURDATE())"
cureg2 <- "SELECT count(*) current_year_reg from registration
WHERE YEAR(date) = YEAR(CURDATE())"
taa <- "SELECT CONCAT(count(case when attendanceStatus ='Absent'  then 1 end),'/',(select count(case when s.`status`='ACTIVE' THEN 1 END ) TTL  from staffprofile s))total_absent from teacher_attendance WHERE attendanceDate = CURDATE()"
paa <- "SELECT CONCAT(count(CASE WHEN feePendingStatus = 'Pending' then 1 end),'/', count(feePendingStatus))  total from student_fee_order"
#create the data frame from above query 
#reporttec <- "SELECT name,staffProfile_STAFF_ID ,attendanceStatus from teacher_attendance where attendanceStatus = 'Absent'"
table <- "SELECT name,staffProfile_STAFF_ID as staffid,attendanceStatus from teacher_attendance where attendanceStatus = 'Absent' and DATE(attendanceDate) = CURDATE()"
table2 <- "SELECT s.student_studentId Id ,st.studentName Name , concat(sc.classInNumeric,'th') Class,
s.balanceAmount Balance_Amount, s.totalPaidCurrentSession Total_Paid , s.totalAmount Total_Amount FROM student_fee_order s
INNER JOIN student st on s.student_studentId = st.studentId
INNER JOIN studentclasses sc on sc.className = s.studentClass
WHERE feePendingStatus = 'Pending'"



get_class <- get_data(queryDD)

uniqueterm <- get_data(queryuniqueterm)

pending_fee_table <- get_data(pendingrendertable)


defaultTdate <- get_data(queryTSD)

defaultSdate <- get_data(querySSD)

renderstaff <- get_data(querystaff)
mcount <- get_data(male)
detail <- get_data(studetail)
classwise <- get_data(cwise)
monthfee <- get_data(feemonth)
dayfee <- get_data(feeday)
sectionwise <- get_data(swise)
lastmonthwsie <- get_data(lastwise)
addcur <- get_data(cureg)
addcur1 <- get_data(cureg1)
addcur2 <- get_data(cureg2)
taa1 <- get_data(taa)
paa1 <- get_data(paa)
table1 <- get_data(table)
table3 <- get_data(table2)

