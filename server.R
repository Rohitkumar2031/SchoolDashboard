library(data.table)
library(ggplot2)
library(plotly)
library(DT)
library(RMySQL)
library(DBI)
library(dplyr)

server<- function(input,output,session) {
  
  getdata <- function(query, class, term, date) {
   
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
  
  
  #############
  query1 <-  "SELECT concat(sc.classInNumeric, 'th') studentClass, count(s.feePendingStatus) Count FROM student_fee_order s INNER JOIN studentclasses sc on sc.className = s.studentClass WHERE feePendingStatus = 'Pending'GROUP BY studentClass ORDER BY sc.classInNumeric ASC"
  #---------------dynamic query-----------------#
  ###############
  query2 <- "SELECT studentId,studentName,studentClass,feePendingStatus,totalAmount,balanceAmount FROM student,student_fee_order where student.studentId=student_fee_order.student_studentId AND studentClass = 'SC' "
  
  query3 <- "SELECT studentId Student_ID,studentName Student_Name, isClassAndSecAllotted from student WHERE isClassAndSecAllotted ='NO'"
  query33 <- "SELECT  isClassAndSecAllotted, count(isClassAndSecAllotted) count from student WHERE isClassAndSecAllotted IN ('YES', 'NO') group BY isClassAndSecAllotted"
  
  query32 <- "SELECT  studentName,isClassAndSecAllotted from student WHERE isClassAndSecAllotted IN ('YES', 'NO')"
  
  query4 <- "SELECT studentId,studentName, isClassAndSecAllotted from student WHERE isClassAndSecAllotted ='NO' OR isClassAndSecAllotted = 'YES'"
  
  query5 <- "SELECT CONCAT(T3.classInNumeric, 'th',' ',T2.sectionCode) as Class_Name, COALESCE(pst.present_count,0) AS Present_Count,COALESCE(abs.Absent_count,0) AS Absent_Count,COALESCE(lev.leave_count,0) AS Leave_Count,(COALESCE(pst.present_count,0)) + (COALESCE(abs.Absent_count,0))  + (COALESCE(lev.leave_count,0)) AS Total FROM studentattendance AS T1 INNER JOIN section AS T2 ON T1.section_sectionId = T2.sectionId INNER JOIN studentclasses AS T3 ON T1.stuClass_classid = T3.classid LEFT JOIN( SELECT T1.stuClass_classid,T1.section_sectionId ,COUNT(attendanceStatus)  AS present_count from studentattendance AS T1 WHERE attendanceStatus IN ('Present') GROUP BY T1.stuClass_classid,T1.section_sectionId ) AS pst ON T3.classid = pst.stuClass_classid AND T2.sectionId = pst.section_sectionId LEFT JOIN( SELECT T1.stuClass_classid,T1.section_sectionId ,COUNT(attendanceStatus)  AS Absent_count from studentattendance AS T1 WHERE attendanceStatus IN ('Absent') GROUP BY T1.stuClass_classid,T1.section_sectionId ) AS abs ON T3.classid = abs.stuClass_classid AND T2.sectionId = abs.section_sectionId LEFT JOIN( SELECT T1.stuClass_classid,T1.section_sectionId ,COUNT(attendanceStatus)  AS leave_count from studentattendance AS T1 WHERE attendanceStatus IN ('Leave') GROUP BY T1.stuClass_classid,T1.section_sectionId ) AS lev ON T3.classid = lev.stuClass_classid AND T2.sectionId = lev.section_sectionId WHERE DATE(T1.attendanceDate)  = 'SD' GROUP BY T3.classname, T2.sectionCode ORDER BY T3.classInNumeric, T2.sectionCode"
  
  query6 <- "SELECT studentName, attendanceStatus FROM studentattendance WHERE DATE(attendanceDate) = 'SD' "
  
  query7 <- "SELECT T3.STAFF_PROFILE_TYPE_NAME,COUNT(*) AS  emp_count FROM (SELECT T1.STAFF_ID, CONCAT(T1.FIRST_NAME ,T1.MIDDLE_NAME ,T1.LAST_NAME) AS Full_Name, T2.STAFF_PROFILE_TYPE_NAME FROM staffprofile AS T1 INNER JOIN staffprofiletype AS T2 ON T1.STAFF_PROFILE_TYPE_ID = T2.STAFF_PROFILE_TYPE_ID WHERE T1.`status` = 'ACTIVE' and T2.STAFF_PROFILE_TYPE_NAME != 'Parents') AS T3 GROUP BY T3.STAFF_PROFILE_TYPE_NAME"
  
  query8 <- "SELECT T1.STAFF_ID, CONCAT(T1.FIRST_NAME ,T1.MIDDLE_NAME ,T1.LAST_NAME) AS Full_Name, T2.STAFF_PROFILE_TYPE_NAME FROM staffprofile AS T1 INNER JOIN staffprofiletype AS T2 ON T1.STAFF_PROFILE_TYPE_ID = T2.STAFF_PROFILE_TYPE_ID WHERE T2.STAFF_PROFILE_TYPE_NAME IN ('accountant', 'admin', 'Faculty', 'management', 'volunteer') AND T1.`status` = 'ACTIVE' ORDER BY T1.STAFF_ID"
  
  query9 <- "select t.teacherAttendId, t.attendanceDate, t.attendanceStatus, t.name, t.staffProfile_STAFF_ID, s.STAFF_PROFILE_TYPE_ID, st.STAFF_PROFILE_TYPE_NAME from teacher_attendance t INNER JOIN staffprofile s on t.staffProfile_STAFF_ID = s.STAFF_ID INNER JOIN staffprofiletype st on st.STAFF_PROFILE_TYPE_ID = s.STAFF_PROFILE_TYPE_ID WHERE DATE(t.attendanceDate) = CURDATE()"
  
  query91 <- "SELECT sp.STAFF_PROFILE_TYPE_NAME profile_name, t.attendanceStatus attendanceStatus FROM teacher_attendance t INNER JOIN staffprofile st on t.staffProfile_STAFF_ID = st.STAFF_ID INNER JOIN staffprofiletype sp on st.STAFF_PROFILE_TYPE_ID = sp.STAFF_PROFILE_TYPE_ID where st.`status` != 'INACTIVE'  AND DATE(t.attendanceDate) = 'SD'"
  
  query11 <- "SELECT mp.termName,concat(sc.classInNumeric,'th',' ',mp.sectionCode) class,COALESCE(ROUND((t2.pass_no / count(student_studentId)) * 100, 2),0) pass_percent,COALESCE(ROUND((t3.fail_no / count(student_studentId)) * 100, 2),0) fail_percent,CONCAT(COALESCE(ROUND((t3.fail_no / count(student_studentId)) * 100, 2),0),'%') FAILREND,CONCAT(COALESCE(ROUND((t2.pass_no / count(student_studentId)) * 100, 2),0),'%') PASSREND FROM marks_percentage mp INNER JOIN studentclasses sc on mp.className = sc.className LEFT JOIN  (SELECT mp.termName,mp.className,mp.sectionCode, count(student_studentId) pass_no FROM marks_percentage mp INNER JOIN studentclasses sc on mp.className = sc.className WHERE percentage > 50  GROUP BY termName, mp.className, sectionCode ORDER BY sc.classInNumeric) t2 on t2.termName = mp.termName AND t2.className = mp.className AND t2.sectionCode = mp.sectionCode LEFT JOIN (SELECT mp.termName,mp.className,mp.sectionCode, count(student_studentId) fail_no FROM marks_percentage mp INNER JOIN studentclasses sc on mp.className = sc.className WHERE percentage <= 50  GROUP BY termName, mp.className, sectionCode ORDER BY sc.classInNumeric) t3 on t3.termName = mp.termName AND t3.className = mp.className AND t3.sectionCode = mp.sectionCode WHERE mp.termName = 'TERM' GROUP BY mp.termName, mp.className, mp.sectionCode ORDER BY sc.classInNumeric"
  
  querygradecountstudent <- "Select termName,CONCAT(sc.classInNumeric,'th',' ',sectionCode) as className,sum(GRADE='A1') as A1,sum(GRADE='A2') as A2,sum(GRADE='B1') as B1,sum(GRADE='B2') as B2,sum(GRADE='C1') as C1,sum(GRADE='C2') as C2,sum(GRADE='D') as D, sum(GRADE='F') as F from marks_percentage  mp INNER JOIN studentclasses sc on sc.className = mp.className WHERE termName = 'TERM' GROUP BY termName,className,sectionCode"
  
  ###################### 
  #-------------creating dataframes--------------------#
  #####################
  
  
  fee_pending_status <- getdata(query1)
  
  fee_pi_percentage <- reactive({getdata(query2,class = input$sel)})
  
  class_allocation_status <- getdata(query33)
  class_allocation_status_no <- getdata(query3)
  class_allocation_status_pie <- getdata(query32)
  
  
  class_allot_percenteage <- getdata(query4)
  
  student_atdnc <- reactive({getdata(query5,date = input$Sselectdate )})
  
  student_atdnc_percent <- reactive({getdata(query6, date = input$Sselectdate)})
  
  total_employes <- getdata(query7)
  
  total_employes_percentage <- getdata(query8)
  
  
  teacher_attendence <- reactive({getdata(query9,date = input$Tselectdate )})
  
  teacher_atd_pie <- reactive({getdata(query91,date = input$Tselectdate )})
  
  pass_fail_percent <- reactive({getdata(query11, term = input$selectterm)})
  
  grade_count_student <- reactive({getdata(querygradecountstudent, term = input$selectterm)})
  
  #-------------------------infoboxes-------------------------
  output$pendingfee_<-renderInfoBox({
    infoBox(title = "Pending Fee",sum(fee_pending_status$Count),a("Click me", onclick = "openTab('tabItem2')", href="#"),icon = icon("rupee-sign","fa-1x"),width = 4,color = "olive")
    
  })
  output$classallotted_<-renderInfoBox({
    infoBox(title = "Class Allotted",length(class_allocation_status_no$isClassAndSecAllotted), a("Click me", onclick = "openTab('tabItem3')", href="#1"),icon = icon("users"),color = "olive")
  })
  output$attendance_<-renderInfoBox({
    infoBox(title = "Attendance", sum(student_atdnc()$Absent_Count), a("Click me", onclick = "openTab('tabItem4')", href="#2"), icon = icon("user-tie"), color = "olive")
  })
  
  output$management<-renderInfoBox({
    infoBox(title = "Administration",sum(total_employes$emp_count), a("Click me", onclick = "openTab('tabItem5')", href="#3"), icon = icon("users-cog"), color = "olive")
  })
  
  observeEvent(input$Home, {
    updateTabItems(session, "sidebar", "home")
  })
  #--------------------------valuebox-------------------------------
  
  output$feee_<-renderInfoBox({
    infoBox(title = "fee",sum(monthfee$total_amt), icon = icon("users-cog"), color = "olive")
  })
  #-----------------------Graph for pending fee--------------
  output$pendingfeeBar <- renderPlotly({
    p <- plot_ly(fee_pending_status,source = 'myclick', x = fee_pending_status$studentClass,y = fee_pending_status$Count, text = fee_pending_status$Count, key=paste('H1'), color = fee_pending_status$studentClass,
                 hoverinfo = 'none', textposition = 'outside', type = "bar")%>%
      layout(xaxis = list(title = "Class Name"),
             yaxis = list(title = "Fee Pending Status"))
  })
  
  selectebar <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = 'myclick'),{
    aa <- event_data("plotly_click", source = 'myclick')
    selectedclass = print(aa$x)
    pend<-pending_fee_table[pending_fee_table$Class == selectedclass,]
    downT1 <- print(pend)
    showModal(modalDialog(size = 'l',
                          
                          if(aa$x== selectedclass)
                            output$tab <- DT::renderDataTable(pend,rownames=FALSE,extensions = 'Buttons',options = list(
                              dom = 'Bfrtip',
                              buttons = c('copy','csv','excel','pdf','print')))
                          else if(aa$key=='H2')
                            output$tab <- DT::renderDataTable(classallotted_,rownames=FALSE)
                          else if(aa$key=='H3')
                            output$tab <- DT::renderDataTable(student_atdnc,rownames=FALSE)
                          else if(aa$key=='H4')
                            output$tab <- DT::renderDataTable(g,rownames=FALSE)
                          else if(aa$key=='H5')
                            output$tab <- DT::renderDataTable(teacher_attendence(),rownames=FALSE),
                          
    )) 
  })
  selectebar <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click", source = 'myclick1'),{
    aa1 <- event_data("plotly_click", source = 'myclick1')
    selectedprofile = print(aa1$x)
    pend1<-renderstaff[renderstaff$Profile_Type == selectedprofile,]
    downT2 <- print(pend1)
    showModal(modalDialog(size = 'l',
                          
                          if(aa1$x== selectedprofile)
                            output$tab <- DT::renderDataTable(pend1,rownames=FALSE,extensions = 'Buttons',options = list(
                              dom = 'Bfrtip',
                              buttons = c('copy','csv','excel','pdf','print')))
    ))
  })
  
  output$pendingfeepiegraph<-renderPlotly({
    
    pa <- plot_ly(fee_pi_percentage(),labels = fee_pi_percentage()$feePendingStatus ,key=paste('H1'), textposition= 'inside', type = 'pie')
  })
  
  #------------------Graph for class not allotted--------------
  
  output$class_allot_bar <- renderPlotly({
    p <- plot_ly(class_allocation_status, x = class_allocation_status$isClassAndSecAllotted, y = class_allocation_status$count,text = class_allocation_status$count, key=paste('H2'),hoverinfo = 'none', color = class_allocation_status$isClassAndSecAllotted, textposition = 'outside', type = "bar")%>%
      layout(xaxis = list(title = "Class Allocation Status"),
             yaxis = list(title = "Count of class Allocation Status"))
  })
  
  
  output$class_allot_pie<-renderPlotly({
    pa <- plot_ly(data = class_allocation_status_pie, labels = class_allocation_status_pie$isClassAndSecAllotted,key=paste('H2'), textposition='inside', type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  #--------------------Graph for student attendance----------------------
  output$stud_atd_bar <- renderPlotly({
    
    pa <- plot_ly(data=student_atdnc(), x = student_atdnc()$Class_Name, y = student_atdnc()$Present_Count, text = student_atdnc()$Present_Count, hoverinfo = 'none', textposition = "outside", type = 'bar',key=paste('H3'), name = 'Present Count') %>%
      add_trace(y = student_atdnc()$Absent_Count,text = student_atdnc()$Absent_Count, hoverinfo = 'none', textposition = "outside", name = 'Absent Count') %>%
      add_trace(y = student_atdnc()$Leave_Count,text = student_atdnc()$Leave_Count, hoverinfo = 'none', textposition = "outside", name = 'Leave Count') %>%
      layout(xaxis = list(title = "Class Name"),
             yaxis = list(title = 'Count'), barmode = 'group')
  })
  
  output$stud_atd_pie<-renderPlotly({
    
    pa <- plot_ly(data = student_atdnc_percent(), labels =student_atdnc_percent()$attendanceStatus,key=paste('H3'), type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #------------teacher attendence -------------
  
  output$bargraphta<-renderPlotly({
    pa <- plot_ly(teacher_attendence(), x = teacher_attendence()$profile_name, y = teacher_attendence()$countP, type = 'bar',key=paste('H5'),hoverinfo = 'none',text =  teacher_attendence()$countP, textposition = 'outside', name = 'Present') %>%
      add_trace(y = teacher_attendence()$countA, hoverinfo = 'none',text =  teacher_attendence()$countA, textposition = 'outside', name = 'Absent') %>%
      layout(yaxis = list(title = 'Count'),xaxis = list(title = 'Teacher'), barmode = 'group')
  })
  
  
  output$piegraphta<-renderPlotly({
    
    pa <- plot_ly(data = teacher_atd_pie(), labels =teacher_atd_pie()$attendanceStatus, key=paste('H5'), textposition = 'inside', type = 'pie') %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #---------------Graph for Staff Member---------------------
  
  output$ttl_emp_bar <- renderPlotly({
    p <- plot_ly(total_employes,source = 'myclick1', x = total_employes$STAFF_PROFILE_TYPE_NAME,y = total_employes$emp_count,text = total_employes$emp_count, key=paste('H6'), color = total_employes$STAFF_PROFILE_TYPE_NAME, hoverinfo = 'none', textposition = 'outside', type = "bar")%>%
      layout(xaxis = list(title = "Staff Profile"),
             yaxis = list(title = "No of Staff Member"))
  })
  
  output$ttl_emp_pie<-renderPlotly({
    
    pa <- plot_ly(data = total_employes_percentage, labels =total_employes_percentage$STAFF_PROFILE_TYPE_NAME,key=paste('H4'),textposition = 'inside', type = 'pie') %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #----------- student reportcard--------------------
  output$bargraphrepo<- renderPlotly({
    pa <- plot_ly(grade_count_student(), x = grade_count_student()$className, y = grade_count_student()$A1,name = 'A1', hoverinfo = 'none',text = grade_count_student()$A1, textposition = 'outside',marker = list(color = 'rgb(204,204,204)'), type = 'bar') %>%
      add_trace(y = grade_count_student()$A2,hoverinfo = 'none',text = grade_count_student()$A2, textposition = 'outside', name = 'A2') %>%
      add_trace(y = grade_count_student()$B1,hoverinfo = 'none',text = grade_count_student()$B1, textposition = 'outside',marker = list(color = 'rgb(58,200,225)'), name = 'B1') %>%
      add_trace(y = grade_count_student()$B2,hoverinfo = 'none',text = grade_count_student()$B2, textposition = 'outside', name = 'B2') %>%
      add_trace(y = grade_count_student()$C1,hoverinfo = 'none',text = grade_count_student()$C1, textposition = 'outside',marker = list(color = 'rgb(204,204,204)'), name = 'C1') %>%
      add_trace(y = grade_count_student()$C2,hoverinfo = 'none',text = grade_count_student()$C2, textposition = 'outside', name = 'C2') %>%
      add_trace(y = grade_count_student()$D,hoverinfo = 'none',text = grade_count_student()$D, textposition = 'outside',marker = list(color = 'rgb(58,200,2254)'), name = 'D') %>%
      add_trace(y = grade_count_student()$F,hoverinfo = 'none',text = grade_count_student()$F, textposition = 'outside', name = 'F') %>%
      layout(yaxis = list(title = 'Student_Count'),xaxis = list(title = 'Class'), barmode = 'group')
  })
  
  #---------------------pass fail plot-------------
  output$pass_fail_percent<- renderPlotly({
    pa <- plot_ly(pass_fail_percent(), x = pass_fail_percent()$class, y = pass_fail_percent()$pass_percent,name = 'Pass', hoverinfo = 'none',text = pass_fail_percent()$pass_percent, textposition = 'outside',marker = list(color = 'rgb(204,204,204)'), type = 'bar') %>%
      add_trace(y = pass_fail_percent()$fail_percent,hoverinfo = 'none',text = pass_fail_percent()$fail_percent, textposition = 'outside', name = 'Fail',marker = list(color = 'rgb(58,200,225)')) %>%
      layout(yaxis = list(title = 'Student_Count'),xaxis = list(title = 'Class'), barmode = 'group')
  })
  #-------------------------------------------Home Page graph--------------
  output$gauge = renderGauge({
    gauge(value=sum(pending_fee_table$Balance_Amount) ,
          min = 0,
          abbreviate =TRUE,
          #symbol = 'RS',
          max = sum(pending_fee_table$Total_Amount),
          sectors = gaugeSectors(success = c(0.5, 1),
                                 warning = c(0.3, 0.5),
                                 danger = c(0, 0.3)))
  })
  #--------------------------------------------------------------------------
  output$mcount = renderPlotly({
    p <- mcount %>%
      group_by(gender) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~gender, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$pendingfeeBarout <- renderPlotly({
    p <- plot_ly(fee_pending_status,source = 'myclick', x = fee_pending_status$studentClass,y = fee_pending_status$Count, text = fee_pending_status$Count, key=paste('H1'), color = fee_pending_status$studentClass,
                 hoverinfo = 'none', textposition = 'outside', type = "bar")%>%
      layout(xaxis = list(title = "Class Name", showlegend = F),
             yaxis = list(title = "Fee Pending Status"))
    
    
  })
   output$bargraphtaout<-renderPlotly({
    p <- plot_ly(teacher_attendence(), x = teacher_attendence()$STAFF_PROFILE_TYPE_NAME,y = teacher_attendence()$attendanceStatus, type = 'scatter', mode = 'lines', key=paste('H5'),name = 'Present',text =  teacher_attendence()$attendanceStatus,textposition = 'outside', fill = 'tozeroy') %>%
      add_trace(y = teacher_attendence()$attendanceStatus,text =  teacher_attendence()$attendanceStatus, textposition = 'outside',name = 'Absent',fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Teacher Attendance', showlegend = F),
             yaxis = list(title = 'Count'))

  })
  
  output$ttl_emp_barout <- renderPlotly({
    p <- plot_ly(total_employes,source = 'myclick1', x = total_employes$STAFF_PROFILE_TYPE_NAME,y = total_employes$emp_count,text = total_employes$emp_count, key=paste('H4'), color = total_employes$STAFF_PROFILE_TYPE_NAME, hoverinfo = 'none', textposition = 'outside', type = "bar")%>%
      layout(xaxis = list(title = "Staff Profile",showlegend = F),
             yaxis = list(title = "No of Staff Member"))
  })
  output$pass_fail_percentout<- renderPlotly({
    pa <- plot_ly(pass_fail_percent(), x = pass_fail_percent()$class, y = pass_fail_percent()$pass_percent,name = 'Pass', hoverinfo = 'none',text = pass_fail_percent()$PASSREND, textposition = 'outside', type = 'bar',marker = list(color = 'rgb(204,204,204)'),showlegend = T) %>%
      add_trace(y = pass_fail_percent()$fail_percent,hoverinfo = 'none',text = pass_fail_percent()$FAILREND, textposition = 'outside', name = 'Fail',marker = list(color = 'rgb(58,200,225)')) %>%
      layout(yaxis = list(title = 'Student_Count'),xaxis = list(title = 'Class'), barmode = 'group')
    
  })
  output$class_allot_barout <- renderPlotly({
    p <- plot_ly(class_allocation_status, x = class_allocation_status$isClassAndSecAllotted, y = class_allocation_status$count,text = class_allocation_status$count, key=paste('H2'),hoverinfo = 'none', color = class_allocation_status$isClassAndSecAllotted, textposition = 'outside', type = "bar")%>%
      layout(xaxis = list(title = "Class Allocation Status", showlegend = F),
             yaxis = list(title = "Count of class Allocation Status"))
  })
  output$stud_atd_barout <- renderPlotly({
    
    pa <- plot_ly(data=student_atdnc(), x = student_atdnc()$Class_Name, y = student_atdnc()$Present_Count, text = student_atdnc()$Present_Count, hoverinfo = 'none', textposition = "outside", type = 'bar',key=paste('H3'),marker = list(color = 'rgb(58,200,225)'), name = 'Present Count') %>%
      add_trace(y = student_atdnc()$Absent_Count,text = student_atdnc()$Absent_Count, hoverinfo = 'none', textposition = "outside",marker = list(color = 'rgb(204,204,204)'), name = 'Absent Count') %>%
      add_trace(y = student_atdnc()$Leave_Count,text = student_atdnc()$Leave_Count, hoverinfo = 'none', textposition = "outside",marker = list(color = 'rgb(0, 128, 255)'), name = 'Leave Count') %>%
      layout(xaxis = list(title = "Class Name", showlegend = F),
             yaxis = list(title = 'Count'), barmode = 'group')
  })
  
  output$mytable = DT::renderDataTable({
    detail
  })
  
  output$wise <- renderPlot({
    ggplot(classwise, aes(class,count, label = paste0(round(classwise$count)))) +
      geom_segment(aes(x=classwise$class, y = classwise$count, xend = classwise$class, yend = classwise$count), color = "blue") +
      geom_point( color="orange", size=7) +
      geom_text(color = "black", size = 4)+
      theme_light()
  })
  
  #----------------------------Fee gauge------------------------------------------
  output$ibox1 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(monthfee$total_amt, style = "font-size: 60%;"),
      subtitle = tags$p("Monthly fee Collection", style = "font-size: 80%;"),
      icon = icon("rupee-sign"),
      color = "olive",
      width = 2
    )
  })
  #-------------------------total absent t--------------------
  output$ibox7 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(taa1$total_absent, style = "font-size: 60%;"),
      subtitle = tags$p("Teacher Attendance", style = "font-size: 80%;"),
      icon = icon("chalkboard-teacher"),
      color = "olive",
      width = 2
    )
    
  })
  output$ibox8 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(paa1$total, style = "font-size: 60%;"),
      subtitle = tags$p("Total student fee Pending ", style = "font-size: 80%;"),
      icon = icon("hand-holding-usd"),
      color = "olive",
      width = 2
      
    )
  })

#---------------day wise fee----------------------------------------
  output$ibox2 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(dayfee$total_amt, style = "font-size: 60%;"),
      subtitle = tags$p("Today fee collection", style = "font-size: 80%;"),
      icon = icon("rupee-sign"),
      color = "olive",
      width = 2
    )
  })
  
  #-------------------------------------------------------------------------------
  output$ibox3 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(sectionwise$current_ssn_sum, style = "font-size: 60%;"),
      subtitle = tags$p("session wise fee collection", style = "font-size: 80%;"),
      icon = icon("rupee-sign"),
      color = "olive",
      width = 2
    )
  })

  output$lastmonthwsie = renderPlotly({
       p <- plot_ly(lastmonthwsie,source = 'myclick', x = lastmonthwsie$last_seven_date,y = lastmonthwsie$last_seven_days_collection, text = lastmonthwsie$last_seven_days_collection, key=paste('H1'), color = lastmonthwsie$last_seven_date ,
                    hoverinfo = 'none', textposition = 'outside', type = "bar")%>%
         layout(xaxis = list(title = "Last 7 Days", showlegend = F),
                yaxis = list(title = "Fee Collection"))
     
    
  })
  #--------------registration------------------------------
  output$ibox4 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(addcur$today_reg,style = "font-size: 60%;"),
      subtitle = tags$p("Today Registration", style = "font-size: 80%;"),
      icon = icon("user-circle"),
      color = "olive",
      width = 2
    )
  })
  
  #------------------------reg month-----------------------------
  output$ibox5 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(addcur1$curent_month_reg, style = "font-size: 60%;"),
      subtitle = tags$p("Monthly Registration", style = "font-size: 80%;"),
      icon = icon("user-circle"),
      color = "olive",
      width = 2
    )
  })
  #-------------------------year reg---------------------
  output$ibox6 <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(addcur1$curent_month_reg, style = "font-size: 60%;"),
      subtitle =tags$p ("Yearly Registration", style = "font-size: 80%;" ),
      icon = icon("user-circle"),
      color = "olive",
      width = 2
    )
  })
  
  #-----------------data table teacher------------------------
  
  onclick('clickdiv', showModal(modalDialog(
    title = "Teacher Attendance Report",
    renderDataTable(table1)
  )))
  
  #-----------------fee table------------------------
  
  onclick('clickdiv1', showModal(modalDialog(
    title = "Pendng Fee Report",
    renderDataTable(table3,rownames=FALSE)
  )))
  
}
