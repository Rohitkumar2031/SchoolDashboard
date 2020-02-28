library(shiny)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinyjs)
library(shinyBS)
library(shinythemes)
library(flexdashboard)
ui<- tagList(
  useShinyjs(),
  dashboardPage(
  skin = "blue",
  dashboardHeader(title = "School Controlboard",tags$li(a(onclick = "openTab('tabItem33')",
                                                          href = NULL,
                                                          icon("home"),
                                                          title = "Homepage",
                                                          style = "cursor: pointer;"),
                                                        class = "dropdown",
                                                        tags$script(HTML("
                                         var openTab = function(tabItem33){
                                         $('a', $('.sidebar')).each(function() {
                                         if(this.getAttribute('data-value') == tabItem33) {
                                         this.click()
                                         };
                                         });
                                         }"))),
                  tags$li(a(onclick = "openTab('tabItem2')",
                                                              href = NULL,
                                                              icon("rupee-sign"),
                                                              title = "Fee Report",
                                                              style = "cursor: pointer;"),
                                                              class = "dropdown",
                                                              tags$script(HTML("
                                                  var openTab = function(tabItem2){
                                                  $('a', $('.sidebar')).each(function() {
                                                  if(this.getAttribute('data-value') == tabItem2) {
                                                  this.click()
                                                  };
                                                  });
                                                  }"))),
                  tags$li(a(onclick = "openTab('tabItem3')",
                            href = NULL,
                            icon("user-graduate"),
                            title = "Registration",
                            style = "cursor: pointer;"),
                          class = "dropdown",
                          tags$script(HTML("
                                                  var openTab = function(tabItem3){
                                                  $('a', $('.sidebar')).each(function() {
                                                  if(this.getAttribute('data-value') == tabItem3) {
                                                  this.click()
                                                  };
                                                  });
                                                  }"))),
                  tags$li(a(onclick = "openTab('tabItem4')",
                            href = NULL,
                            icon("users"),
                            title = "Registration",
                            style = "cursor: pointer;"),
                          class = "dropdown",
                          tags$script(HTML("
                                                  var openTab = function(tabItem4){
                                                  $('a', $('.sidebar')).each(function() {
                                                  if(this.getAttribute('data-value') == tabItem4) {
                                                  this.click()
                                                  };
                                                  });
                                                  }"))),
                  tags$li(a(onclick = "openTab('tabItem5')",
                            href = NULL,
                            icon("users-cog"),
                            title = "Registration",
                            style = "cursor: pointer;"),
                          class = "dropdown",
                          tags$script(HTML("
                                                  var openTab = function(tabItem5){
                                                  $('a', $('.sidebar')).each(function() {
                                                  if(this.getAttribute('data-value') == tabItem5) {
                                                  this.click()
                                                  };
                                                  });
                                                  }")))),
                 
  dashboardSidebar( disable = FALSE, width = NULL, collapsed = TRUE,
                    sidebarMenu(
                      menuItem("Home", tabName = "tabItem33", selected = T),
                      menuItem("Pending Fee", tabName = "tabItem2", icon = icon("th")),
                      menuItem("Class Allocation", tabName = "tabItem3", icon = icon("th")),
                      menuItem("Attendance", tabName = "tabItem4", icon = icon("th")),
                      menuItem("Management", tabName = "tabItem5", icon = icon("th")),
                      menuItem("Student Report Card", tabName = "tabItem6", icon = icon("th"))
                      
                    )
                    
  ),
  dashboardBody(
    tags$style(".small-box.bg-olive { background-color: white !important; color: black !important; }"),
    tags$head(tags$style(HTML("a {color: black}"))),
    tags$head( tags$style( HTML(".fa { font-size: 15px; }"))),
    tags$style(".fa-rupee-sign {color:#E87722}"),
    tags$style(".fa-chalkboard-teacher {color:#E87722}"),
    tags$style(".fa-hand-holding-usd {color:#E87722}"),
    tags$style(".fa-user-circle {color:#E87722}"),
    #tags$style(".fa fa-rupee-sign {color:#E87722}"),
    tags$head(tags$style(HTML('.info-box {min-height: 45px;} .info-box-icon {height: 45px; line-height: 45px; background-color: #339FFF;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    tags$script(HTML("
              var openTab = function(tabName){2031
              
                $('a', $('.sidebar')).each(function() {
                  if(this.getAttribute('data-value') == tabName) {
                    this.click()
                  };
                });
              }
            ")),
    tags$script(HTML("
               var openTab = function(tabName){2031
               
                 $('a', $('.sidebar')).each(function() {
                   if(this.getAttribute('data-value') == tabName) {
                     this.click()
                   };
                 });
               }
             ")),
    (tags$head(
      tags$style(HTML("
                      .my_class {
                      font-weight: bold;
                      color:white;
                      }"))
    )),
    frow1 <- fluidRow(
     # infoBoxOutput("pendingfee_", width = 3),
      #infoBoxOutput("classallotted_", width = 3),
     # infoBoxOutput("attendance_", width = 3),
      #infoBoxOutput("management", width = 3),
      valueBoxOutput("ibox1", width = 2),
     # infoBoxOutput("ibox1", width = 3),
     valueBoxOutput("ibox2", width = 2),
     valueBoxOutput("ibox3" , width = 2),
     valueBoxOutput("ibox4" , width = 2),
     valueBoxOutput("ibox5" , width = 2),
     valueBoxOutput("ibox6" , width = 2)),
     fluidRow(
       div(id='clickdiv',
     valueBoxOutput("ibox7" , width = 2)),
     div(id='clickdiv1',
     valueBoxOutput("ibox8" , width = 2))
    ),
    
    
    #---------------------------------------------
    tabItems(
      tabItem(tabName = "tabItem33",
              fluidRow(
                box(
                  title = "Fee Pending Status Class Wise",
                  status = "primary",
                  collapsible = TRUE,
                  width = 4,
                  plotlyOutput("pendingfeeBarout", height = "380px")
                  
                ),
                box(
                  title = "Class wise Pass Fail Count",
                  status = "primary",
                  collapsible = TRUE,
                  width = 4,
                  plotlyOutput("pass_fail_percentout", height = "380px")
                ),
                box(
                  title = "Total Staff Count",
                  status = "primary",
                  collapsible = TRUE,
                  width = 4,
                  plotlyOutput("ttl_emp_barout", height = "380px")
                )),
              fluidRow(
                box(
                  title = "Student Attendance",
                  status = "primary",
                  collapsible = TRUE,
                  width = 4,
                  plotlyOutput("stud_atd_barout", height = "380px")
                ),
                box(
                  title = "Teacher Attendance",
                  status = "primary",
                  collapsible = TRUE,
                  width = 4,
                  plotlyOutput("bargraphtaout", height = "380px")
                 )),
                fluidRow(
                h4("Student Information"),
                DT::dataTableOutput("mytable"))),
      #----------------------------------------------------
      tabItem(tabName = "tabItem2",
              frow2 <- fluidRow(
                box(
                  title = "Fee Pending Status",
                  status = "primary",
                  collapsible = TRUE,
                  width = 5,
                  plotlyOutput("pendingfeeBar", height = "400px")
                  
                ),
                
                box(
                  selectInput('sel', "Select the class", c(get_class$studentclass), selected = "FIVE", selectize = TRUE),
                  
                  title = "Fee Pending Status Percentage",
                  status = "primary",
                  width = 5,
                  collapsible = TRUE,
                  plotlyOutput("pendingfeepiegraph", height = "360px")
                )),
                fluidRow(
                box(
                  title = "Last 7 Days collection",
                  status = "primary",
                  collapsible = TRUE,
                  width = 5,
                  plotlyOutput("lastmonthwsie", height = "400px")
                ),
                  box(
                    title = "Total Fee Pending",
                    status = "primary",
                    collapsible = TRUE,
                    width = 5,
                    gaugeOutput("gauge",height = "400px")
                    
                  ))
                
              
      ),
      #----------------------------------------------
      tabItem(tabName = "tabItem3",
              frow3 <- fluidRow(
                box(
                  title = "Class Allocation Status",
                  status = "primary",
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("class_allot_bar", height = "400px")
                ),
                box(
                  title = "Class Allocation Percentage",
                  status = "primary",
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("class_allot_pie", height = "400px")
                )
                ),
              fluidRow(
                box(
                  title = "Class wise Total Student count ",
                  status = "primary",
                  collapsible = TRUE,
                  width = 4,
                  plotOutput("wise", height = "400px")
                ),
                box(
                  title = "Total Student",
                  status = "primary",
                  collapsible = TRUE,
                  width = 4,
                  plotlyOutput("mcount", height = "400px")
                  
                )
              )
                
                
              ),
      
      #-----------------------------------------------
      tabItem(tabName = "tabItem4",
              #frow4 <- fluidRow(
              #fluidPage(
              dateInput('Sselectdate',
                        label = 'select STUDENT date',
                        value = defaultSdate$date,
                        max = max(Sys.Date())),
              dateInput('Tselectdate',
                        label = 'select TEACHER date',
                        value = defaultTdate$date,
                        max = max(Sys.Date())),
              fluidRow(
              box(
                title = "Student Attendance",
                status = "primary",
                collapsible = TRUE,
                width = 4,
                plotlyOutput("stud_atd_bar", height = "400px")
              ),
              box(
                title = "Student Attendance Percentage",
                status = "primary",
                width = 4,
                plotlyOutput("stud_atd_pie", height = "400px")
                # )
              )),
             fluidRow(
                box(
                  title = "Teacher Attendance",
                  status = "primary",
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("bargraphta", height = "400px")
                ),
                box(
                  title = "Staff Attendance Percentage Count",
                  status = "primary",
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("piegraphta", height = "400px")
                )
              )
              
      ),
      #--------------------------------------------------
      tabItem(tabName = "tabItem5",
              frow5 <- fluidRow(
                box(
                  title = "Total Staff Member",
                  status = "primary",
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("ttl_emp_bar", height = "400px")
                ),
                box(
                  title = "Total Staff Member Percentage",
                  status = "primary",
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("ttl_emp_pie", height = "400px")
                )
              )
              
      ),
      #----------------report card---------------
      #fluidRow(
      tabItem(tabName = "tabItem6",
              box(
                selectInput("selectterm", "Select the TERM",uniqueterm$termName, selected = "TERM1", selectize = TRUE),
                title = "Class Report Card",
                status = "primary",
                width = "860px",
                collapsible = TRUE,
                plotlyOutput("bargraphrepo", height = "350px")
              ),
              #),
              
              #-------------------------------pass fail count----------------------
              #tabItem(tabName = "tabItem7",
              box(
                title = "Pass Fail Count",
                status = "primary",
                collapsible = TRUE,
                width = "860px",
                plotlyOutput("pass_fail_percent", height = "350px")
              ),
              #-----------------------------fee-----------------------------------
              tabItems(
                tabItem(tabName = "plot_tab",
                        frow19 <- fluidRow(
                          box(
                            title = "Total Staff Member",
                            status = "primary",
                            width = 4,
                            collapsible = TRUE,
                            infoBoxOutput("feee_", width = NULL)
                          ))
                        
                        
                )
              )
              
      ))
  )
)
)



#shinyApp(ui,server)
