
### Financial Aid Calculator ###
### 11/18/2022 ###
### libraries
library(shiny)
library(DT)
library(scales)
library(tidyverse)
library(janitor)
library(shinydashboard)
library(shinyWidgets)
library(echarts4r)
library(plotly)
library(shinythemes)

 
 
### Import data set ###

acex_data_year <- readRDS("FTIC_FIN_DATA_YEAR_end202201.rds") # by year (filter duplicated id and year)
app2022_temp <- readRDS("app_2022_temp.rds") # decision code (C:Confirmed / O1:0-29 hrs offered: not confirmed) 
ftic_detail_2223_fill <- readRDS("ftic_detail_2223_fill.rds") 
# fin master data
acex_code <-  readRDS("acex_code_master.rds")
#PELL GRANTS
pell_grants <- read.csv("2022-2023_Pell_Grant_Scheduled_Award_Chart_download.csv") # cause issue


# begin
### UI

ui <- fluidPage( theme = shinytheme("cerulean"),
    navbarPage(title="Financial Aid Calculator", 
                 collapsible = TRUE,
                 # header = tags$head(
                 #     tags$style(HTML(
                 #         "
                 #         .navbar-default {
                 #     background-color: #004C97 !important;
                 # }
                 # 
                 #  .navbar-default:hover {
                 #      background-color: #8DC8E8 !important;
                 #          
                 #  }"
                 #     ))
                 # ),
                 
                 tabPanel(title = "Estimated Results - Student",
                          #cohort
                          
                          sidebarLayout(
                              sidebarPanel(   
                                  h4("Financial Need"),
                                  numericInput(inputId = "coa", label = "Cost of Attendance (COA)", 
                                               min = 0, max = 40000, value = 22624, step = 100),
                                  numericInput(inputId = "efc", label = "Expected Family Contribution (EFC)", 
                                               value=4000, min=0, max=14000, step = 100 ), #max6206
                                  #pell grants table
                                  downloadButton("downloadpell", "Pell Grant Table", icon = shiny::icon("download")),
                                  helpText( a( "Go to EFC Calculator", href="https://studentaid.gov/aid-estimator/", target = "_blank" )),
                                  
                                  
                                  h4("Student Type"),
                                  #count
                                  selectInput(
                                      inputId = "student_type",
                                      label = "Choose a Student Type",
                                      choices = c("Continuing","FTIC","Transfer"),
                                      selected = "FTIC"
                                  ),
                                  
                                  selectInput(
                                      inputId = "state_residency",
                                      label = "Choose a State of Residency",
                                      choices = c("Florida","Out of State","Alabama"), selected = "Florida"
                                  ),
                                  selectInput(
                                      inputId = "firstgen",
                                      label = "First-generation College Student",
                                      choices = c("No","Yes"), selected = "Yes"
                                  ),
                                  
                                  
                                  
                                  
                                  h4("Student Academics"),
                                  #count
                                  numericInput(
                                      inputId = "tier",
                                      label = "Choose a Tier",
                                      min = 1,
                                      max = 5,
                                      value = 3,
                                      step = 1
                                  ),
                                  #target prop
                                  numericInput(
                                      inputId = "target_need",
                                      label = "Target Need (.00)",
                                      min = 0,
                                      max = 1,
                                      value = 0.4,
                                      step = 0.05
                                  ),
                                  #merti aid
                                  conditionalPanel(
                                      
                                      # 'input.student_type != ""',
                                      condition =  'input.tier <= 3',
                                      selectizeInput("merit_name","Merit Aid", choices =NULL, selected="ACEF"),
                                      numericInput("merit_amount","Amount",value=3000, min=0, max = 10000),
                                      actionBttn("add", "Add"), actionBttn("delet","Delet All")
                                  ),
                                  
                                  
                                  
                                  # condition =
                                  # actionButton(
                                  #     inputId = "simulate", label = "Simulate", class = "btn btn-success action-button disabled"),
                                  
                                  tags$button("Restart", id = "restart", type= "button", class = "btn btn-danger action-button", onclick="history.go(0)" )
                              ),  # sidebar panel end
                              
                              
                              
                              #right side
                              mainPanel( style="background:white",
                                         fluidRow( box(width=6, solidHeader = T, title = "Total Aid Gauge",
                                                       h5("Showing the gap between estimated aid amount and target amount"),
                                                       plotly::plotlyOutput("gauge_aid", width = "85%")),
                                                   box(width=6,solidHeader = T, title = "Estimated Aids by Entry",
                                                       h5("Fund code and offer amount"),
                                                       echarts4r::echarts4rOutput("bar_aids_stu", width = "100%"))),
                                         hr(), 
                                         h3("Your Entry"),
                                         DT::dataTableOutput("test_df"), 
                                         hr(),
                                         h3("Florida Bright Futures Scholarships"),
                                         a( "Go to Bright Future Program Info", href="https://www.floridastudentfinancialaidsg.org/SAPBFMAIN/SAPBFMAIN", target = "_blank" ),
                                         numericInput(width = "30%", inputId = "fascredithours", label = "How many credit hours funded?", min = 1, max = 30, step = 1, value = 12),
                                         radioGroupButtons(inputId = "BFname", "Choose a scholarships", 
                                                           choices = c("Florida Academic Sch (FAS)"=1,"Florida Medallion Sch (FMS)"=2), selected = 1),
                                         DT::dataTableOutput("bf_amount_output_table"),
                                         hr(),
                                         h3("UWF Grants and Scholarships"),
                                         DT::dataTableOutput("total_amount_merit"),
                                         h3("Your Estimated Aid"),
                                         DT::dataTableOutput("aid_results")    
                                         
                                         
                                         
                              ) # main panel end
                          ) # sidebarLayout end
                 ),
                 tabPanel(
                     title = "Estimated Funds - UWF Projection 2022" ,
                     column(6,
                            
                            h3("Federal Grants"), 
                            DT::dataTableOutput("federaltable"),
                            h3("State Grants"),  
                            DT::dataTableOutput("statetable"),
                            h3("UWF Grants and Scholarships"),  
                            DT::dataTableOutput("uwfgranttable")),
                     column(6,
                            
                            h3("Financial Aid Amount") ,
                            echarts4rOutput("fund_plot", width = "700px")
                     )
                 ),
                 navbarMenu("Simple Funds Planner",
                            
                            tabPanel(title="ACEX/ACEF Planner",
                                     h3("Simple Finacial Aid Calculator (FAC)"),      
                                     h4("Actual & Eatimated ACEF(X) Award Counts and Amounts"), 
                                     column(6,radioButtons("fcstyear", label = "Choose a Planning Year",
                                                           choices = list("2023" = 1) ,# "2024" = 2 , "2025"=3, "2026"=4), 
                                                           selected = 1, inline = TRUE)),
                                     column(6, actionButton("showfacworks", "How FAC Works:",style= "backgroud-color: #007A33")),
                                     
                                     br(),
                                     
                                     
                                     fluidRow( 
                                         numericInput("acex_budget", label = "1. Budget Amount ($):", value = 3000000 ),
                                         numericInput("initialacex2023", label = "2. Initial Total Award Counts:", value = 300),
                                         radioGroupButtons("restorationvalue", label = "3. Choose Restoration Goal:",
                                                           #h3("Values from historical data: average, minimun, and maximun restoriaton rate (see the Restoration Goal table below)"),
                                                           choices = list("Average" = 1, "Maximun" = 2 , "Minimun"=3, "Custom"=4), 
                                                           selected = 1 ),
                                         conditionalPanel(
                                             
                                             # 'input.student_type != ""',
                                             condition =  'input.restorationvalue == 4',
                                             numericInput("aid2", label = "your case: aid 2", value = 0.7, step = 0.1, min=0, max=1),
                                             numericInput("aid3", label = "your case: aid 3", value = 0.8, step = 0.1, min=0, max=1),
                                             numericInput("aid4", label = "your case: aid 4", value = 0.5, step = 0.1, min=0, max=1)
                                             )
                                         ),
                                     
                                     
                                     hr(),br(),
                                     
                                     h4("ACEX/ACEF Award Restoration Goal"),  
                                     div( DT::dataTableOutput("acex_yoy_value_table",height = "auto")
                                     ),
                                     hr(),
                                     h4("ACEX/ACEF Award Count by Aid Year"),   
                                     div( DT::dataTableOutput("acex_awards",height = "auto")
                                     ), #, style = "font-size:100%;height:100%; width: 100%; overflow-y:scorll;overflow-x:scroll;"),
                                     hr(),
                                     #, style = "font-size:100%; width: 100%; overflow-y:scorll;overflow-x:scroll;"),
                                     
                                     
                                     column(4,  
                                            radioGroupButtons("estimatekeys", label = "4. Choose Initial Award Amount & Count by Tier (Aid 1)",
                                                              #h3("Values from historical data: average, minimun, and maximun restoriaton rate (see the Restoration Goal table below)"),
                                                              choices = list("Initial Amounts" = 1, "Initial Tier Proportion" = 2  ), 
                                                              selected = 1 ),
                                            conditionalPanel(
                                                # 'input.student_type != ""',
                                                condition =  'input.estimatekeys == 1',
                                                numericInput("t1", label = "Tier 1", value = 4000, step = 500, min=0, max=10000),
                                                numericInput("t2", label = "Tier 2", value = 3000, step = 500, min=0, max=10000),
                                                numericInput("t3", label = "Tier 3", value = 2000, step = 500, min=0, max=10000)
                                                
                                            ),      
                                            conditionalPanel(
                                                # 'input.student_type != ""',
                                                condition =  'input.estimatekeys == 2',
                                                numericInput("perctier1", label = "Tier 1", value = 0.2, step = 0.1, min=0, max=1),
                                                numericInput("perctier2", label = "Tier 2", value = 0.4, step = 0.1, min=0, max=1),
                                                numericInput("perctier3", label = "Tier 3", value = 0.4, step = 0.1, min=0, max=1)
                                                
                                            )
                                     ),    
                                     column(8,
                                            
                                            h5("Introducing Formulas and Definition of Terms"),
                                            tags$ul(  
                                                tags$li("Budget Amount: the amount of ACEF/X that will be awarded during the academic year (from first time aid to final year)"),
                                                tags$li("Initial Award Count (IC): initial counts from expected admissions in the ACEF scholarship consideraton process"),
                                                tags$li("Initial Award Amount (IA), Proportion Tier: annual award amounts and tier proportions based on total initial award count"),
                                                tags$li("Aid Year: number of repeated award year"),
                                                tags$li("First Aid Year Amount(Aid 1): sum of initiall year award amounts"),
                                                tags$li("Repeat Aid (Aid 2 to 4): number of repeating aid years (i.e. aid 1 is first year awarded year, aid2 is 2nd year awarded, etc.)"),
                                                tags$li("Restoration Rate: returning rate based on previous aid year counts (i.e. aid2 retention rate based on first year aid counts)"),
                                                tags$li("Expected Returning Counts (EC): previous award counts*restoration rate"),
                                                tags$li("Expected Award Amounts (EA): average of previously awarded amounts"),
                                                tags$li("Total Estimated Amount of ACEF/X: sum of expected returning counts*by expected award amounts for each aid year")),
                                            uiOutput("ex1")
                                     ), 
                                     br(),
                                     h4("Estimated ACEF/X Award Amount by Aid Year"),
                                     h5("5. Click any cell in the table to see additional details"),
                                     div(  
                                         
                                         DT::dataTableOutput("acex_yoy_amount_table",height = "300px")
                                     ), # style = "font-size:100%; width: 100%; overflow-y:scorll;overflow-x:scroll;")   
                                     h4("Gap between the budget and estimated amounts"),
                                     div(
                                         valueBoxOutput("gapbudget")
                                         
                                     )
                            )#,
                            
                            
                            # save for next fund type
                            # tabPanel(title = "FUND2",
                            #          h5("Summary ACEX data by Cohort and Year (Count)"),
                            #          #downloadButton("downloadTable", "Download Table", class = "btn-xs btn-info"),
                            #          div(DT::dataTableOutput("summary_data_table_count"), style = "font-size:100%; width: 100%; overflow-y:scorll;overflow-x:scroll;"),
                            #          br(),
                            #          h5("Summary ACEX Data by Cohort and Year ($)"),
                            #          div(DT::dataTableOutput("summary_data_table_dollar"), style = "font-size:100%; width: 100%; overflow-y:scorll;overflow-x:scroll;")
                            # )
                            
                            
                 ),
                 
                 inverse = T
                 
) #end ui
) #fuildpage


# Define server 
server <- function(input, output, session) {
    
    # formulas
    output$ex1 <- renderUI({
        withMathJax(helpText('
        $$Total\\ Estimated~Amount =  \\sum_{i=1}^n(IC_i*IA_i) + \\sum_{j=2}^m \\sum_{i=1}^n(EC_i*EA_i)$$',
                             '$$i: 1, 2,..., n~ Number~of~tier~system$$',
                             '$$j: 1, 2,..., m~ Number~of~aid~year$$'))
    })
    # budget gap
    output$gapbudget <- renderValueBox({
        valueBox(
            paste0("GAP:  ",scales::dollar(input$acex_budget - acef_proj_amount()$Fcst2023[5])), "" 
            # icon = icon("")
            
        )
    })
    
    
    
    ## to get retention values
    fin_dtl <- reactive({
        
        # fiter fund/ 20222023 included fall and summer
        acex_code <- acex_code
    })
    
    
    
    # case values: restoration historical values
    acex_yoy_value <- reactive({
        
        #restoration
        df_acex_YoY <- # acex_code %>% 
            fin_dtl()  %>% 
            group_by(  first_year_award, aid_year) %>% 
            summarise(count=n(), .groups = "drop") %>% 
            pivot_wider(names_from = c(first_year_award), values_from = c(count), names_prefix = "act_") %>% 
            mutate( FTIC2017 =act_2017/lag(act_2017 , default = act_2017 [1])) %>% 
            mutate( FTIC2018 =act_2018/lag(act_2018 , default = act_2018 [1])) %>%
            mutate( FTIC2019 =act_2019/lag(act_2019 , default = act_2019 [1])) %>%
            mutate( FTIC2020 =act_2020/lag(act_2020 , default = act_2020 [1])) %>%
            mutate( FTIC2021 =act_2021/lag(act_2021 , default = act_2021 [1])) %>%
            mutate( FTIC2022 =act_2022/lag(act_2022 , default = act_2022 [1])) %>%
            select(1,8:13) 
        
        #average, min, and max restoration values
        ave_aid <- df_acex_YoY %>%  
            pivot_longer(c(2:7), names_to = "first_year", values_to = "restoration") %>% 
            group_by(aid_year) %>% 
            summarise(
                ave_restoration = round(mean(restoration, na.rm=T),2),
                max_restoration = round(max(restoration,  na.rm=T),2),
                min_restoration = round(min(restoration,  na.rm=T),2)) %>% 
            mutate( #select_restoration = c(1, 0.7, 0.8,0.5) ),
                select_restoration = c(1, input$aid2, input$aid3,input$aid4))
        
        df_acex_YoY1 <- df_acex_YoY %>%
            mutate(ave_resto.=  ave_aid$ave_restoration)  %>% # not updating
            mutate(max_resto.=  ave_aid$max_restoration)  %>%
            mutate(min_resto.=  ave_aid$min_restoration)  %>%
            mutate(select_resto. = c(1, input$aid2, input$aid3,input$aid4)) %>% data.frame()# stop here
        
        
    })
    
    # restoration select values
    output$acex_yoy_value_table <- DT::renderDataTable({
        
        DT::datatable( acex_yoy_value() , 
                   extensions = c("Buttons", 'FixedHeader'),
                   class = 'cell-border stripe',
                   
                   options = list(searching = FALSE)
                   
        ) %>% 
            formatStyle(c(8:11), color="blue",backgroundColor = "orange", fontWeight = "bold") %>% 
            formatRound(c(2:7), digits = 4)
    }) 
    
    
    # 
    # ### tab name: acex_awards
    # acex_table <- reactive({
    #     
    #     acex_tab <- data.frame(
    #         
    #         #Tier = numeric(),
    #         First_Aid_Year = numeric(),
    #         count_aid1 = numeric(),
    #         count_aid2 = numeric(),
    #         count_aid3 = numeric(),
    #         count_aid4 = numeric())
    #     
    #     trans_aid_table <- axex_aid_year
    #     
    #     
    # })
    
    
    
    ### Applied selected values :ACEF Counts
    proj_acef_count <- reactive({
        
        act_count_df <-   fin_dtl()  %>% 
            #acex_code %>%
            mutate(act_year = substr(REPT_TIME_FRAME, 1,4)) %>%
            arrange(act_year) %>% 
            group_by( act_year, aid_year ) %>% #, act_year) %>% 
            summarise(count=n(), .groups = "drop") %>% 
            pivot_wider(names_from = c(act_year), values_from = c(count), names_prefix = "Act")
        
        # selected goals
        if(input$restorationvalue == 1){
            #goal =    df_acex_YoY1$min_resto. 
            goal =  acex_yoy_value()$ave_resto.   
        }else if(input$restorationvalue == 2){
            goal = acex_yoy_value()$max_resto. 
        }else if(input$restorationvalue == 3){
            goal =  acex_yoy_value()$min_resto. 
        }else{
            goal =  acex_yoy_value()$select_resto. 
        } 
        
        # capture last value
        lastValue <- function(x)   tail(x[!is.na(x)], 1)
        # mycase =  as.integer(c(300,  apply(act_count_df, 1, lastValue)[1:3]))
        mycase =  as.integer(c(input$initialacex2023,  apply(act_count_df, 1, lastValue)[1:3]))
        #Fcst2023 =   mycase*goal
        act_count_df2 <-    act_count_df %>%  mutate(Fcst2023 =  round(mycase*goal[1:4],0))
        
        
        # output table
        proj_acef_count2 <- act_count_df2 %>% 
            janitor::adorn_totals("row")
    })
    
    
    
    output$acex_awards <- DT::renderDataTable({    # 
        # first table
        DT::datatable(proj_acef_count(), extensions = c("Buttons", 'FixedHeader'),
                  class = 'cell-border stripe',
                  options = list(
                      #dom="Blfrtip", 
                      scrollX=TRUE,scrollY=TRUE,searching = FALSE 
                      #buttons=c("copy","csv","excel","pdf","print"),
                      #lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")), pageLength=27
                  )
        ) %>% 
            formatStyle(c(8), color="blue",backgroundColor = "orange", fontWeight = "bold") #%>% 
        #ormatRound(c(8), digits = 0)
        
    }) 
    
    
    # ACEF 
    acef_proj_amount <- reactive({
        # average
        df_acex_yoy_amount <-    fin_dtl()  %>% 
            #acex_code %>% 
            filter(REPT_TIME_FRAME != 20222023) %>% 
            # mutate(year_amount2 = ifelse(REPT_TIME_FRAME  == 20222023 , year_amount*2, year_amount)) %>% 
            mutate(act_year = substr(REPT_TIME_FRAME, 1,4)) %>%
            group_by(   aid_year) %>% 
            summarise( ave_amount=round( mean(year_amount),0), .groups = "drop") #%>% 
        #pivot_wider(names_from = c(act_year), values_from = c(ave_amount), names_prefix = "Act_")  
        
        #projected 2023
        df_acex_amount <-   fin_dtl()  %>% 
            #acex_code %>% 
            mutate(year_amount2 = ifelse(REPT_TIME_FRAME  == 20222023 , year_amount*2, year_amount)) %>% 
            mutate(act_year = substr(REPT_TIME_FRAME, 1,4)) %>% 
            group_by(  act_year , aid_year) %>% 
            summarise( total_amount=round( sum(year_amount2),0), .groups = "drop") %>% 
            pivot_wider(names_from = c(act_year), values_from = c(total_amount), names_prefix = "Act_")  
        
        #df_acex_amount$Fcst2023 =   proj_acef_count()$Fcst2023[1:4]*df_acex_yoy_amount$ave_amount
        
        
        proj_2023 <-   tab_by_data() %>% mutate(aid_year= repeat_aid) %>% 
            group_by(aid_year) %>% summarise(Fcst2023 = sum(proj.amount))
        # final table
        df_acex_yoy_final <-    df_acex_amount %>% left_join(proj_2023, by="aid_year") %>% 
            janitor::adorn_totals("row")
        
    }) 
    
    
    output$acex_yoy_amount_table <- DT::renderDataTable({ 
        # first table
        DT::datatable(acef_proj_amount(), 
                  callback = JS("table.on('click.dt', 'td', function() {
                               Shiny.onInputChange('click', Math.random());
                });"), 
                  selection = 'none',
                  extensions = c("Buttons", 'FixedHeader'),
                  class = 'cell-border stripe',
                  options = list(
                      #dom="Blfrtip", 
                      scrollX=TRUE,scrollY=TRUE,searching = FALSE 
                  )
        ) %>% formatStyle(c(7), color="blue",backgroundColor = "#DAF7A6", fontWeight = "bold") %>% 
            formatStyle(c(8), color="blue",backgroundColor = "orange", fontWeight = "bold") %>% 
            formatRound(c(2:8), digits = 0) %>% # checking format style
            formatCurrency(c(2:8),
                           currency = "$",
                           interval = 3,
                           mark = ",",
                           digits = 0,
                           dec.mark = getOption("OutDec"),
                           before = TRUE,
                           zero.print = NULL,
                           rows = NULL
            )
        
    }) 
    
    ### popup for FAC steps
    # define modal
    howFACworksModal <- function() {
        modalDialog(title = "How Financial Aid Calculator (FAC) works:", size ="l",
                    easyClose = TRUE,
                    pre(includeHTML("howFACworks.html"))
        )
    }
    
    observeEvent(input$showfacworks, {
        #print("Clicked!")
        removeModal()
        showModal(howFACworksModal())
    })
    
    
    
    ### popup table - projected amounts
    
    # define modal
    tableModal <- function() {
        modalDialog(title = "2023 Estimated ACEF/X Amounts by Aid Year & Tier", size = "l",
                    dataTableOutput("tab_by")
        )
    }
    
    observeEvent(input$click, {
        #print("Clicked!")
        removeModal()
        showModal(tableModal())
    })
    
    tab_by_data <- reactive({
        
        # selected goals
        if(input$restorationvalue == 1){
            #goal =    df_acex_YoY1$min_resto. 
            goal =  acex_yoy_value()$ave_resto. 
        }else if(input$restorationvalue == 2){
            goal = acex_yoy_value()$max_resto. 
        }else if(input$restorationvalue == 3){
            goal =  acex_yoy_value()$min_resto. 
        }else{
            goal =  acex_yoy_value()$select_resto. 
        } 
        # apply goals
        goal_aid <- data.frame(goal, "repeat_aid2"=c("Aid1", "Aid2","Aid3","Aid4")) 
        new_aid1 <- data.frame("repeat_aid" = c("Aid1", "Aid1", "Aid1"), "amount.tier"=c(1,2,3),
                               "expected.amount" = c(input$t1,input$t2,input$t3), "perc.tier" = c(input$perctier1, input$perctier2, input$perctier3),
                               "expected.count"= c(round(input$initialacex2023*input$perctier1,0), 
                                                   round(input$initialacex2023*input$perctier2,0), 
                                                   round(input$initialacex2023*input$perctier3,0)))
        # data shape
        #all by aid year
        tab2023_ratio <- fin_dtl() %>% 
            #acex_code %>%   
            filter(REPT_TIME_FRAME == 20222023) %>% mutate(repeat_aid2 = paste("Aid", repeat_aid + 1, sep="")) %>% # collect aid 1 to 3 (aid4 dismissed)
            mutate(year_amount = ifelse(REPT_TIME_FRAME  == 20222023 , year_amount*2, year_amount)) %>% 
            mutate(year_amount = ifelse(year_amount > 5000, 5000, year_amount))  %>% group_by(repeat_aid2) %>% 
            summarise(total_count= n()) %>% select(repeat_aid2, total_count)
        # merge
        tab2023 <- fin_dtl() %>% 
            #acex_code %>% 
            mutate(year_amount = ifelse(REPT_TIME_FRAME  == 20222023 , year_amount*2, year_amount)) %>% 
            mutate(year_amount = ifelse(year_amount > 5000, 5000, year_amount)) %>% 
            filter(REPT_TIME_FRAME == 20222023 & repeat_aid < 4) %>%  # award year 2022 from aid 1 to 3
            mutate(repeat_aid2 = paste("Aid", repeat_aid + 1, sep="")) %>%  # project 2023
            mutate(year_amount2 = round( year_amount, -3)) %>% 
            group_by(  repeat_aid2 , year_amount2) %>%
            summarise(count=n(), expected.amount=round(mean(year_amount),0), .groups = "drop") %>% unique() %>%
            group_by(repeat_aid2) %>% arrange(repeat_aid2, -year_amount2) %>% 
            mutate(amount.tier = row_number()) %>% left_join(tab2023_ratio, by ="repeat_aid2") %>%  # joined aid 1 to 3 to project aid 2 to 4
            mutate(perc.tier= round( count/total_count, digits = 3)) %>% 
            left_join(goal_aid, by="repeat_aid2") %>% mutate(count_goal= round(total_count*goal,0)) %>% # restoration rate
            mutate(count_goal2 = round(count_goal*perc.tier, 0)) %>% 
            select("repeat_aid"=repeat_aid2, amount.tier, expected.amount, perc.tier,"expected.count"=count_goal2)
        ## stop here need add total sum for popup table
        
        proj_tab <- rbind(new_aid1, tab2023) %>% mutate(perc.tier = paste0(perc.tier*100, "%",sep="")) %>% 
            mutate(proj.amount = round(expected.amount*expected.count, 0))  
        
    })
    # 2023 estimation results
    output$tab_by <- DT::renderDataTable({
        
        tab_by_data_final <-  tab_by_data()  %>% mutate_at(c(1:4), as.character) %>% 
            janitor::adorn_totals(where = "row", name = "Total Estimation")
        
        DT::datatable(tab_by_data_final,
                  extensions = c("Buttons", 'FixedHeader'),
                  filter = "top",
                  options = list(
                      dom="Blfrtip", 
                      scrollX=TRUE,scrollY=TRUE,searching = FALSE, 
                      buttons=c("copy","csv","excel","pdf","print"),
                      lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")), pageLength=27)) %>% 
            formatCurrency(c(3,6),
                           currency = "$",
                           interval = 3,
                           mark = ",",
                           digits = 0,
                           dec.mark = getOption("OutDec"),
                           before = TRUE,
                           zero.print = NULL,
                           rows = NULL)
        
        
    })
    
    
    
    
    
    
    ############## A Student Aid Calculator ########
    ### Pell and EFC 2022
    EFC <- seq(0,6200,100)
    EFC1 <- c(EFC, 6206, 6300,9309,14000)
    Pellgrant <- seq(745, 6905,100)
    Pellgrant1 <- c(0,0,0, 692, Pellgrant, 6895)
    EstimatedPell2022 <- rev(Pellgrant1)
    
    Pell_Efc_Chart <- data.frame(EFC1, EstimatedPell2022) %>% mutate(rownum= row_number())
    
    # Download Pell chart 2022-23
    output$downloadpell <- downloadHandler(
        filename = function(){paste0( "2022-2023_Pell_Grant_Scheduled_Award_Chart_download", Sys.Date(), ".csv", sep="")},
        content = function(file){
            write.csv(pell_grants, file)
        }
    )
    
    ### reactive data frame with input parameters
    
    # par_df <- reactiveValues()
    #input data frame with parameters
    par_df <- reactive({
        
        df <- data.frame(
            coa = numeric(),
            efc = numeric(),
            tier = numeric(),
            stu_type = character(),
            residency = character(),
            target_need = numeric(),
            first_gen = character()
        )
        
        input_df <- data.frame( coa = input$coa, efc= input$efc,  tier = input$tier, 
                                stu_type = input$student_type, residency = input$state_residency,
                                target_need = input$target_need,
                                first_gen =input$firstgen)
        par_df  <- rbind( df , input_df)
        
    })
    
    ## calculation fund
    #entry data frame (skip inputs)
    aid_need_data_frame <- reactive({
        
        aid_need <-   par_df() %>% 
            mutate(aid_need = ifelse(coa - efc >= 0, coa-efc, 0)) %>% 
            mutate(target_aid = aid_need*target_need)  
    })
    # your entry table
    output$test_df <-  DT::renderDataTable({
        entry_tab <-  aid_need_data_frame()
        names(entry_tab) <- toupper(names(entry_tab))
        DT::datatable(entry_tab) %>% formatCurrency(c("COA","EFC","AID_NEED","TARGET_AID"), currency = "$", digits = 0)
        
    })
    
    pell_cal <- reactive({
        efc_est <- round(input$efc ,-2) # 100
        
        pell_est <- if(input$efc < 100){
            Pell_Efc_Chart %>% 
                filter(EFC1 == 0 ) %>% 
                filter(EstimatedPell2022 == min(EstimatedPell2022 ))
        }else if(input$efc <= 6200 & input$efc >= 100){ 
            Pell_Efc_Chart %>% 
                filter(EFC1 <= efc_est ) %>% 
                filter(EstimatedPell2022 == min(EstimatedPell2022 )) 
        }else if(input$efc > 6200 & input$efc <= 6206){
            Pell_Efc_Chart %>% filter(EFC1 == 6206)
        }else if(input$efc >= 6207){
            Pell_Efc_Chart %>% filter(EFC1 == 6300)
        }
        
        # Pell table  
        Fund_Code = "PELL"
        amount= pell_est[,2[1]]
        pell_cal <- data.frame(Fund_Code, amount)
        
    })
    
    observe({
        if(input$student_type == "FTIC" & input$tier <= 3){
            updateSelectizeInput(session,"merit_name", "Merit Aid", choices = c("ACEF","NAUF"))
        }else if(input$student_type == "Transfer" & input$tier <= 3){
            updateSelectizeInput(session, "merit_name", "Merit Aid", choices = c("ARG","NATR"))
        }else if(input$student_type == "Continuing" & input$tier <= 3){
            updateSelectizeInput(session, "merit_name", "Merit Aid", choices = c("ACEX","NAUT"))
        } 
    })
    
    # ACEF ARG
    merit_name_aid <- reactive({
        merit_names <- c("ACEF","ACEX","ARG","NATR","NAUT")
        merit_name <- if(input$merit_name %in% merit_names & input$tier <= 3){
            name <-  input$merit_name 
            amount <- input$merit_amount 
            merit_df <- data.frame("Fund_Code"=name, "amount"=amount)
        }else{
            merit_df <- data.frame("Fund_Code"="None", "amount"=0)
        }
    })
    # reactive added merit df
    valuett  <- reactiveValues(df=NULL)
    
    # merit aids
    observeEvent( input$tier >=4,{
        valuett$df <- merit_name_aid()
        # new_merit <- data.frame(Fund_Code = input$merit_name,amount=input$merit_amount)
        # added_merits$df  <- rbind(new_merit,  merit_name_aid())
        # print(added_merits$df)
        
    })
    observeEvent(input$add,{
        valuett$df <- rbind( valuett$df,data.frame(Fund_Code = input$merit_name,amount=input$merit_amount)) %>% 
            filter(Fund_Code != "None")
    })
    observeEvent(input$delet,{
        valuett$df <-  data.frame(Fund_Code = "None",amount=0)
    })
    #additional merit aid
    output$total_amount_merit <- DT::renderDataTable({
        m_aid <-  valuett$df %>% filter(!duplicated(Fund_Code)) %>% 
            janitor::adorn_totals("row")
        names(m_aid) <- toupper(names(m_aid))
        DT::datatable(m_aid) %>%  formatCurrency("AMOUNT", currency = "$", digits = 0)
    })  
    
    
    
    #auto package
    total_amount_auto <- reactive({
        #results_df <- rbind(pell_cal(),acef_cal(),naut_cal()) %>% janitor::adorn_totals("row")
        results_df <-  valuett$df %>% filter(!duplicated(Fund_Code)) 
        
    }) 
    
    #SEOF/SEOT/SEOG/FSAG/FGMF
    all_aid <- reactive({
        
        #auto package total amount
        auto_total <-   total_amount_auto() %>%  
            janitor::adorn_totals("row") %>% 
            filter(Fund_Code =="Total")
        #sub need  1
        need_after_auto <- if(input$coa >= input$efc){
            aid_need <-   aid_need_data_frame()$target_aid - pell_cal()$amount - auto_total$amount
        }else if(input$coa < input$efc){
            value <- 0
        }
        
        aids <- 
            if(input$student_type=="FTIC"  ){ 
                
                aid_need_data_frame() %>% 
                    mutate(SEOF = 
                               ifelse(need_after_auto >= 2000  & input$efc==0, 2000,
                                      ifelse(need_after_auto >=  200  & input$efc==0, need_after_auto , 0 ))) %>%
                    mutate(FGMF =
                               ifelse( input$efc <= 9309 & input$firstgen == "Yes", 2000, 0)) %>% 
                    mutate(FSGF = 
                               ifelse( efc <= 9309 &  residency =="Florida", 3200, 0)) %>% 
                    mutate(FAGF =
                               ifelse(need_after_auto-SEOF >= 3000 & efc <= 14000 & residency =="Florida", 3000,
                                      ifelse(need_after_auto-SEOF >= 200 & efc <= 14000 & residency =="Florida", 
                                             need_after_auto-SEOF , 0))) %>% 
                    mutate(EHNF=
                               ifelse(need_after_auto-SEOF >= 3000 &  residency =="Alabama", 3000,
                                      ifelse(need_after_auto-SEOF >= 6000 &  residency =="Out of State", 6000,       
                                             ifelse(need_after_auto-SEOF >= 200 & (residency =="Alabama" | residency =="Out of State"),
                                                    need_after_auto-SEOF-FAGF , 0)))) %>% 
                    mutate(GAP =
                               ifelse(need_after_auto-SEOF-FAGF-EHNF >= 8000, 8000,
                                      ifelse(need_after_auto-SEOF-FAGF-EHNF >= 200, 
                                             need_after_auto-SEOF-FAGF-EHNF,  0))) %>% 
                    mutate(GAP = ifelse(tier >= 4 & residency == "Out of State", 0, GAP )) %>% 
                    pivot_longer(cols = c(SEOF,FGMF, FSGF,FAGF,EHNF,GAP), names_to = "Fund_Code", values_to = "amount") %>% 
                    select(Fund_Code, amount) 
                
            }else if(input$student_type=="Transfer" ){
                aid_need_data_frame() %>% 
                    mutate(SEOT =  
                               ifelse(need_after_auto >= 2000 & input$efc== 0 ,2000,
                                      ifelse(need_after_auto >=  200 & input$efc== 0 , need_after_auto , 0 ))) %>% 
                    mutate(FSGT = 
                               ifelse( efc <= 9309 &  residency =="Florida", 3200, 0)) %>% 
                    mutate(FAGT =
                               ifelse(need_after_auto-SEOT-FSGT  >= 3000 & efc <= 14000 & residency =="Florida", 3000,
                                      ifelse(need_after_auto-SEOT-FSGT >= 200 & efc <= 14000 & residency =="Florida", need_after_auto--SEOT-FSGT , 0))) %>%
                    pivot_longer(cols = c(SEOT,FSGT,FAGT), names_to = "Fund_Code", values_to = "amount") %>% 
                    select(Fund_Code, amount)
                
            }else if(input$student_type=="Continuing" ){
                aid_need_data_frame() %>% 
                    mutate(FSAG = case_when(input$efc <= 9309 & residency =="Florida" ~3200)) %>% 
                    mutate(SEOG =  
                               ifelse(need_after_auto - FSAG >= 2000 & input$efc== 0 ,2000,
                                      ifelse(need_after_auto - FSAG >=  200 & input$efc== 0, need_after_auto , 0 ))) %>% 
                    pivot_longer(cols = c(FSAG,SEOG), names_to = "Fund_Code", values_to = "amount") %>% 
                    select(Fund_Code, amount)
            } 
        
     
    })
    
    # final aid data  / used total gauge amount
    total_funds <- reactive({
        results_df <- rbind(pell_cal(),total_amount_auto(),all_aid()) %>%
            janitor::adorn_totals("row") %>% # rest of fund codes
            filter(!duplicated(Fund_Code)) %>% filter(Fund_Code != "None") %>%
            mutate(amount = ifelse(amount < 0, 0, amount)) %>% 
            filter(amount != 0) 
        
    }) 
    
    
    
    # Bright future sch - table
    
    bf_amount_input_table <- reactive( {
        #create data headers
        bf <- data.frame( 
            FUND_CODE=character(),  FundedCreditHours=numeric(), CostHours= numeric(), AMOUNT=numeric() 
        )
        
        if(input$BFname == 1){
            fas_input <- data.frame(FUND_CODE ="FAS", FundedCreditHours = input$fascredithours, CostHours = 211 )
            final_fas <- rbind(bf, fas_input) %>% mutate(AMOUNT = FundedCreditHours*CostHours )
        }else if(input$BFname == 2){
            fms_input <- data.frame(FUND_CODE ="FMS", FundedCreditHours = input$fascredithours, CostHours = 158 )
            final_fas <- rbind(bf, fms_input) %>% mutate(AMOUNT = FundedCreditHours*CostHours ) 
        }
        
    })
    
    output$bf_amount_output_table <- DT::renderDataTable({
        DT::datatable(bf_amount_input_table()) %>% 
            formatCurrency("AMOUNT", currency = "$", digits = 0)
        
    })
    
    ######## Final Aid Table #####
    # final aid data table with total    
    output$aid_results <- DT::renderDataTable({
        
        brightfuture <- bf_amount_input_table() %>% select(FUND_CODE, AMOUNT) # BF table
        estimated_aid_tab <- total_funds() 
        names(estimated_aid_tab) <- toupper(names(estimated_aid_tab))
        
        # not include BF (interacting all aid: target aid)
        # estimated_aid_tab_all <- rbind(estimated_aid_tab, brightfuture) %>% 
        #     janitor::adorn_totals("row") 
        #     
        
        DT::datatable(estimated_aid_tab, rownames = FALSE , options = list(pageLength = 10, dom = 'tip')) %>% 
            formatCurrency("AMOUNT", currency = "$", digits = 0)
        
    })
    
    ############ Viz ###########
    
    # Total aid gauge 
    output$gauge_aid <- renderPlotly({
        vls <- total_funds() %>% filter(Fund_Code =="Total") %>% select(amount)
        vls2 <- vls$amount # total aids
        coa <- aid_need_data_frame()$coa
        targetaid <- aid_need_data_frame()$target_aid
        aid_need <-   aid_need_data_frame()$coa - aid_need_data_frame()$efc 
        fig <- plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = vls2,
            title = list(text = "Estimated Aid Amount"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = targetaid, increasing = list(color="#215732")), # diff value and reference
            gauge = list(
                axis =list(range = list(NULL, coa), tickwidth = 1.3, tickcolor="green"),
                bar=list(color="#004C97"),
                steps = list(
                    list(range = c(0, targetaid), color = "#009CDE"),
                    list(range = c(targetaid, aid_need), color = "#FFB81C")),
                threshold = list(
                    line = list(color = "#DE4200", width = 10),
                    thickness = 3,
                    value = aid_need))) %>% 
            layout(margin = list(l=20,r=30),
                   paper_bgcolor="white", font =list(color="#002697", family="Arial")) # check colors
        
        fig
        
    })
    
    output$bar_aids_stu <- renderEcharts4r({
        
        total_funds() %>% 
            e_charts(Fund_Code) %>% 
            e_bar(amount, name = "Offer Amount ($)",
                  itemStyle = list(color ="#004C97", borderColor = "#3091BB", borderWidth = "1")) %>% 
            e_axis_labels( y= "$" ) %>% 
            e_tooltip(trigger = c("item","axis")) %>% 
            e_axis_stagger() 
        
    })
    
    ################################################ 2nd page #################################
    # 2nd page - estimator for new aid year
    
    filterd_df <- reactive({
        ftic_detail_2223_fill %>% 
            #filter(input$coa == cost_of_attendance_budget ) %>% 
            #filter(input$efc == efc) %>% 
            #filter(input$tier == tier)  %>% 
            #filter(input$student_type == stu_type) %>% 
            #filter(input$state_residency == residency) %>% 
            #group_by(uwfid) %>% 
            select(uwfid, source, fund_type,fund_code,offer_amount)  %>% filter(!is.na(offer_amount))   
    })
    
    output$federalvalue <- renderValueBox({
        federal <- filterd_df()  %>% filter(source =="FED") %>% filter(offer_amount > 0) %>% 
            group_by(uwfid, source) %>% summarise(sum = sum(offer_amount), .groups = "drop") %>% arrange(-sum)
        #valueBox(paste0("$",federal$sum), "Federal Grants", color = "aqua")
        
    })
    
    
    
    output$federaltable <- DT::renderDataTable({
        federal_DT <- filterd_df() %>% filter(source =="FED") %>% 
            group_by(fund_type,fund_code) %>% 
            summarise(amount =sum(offer_amount), .groups = "drop") %>% 
            filter(!is.na(amount))
        federal_DT_Total <- rbind(federal_DT, data.frame(fund_type="Total",fund_code="Total", amount=sum(federal_DT$amount)))
        
        
        DT::datatable(federal_DT_Total, rownames = FALSE , options = list(pageLength = 10, dom = 'tip') ) %>% 
            formatCurrency("amount", currency = "$", digits = 0)
        
    })
    
    output$statetable <- DT::renderDataTable({
        state_DT <- filterd_df() %>% filter(source =="STAT") %>% 
            group_by(fund_type,fund_code) %>% summarise(amount =sum(offer_amount), .groups = "drop") %>% 
            filter(!is.na(amount))
        state_DT_Total <-  rbind(state_DT, data.frame(fund_type="Total",fund_code="Total", amount=sum(state_DT$amount)))
        
        DT::datatable(state_DT_Total, rownames = FALSE , options = list(pageLength = 10, dom = 'tip')) %>% 
            formatCurrency("amount", currency = "$", digits = 0)
        
    })
    output$uwfgranttable <- DT::renderDataTable({
        uwf_DT <- filterd_df() %>% filter(source =="INST") %>% 
            group_by(fund_type, fund_code) %>% summarise(amount =sum(offer_amount), .groups = "drop") %>% 
            filter(!is.na(amount))
        uwf_DT_Total <-  rbind(uwf_DT, data.frame(fund_type="Total",fund_code="Total", amount=sum(uwf_DT$amount)))
        
        DT::datatable(uwf_DT, rownames = FALSE , options = list(pageLength = 10, dom = 'tip')) %>% 
            formatCurrency("amount", currency = "$", digits = 0)
        
    })
    output$fund_plot <- renderEcharts4r({
        
        federal_plot <- filterd_df() %>%  
            group_by(fund_code) %>% 
            summarise(Total=sum(offer_amount), .groups = "drop") %>% 
            e_charts( x= fund_code) %>% 
            e_bar(Total) %>% 
            e_flip_coords() %>% 
            e_tooltip(trigger = c("item","axis"))
        
    })
    
    
    acex_sum_his <- acex_data_year %>%  
        mutate(Tier = as.character(APPLICANT_TIER)) %>% 
        select(FTIC_Cohort, Tier, REPT_TIME_FRAME, Year_Amount ) 
    acex_sum_new <- app2022_temp %>% 
        mutate(Tier = as.character(Tier)) %>% 
        select(FTIC_Cohort, Tier, REPT_TIME_FRAME, Year_Amount )
    
    acex_cal_data <- rbind(acex_sum_his, acex_sum_new)
    
    acex_temp_data <-  acex_cal_data %>% group_by(FTIC_Cohort, Tier, REPT_TIME_FRAME) %>% 
        dplyr::summarise(ACEX = sum(Year_Amount), Count=n(), .groups = "drop") %>% 
        pivot_wider(names_from = REPT_TIME_FRAME, values_from = c(ACEX ,Count)) %>% 
        janitor::adorn_totals("row") %>%  
        mutate_at(c(3:8), dollar)
    
    
    #updating count part 1
    t1_count_up <- acex_temp_data %>%  
        mutate( Count_20232024 = NA, Count_20242025= NA, Count_20252026= NA ) %>% 
        #updating acex amount part 1
        mutate( ACEX_20232024 = NA, ACEX_20242025=NA, ACEX_20252026= NA )  #%>% #8 to 11 
    
    # mutate(ACEX_20222023 =
    #     ifelse( Tier == 1, Count_20222023*50000,
    #     ifelse( Tier == 2, Count_20222023*30000,
    #     ifelse( Tier == 3, Count_20222023*10000, Count_20222023*20000 )))) %>%
    # mutate(ACEX_20232024 =
    #     ifelse( Tier == 1, Count_20232024*50000,
    #     ifelse( Tier == 2, Count_20232024*30000,
    #     ifelse( Tier == 3, Count_20232024*10000, Count_20232024*20000 )))) %>%
    # mutate(ACEX_20242025 =
    #     ifelse( Tier == 1, Count_20242025*50000,
    #     ifelse( Tier == 2, Count_20242025*30000,
    #     ifelse( Tier == 3, Count_20242025*10000, Count_20242025*20000)))) %>%
    # mutate(ACEX_20252026 =
    #     ifelse( Tier == 1, Count_20252026*50000,
    #     ifelse( Tier == 2, Count_20252026*30000,
    #     ifelse( Tier == 3, Count_20252026*10000, Count_20252026*20000 ))))
    # 2022 with tier information ACS criterias
    
    
    
    
    output$summary_data_table_count <- DT::renderDataTable({
        
        DT::datatable(t1_count_up[,c(1,2,9:17)], extensions = c("Buttons", 'FixedHeader'),
                  class = 'cell-border stripe',editable = 'cell', 
                  #filter = "top",
                  options = list(dom="Blfrtip", scrollX=TRUE,scrollY=TRUE,
                                 buttons=c("copy","csv","excel","pdf","print"), 
                                 lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")), pageLength=27)) %>% 
            formatStyle(c("Count_20222023","Count_20232024","Count_20242025","Count_20252026"), color="red",backgroundColor = "lightcyan", fontWeight = "bold") #%>% 
        #formatStyle("FTIC_Cohort", target = "row",  backgroundColor =  styleEqual(2022, c("lightyellow")))
        
    })
    
    output$summary_data_table_dollar <- DT::renderDataTable({
        DT::datatable(t1_count_up[,c(1:8,18:20)], extensions = c("Buttons",'FixedHeader'), class = 'cell-border stripe', 
                  #filter = "top",
                  options = list(dom="Blfrtip",scrollX=TRUE,
                                 buttons=c("copy","csv","excel","pdf","print"), 
                                 lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=27)) %>% 
            formatStyle(c("ACEX_20222023","ACEX_20232024","ACEX_20242025","ACEX_20252026"), color="red",backgroundColor = "lightcyan", fontWeight = "bold")
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

