
library(shinydashboard)
library(shiny)
library(rsconnect)
library(reshape2)
library(RCurl)
library(tidyr)
library(dplyr)
library(pmdplyr)
library(tidyverse)
library(purrr)
library(zoo)

###################################################################################
###################################################################################
##
###################################################################################
###################################################################################

#data for tab1
# cases_final <- readRDS("cases_final.rds")
# cases_admin0<-cases_final%>%filter(admin %in% 0)
# cases_admin1<-cases_final%>%filter(admin %in% 1)
# cases_admin2<-cases_final%>%filter(admin %in% 2)

#USE REDUCED FILE WITH FULL LOCAL DATA FOR CANADA BUT LOCAL DATA ONLY FOR NEW YORK STATE
cases_final_reduced <- readRDS("cases_final_reduced.rds")
cases_final <- readRDS("cases_final.rds")

cases_admin0<-cases_final_reduced%>%filter(admin %in% 0)
cases_admin1<-cases_final_reduced%>%filter(admin %in% 1)

cases_admin2<-cases_final%>%filter(admin %in% 2)
cases_admin2<-cases_final_reduced%>%filter(admin %in% 2)

#maximum date in data
updated=(max(cases_admin0$date))

maps_final <- readRDS("maps_final.rds")

nrows <- cases_admin0 %>% ungroup() %>% count(date) %>% select(-n) %>% count()

###################################################################################
###################################################################################
#UI
###################################################################################
###################################################################################

####################
#HEADER
####################

header <- dashboardHeader(title = "COVID-19 Dashboard")
#header <- dashboardHeader(title = paste0("COVID-19 Dashboard (last updated: ",updated,")"))

####################
#SIDEBAR
####################

sidebar <- dashboardSidebar(
  sidebarMenu(id="sidebar",
    menuItem("Dashboard", tabName = "tab1", icon = icon("dashboard")),
    menuItem("Country", tabName = "tab2", icon = icon("th")),
    menuItem("State/province", tabName = "tab3", icon = icon("th")),
    menuItem("Local", tabName = "tab4", icon = icon("th")),
    menuItem("Map", tabName = "tab5", icon = icon("th")),
    menuItem("Data Browser", tabName = "tab6", icon = icon("th")),
    menuItem("Sources", tabName = "tab99", icon = icon("th"))
    # menuItem("Test", tabName = "test", icon = icon("th"))

  ),
  
  #   #display current date
  # fluidRow(
  #   htmlOutput('text_sidebar_1')
  # ),

  # #display current date
  # tags$footer(
  #   tags$p(paste("Last updated: ",Sys.time()," ",Sys.timezone()))
  # )
  
  h5(paste("Last updated: ",Sys.time()," ",Sys.timezone()),style='padding-left: 15px')
  
  #UPDATE FOR REACTIVE INPUT
  # #enable bookmarking of current state of application
  #   bookmarkButton()
  
)

###################################################################################
###################################################################################
#BODY
#THIS SECTION DESCRIBES THE LAYOUT OF THE INPUT WIDGETS AND OUTPUTS (e.g., SLIDER WILL SEND INPUT TO SERVER, SERVER WILL RETURN OUTPUT)
###################################################################################
###################################################################################

# Output function 	Creates
# dataTableOutput 	DataTable
# htmlOutput 	raw HTML
# imageOutput 	image
# plotOutput 	plot
# tableOutput 	table
# textOutput 	text
# uiOutput 	raw HTML
# verbatimTextOutput 	text

body <- dashboardBody(
  # Boxes need to be put in a row (or column)
  

  
  

#ADD HORIZONTAL SCROLLING TO TABLES
tags$head(
tags$style(HTML("
.sidebar { height: 90vh; overflow-y: auto; }
.dataTables_wrapper { overflow-x: scroll; }
" )
)
),
  
  
  
#ADD COLOR TO BOXES
#MUST ADD "status = "primary", solidHeader = TRUE" TO BOXES
#"primary", "success", "info", "warning", or "danger"

tags$style(HTML("

####################
#DANGER = RED
####################

.box.box-solid.box-danger>.box-header {
  color:#e88b8b;
  background:#e88b8b
                    }

.box.box-solid.box-danger{
border-bottom-color:#e88b8b;
border-left-color:#e88b8b;
border-right-color:#e88b8b;
border-top-color:#e88b8b;
}

.box.box-danger>.box-header {
  color:#fff; 
  background:#e88b8b
                    }

.box.box-danger{
border-bottom-color:#e88b8b;
border-left-color:#e88b8b;
border-right-color:#e88b8b;
border-top-color:#e88b8b;
background: #e88b8b;
}






####################
#INFO = GREEN
####################

.box.box-solid.box-info>.box-header {
  color:#000000;
  background:#a7dbb5
                    }

.box.box-solid.box-info{
border-bottom-color:#a7dbb5;
border-left-color:#a7dbb5;
border-right-color:#a7dbb5;
border-top-color:#a7dbb5;
}

.box.box-info>.box-header {
  color:#fff; 
  background:#a7dbb5
                    }

.box.box-info{
border-bottom-color:#a7dbb5;
border-left-color:#a7dbb5;
border-right-color:#a7dbb5;
border-top-color:#a7dbb5;
background: #a7dbb5;
}


")),
  




  tabItems(

    ####################
    #tab content
    ####################
    
    tabItem(tabName = "tab1",
            
            # #enable bookmarking of current state of application
            # bookmarkButton(),
            
            h2("Daily cases in Canada"),
            
            fluidRow(
              
              # box(plotOutput("plot999", height = 250)),
              
              # box(
              #   title = "Controls",
              #   sliderInput("slider1", "Number of observations:", 1, 100, 50)
              # ),

              #, height = 300, width = 900
              box(width=12,
                  plotOutput("plot_tab1_1",
                             click = "plot_tab1_1_click",
                             hover = "plot_tab1_1_hover",
                             brush = "plot_tab1_1_brush"),
                  
                  
                  #display info from graph interaction
                  verbatimTextOutput("plot_tab1_1_info")
                  
              ),

              
              

              
              
              column(12,

              #REACTIVE; CREATE BOX IN SERVER SIDE, RENDER IN UI SIDE
              #BASICALLY, MOVES THE UI OUPUT BOX INTO A CONTAINER ON THE SERVER SIDE TO ALLOW FOR THE UI OUTPUT BOX TO BE DEFINED ON BY SERVER SIDE INPUTS (NOT JUST DISPLAYING THE RESULTS OF SERVER SIDE INPUTS)
              
              # Select type of trend to plot
              box(
                selectInput(
                inputId = "dropdown_tab1_1",
                label = strong(""),
                choices = unique(cases_final_reduced$xid),
                selected = "BRITISH COLUMBIA, CANADA"),
                plotOutput("miniplot_tab1_1",height = 100),
              ),
              
              uiOutput("reactivetext_tab1_1"),

              ),
              
              
              
              column(12,
                     
              # Select type of trend to plot
              box(
                selectInput(
                inputId = "dropdown_tab1_2",
                label = strong(""),
                choices = unique(cases_final_reduced$xid),
                selected = "ONTARIO, CANADA"),
                plotOutput("miniplot_tab1_2",height = 100),
              ),
              
              uiOutput("reactivetext_tab1_2"),
              
            ),

              
              
            column(12,
                   
              # Select type of trend to plot
              box(
                selectInput(
                inputId = "dropdown_tab1_3",
                label = strong(""),
                choices = unique(cases_final_reduced$xid),
                selected = "CANADA, BC, FRASER"),
                plotOutput("miniplot_tab1_3",height = 100),
              ),
              
              uiOutput("reactivetext_tab1_3"),
              
    ),

              
              
    column(12,
           
              # Select type of trend to plot
              box(
                selectInput(
                inputId = "dropdown_tab1_4",
                label = strong(""),
                choices = unique(cases_final_reduced$xid),
                selected = "CANADA, BC, VANCOUVER COASTAL"),
                plotOutput("miniplot_tab1_4",height = 100),
              ),
              
              uiOutput("reactivetext_tab1_4"),
              
  ),

              
              
  column(12,
         
              # Select type of trend to plot
              box(
                selectInput(
                inputId = "dropdown_tab1_5",
                label = strong(""),
                choices = unique(cases_final_reduced$xid),
                selected = "CANADA, ONTARIO, TORONTO"),
                plotOutput("miniplot_tab1_5",height = 100),
              ),
              
              uiOutput("reactivetext_tab1_5"),
              
),

              
              
column(12,
       
              # Select type of trend to plot
              box(
              selectInput(
                inputId = "dropdown_tab1_6",
                label = strong(""),
                choices = unique(cases_final_reduced$xid),
                selected = "NEW YORK, NEW YORK, UNITED STATES"),
                plotOutput("miniplot_tab1_6",height = 100),
              ),
              
              uiOutput("reactivetext_tab1_6"),

),


              
              # #USE REACTIVE BOXES INSTEAD TO AUTOMATICALLY SELECT COLOR BASED ON TREND
              # box(status = "info", solidHeader = TRUE,
              #   #helpText('Summary:'),
              #   textOutput('text_tab1_1')
              # ),
              # 
              # 
              # box(status = "danger", solidHeader = TRUE,
              #   #helpText('Summary:'),
              #   textOutput('text_tab1_2')
              # ),
              # 
              # 
              # box(status = "danger", solidHeader = TRUE,
              #   #helpText('Summary:'),
              #   textOutput('text_tab1_3')
              # ),
              # 
              # 
              # box(status = "danger", solidHeader = TRUE,
              #   #helpText('Summary:'),
              #   textOutput('text_tab1_4')
              # ),
              # 
              # 
              # box(status = "danger", solidHeader = TRUE,
              #   #helpText('Summary:'),
              #   textOutput('text_tab1_5')
              # ),
              # 
              # 
              # box(status = "danger", solidHeader = TRUE,
              #   #helpText('Summary:'),
              #   textOutput('text_tab1_6')
              # ),
              # 
              
              
              
              
              
              

              # box(width=20,
              #     #helpText('Summary:'),
              #     dataTableOutput('data_tab1_1')
              # ),

              
              
              
              
              
              
              
              
            )
    ),

    
    
    
    
    
    
    
    
    
    
    
    
    
    ####################
    #tab content
    ####################
    
    tabItem(tabName = "tab2",
            
            h2("Country-level case counts"),
            
            fluidRow(

              
              
              box(
                #helpText('Summary:'),
                #verbatimTextOutput('text_tab2_1'),
                textOutput('text_tab2_1')
              ),
              
              
              
              box(width=12,
                  
                  # Select type of trend to plot
                  selectInput(
                    inputId = "dropdown_tab2_1",
                    label = strong("Select Region"),
                    choices = unique(cases_admin0$xid),
                    selected = "CANADA"),
                  
                  #display plot
                  #allow for clicking on graph
                  #, height = 300, width = 900
                  plotOutput("plot_tab2_1", 
                             click = "plot_tab2_1_click",
                             hover = "plot_tab2_1_hover",
                             brush = "plot_tab2_1_brush"),
                  
                  #display info from graph interaction
                  verbatimTextOutput("plot_tab2_1_info"),
                  
                  # #this switches input and output between the UI and server to allow for dynamic inputs
                  # uiOutput("range_tab2_1"),
                  
              ),
              
              
              
              
              
              
              
              
              box(width=12,
                  
                  # Select whether to restrict to past 90 days
                  checkboxInput(inputId = "checkbox_tab2_1", label = strong("Restrict to past 90 days"), value = FALSE),
                  
                  # Select date range to display only if restriction to past 90 days is false
                  conditionalPanel(condition = "input.checkbox_tab2_1 == false",
                                   title = "Date range",
                                   sliderInput("range_tab2_1", "Date range:", min = 1, max = as.numeric(nrows), value = c(1, as.numeric(nrows)))
                  ),
                  
              ),
              
              
              
              
              
              
              
              
              box(width=12,
                  
                  # Select whether to overlay smooth trend line
                  checkboxInput(inputId = "checkbox_tab2_2", label = strong("Overlay smooth trend line"), value = TRUE),
                  
                  # Display only if the smoother is checked
                  conditionalPanel(condition = "input.checkbox_tab2_2 == true",
                                   title = "Smoothing line",
                                   sliderInput("slider_tab2_1", "Degrees of freedom:", 3, 20, 16, animate = animationOptions(interval = 1000, loop = FALSE)),
                                   HTML("Higher values give more smoothness")
                  ),
                  
                  # Select whether to overlay smooth trend line
                  conditionalPanel(condition = "input.checkbox_tab2_2 == true",
                                   checkboxInput(inputId = "checkbox_tab2_3", label = strong("Overlay change in trend lines"), value = TRUE)
                  ),
                  
              ),
              
              
              
              
              
            )
            
    ),
    
    ####################
    #tab content
    ####################
    
    tabItem(tabName = "tab3",
            
            h2("State/province-level case counts"),
            
            fluidRow(
              
              

              box(
                #helpText('Summary:'),
                #verbatimTextOutput('text_tab3_1'),
                textOutput('text_tab3_1')
              ),
              
              
              
              
              box(width=12,
                  
                  
                  
                  # Select type of trend to plot
                  selectInput(
                    inputId = "dropdown_tab3_0",
                    label = strong("Select Country"),
                    choices = unique(cases_admin1$admin0), #select from cases_admin1 to ensure only data with admin1 available
                    selected = "Canada"),
                  
                  uiOutput("reactivetext_tab3_1"),
                  
                  # # Select type of trend to plot
                  # selectInput(
                  #   inputId = "dropdown_tab3_1",
                  #   label = strong("Select Region"),
                  #   choices = unique(cases_admin1$xid),
                  #   selected = "BRITISH COLUMBIA, CANADA"),
                  
                  #display plot
                  #allow for clicking on graph
                  #, height = 300, width = 900
                  plotOutput("plot_tab3_1", 
                             click = "plot_tab3_1_click",
                             hover = "plot_tab3_1_hover",
                             brush = "plot_tab3_1_brush"),
                  
                  #display info from graph interaction
                  verbatimTextOutput("plot_tab3_1_info"),
                  
                  # #this switches input and output between the UI and server to allow for dynamic inputs
                  # uiOutput("range_tab3_1"),

                  ),

              
              
              
              
              
              
              
              box(width=12,
 
                  # Select whether to restrict to past 90 days
                  checkboxInput(inputId = "checkbox_tab3_1", label = strong("Restrict to past 90 days"), value = FALSE),
                  
                  # Select date range to display only if restriction to past 90 days is false
                  conditionalPanel(condition = "input.checkbox_tab3_1 == false",
                                   title = "Date range",
                                   sliderInput("range_tab3_1", "Date range:", min = 1, max = as.numeric(nrows), value = c(1, as.numeric(nrows)))
                  ),

              ),
              
              
              
              

              
              
              
              box(width=12,

                  # Select whether to overlay smooth trend line
                  checkboxInput(inputId = "checkbox_tab3_2", label = strong("Overlay smooth trend line"), value = TRUE),
                  
                  # Display only if the smoother is checked
                  conditionalPanel(condition = "input.checkbox_tab3_2 == true",
                                   title = "Smoothing line",
                                   sliderInput("slider_tab3_1", "Degrees of freedom:", 3, 20, 16, animate = animationOptions(interval = 1000, loop = FALSE)),
                                   HTML("Higher values give more smoothness")
                  ),
                  
                  # Select whether to overlay smooth trend line
                  conditionalPanel(condition = "input.checkbox_tab3_2 == true",
                                   checkboxInput(inputId = "checkbox_tab3_3", label = strong("Overlay change in trend lines"), value = TRUE)
                  ),
                  
              ),
              
              
              
              
              
              
              
              
              
                            
              
            )
            
    ),
    
    ####################
    #tab content
    ####################
    
    tabItem(tabName = "tab4",
            
            h2("Local-level case counts"),
            
            fluidRow(
              
              
              
              
              
              
              box(
                #helpText('Summary:'),
                #verbatimTextOutput('text_tab4_1'),
                textOutput('text_tab4_1')
              ),
              
              
              
            
              
              
              
              box(width=12,
                  
                  # Select type of trend to plot
                  selectInput(
                    inputId = "dropdown_tab4_0",
                    label = strong("Select Country"),
                    choices = unique(cases_admin2$admin0), #select from cases_admin2 to ensure only data with admin2 available
                    selected = "Canada"),
                  
                  uiOutput("reactivetext_tab4_1"),
                  uiOutput("reactivetext_tab4_2"),
                  
                  # # Select type of trend to plot
                  # selectInput(
                  #   inputId = "dropdown_tab4_1",
                  #   label = strong("Select Region"),
                  #   choices = unique(cases_admin2$xid),
                  #   selected = "CANADA, ONTARIO, TORONTO"),
                  
                  #display plot
                  #allow for clicking on graph
                  #, height = 300, width = 900
                  plotOutput("plot_tab4_1", 
                             click = "plot_tab4_1_click",
                             hover = "plot_tab4_1_hover",
                             brush = "plot_tab4_1_brush"),
                  
                  #display info from graph interaction
                  verbatimTextOutput("plot_tab4_1_info"),
                  
                  # #this switches input and output between the UI and server to allow for dynamic inputs
                  # uiOutput("range_tab4_1"),
                  
              ),
              
              
              
              
              
              
              
              
              box(width=12,
                  
                  # Select whether to restrict to past 90 days
                  checkboxInput(inputId = "checkbox_tab4_1", label = strong("Restrict to past 90 days"), value = FALSE),
                  
                  # Select date range to display only if restriction to past 90 days is false
                  conditionalPanel(condition = "input.checkbox_tab4_1 == false",
                                   title = "Date range",
                                   sliderInput("range_tab4_1", "Date range:", min = 1, max = as.numeric(nrows), value = c(1, as.numeric(nrows)))
                  ),
                  
              ),
              
              
              
              
              
              
              
              
              box(width=12,
                  
                  # Select whether to overlay smooth trend line
                  checkboxInput(inputId = "checkbox_tab4_2", label = strong("Overlay smooth trend line"), value = TRUE),
                  
                  # Display only if the smoother is checked
                  conditionalPanel(condition = "input.checkbox_tab4_2 == true",
                                   title = "Smoothing line",
                                   sliderInput("slider_tab4_1", "Degrees of freedom:", 3, 20, 16, animate = animationOptions(interval = 1000, loop = FALSE)),
                                   HTML("Higher values give more smoothness")
                  ),
                  
                  # Select whether to overlay smooth trend line
                  conditionalPanel(condition = "input.checkbox_tab4_2 == true",
                                   checkboxInput(inputId = "checkbox_tab4_3", label = strong("Overlay change in trend lines"), value = TRUE)
                  ),
                  
              ),
              
              
              
              
              
            )
            
    ),
    
    ####################
    #tab content
    ####################
    
    tabItem(tabName = "tab6",
            
            h2("Data browser"),
            
            fluidRow(

              
              
              
              
              
              
              
              
              
              box(width=20,

                  # Select type of trend to plot
                    selectInput(
                      inputId = "dropdown_tab6_1",
                      label = strong("Select Region"),
                      choices = unique(cases_final_reduced$xid),
                      selected = "NEW YORK, NEW YORK, UNITED STATES")

              ),
              
              
              
              
              box(width=20,
                  #helpText('Summary:'),
                  dataTableOutput('data_tab6_1')
              ),
              
              
              
              
              
              
              
              
              
            )
            
    ),
    
    ####################
    #tab content
    ####################
    
    tabItem(tabName = "tab99",
            
            h2("Data sources"),
            
            fluidRow(
              

              
              box(width=12,
                #helpText('Data sources:'),
                #verbatimTextOutput('text_tab99_1'),
                #textOutput('text_tab99_1')
                htmlOutput('text_tab99_1')
              ),
  

              
            )
            
    ),
    
    ####################
    #Third tab content
    ####################
    
    tabItem(tabName = "tab5",
            
            h2("Daily case counts mapped to county level"),
            
            fluidRow(
              
              box(width=12,
                
                # Select type of trend to plot
                selectInput(
                  inputId = "dropdown999",
                  label = strong("Select State"),
                  choices = unique(maps_final$STATEFIPS_DESC),
                  selected = "NY New York 36"),
                
                plotOutput("plot4", height = 500, width = 900,
                           click = "plot4_click",
                           hover = "plot4_hover",
                           brush = "plot4_brush"),
                
                # #display info from graph interaction
                # verbatimTextOutput("plot4info"),
                
              ),
              
              # box(
              #   title = "Select State",
              #   selectInput("select1", h3("Select box"), 
              #               choices = list(
              #                 "AL Alabama 01" = 1,
              #                 "AK Alaska 02" = 2,
              #                 "AS American Samoa 60" = 60,
              #                 "AZ Arizona 04" = 4,
              #                 "AR Arkansas 05" = 5,
              #                 "BI Baker Island 81" = 81,
              #                 "CA California 06" = 6,
              #                 "CO Colorado 08" = 8,
              #                 "CT Connecticut 09" = 9,
              #                 "DE Delaware 10" = 10,
              #                 "DC District of Columbia 11" = 11,
              #                 "FL Florida 12" = 12,
              #                 "FM Federated States of Micronesia 64" = 64,
              #                 "GA Georgia 13" = 13,
              #                 "GU Guam 66" = 66,
              #                 "HI Hawaii 15" = 15,
              #                 "HI Howland Island 84" = 84,
              #                 "ID Idaho 16" = 16,
              #                 "IL Illinois 17" = 17,
              #                 "IN Indiana 18" = 18,
              #                 "IA Iowa 19" = 19,
              #                 "JI Jarvis Island 86" = 86,
              #                 "JA Johnston Atoll 67" = 67,
              #                 "KS Kansas 20" = 20,
              #                 "KY Kentucky 21" = 21,
              #                 "KR Kingman Reef 89" = 89,
              #                 "LA Louisiana 22" = 22,
              #                 "ME Maine 23" = 23,
              #                 "MH Marshall Islands 68" = 68,
              #                 "MD Maryland 24" = 24,
              #                 "MA Massachusetts 25" = 25,
              #                 "MI Michigan 26" = 26,
              #                 "MI Midway Islands 71" = 71,
              #                 "MN Minnesota 27" = 27,
              #                 "MS Mississippi 28" = 28,
              #                 "MO Missouri 29" = 29,
              #                 "MT Montana 30" = 30,
              #                 "NI Navassa Island 76" = 76,
              #                 "NE Nebraska 31" = 31,
              #                 "NV Nevada 32" = 32,
              #                 "NH New Hampshire 33" = 33,
              #                 "NJ New Jersey 34" = 34,
              #                 "NM New Mexico 35" = 35,
              #                 "NY New York 36" = 36,
              #                 "NC North Carolina 37" = 37,
              #                 "ND North Dakota 38" = 38,
              #                 "MP Northern Mariana Islands 69" = 69,
              #                 "OH Ohio 39" = 39,
              #                 "OK Oklahoma 40" = 40,
              #                 "OR Oregon 41" = 41,
              #                 "PW Palau 70" = 70,
              #                 "PA Palmyra Atoll 95" = 95,
              #                 "PA Pennsylvania 42" = 42,
              #                 "PR Puerto Rico 72" = 72,
              #                 "RI Rhode Island 44" = 44,
              #                 "SC South Carolina 45" = 45,
              #                 "SD South Dakota 46" = 46,
              #                 "TN Tennessee 47" = 47,
              #                 "TX Texas 48" = 48,
              #                 "UM U.S. Minor Outlying Islands 74" = 74,
              #                 "UT Utah 49" = 49,
              #                 "VT Vermont 50" = 50,
              #                 "VA Virginia 51" = 51,
              #                 "VI Virgin Islands of the U.S. 78" = 78,
              #                 "WI Wake Island 79" = 79,
              #                 "WA Washington 53" = 53,
              #                 "WV West Virginia 54" = 54,
              #                 "WI Wisconsin 55" = 55,
              #                 "WY Wyoming 56" = 56
              #               ), selected = 36)
              # ),
              
            )
            
            
    )
    
    ####################
    #Test tab content
    ####################
    
    # tabItem(tabName = "test",
    #         
    #         h2("Test content"),
    #         
    #         fluidRow(
    #           
    #           box(
    #             helpText('Output of the examples in the left:'),
    #             verbatimTextOutput('ex_out1'),
    #           ),
    #           
    #           box(
    #             title = "Date range",
    #             sliderInput("range1", "Number of observations:", min=1, max=180, value=c(1,180)),
    #             #this switches input and output between the UI and server to allow for dynamic inputs
    #             uiOutput("range3"),
    #           ),
    #           
    #         )
    #         
    #         
    # )
    
  ) #end tabIems
  
) #end dashboardBody

###################################################################################
###################################################################################
#COMBINE UI
###################################################################################
###################################################################################

ui <- dashboardPage(header,sidebar,body)

























































###################################################################################
###################################################################################
#SERVER LOGIC
#THIS TAKES THE INPUTS FROM UI AND CREATES OUTPUTS FOR UI
###################################################################################
###################################################################################

server <- function(input, output) {
  
  ####################
  #create output to "plot999", using input from "slider"
  ####################
  
  set.seed(122)
  histdata <- rnorm(500) #generate 500 random numbers
  
  output$plot999 <- renderPlot({
    
    data1 <- histdata[seq_len(input$slider1)] #plot histogram using sequence of numbers equal to length of slider
    hist(data1)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to "plot_tab1_1"
  ####################
  
  output$plot_tab1_1 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    
    df1 <- cases_admin0 %>% filter(xid %in% inputregion) %>% arrange(date)
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    # ggplot(df1) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7"), size=1.5) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=5, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #         ) +
    #   labs(
    #     title=paste0(""),
    #     subtitle=paste0(""),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date",
    #     color=""
    #   ) +
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    plot<-
      ggplot(df1) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(title=paste0(inputregion),subtitle=paste0("Total cases as of ",maxdate,": ",total," (",latestcases," new cases, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average)"),y="Cases",x="",color="") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #legend.position = "none"
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    plot +
      scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
      scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
    
  })
  
  ####################
  #create output to "plot_tab1_1_info"
  ####################
  
  output$plot_tab1_1_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Date=", as.Date(e$x, origin = "1970-01-01"), ", Cases=", round(e$y), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Min date=", as.Date(e$xmin, origin = "1970-01-01"), ", Max date=", as.Date(e$xmax, origin = "1970-01-01"), 
             ", Min cases=", round(e$ymin), ", Max cases=", round(e$ymax))
    }
    
    paste0(
      "", xy_str(input$plot_tab1_1_click)
      #"hover: ", xy_str(input$plot_tab1_1_hover),
      #"brush: ", xy_range_str(input$plot_tab1_1_brush)
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # #SLIDER BOX FOR REACTIVE OUTPUT
  # output$reactivetext_dropdown1_1 <- renderUI({
  #   
  #   # Select type of trend to plot
  #   selectInput(
  #     inputId = "dropdown_tab1_1",
  #     #label = strong("Select Region"),
  #     choices = unique(cases_final_reduced$xid),
  #     selected = "CANADA")
  #   
  # })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to "miniplot_tab1_1"
  ####################
  
  output$miniplot_tab1_1 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    inputregion=input$dropdown_tab1_1
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number())
      
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    # ggplot(df1) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7"), size=1.5) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=5, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #         ) +
    #   labs(
    #     title=paste0(""),
    #     subtitle=paste0(""),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date",
    #     color=""
    #   ) +
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #limit x axis to recent dates
    df2<-df1%>%filter(idi<=90)
    df2<-df1%>%filter(idi<=60)
    
    plot<-
      ggplot(df2) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(y="",x="",color="") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.position = "none")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal") +
      
    #with no smoothed trend line or change in trend (cases and rolling average only)
    plot +
      scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
      scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
    
  })
  
  ####################
  #create output to "miniplot_tab1_2"
  ####################
  
  output$miniplot_tab1_2 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    inputregion=input$dropdown_tab1_2
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number())
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    # ggplot(df1) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7"), size=1.5) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=5, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #         ) +
    #   labs(
    #     title=paste0(""),
    #     subtitle=paste0(""),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date",
    #     color=""
    #   ) +
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #limit x axis to recent dates
    df2<-df1%>%filter(idi<=90)
    df2<-df1%>%filter(idi<=60)
    
    plot<-
      ggplot(df2) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(y="",x="",color="") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.position = "none")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal") +
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    plot +
      scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
      scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
    
  })
  
  ####################
  #create output to "miniplot_tab1_3"
  ####################
  
  output$miniplot_tab1_3 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    inputregion=input$dropdown_tab1_3
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number())
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    # ggplot(df1) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7"), size=1.5) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=5, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #         ) +
    #   labs(
    #     title=paste0(""),
    #     subtitle=paste0(""),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date",
    #     color=""
    #   ) +
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #limit x axis to recent dates
    df2<-df1%>%filter(idi<=90)
    df2<-df1%>%filter(idi<=60)
    
    plot<-
      ggplot(df2) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(y="",x="",color="") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.position = "none")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal") +
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    plot +
      scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
      scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
    
  })
  
  ####################
  #create output to "miniplot_tab1_4"
  ####################
  
  output$miniplot_tab1_4 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    inputregion=input$dropdown_tab1_4
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number())
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    # ggplot(df1) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7"), size=1.5) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=5, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #         ) +
    #   labs(
    #     title=paste0(""),
    #     subtitle=paste0(""),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date",
    #     color=""
    #   ) +
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #limit x axis to recent dates
    df2<-df1%>%filter(idi<=90)
    df2<-df1%>%filter(idi<=60)
    
    plot<-
      ggplot(df2) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(y="",x="",color="") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.position = "none")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal") +
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    plot +
      scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
      scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
    
  })
  
  ####################
  #create output to "miniplot_tab1_5"
  ####################
  
  output$miniplot_tab1_5 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    inputregion=input$dropdown_tab1_5
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number())
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    # ggplot(df1) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7"), size=1.5) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=5, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #         ) +
    #   labs(
    #     title=paste0(""),
    #     subtitle=paste0(""),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date",
    #     color=""
    #   ) +
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #limit x axis to recent dates
    df2<-df1%>%filter(idi<=90)
    df2<-df1%>%filter(idi<=60)
    
    plot<-
      ggplot(df2) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(y="",x="",color="") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.position = "none")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal") +
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    plot +
      scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
      scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
    
  })
  
  ####################
  #create output to "miniplot_tab1_6"
  ####################
  
  output$miniplot_tab1_6 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    inputregion=input$dropdown_tab1_6
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number())
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    # ggplot(df1) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7"), size=1.5) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=5, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #         ) +
    #   labs(
    #     title=paste0(""),
    #     subtitle=paste0(""),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date",
    #     color=""
    #   ) +
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #limit x axis to recent dates
    df2<-df1%>%filter(idi<=90)
    df2<-df1%>%filter(idi<=60)
    
    plot<-
      ggplot(df2) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(y="",x="",color="") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.position = "none")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal") +
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    plot +
      scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
      scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  ####################
  #REACTIVE INPUT; CREATE CONTAINER FOR OUTPUT BOX TO DEFINE INPUT ON SERVER SIDE THAT CAN IMPACT SETTINGS FOR OUTPUT BOX (NORMALLY THE OUTPUT BOX ONLY DISPLAYS THE RESULTS OF SERVER SIDE, BUT CANNOT BE MODIFIED BY SERVER SIDE, AND THEN PUBLISH BACK ON UI SIDE
  ####################
  
  output$reactivetext_tab1_1 <- renderUI({

    #SEED VALUES
    inputregion="BRITISH COLUMBIA, CANADA"
    inputregion=input$dropdown_tab1_1
    degrees=16
    inputrangelower=1
    inputrangeupper=2

    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"upwards","downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    
    if (paste0(latesttrend)=="upwards") {
      colorchoice="danger"
    }
    else if (paste0(latesttrend)=="downwards") {
      colorchoice="info"
    }
    
    box(status = paste0(colorchoice), solidHeader = TRUE,
        #helpText(paste0(inputregion,":")),
        HTML(texttodisplay),
    )
    
  })
  
  ####################
  #REACTIVE INPUT; CREATE CONTAINER FOR OUTPUT BOX TO DEFINE INPUT ON SERVER SIDE THAT CAN IMPACT SETTINGS FOR OUTPUT BOX (NORMALLY THE OUTPUT BOX ONLY DISPLAYS THE RESULTS OF SERVER SIDE, BUT CANNOT BE MODIFIED BY SERVER SIDE, AND THEN PUBLISH BACK ON UI SIDE
  ####################
  
  output$reactivetext_tab1_2 <- renderUI({
    
    #SEED VALUES
    inputregion="ONTARIO, CANADA"
    inputregion=input$dropdown_tab1_2
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"upwards","downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    
    if (paste0(latesttrend)=="upwards") {
      colorchoice="danger"
    }
    else if (paste0(latesttrend)=="downwards") {
      colorchoice="info"
    }
    
    box(status = paste0(colorchoice), solidHeader = TRUE,
        #helpText(paste0(inputregion,":")),
        HTML(texttodisplay)
    )
    
  })
  
  ####################
  #REACTIVE INPUT; CREATE CONTAINER FOR OUTPUT BOX TO DEFINE INPUT ON SERVER SIDE THAT CAN IMPACT SETTINGS FOR OUTPUT BOX (NORMALLY THE OUTPUT BOX ONLY DISPLAYS THE RESULTS OF SERVER SIDE, BUT CANNOT BE MODIFIED BY SERVER SIDE, AND THEN PUBLISH BACK ON UI SIDE
  ####################
  
  output$reactivetext_tab1_3 <- renderUI({
    
    #SEED VALUES
    inputregion="CANADA, BC, FRASER"
    inputregion=input$dropdown_tab1_3
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"upwards","downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    
    if (paste0(latesttrend)=="upwards") {
      colorchoice="danger"
    }
    else if (paste0(latesttrend)=="downwards") {
      colorchoice="info"
    }
    
    box(status = paste0(colorchoice), solidHeader = TRUE,
        #helpText(paste0(inputregion,":")),
        HTML(texttodisplay)
    )
    
  })
  
  ####################
  #REACTIVE INPUT; CREATE CONTAINER FOR OUTPUT BOX TO DEFINE INPUT ON SERVER SIDE THAT CAN IMPACT SETTINGS FOR OUTPUT BOX (NORMALLY THE OUTPUT BOX ONLY DISPLAYS THE RESULTS OF SERVER SIDE, BUT CANNOT BE MODIFIED BY SERVER SIDE, AND THEN PUBLISH BACK ON UI SIDE
  ####################
  
  output$reactivetext_tab1_4 <- renderUI({
    
    #SEED VALUES
    inputregion="CANADA, BC, VANCOUVER COASTAL"
    inputregion=input$dropdown_tab1_4
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"upwards","downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    
    if (paste0(latesttrend)=="upwards") {
      colorchoice="danger"
    }
    else if (paste0(latesttrend)=="downwards") {
      colorchoice="info"
    }
    
    box(status = paste0(colorchoice), solidHeader = TRUE,
        #helpText(paste0(inputregion,":")),
        HTML(texttodisplay)
    )
    
  })
  
  ####################
  #REACTIVE INPUT; CREATE CONTAINER FOR OUTPUT BOX TO DEFINE INPUT ON SERVER SIDE THAT CAN IMPACT SETTINGS FOR OUTPUT BOX (NORMALLY THE OUTPUT BOX ONLY DISPLAYS THE RESULTS OF SERVER SIDE, BUT CANNOT BE MODIFIED BY SERVER SIDE, AND THEN PUBLISH BACK ON UI SIDE
  ####################
  
  output$reactivetext_tab1_5 <- renderUI({
    
    #SEED VALUES
    inputregion="CANADA, ONTARIO, TORONTO"
    inputregion=input$dropdown_tab1_5
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"upwards","downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    
    if (paste0(latesttrend)=="upwards") {
      colorchoice="danger"
    }
    else if (paste0(latesttrend)=="downwards") {
      colorchoice="info"
    }
    
    box(status = paste0(colorchoice), solidHeader = TRUE,
        #helpText(paste0(inputregion,":")),
        HTML(texttodisplay)
    )
    
  })
  
  ####################
  #REACTIVE INPUT; CREATE CONTAINER FOR OUTPUT BOX TO DEFINE INPUT ON SERVER SIDE THAT CAN IMPACT SETTINGS FOR OUTPUT BOX (NORMALLY THE OUTPUT BOX ONLY DISPLAYS THE RESULTS OF SERVER SIDE, BUT CANNOT BE MODIFIED BY SERVER SIDE, AND THEN PUBLISH BACK ON UI SIDE
  ####################
  
  output$reactivetext_tab1_6 <- renderUI({
    
    #SEED VALUES
    inputregion="NEW YORK, NEW YORK, UNITED STATES"
    inputregion=input$dropdown_tab1_6
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"upwards","downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")

    if (paste0(latesttrend)=="upwards") {
      colorchoice="danger"
    }
    else if (paste0(latesttrend)=="downwards") {
      colorchoice="info"
    }
    
    box(status = paste0(colorchoice), solidHeader = TRUE,
        #helpText(paste0(inputregion,":")),
        HTML(texttodisplay)
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############################################
  #USE ADMIN0, XID FOR BOX CHOICES
  ###############################################
  
  #SECONDARY BOX THAT DEPENDS ON FIRST SELECTION
  
  output$reactivetext_tab3_1 <- renderUI({
    
    #SEED VALUES
    inputregion="Canada"
    inputregion=input$dropdown_tab3_0
    
    df1 <- cases_final_reduced %>% filter(admin==1 & admin0 %in% inputregion)

    # Select type of trend to plot
    selectInput(
      inputId = "dropdown_tab3_1",
      label = strong("Select Region"),
      choices = unique(df1$xid),
      selected = "BRITISH COLUMBIA, CANADA")

  })
  
  
  
  
  
  
  
  
  
  
  
  
  ###############################################
  #USE ADMIN0, ADMIN1, XID FOR BOX CHOICES
  ###############################################
  
  #SECONDARY BOX THAT DEPENDS ON FIRST SELECTION
  
  output$reactivetext_tab4_1 <- renderUI({
    
    #SEED VALUES
    inputregion="Canada"
    inputregion=input$dropdown_tab4_0
    
    df1 <- cases_final_reduced %>% filter(admin==2 & admin0 %in% inputregion)
    
    # Select type of trend to plot
    selectInput(
      inputId = "dropdown_tab4_1",
      label = strong("Select Region"),
      choices = unique(df1$admin1),
      selected = "Ontario")
    
  })
  
  
  
  
  
  #SECONDARY BOX THAT DEPENDS ON FIRST SELECTION
  
  output$reactivetext_tab4_2 <- renderUI({
    
    #SEED VALUES
    inputregion="Ontario"
    inputregion=input$dropdown_tab4_1
    
    df1 <- cases_final_reduced %>% filter(admin==2 & admin1 %in% inputregion)
    
    # Select type of trend to plot
    selectInput(
      inputId = "dropdown_tab4_2",
      label = strong("Select Local"),
      choices = unique(df1$xid),
      selected = "CANADA, ONTARIO, TORONTO")
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  #USE REACTIVE BOXES INSTEAD TO AUTOMATICALLY SELECT COLOR BASED ON TREND
  
  ####################
  #create output to "text_tab1_1"
  ####################
  
  output$text_tab1_1 <- renderText({
    
    #SEED VALUES
    inputregion=input$dropdown_tab1_1
    inputregion="BRITISH COLUMBIA, CANADA"
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_admin1 %>% filter(admin %in% 1) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"Cases are trending upwards","Cases are trending downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    print(paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    
  })
  
  ####################
  #create output to "text_tab1_2"
  ####################
  
  output$text_tab1_2 <- renderText({
    
    inputregion=input$dropdown_tab1_2
    inputregion="ONTARIO, CANADA"
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_admin1 %>% filter(admin %in% 1) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"Cases are trending upwards","Cases are trending downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    print(paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    
  })
  
  ####################
  #create output to "text_tab1_3"
  ####################
  
  output$text_tab1_3 <- renderText({
    
    inputregion=input$dropdown_tab1_3
    inputregion="CANADA, BC, FRASER"
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_admin2 %>% filter(admin %in% 2) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"Cases are trending upwards","Cases are trending downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    print(paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    
  })
  
  ####################
  #create output to "text_tab1_4"
  ####################
  
  output$text_tab1_4 <- renderText({
    
    inputregion=input$dropdown_tab1_4
    inputregion="CANADA, BC, VANCOUVER COASTAL"
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_admin2 %>% filter(admin %in% 2) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"Cases are trending upwards","Cases are trending downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    print(paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    
  })
  
  ####################
  #create output to "text_tab1_5"
  ####################
  
  output$text_tab1_5 <- renderText({
    
    inputregion=input$dropdown_tab1_5
    inputregion="CANADA, ONTARIO, TORONTO"
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_admin2 %>% filter(admin %in% 2) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"Cases are trending upwards","Cases are trending downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    print(paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    
  })
  
  ####################
  #create output to "text_tab1_6"
  ####################
  
  output$text_tab1_6 <- renderText({
    
    inputregion=input$dropdown_tab1_6
    inputregion="NEW YORK, NEW YORK, UNITED STATES"
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_admin2 %>% filter(admin %in% 2) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
      mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
      select(-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-cases_new_per_100k,-cases_new_rolling7,-cases_new_change1,-idi,-id,-x,-id_since100,-id_past90,-id_past5)
    
    #latest smoothed case count
    latestcasessmoothed=df2[maxdaterow,]$y_smoothed
    latestcasessmoothed=df2$y_smoothed[maxdaterow]
    latestcasessmoothed=round(latestcasessmoothed)
    latestcasessmoothed=format(latestcasessmoothed,big.mark=",")
    
    #latest trend
    df3=df2%>%
      mutate(latesttrendtext=if_else(trend==1,"Cases are trending upwards","Cases are trending downwards")) %>%
      mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
      mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN))
    latesttrend=df3$latesttrendtext[maxdaterow]
    df4=df3%>%filter(!is.na(inflectiondate))
    latesttrenddate=(max(df4$inflectiondate))
    
    print(paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to plot_tab2_1
  ####################
  
  output$plot_tab2_1 <- renderPlot({
    
    #SEED VALUES
    inputregion="CANADA"
    inputregion=input$dropdown_tab2_1
    degrees=16
    degrees=input$slider_tab2_1
    inputrangelower=1
    inputrangelower=input$range_tab2_1[1]
    inputrangeupper=2
    inputrangeupper=input$range_tab2_1[2]

    # choice_region3 <- reactive({
    #   maps_filtered <- maps_final %>%
    #     filter(region1 == input$var1) %>%
    #     filter(region2 == input$var2) %>%
    #     filter(region3 == input$var3)
    # })

    df1 <- cases_admin0 %>% filter(admin %in%0) %>% filter(xid %in% inputregion) %>% 
    arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab2_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")

    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0))
    
    if(input$checkbox_tab2_1==TRUE){
      df3 <- df2 %>% 
        filter(id_past90>0)
    }
    
    else if(input$checkbox_tab2_1==FALSE){
      df3 <- df2 %>% 
        filter(id>=inputrangelower & id<=inputrangeupper) #slider date selection
    }

    plot<-
      ggplot(df3) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(title=paste0(inputregion),subtitle=paste0("Total cases as of ",maxdate,": ",total," (",latestcases," new cases, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average)"),y="Cases",x="",color="") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #legend.position = "none"
    
    
    # plot<-
    #   ggplot(df3) + 
    #   geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
    #   geom_line(aes(x=date,y=cases_new_rolling7,color=xid), size=1) +
    #   labs(title=paste0(input$dropdown_tab2),y="Cases",x="",color="") +
    #   theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    #   theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
    #   theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal")
    # #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    # #legend.direction = "horizontal"
    # #legend.position = "none"
    
    
    
    
    
    
    
    
    
    
        
    # plot <- ggplot(df3) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7 days"), size=1.5) +
    #   #        geom_line(mapping=aes(x=date,y=y_smoothed, color="Smoothed (df 16)"), size=1.5) +
    #   #geom_line(mapping=aes(x=id,y=y_dydx,colour=ifelse(y_dydx<0,"green",ifelse(y_dydx>0,"red","grey")))) +
    #   #geom_line(mapping=aes(x=id,y=y_dydx,colour=ifelse(y_dydx<0,"green","red"))) +
    #   #geom_point(mapping=aes(x=date,y=y_dydx,color=y_dydx<0),size=1) +
    #   #        geom_line(mapping=aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
    #   ##      geom_ribbon(mapping=aes(x=date,ymin=-15,ymax=y_dydx_neg),fill="red") +
    #   ##      geom_ribbon(mapping=aes(x=date,ymin=15,ymax=y_dydx_pos),fill="green") +
    #   #scale_colour_manual(name = 'trending down', values = setNames(c('green','red'),c(T, F)))
    #   labs(
    #     title=paste0(input$dropdown_tab2),
    #     subtitle=paste0("Daily cases as of ",maxdate," (",total," cum.; ",latestcases," new cases; ",latestchange,"% change in rolling average)"),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date"
    #   ) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=8, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #     ) +
    #   #theme(legend.position = "none") +
    #   
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #full graph
    if(input$checkbox_tab2_2==TRUE & input$checkbox_tab2_3==TRUE){
      plot +
        geom_line(aes(x=date,y=y_smoothed, color="Smoothed"), size=1.5) +
        geom_line(aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual(name = "", values=c("green","red","Rolling average (past 7 days)"="orange","Smoothed"="black"), labels = c("Trending down","Trending up","Rolling average (past 7 days)","Smoothed"))
    }

    #with smoothed trend lines, but not change in trend
    else if(input$checkbox_tab2_2==TRUE & input$checkbox_tab2_3==FALSE){
      plot +
        geom_line(aes(x=date,y=y_smoothed, color="Smoothed"), size=1.5) +
        #geom_line(mapping=aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual(name = "", values=c("Rolling average (past 7 days)"="orange","Smoothed"="black"))
    }
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    else if(input$checkbox_tab2_2==FALSE){
      plot +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))

    }
    

  })

  ####################
  #create output to "plot_tab2_1_info"
  ####################
  
  output$plot_tab2_1_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Date=", as.Date(e$x, origin = "1970-01-01"), ", Cases=", round(e$y), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Min date=", as.Date(e$xmin, origin = "1970-01-01"), ", Max date=", as.Date(e$xmax, origin = "1970-01-01"), 
             ", Min cases=", round(e$ymin), ", Max cases=", round(e$ymax))
    }
    
    paste0(
      "", xy_str(input$plot_tab2_1_click)
    )
  })
  
  ####################
  #create output to "text_tab2_1"
  ####################
  
  output$text_tab2_1 <- renderText({
  
    inputregion=input$dropdown_tab2_1
    
    df1 <- cases_admin0 %>% filter(admin %in% 0) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab2_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    print(paste0(inputregion,": As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population."))
    #print(paste0(inputregion,": As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    # texttodisplay=paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point).")
    # texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    # print(texttodisplay)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to plot_tab3_1
  ####################
  
  output$plot_tab3_1 <- renderPlot({

    #delay rendering of plot until reactive inputs received from server
    shiny::validate(
      need(input$dropdown_tab3_1, "Waiting...")
    )
    
    #SEED VALUES
    inputregion="BRITISH COLUMBIA, CANADA"
    inputregion=input$dropdown_tab3_1
    degrees=16
    degrees=input$slider_tab3_1
    inputrangelower=1
    inputrangelower=input$range_tab3_1[1]
    inputrangeupper=2
    inputrangeupper=input$range_tab3_1[2]

    # choice_region3 <- reactive({
    #   maps_filtered <- maps_final %>%
    #     filter(region1 == input$var1) %>%
    #     filter(region2 == input$var2) %>%
    #     filter(region3 == input$var3)
    # })
    
    df1 <- cases_admin1 %>% filter(admin %in%1) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab3_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0))
    
    if(input$checkbox_tab3_1==TRUE){
      df3 <- df2 %>% 
        filter(id_past90>0)
    }
    
    else if(input$checkbox_tab3_1==FALSE){
      df3 <- df2 %>% 
        filter(id>=inputrangelower & id<=inputrangeupper) #slider date selection
    }
    
    plot<-
      ggplot(df3) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(title=paste0(inputregion),subtitle=paste0("Total cases as of ",maxdate,": ",total," (",latestcases," new cases, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average)"),y="Cases",x="",color="") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #legend.position = "none"
    
    
    # plot<-
    #   ggplot(df3) + 
    #   geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
    #   geom_line(aes(x=date,y=cases_new_rolling7,color=xid), size=1) +
    #   labs(title=paste0(input$dropdown_tab3_1),y="Cases",x="",color="") +
    #   theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    #   theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
    #   theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal")
    # #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    # #legend.direction = "horizontal"
    # #legend.position = "none"
    
    
    
    
    
    
    
    
    
    
    
    # plot <- ggplot(df3) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7 days"), size=1.5) +
    #   #        geom_line(mapping=aes(x=date,y=y_smoothed, color="Smoothed (df 16)"), size=1.5) +
    #   #geom_line(mapping=aes(x=id,y=y_dydx,colour=ifelse(y_dydx<0,"green",ifelse(y_dydx>0,"red","grey")))) +
    #   #geom_line(mapping=aes(x=id,y=y_dydx,colour=ifelse(y_dydx<0,"green","red"))) +
    #   #geom_point(mapping=aes(x=date,y=y_dydx,color=y_dydx<0),size=1) +
    #   #        geom_line(mapping=aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
    #   ##      geom_ribbon(mapping=aes(x=date,ymin=-15,ymax=y_dydx_neg),fill="red") +
    #   ##      geom_ribbon(mapping=aes(x=date,ymin=15,ymax=y_dydx_pos),fill="green") +
    #   #scale_colour_manual(name = 'trending down', values = setNames(c('green','red'),c(T, F)))
    #   labs(
    #     title=paste0(input$dropdown_tab3_1),
    #     subtitle=paste0("Daily cases as of ",maxdate," (",total," cum.; ",latestcases," new cases; ",latestchange,"% change in rolling average)"),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date"
    #   ) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=8, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #     ) +
    #   #theme(legend.position = "none") +
    #   
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #full graph
    if(input$checkbox_tab3_2==TRUE & input$checkbox_tab3_3==TRUE){
      plot +
        geom_line(aes(x=date,y=y_smoothed, color="Smoothed"), size=1.5) +
        geom_line(aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual(name = "", values=c("green","red","Rolling average (past 7 days)"="orange","Smoothed"="black"), labels = c("Trending down","Trending up","Rolling average (past 7 days)","Smoothed"))
    }
    
    #with smoothed trend lines, but not change in trend
    else if(input$checkbox_tab3_2==TRUE & input$checkbox_tab3_3==FALSE){
      plot +
        geom_line(aes(x=date,y=y_smoothed, color="Smoothed"), size=1.5) +
        #geom_line(mapping=aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual(name = "", values=c("Rolling average (past 7 days)"="orange","Smoothed"="black"))
    }
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    else if(input$checkbox_tab3_2==FALSE){
      plot +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
      
    }
    
    
  })
  
  ####################
  #create output to "plot_tab3_1_info"
  ####################
  
  output$plot_tab3_1_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Date=", as.Date(e$x, origin = "1970-01-01"), ", Cases=", round(e$y), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Min date=", as.Date(e$xmin, origin = "1970-01-01"), ", Max date=", as.Date(e$xmax, origin = "1970-01-01"), 
             ", Min cases=", round(e$ymin), ", Max cases=", round(e$ymax))
    }
    
    paste0(
      "", xy_str(input$plot_tab3_1_click)
    )
  })
  
  ####################
  #create output to "text_tab3_1"
  ####################
  
  output$text_tab3_1 <- renderText({
    
    inputregion=input$dropdown_tab3_1
    
    df1 <- cases_admin1 %>% filter(admin %in% 1) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab2_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #print(paste0("As of ",maxdate,", there were a total of ",total," cases in ",inputregion,", with ",latestcases," new cases over the past period (",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population."))
    
    print(paste0(inputregion,": As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population."))
    #print(paste0(inputregion,": As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    # texttodisplay=paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point).")
    # texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    # print(texttodisplay)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to plot_tab4_1
  ####################
  
  output$plot_tab4_1 <- renderPlot({

    #delay rendering of plot until reactive inputs received from server
    shiny::validate(
      need(input$dropdown_tab4_2, "Waiting...")
    )
    
    #SEED VALUES
    inputregion="CANADA, ONTARIO, TORONTO"
    inputregion=input$dropdown_tab4_1
    inputregion=input$dropdown_tab4_2
    degrees=16
    degrees=input$slider_tab4_1
    inputrangelower=1
    inputrangelower=input$range_tab4_1[1]
    inputrangeupper=2
    inputrangeupper=input$range_tab4_1[2]
    
    # choice_region3 <- reactive({
    #   maps_filtered <- maps_final %>%
    #     filter(region1 == input$var1) %>%
    #     filter(region2 == input$var2) %>%
    #     filter(region3 == input$var3)
    # })
    
    df1 <- cases_admin2 %>% filter(admin %in% 2) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab4_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))
    
    #merge raw cases, smoothed and dydx, or can just combine plots below
    df2 <- df1 %>% 
      left_join(smoothed, by = "id") %>% 
      left_join(dydx, by = "id") %>%
      mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
      mutate(id_since100=cumsum(id_since100)) %>%
      mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0))
    
    if(input$checkbox_tab4_1==TRUE){
      df3 <- df2 %>% 
        filter(id_past90>0)
    }
    
    else if(input$checkbox_tab4_1==FALSE){
      df3 <- df2 %>% 
        filter(id>=inputrangelower & id<=inputrangeupper) #slider date selection
    }
    
    plot<-
      ggplot(df3) + 
      geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
      geom_line(aes(x=date,y=cases_new_rolling7,color="Rolling average (past 7 days)"), size=1) +
      labs(title=paste0(inputregion),subtitle=paste0("Total cases as of ",maxdate,": ",total," (",latestcases," new cases, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average)"),y="Cases",x="",color="") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
      theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal")
    #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    #legend.direction = "horizontal"
    #legend.position = "none"
    
    
    # plot<-
    #   ggplot(df3) + 
    #   geom_bar(aes(x=date,y=cases_new,fill=xid),stat="identity", show.legend = F) +
    #   geom_line(aes(x=date,y=cases_new_rolling7,color=xid), size=1) +
    #   labs(title=paste0(input$dropdown_tab4_1),y="Cases",x="",color="") +
    #   theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    #   theme(plot.title = element_text(size=12), axis.text.x = element_text(size=10, angle=0)) +
    #   theme(legend.justification=c(1,1),legend.key.size = unit(.2,'cm'), legend.position=c(1,1),legend.text = element_text(size=8),legend.direction = "horizontal")
    # #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
    # #legend.direction = "horizontal"
    # #legend.position = "none"
    
    
    
    
    
    
    
    
    
    
    
    # plot <- ggplot(df3) +
    #   geom_bar(aes(x=date,y=cases_new,color="Raw"),fill="grey", stat="identity") +
    #   geom_line(mapping=aes(x=date,y=cases_new_rolling7, color="Rolling average, past 7 days"), size=1.5) +
    #   #        geom_line(mapping=aes(x=date,y=y_smoothed, color="Smoothed (df 16)"), size=1.5) +
    #   #geom_line(mapping=aes(x=id,y=y_dydx,colour=ifelse(y_dydx<0,"green",ifelse(y_dydx>0,"red","grey")))) +
    #   #geom_line(mapping=aes(x=id,y=y_dydx,colour=ifelse(y_dydx<0,"green","red"))) +
    #   #geom_point(mapping=aes(x=date,y=y_dydx,color=y_dydx<0),size=1) +
    #   #        geom_line(mapping=aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
    #   ##      geom_ribbon(mapping=aes(x=date,ymin=-15,ymax=y_dydx_neg),fill="red") +
    #   ##      geom_ribbon(mapping=aes(x=date,ymin=15,ymax=y_dydx_pos),fill="green") +
    #   #scale_colour_manual(name = 'trending down', values = setNames(c('green','red'),c(T, F)))
    #   labs(
    #     title=paste0(input$dropdown_tab4_1),
    #     subtitle=paste0("Daily cases as of ",maxdate," (",total," cum.; ",latestcases," new cases; ",latestchange,"% change in rolling average)"),
    #     #caption=paste0("Source: JHU CSSE"),
    #     y="Daily cases",
    #     x="Date"
    #   ) +
    #   theme(
    #     plot.title = element_text(size=8), 
    #     axis.text.x = element_text(size=8, angle=45),
    #     panel.background = element_blank(),
    #     legend.justification=c(1,1), legend.position=c(1,1),legend.direction = "horizontal"
    #     ) +
    #   #theme(legend.position = "none") +
    #   
    #   scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    
    #full graph
    if(input$checkbox_tab4_2==TRUE & input$checkbox_tab4_3==TRUE){
      plot +
        geom_line(aes(x=date,y=y_smoothed, color="Smoothed"), size=1.5) +
        geom_line(aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual(name = "", values=c("green","red","Rolling average (past 7 days)"="orange","Smoothed"="black"), labels = c("Trending down","Trending up","Rolling average (past 7 days)","Smoothed"))
    }
    
    #with smoothed trend lines, but not change in trend
    else if(input$checkbox_tab4_2==TRUE & input$checkbox_tab4_3==FALSE){
      plot +
        geom_line(aes(x=date,y=y_smoothed, color="Smoothed"), size=1.5) +
        #geom_line(mapping=aes(x=date,y=y_dydx,group=0,color=as.factor(trend)), size=1) +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual(name = "", values=c("Rolling average (past 7 days)"="orange","Smoothed"="black"))
    }
    
    #with no smoothed trend line or change in trend (cases and rolling average only)
    else if(input$checkbox_tab4_2==FALSE){
      plot +
        scale_fill_manual("", labels = c("Daily cases"), values = c("dodgerblue2")) +
        scale_colour_manual("", values=c("Rolling average (past 7 days)"="orange"))
      
    }
    
    
  })
  
  ####################
  #create output to "plot_tab4_1_info"
  ####################
  
  output$plot_tab4_1_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Date=", as.Date(e$x, origin = "1970-01-01"), ", Cases=", round(e$y), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("Select point on graph\n")
      paste0("Min date=", as.Date(e$xmin, origin = "1970-01-01"), ", Max date=", as.Date(e$xmax, origin = "1970-01-01"), 
             ", Min cases=", round(e$ymin), ", Max cases=", round(e$ymax))
    }
    
    paste0(
      "", xy_str(input$plot_tab4_1_click)
    )
  })
  
  ####################
  #create output to "text_tab4_1"
  ####################
  
  output$text_tab4_1 <- renderText({
    
    inputregion=input$dropdown_tab4_2
    
    df1 <- cases_admin2 %>% filter(admin %in% 2) %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab2_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #print(paste0("As of ",maxdate,", there were a total of ",total," cases in ",inputregion,", with ",latestcases," new cases over the past period (",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population."))
    
    print(paste0(inputregion,": As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population."))
    #print(paste0(inputregion,": As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point)."))
    # texttodisplay=paste0(inputregion,": ","As of ",maxdate,", there were a total of ",total," cases, with ",latestcases," new cases over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. ",latesttrend," since ",latesttrenddate," (based on smoothed inflection point).")
    # texttodisplay=paste0("<b>",inputregion,"</b>: ","As of ",maxdate,", there were a total of ",total," cases, with <b>",latestcases," new cases</b> over the past period (",latestcasessmoothed," new cases smoothed, ",latestcasesrolling," new cases rolling average, ",latestchange,"% change in rolling average). This equates to a rate of ",latestnewcaserate," new cases per 100,000 population. Cases are <b>trending ",latesttrend,"</b> since ",latesttrenddate," (based on smoothed inflection point).")
    # print(texttodisplay)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to "data_tab6_1"
  ####################

  output$data_tab6_1 <- renderDataTable({
    
    #SEED VALUES
    inputregion="BRITISH COLUMBIA, CANADA"
    inputregion=input$dropdown_tab6_1
    
    degrees=16
    inputrangelower=1
    inputrangeupper=2
    
    df1 <- cases_final_reduced %>% filter(xid %in% inputregion) %>% 
      arrange(desc(date)) %>% mutate(idi=row_number()) %>%
      arrange(date) %>% mutate(id=row_number()) %>%
      #filter(id<=input$slider_tab1_1) %>% 
      mutate(cases_new=if_else(is.na(cases_new),0,as.numeric(cases_new)))
    
    #extract values
    total=max(df1$cases_cum)
    total=format(total,big.mark=",")
    
    #maximum date in data
    maxdate=(max(df1$date))
    
    #maximum row in data
    maxrow=nrow(df1)
    
    #row with maximum date
    maxdaterow=which(df1$date==maxdate)
    
    #latest case count
    latestcases=df1[maxdaterow,]$cases_new
    latestcases=df1$cases_new[maxdaterow]
    latestcases=format(latestcases,big.mark=",")
    
    #latest rolling case count
    latestcasesrolling=df1[maxdaterow,]$cases_new_rolling7
    latestcasesrolling=df1$cases_new_rolling7[maxdaterow]
    latestcasesrolling=round(latestcasesrolling)
    latestcasesrolling=format(latestcasesrolling,big.mark=",")
    
    #latest rolling case change
    latestchange=df1[maxdaterow,]$cases_new_change1
    latestchange=round(df1$cases_new_change1[maxdaterow],1)
    
    #latest population rate
    latestnewcaserate=df1[maxdaterow,]$cases_new_per_100k
    latestnewcaserate=round(df1$cases_new_per_100k[maxdaterow],1)
    
    #fitting smoothing splines using smooth.spline(X,Y,df=...)
    fit.sp<-smooth.spline(df1$cases_new ~ df1$id,df=degrees) #16 degrees of freedom
    smoothed <- data.frame(predict(fit.sp)) %>% 
      mutate(id=row_number()) %>% 
      rename(y_smoothed=y)
    
    #dydx, also shift row number since _n=1 does not exist
    dydx <- diff(smoothed$y)/diff(smoothed$x)
    dydx <- data.frame(dydx) %>% 
      mutate(id=row_number()+1) %>% 
      rename(y_dydx=dydx) %>% 
      mutate(y_dydx_pos=if_else(y_dydx>0,as.double(NA),y_dydx)) %>% 
      mutate(y_dydx_neg=if_else(y_dydx<0,as.double(NA),y_dydx)) %>%
      mutate(trend=if_else(y_dydx>0,1,0))

      #merge raw cases, smoothed and dydx, or can just combine plots below
      df2 <- df1 %>% 
        left_join(smoothed, by = "id") %>% 
        left_join(dydx, by = "id") %>%
        mutate(id_since100=if_else(cases_cum>100,1,0)) %>%
        mutate(id_since100=cumsum(id_since100)) %>%
        mutate(id_past90=if_else(row_number()>=(maxrow-90),1,0)) %>%
        mutate(id_past5=if_else(row_number()>=(maxrow-5),1,0)) %>%
        filter(id_past90==1) %>%
        mutate(latesttrendtext=if_else(trend==1,"upwards","downwards")) %>%
        mutate(inflectionpoint=if_else((trend==1 & lag(trend,n=1)==0) | (trend==0 & lag(trend,n=1)==1),1,0,NA_real_)) %>%
        mutate(inflectiondate=if_else(inflectionpoint==1,date,NaN,NaN)) %>%
        ungroup() %>%
        select(-xid,-xid_seq,-admin,-admin0,-admin1,-admin2,-cases_cum,-cases_cum_per_100k,-idi,-id,-y_dydx,-y_dydx_pos,-y_dydx_neg,-trend,-inflectiondate,-x,-id_since100,-id_past90,-id_past5)

  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  ####################
  #create output to "text_sidebar_1"
  ####################
  
  output$text_sidebar_1 <- renderText({

  paste("Last updated: ",Sys.time()," ",Sys.timezone())
  
  })
  
  
  
  
  
  
  
  ####################
  #create output to "text_tab99_1"
  ####################
  
  output$text_tab99_1 <- renderText({
    
    paste("1. Data for global, state and local US cases: Unified COVID-19 Dataset from JHU CSSE (https://github.com/CSSEGISandData/COVID-19_Unified-Dataset)","","2. Data for Canadian local cases: COVID-19 Canada Open Data Working Group (https://github.com/ccodwg/Covid19Canada)",sep="<br/>")
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to "plot_tab2_1"
  ####################
  
  ####################
  #this switches input and output between the UI and server to allow for dynamic inputs
  ####################
  
  # output$range_tab2_1 <- renderUI({
  # 
  #   #number of rows/dates
  #   nrows <- cases_admin0 %>% ungroup() %>% count(date) %>% select(-n) %>% count()
  #   
  #   sliderInput("range_tab2_1", label = h3("Date range"), min = 1, max = as.numeric(nrows), value = c(1, as.numeric(nrows)))
  #   
  # })
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to "plot_tab3_1"
  ####################
  
  ####################
  #this switches input and output between the UI and server to allow for dynamic inputs
  ####################
  
  # output$range_tab3_1 <- renderUI({
  #   
  #   #number of rows/dates
  #   nrows <- cases_admin0 %>% ungroup() %>% count(date) %>% select(-n) %>% count()
  #   
  #   sliderInput("range_tab3_1", label = h3("Date range"), min = 1, max = as.numeric(nrows), value = c(1, as.numeric(nrows)))
  #   
  # })
  
  
  
  
  
  
  
  
  ####################
  #create output to "plot_tab4_1"
  ####################
  
  ####################
  #this switches input and output between the UI and server to allow for dynamic inputs
  ####################
  
  # output$range_tab4_1 <- renderUI({
  #   
  #   #number of rows/dates
  #   nrows <- cases_admin0 %>% ungroup() %>% count(date) %>% select(-n) %>% count()
  #   
  #   sliderInput("range_tab4_1", label = h3("Date range"), min = 1, max = as.numeric(nrows), value = c(1, as.numeric(nrows)))
  #   
  # })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 
  # ####################
  # #create output to "ex_out"
  # ####################
  # 
  # output$select_var1 <- renderUI({
  #   
  #   selectizeInput('var1', 'Select variable 1', choices = c("select" = "", unique(cases_final$region1)), selected = "US")
  #   
  # })
  # 
  # output$select_var2 <- renderUI({
  #   
  #   choice_var2 <- reactive({
  #     
  #     cases_final %>% ungroup() %>%
  #       filter(region1 == input$var1) %>% 
  #       pull(region2) %>% 
  #       unique() %>%
  #       as.character()
  #     
  #     # temp<-cases_final %>%
  #     #   filter(region1 == "US") %>%
  #     #   pull(region2) %>%
  #     #   unique() %>%
  #     #   as.character()
  #     #   view(temp)
  #     
  #   })
  #   
  #   selectizeInput('var2', 'Select variable 2', choices = c("select" = "", choice_var2()), selected = "NEW YORK") # <- put the reactive element here
  #   
  # })
  # 
  # output$select_var3 <- renderUI({
  #   
  #   choice_var3 <- reactive({
  #     
  #     cases_final %>% ungroup() %>%
  #       filter(region1 == input$var1) %>% 
  #       filter(region2 == input$var2) %>% 
  #       pull(region3) %>% 
  #       unique() %>%
  #       as.character()
  #     
  #     # temp<-cases_final %>%
  #     #   filter(region1 == "US") %>%
  #     #   filter(region2 == "NEW YORK") %>%
  #     #   pull(region3) %>%
  #     #   unique() %>%
  #     #   as.character()
  #     #   view(temp)
  #     
  #   })
  #   
  #   selectizeInput('var3', 'Select variable 3', choices = c("select" = "", choice_var3()), selected = "NEW YORK CITY")
  #   
  # })  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  #create output to "plot4"
  ####################
  
  output$plot4 <- renderPlot({
    
    maps_final_1<-maps_final%>%filter(STATEFIPS=="36") #NY
    maps_final_1<-maps_final%>%filter(STATEFIPS=="34") #NJ
    maps_final_1<-maps_final%>%filter(STATEFIPS=="12") #FL
    maps_final_1<-maps_final%>%filter(STATEFIPS=="53") #WA
    
    # #state1=sprintf("%02s",input$select1)
    # #maps_final_1<-maps_final%>%filter(STATEFIPS==state1)
    #                       
    #                       #choice_var3 <- reactive({
    #                       maps_filtered <- maps_final %>%
    #                         filter(region1 == input$var1) %>% 
    #                         filter(region2 == input$var2) %>% 
    #                         filter(region3 == input$var3)
    #                       #})
    #                         
    #                         saveRDS(maps_filtered, "C:/Users/Jonathan/OneDrive/R DASHBOARD/app/maps_filtered.rds")
    #                         #maps_filtered <- readRDS("C:/Users/Jonathan/OneDrive/R DASHBOARD/app/maps_filtered.rds")
    #                         #view(maps_filtered)
    #   
    # maps_final_1<-maps_final%>%filter(STATEFIPS_DESC==input$dropdown999)
    #                         
    #                         saveRDS(maps_filtered, "C:/Users/Jonathan/OneDrive/R DASHBOARD/app/maps_filtered.rds")
    #                         #maps_filtered <- readRDS("C:/Users/Jonathan/OneDrive/R DASHBOARD/app/maps_filtered.rds")
    #                         #view(maps_filtered)
    #                         
    # #maps_final_1<-choice_var3()
    # maps_final_1<-maps_final %>%
    #   filter(region1 == input$var1) %>% 
    #   filter(region2 == input$var2) %>% 
    #   filter(region3 == input$var3)
    
    maps_final_1<-maps_final%>%filter(STATEFIPS_DESC==input$dropdown999)
    
    #temp<-maps_final_1[1:100,]
    #view(temp)
    
    ggplot() +
      geom_polygon(data = maps_final_1, aes(x = long, y = lat, group = group, fill = cases_new), color="white") +
      theme_void()
    #+ theme(legend.position = "none")
    
  })
  
  ####################
  #create output to "ex_out"
  ####################
  
  output$ex_out1 <- renderText({
    print(input$dropdown999)
    print(input$range1)
    print(input$range1[1])
    print(input$range1[2])
    print(input$range1[3])
    print("test1")
    print("test2")
    print(input$range_tab2_1[1])
    print(input$range_tab2_1[2])
    print(input$range3[1])
    print(input$range3[2])
    print(input$var1)
    print(input$var2)
    print(input$var3)
  })
  
  ####################
  #create output to "ex_out"
  ####################
  
  #this switches input and output between the UI and server to allow for dynamic inputs
  output$range3 <- renderUI({
    
    sliderInput("range3", label = h3("Slider Range 3"), min = input$range1[1], max = input$range1[2], value = c(input$range1[1], input$range1[2]))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ####################
  # #create output to "ex_out"
  # ####################
  # 
  # #create table
  # table1 <- reactive({ 
  #   
  #   cases_final %>% ungroup() %>%
  #     filter(region1 == input$var1) %>% 
  #     filter(region2 == input$var2) %>% 
  #     filter(region3 == input$var3) %>%
  #     select(date,cases_cum,cases_new,cases_new_rolling7,cases_new_change1,pop,cases_cum_per_100k,cases_new_per_100k)
  #   
  # })
  # 
  # #display table
  # output$table1 <- renderTable({
  #   
  #   table1()
  #   
  # })
  
  # #create table
  # table2 <- reactive({ 
  #   
  #   maps_filtered <- maps_final %>%
  #     filter(region1 == input$var1) %>% 
  #     filter(region2 == input$var2) %>% 
  #     filter(region3 == input$var3)
  #   
  # })
  
  # tab <- reactive({ 
  #     
  #     cases_final %>% 
  #       filter(region1 == input$var1) %>% 
  #       filter(region2 == input$var2) %>% 
  #       filter(region3 == input$var3)
  #     
  #   })
  # 
  # output$select_var1 <- renderUI({
  #   
  #   selectizeInput('var1', 'Select variable 1', choices = c("select" = "", levels(cases_final$region1)))
  #   
  # })
  # 
  # output$select_var2 <- renderUI({
  #   
  #   choice_var2 <- reactive({
  #     
  #     cases_final %>% 
  #       filter(region1 == input$var1) %>% 
  #       pull(region2) %>% 
  #       as.character()
  #     
  #   })
  #   
  #   selectizeInput('var2', 'Select variable 2', choices = c("select" = "", choice_var2())) # <- put the reactive element here
  #   
  # })
  # 
  # output$select_var3 <- renderUI({
  #   
  #   choice_var3 <- reactive({
  #     cases_final %>% 
  #       filter(region1 == input$var1) %>% 
  #       filter(region2 == input$var2) %>% 
  #       pull(region3) %>% 
  #       as.character()
  #     
  #   })
  # 
  #   selectizeInput('var3', 'Select variable 3', choices = c("select" = "", choice_var3()))
  #   
  # })  
  # 
  # #display table
  # output$table1 <- renderTable({
  # 
  #   tab()
  # 
  # })
  
  
  
  
  
  
  
} #end server




















































###################################################################################
###################################################################################
#COMPILE AND VIEW
###################################################################################
###################################################################################

#use "url" to save values in url,  or "server" to save unique state id on server
shinyApp(ui, server, enableBookmarking = "url")

###################################################################################
###################################################################################
#PUBLISH#
###################################################################################
###################################################################################

#rsconnect::deployApp('C:/Users/Jonathan/Desktop/app.R')






