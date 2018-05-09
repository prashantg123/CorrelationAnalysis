library(shiny)
library(ggplot2)
library(dygraphs)
dataset <- diamonds

instrumetnsList<-c(Choose='', "((AD*.47)-(TY*.12)+(ax*1))"="AudStruc", "((EU*.21) - (TY * .12) + (dx * 1))"="EurStruc", 
                   "((bp*.47) - (TY * .12) + (gl * 1))"="PudStruc", "((cd*.47) - (TY * .12) + (cb * 1))"="CadStruc",
                   "AUD"="ad", "EUR"="eu", "CAD"="cd", "YEN"="jy", "BPD"="bp", "NOTES"="ty", "TENS"="ax", "BUND"="bn", "CDBND"="cb",
                   "GILT"="gl", "JYBND"="jb", "Fives"="fv", "THREES"="ay", "IR1"="au1", "IR2"="au2", "IR3"="au3", "IR4"="au4", "IR5"="au5",
                   "Emini"="es", "DAX"="dx", "DOW"="dw", "Stoxx"="stx", "FTSE"="tsx", "Oil"="cl")

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green;
                    }
                    "))
    ),
  #headerPanel("Hello Shiny!"),
  titlePanel("MA Distance", windowTitle="MA Distance"),
  #img(src="win.jpg",height=100,width=500,align="center"),
  
  #title = "Diamonds Explorer",
  hr(),
  
  sidebarLayout(
    # Sidebar with a slider input
    tags$head(
      sidebarPanel(
        HTML('<script type="text/javascript">
             $(document).ready(function() {
             $("#DownloadButton").click(function() {
             $("#Download").text("Loading...");
             });
             });
             </script>
             '),
        tags$link(rel="stylesheet", type="text/css", href="StyleFile.css"),
        fluidRow(
          h4("Select Date Range"),
          dateRangeInput("Date", "", start = "2015-07-14", end = "2016-02-01" , min = Sys.Date()-4000,
                         max = Sys.Date(), format = "yyyy-mm-dd", startview = "year", weekstart = 1,
                         language = "en", separator = " to ", width = '250px'),
          hr()
        ),
        fluidRow(
          h4("Create Model/Structure/Portfolio"),
          column( width = 12,selectInput('Models', "Select Models/Instruments" , instrumetnsList, multiple=T, selectize=T)
          )
          #tagList(
            #div(strong(label), style="margin-top: 5px;"),
            #tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}")
            #tags$textarea(id="textar", placeholder = "please select.", rows = 10, cols=1, value="aa")),
            #verbatimTextOutput("output_text")
          ),
        fluidRow(
          hr(),
          h4("Select Time interval"),
          column(4, numericInput("nInterval",'',10, width = '150px')
          ),
          column(5, selectInput('Interval', '', c("mins", "hours", "daily", "weekly"), selectize=TRUE, width = '150px')
          )
        ),
        fluidRow(
          hr(),
          column(5, actionButton("goButton", "Test Structure") #submitButton("Create Charts")
          )
        )
        )#head
    ),#sidebarPanel
    mainPanel(
      fixedRow(
        #column(1,strong("SMA: ")),
        column(4,numericInput("SMA","SMA", 200, width = '150px')),
        column(5,numericInput("RefValue","RefValue", 4, width = '150px'))
        #column(3, actionButton("goButton", "Create Charts") #submitButton("Create Charts")
        #),
        #div(strong("SMA: "), numericInput("SMA","", 200, width = '150px'),width = '350px')
        #div(strong("To: "), textOutput("to", inline = TRUE))
        #column(2, textOutput("VolOut")
        #)
      ),
      fluidRow(
        # hr(),
        
        tabsetPanel(id= "tabPanels", type = "tabs", 
                    tabPanel("Price Chart", dygraphOutput("PricePlot"), value="panel1"), 
                    #tabPanel("Probability Chart", plotOutput("ProbPlot"), value="panel2"),
                    #tabPanel("Summary",  plotOutput("BoxPlot"), verbatimTextOutput("SummaryAbove"), verbatimTextOutput("SummaryBelow"), value="panel3"),
                    tabPanel("Corr",  plotOutput("CorrPlot"), plotOutput("VolatilityPlot"), plotOutput("RangePlot"), value="panel4")
                    #tabPanel("Change Chart", plotOutput("ChangePlot")) 
        ),
        hr()
      )
    ),
    
    position = c("left"),
    fluid = T
    )#sidebarLayout
  ))


#runApp('D:/SHINYPROJECT',host="192.168.115.234",port=3700)

#runApp('D:/SHINYPROJECT',host="0.0.0.0",port=3700)
