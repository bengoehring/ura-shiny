library(tidyverse)
library(ura)
library(shinythemes)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Evaluate RAs",
             tabPanel("Introduction",
                      # Introduction Text
                      mainPanel(
                        h3("Introduction"),
                        p("Researchers often employ students and research assistants to assist in coding data. This application calculates the share of actions for which a rater agrees with the other raters performing the task, offering a sense of the relative consistency of each rater."), 
                        br(),
                        p("This application implements code from the ura package. If you would like to calcuate additional statistics or examine the underlying code, you can install the R package from GitHub: "),
                        code('devtools::install_github("bengoehring/ura")'),
                        br(),
                        br(),
                        p("If you would like to learn more about working with research assistants and tips for training and managing them conducting common coding tasks, please refer to the working paper available to download "), a("here.", href = "https://bengoehring.github.io/files/perspectives-paper.pdf"),
                        br(),
                        h3("Instructions:"),
                        p("1) Read in the file containing the coding putput from your raters. Each row should contain the information for a given rater-subject (i.e., the coding value inputted for subject 10 by the rater named Ben). It is OK for the csv to contain additional columns."),
                        p("2) Provide the names of the rater, subject, and coding output columns."),
                        p("3) Press Enter."),
                        br(),
                        h3("Additional Information"),
                        p("Please report any issues or questions on Github via this link INSERT HYPERLINK. You can also reach out to me directly at bengoehr@umich.edu.")
                      ),
             ),
             tabPanel("Evaluate Raters",
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("dataset", "Choose File"),
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          # Input: Select separator ----
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          selectInput("select_rater", "Select rater column", c('')),
                          helpText(em("The rater column should be numeric. If it is not numeric, it will be recoded.")),
                          
                          selectInput("select_subject", "Select subject column", c('')),
                          helpText(em("The subject column should be numeric. If it is not numeric, it will be recoded.")),
                          
                          selectInput("select_coding", "Select coding column", c('')),
                          helpText(em("The rater column must be numeric.")),
                          
                          actionButton("update", "Enter", class = "btn-primary",style='padding:4px; font-size:120%')
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Input Data", DT::dataTableOutput("mytable")), 
                            tabPanel("Coder Agreement", DT::dataTableOutput("percent_agree")), 
                          )
                        )
                      )
             )
             )
  )

server <- function(session, input, output) {
  
  # input dataset
  data <- reactive({
    req(input$dataset)
    read.csv(input$dataset$datapath, 
             header = input$header,
             sep = input$sep)
  })
  
  filtereddata <- eventReactive({
    input$update
    data()
  },  {
    req(data())
    if(is.null(input$select) || input$select == "")
      data() else 
        data()[, colnames(data()) %in% input$select]
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "select_rater", choices=colnames(data()))
    updateSelectInput(session, "select_subject", choices=colnames(data()))
    updateSelectInput(session, "select_coding", choices=colnames(data()))
  })
  
  # output coder agreement
  output$mytable <- renderDataTable(filtereddata())
  
  
  output$percent_agree <- renderDataTable({
    ura_out <- ura::coder_agreement(filtereddata(),
                                    rater_column = input$select_rater,
                                    subject_column = input$select_subject,
                                    coding_column = input$select_coding)
  })
} 


shinyApp(ui = ui, server = server)

