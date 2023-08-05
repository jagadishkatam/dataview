#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(haven)
library(shinythemes)
library(DataExplorer)
library(labelled)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("spacelab"),
                
                # Application title
                titlePanel("DataView App"),
                
                # Show a plot of the generated distribution
                sidebarLayout(
                  sidebarPanel(
                    # Sidebar with a slider input for number of bins 
                    fileInput("data",
                              "Upload the Data"),
                    
                    textInput('tdata', 'Filter', value = NULL),
                    varSelectInput("variables1", "Choose the Variable to subset", NULL, selected = NULL, multiple = FALSE),
                    selectInput('var1','Select Values:',choices = '', selected = NULL, multiple = TRUE),
                    selectInput('cond1','Coondition:',choices = c('%in%','<','>','<=','>=','!=','&'), selected = NULL, multiple = FALSE),
                    varSelectInput("variables", "Variables to View:", NULL, multiple = TRUE),
                    actionButton("load","Load"),
                    h5(''),
                    verbatimTextOutput("input_dict")
                  ), #sidebarPanel
                  
                  mainPanel(
                    tabsetPanel(id='dataset',
                                tabPanel('Basics',verbatimTextOutput("input_intro")),
                                tabPanel('Dataset',DTOutput("input_file")),
                                tabPanel('Summary',verbatimTextOutput("input_sum"))
                    ) #tabsetPanel
                  ) #mainPanel
                  
                ) #sidebarpanel    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  sampled <- reactive({
 mtcars %>% rownames_to_column(var = 'cars')
  })
  
  
  observeEvent(input$variables1, {
    updateSelectInput(session,inputId = "var1", 'Select Values:', choices = levels(as.factor(sampled()[[input$variables1]])),selected = NULL)
    
  })
  

  keeptabs <- reactive({
    paste(list(input$variables1, input$var1, input$cond1), sep = '&')
  })
  
  # Now, using keep tabs we can update the value of filter
  
  observeEvent(keeptabs(), {
    variable1 <- input$variables1
  
    if(input$cond1 %in% c('%in%','!=') ){
    updateTextInput(session,inputId = "tdata", 'Filter', 
                    value = paste(input$variables1,input$cond1,'c(',paste0('"',input$var1,'"',collapse = ','),')'))
    # } else if(input$cond1 %in% c('%in%','!=') & class(input$variables1)!='character'){
    #   updateTextInput(session,inputId = "tdata", 'Filter', 
    #                   value = paste(input$variables1,input$cond1,'c(',paste0(input$var1,collapse = ','),')'))
    } else {
      updateTextInput(session,inputId = "tdata", 'Filter', 
                      value = paste(input$variables1,input$cond1,input$var1))
    }
  })
  
  observe({
    req(sampled())
    updateVarSelectInput(session, "variables", "Variables to View:", sampled())
    updateVarSelectInput(session, "variables1", "Choose the Variable to subset", sampled())
  })
  
  
  
  nsampled <- reactive({
    # tdata2 <- unique(input$tdata)
    # input$saveFilterButton
    expre <- parse(text = input$tdata)
    if(input$tdata != ''){
      if (length(input$variables)==0) {
        sampled() %>% 
          filter(eval(expre)) 
      } else{
        sampled() %>% 
          filter(eval(expre)) %>% dplyr::select(!!!input$variables)
      }
    } else {
      if (length(input$variables)==0) {
        sampled() 
      } else {
        sampled() %>% dplyr::select(!!!input$variables)
      }
    }
  }) %>%  
    bindEvent(input$load)
  
  
  output$input_file <- renderDataTable({
    # tdata2 <- unique(input$tdata)
    # input$saveFilterButton
    expre <- parse(text = input$tdata)
    if(input$tdata != ''){
      if (length(input$variables)==0) {
        sampled() %>% 
          filter(eval(expre)) 
      } else{
        sampled() %>% 
          filter(eval(expre)) %>% dplyr::select(!!!input$variables)
      }
    } else {
      if (length(input$variables)==0) {
        sampled2() 
      } else {
        sampled() %>% dplyr::select(!!!input$variables)
      }
    }
  }) %>%  
    bindEvent(input$load)
  
  output$input_sum <- renderPrint({
    Hmisc::describe(nsampled())
  }) %>%  
    bindEvent(input$load)  
  
  output$input_dict <- renderPrint({
    labelled::generate_dictionary(nsampled()) %>% dplyr::select(variable, label, col_type)
  }) %>%  
    bindEvent(input$load)   
  
  
  output$input_intro <- renderPrint({
    nsampled2 <- DataExplorer::introduce(nsampled())
    nsampled3 <- nsampled2 %>% pivot_longer(everything(), names_to = 'name3', values_to = 'Value') %>% 
      mutate(name2=stringr::str_to_sentence(name3), Name=stringr::str_replace_all(name2,'\\_',' ')) %>%
      slice_head(n=5) %>% dplyr::select(Name, Value)
    nsampled3
  }) %>%  
    bindEvent(input$load)  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
