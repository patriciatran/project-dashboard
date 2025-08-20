library(shiny)
library(tidyverse)
library(lubridate)

ui <- fluidPage(
  titlePanel("Projects Summary Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload projects.tsv",
                accept = c(".tsv", ".txt")),
      selectInput("college", "Select College:", 
                  choices = c("All", "CALS", "Other")),
      selectInput("year", "Select Year:", choices = NULL),
      uiOutput("department_ui")
    ),
    
    mainPanel(
      
      wellPanel(
        h4("About this Dashboard"),
        p("This dashboard summarizes project activity by year, college, 
           department, and user group. Use the filters on the left to 
           update the tables and plots. The summary statement below updates 
           dynamically depending on your selected filters.
          
          The Bioinformatics Research Support Services is a research support resource that helps researchers with bioinformatics and computing for their research. We are housed in the Department of Bacteriology at UW-Madison. The service started in Fall 2023.")
      ),
      
      h4("Summary Statement"),
      textOutput("summary_text"),
      strong(textOutput("file_info")) ,
      br(),
      
      
      tabsetPanel(
        tabPanel("Summary (All Colleges)", 
                 tableOutput("summary_all")),
        tabPanel("Summary (CALS only)", 
                 tableOutput("summary_cals")),
        tabPanel("Plots", 
                 plotOutput("plot_meetings"))
      ),
      tags$footer(
        p("Copyright © 2025. All rights reserved. Dashboard by Patricia Tran (ptran5@wisc.edu) | bioinformatics.bact.wisc.edu"),
        align = "left",
        style = "position: fixed; bottom: 0; width: 100%; background-color: #f0f0f0; padding: 10px;"
      )
    )
  )
)

server <- function(input, output, session) {
  
  projects_data <- reactive({
    req(input$file)
    df <- read.table(input$file$datapath, sep = "\t", header = TRUE, quote = "", encoding="UTF-8")
    df$Year <- year(mdy(df$Date))
    df <- df %>% mutate(College = ifelse(College == "CALS", "CALS", "Other"))
    return(df)
  })
  
  output$summary_text <- renderText({
    req(projects_data(), input$year)
    df <- projects_data()
    
    # if user selected "All", then combine all years
    if (input$year != "All") {
      df <- df %>% filter(Year == input$year)
    }
    
    dep_count <- df %>% select(Department) %>% distinct() %>% nrow()
    user_count <- df %>% select(Name) %>% distinct() %>% nrow()
    pi_count <- df %>% select(Lab) %>% distinct() %>% nrow()
    meeting_count <- nrow(df)
    
    year_label <- ifelse(input$year == "All", "all years", input$year)
    
    paste0("We served a total of ",
           dep_count," departments, ",
           user_count," clients, ",
           pi_count," PIs, in ",
           meeting_count," meetings in ", year_label, ".")
  })
  projects_data <- reactive({
    req(input$file)
    df <- read.table(input$file$datapath, sep = "\t", header = TRUE, quote = "", encoding="UTF-8")
    df$Year <- year(mdy(df$Date))
    df <- df %>% mutate(College = ifelse(College == "CALS", "CALS", "Other"))
    return(df)
  })
  
  observe({
    df <- projects_data()
    years <- sort(unique(df$Year))
    updateSelectInput(session, "year", choices = c("All", years))
  })
  
  output$department_ui <- renderUI({
    req(input$college)
    df <- projects_data()
    if (input$college == "CALS") {
      deps <- sort(unique(df$Department[df$College == "CALS"]))
      selectInput("department", "Select Department:", 
                  choices = c("All", deps))
    }
  })
  
  output$file_info <- renderText({
    req(input$file)
    
    info <- file.info(input$file$datapath)
    last_modified <- info$mtime
    
    paste("The input data table was last modified on:", format(last_modified, "%Y-%m-%d %H:%M:%S"))
  })
  
  # ---- Summary All Colleges ----
  summary_all <- reactive({
    df <- projects_data()
    meetings <- df %>% group_by(Year, College) %>% tally() %>% rename("Meetings"=n)
    pis <- df %>% select(Year, College, Lab) %>% distinct() %>% 
      group_by(Year, College) %>% tally() %>% rename("PI"="n")
    users <- df %>% select(Year, College, Name) %>% distinct() %>% 
      group_by(Year, College) %>% tally() %>% rename("User"="n")
    
    out <- full_join(full_join(meetings, users), pis)
    if (input$year != "All") out <- out %>% filter(Year == input$year)
    if (input$college != "All") out <- out %>% filter(College == input$college)
    return(out)
  })
  
  output$summary_all <- renderTable({
    summary_all()
  })
  
  
  # ---- Summary CALS ----
  summary_cals <- reactive({
    df <- projects_data() %>% filter(College == "CALS")
    
    meetings <- df %>% group_by(Year, Department) %>% tally() %>% rename("Meetings"=n)
    pis <- df %>% select(Year, Department, Lab) %>% distinct() %>% 
      group_by(Year, Department) %>% tally() %>% rename("PI"="n")
    users <- df %>% select(Year, Department, Name) %>% distinct() %>% 
      group_by(Year, Department) %>% tally() %>% rename("User"="n")
    
    out <- full_join(full_join(meetings, users), pis)
    
    if (input$year != "All") out <- out %>% filter(Year == input$year)
    if (!is.null(input$department) && input$department != "All") {
      out <- out %>% filter(Department == input$department)
    }
    return(out)
  })
  
  output$summary_cals <- renderTable({
    summary_cals()
  })
  
  # ---- Plot ----
  output$plot_meetings <- renderPlot({
    df <- summary_all()
    ggplot(df, aes(x = Year, y = Meetings, fill = College)) +
      geom_col(position = "dodge") +
      labs(title = "Meetings per Year by College",
           x = "Year", y = "Meetings")+
      scale_fill_manual(values=c("skyblue2","pink4"))+
      theme(text = element_text(size=40))
  })
}

shinyApp(ui, server)
