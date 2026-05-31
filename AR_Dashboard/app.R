##required packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(scales)

# Loading the generated dataset
ar_data <- read.csv("synthetic_ar_data.csv")
ar_data$Aging_Bucket <- factor(ar_data$Aging_Bucket, levels = c("Current", "1-30 Days", "31-60 Days", "61-90 Days", "90+ Days"))

# --- USER INTERFACE (UI) ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "AR Aging Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Detailed Ledger", tabName = "ledger", icon = icon("table"))
    ),
    hr(),
    # Interactive Filters
    selectInput("customer_filter", "Select Customer:", 
                choices = c("All Customers", unique(as.character(ar_data$Customer)))),
    sliderInput("amount_filter", "Invoice Amount Range ($):", 
                min = 0, max = 25000, value = c(0, 25000), step = 500)
  ),
  
  dashboardBody(
    tabItems(
      # Main Dashboard Tab
      tabItem(tabName = "dashboard",
              # Key Performance Indicator (KPI) Cards
              fluidRow(
                valueBoxOutput("total_ar_box", width = 4),
                valueBoxOutput("past_due_box", width = 4),
                valueBoxOutput("dso_box", width = 4)
              ),
              # Charts Section
              fluidRow(
                box(title = "Aging Bucket Distribution (Total Value)", status = "primary", solidHeader = TRUE, width = 7,
                    plotOutput("aging_bar_chart")),
                box(title = "Credit Risk Concentration by Customer", status = "warning", solidHeader = TRUE, width = 5,
                    plotOutput("customer_pie_chart"))
              )
      ),
      # Detailed Ledger Tab
      tabItem(tabName = "ledger",
              fluidRow(
                box(title = "Outstanding Invoice Ledger", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("ledger_table"))
              )
      )
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output) {
  
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    data <- ar_data
    
    if (input$customer_filter != "All Customers") {
      data <- data %>% filter(Customer == input$customer_filter)
    }
    
    data <- data %>% filter(Invoice_Amount >= input$amount_filter[1] & 
                             Invoice_Amount <= input$amount_filter[2])
    return(data)
  })
  
  # KPI Box 1: Total Outstanding AR
  output$total_ar_box <- renderValueBox({
    total_val <- sum(filtered_data()$Invoice_Amount)
    valueBox(
      dollar(total_val), "Total Outstanding AR", icon = icon("dollar-sign"), color = "blue"
    )
  })
  
  # KPI Box 2: Total Past Due
  output$past_due_box <- renderValueBox({
    past_due_val <- filtered_data() %>% 
      filter(Aging_Bucket != "Current") %>% 
      summarise(Total = sum(Invoice_Amount)) %>% 
      pull(Total)
    
    valueBox(
      dollar(ifelse(is.na(past_due_val), 0, past_due_val)), "Total Past Due (>0 Days)", 
      icon = icon("exclamation-triangle"), color = "red"
    )
  })
  
  # KPI Box 3: Proxy Days Sales Outstanding (Avg Collection Delay)
  output$dso_box <- renderValueBox({
    avg_dpd <- mean(filtered_data()$Days_Past_Due, na.rm = TRUE)
    valueBox(
      paste0(round(ifelse(is.nan(avg_dpd), 0, avg_dpd), 1), " Days"), "Avg Days Past Due", 
      icon = icon("clock"), color = "purple"
    )
  })
  
  # Chart 1: Bar Chart of Aging Buckets
  output$aging_bar_chart <- renderPlot({
    summary_bucket <- filtered_data() %>%
      group_by(Aging_Bucket) %>%
      summarise(Total_Amount = sum(Invoice_Amount)) %>%
      complete(Aging_Bucket, fill = list(Total_Amount = 0)) # Keep empty buckets on graph
    
    ggplot(summary_bucket, aes(x = Aging_Bucket, y = Total_Amount, fill = Aging_Bucket)) +
      geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
      scale_fill_brewer(palette = "Reds") +
      scale_y_continuous(labels = dollar) +
      labs(x = "Aging Category", y = "Outstanding Portfolio Value ($)") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))
  })
  
  # Chart 2: Customer Concentration Chart
  output$customer_pie_chart <- renderPlot({
    customer_summary <- filtered_data() %>%
      group_by(Customer) %>%
      summarise(Total_Amount = sum(Invoice_Amount)) %>%
      arrange(desc(Total_Amount)) %>%
      slice_max(Total_Amount, n = 5) # Show top 5 exposure clients
    
    ggplot(customer_summary, aes(x = reorder(Customer, Total_Amount), y = Total_Amount, fill = Customer)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() + # Horizontal bar chart for clean text reading
      scale_y_continuous(labels = dollar) +
      labs(x = "", y = "Total Exposure ($)") +
      theme_minimal() +
      theme(axis.text = element_text(size = 11))
  })
  
  # Table: Interactive Data Ledger
  output$ledger_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, order = list(list(5, 'desc'))), # Default sort by Days Past Due
      rownames = FALSE
    ) %>% 
      formatCurrency('Invoice_Amount', currency = "$")
  })
}

# Running the Application 
shinyApp(ui = ui, server = server)