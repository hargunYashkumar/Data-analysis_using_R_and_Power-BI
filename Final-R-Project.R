#data cleaning and changing data type

data = read.csv("D://codes//projects//Diwali_Sales_Analysis_Project//Diwali_Sales_Data.csv")
head(data)

data$Status <- NULL
data$unnamed1 <- NULL

sum(is.na(data))
str(data)
summary(data)

data$Amount = ifelse(is.na(data$Amount), median(data$Amount, na.rm = TRUE), data$Amount)
sum(is.na(data))
str(data)

data$Gender <- ifelse(data$Gender == "F", "Female", 
                      ifelse(data$Gender == "M", "Male", data$Gender))

data$Marital_Status = ifelse(data$Marital_Status==0,"Un-married",
                             ifelse(data$Marital_Status==1,"Married", data$Marital_Status))

print(unique(data$State))
data$State = ifelse(data$State == "Andhra\xa0Pradesh", "Andhra Pradesh" , data$State)

write.csv(data, file = "D://codes//projects//Diwali_Sales_Analysis_Project//cleaned_data.csv", row.names = FALSE)


# Stats about the data
data = read.csv("D://codes//projects//Diwali_Sales_Analysis_Project//cleaned_data.csv")


mean(data$Age)
median(data$Age)
min(data$Age)
max(data$Age)

mean(data$Orders)
median(data$Orders)
sum(data$Orders)

mean(data$Amount)
median(data$Amount)
sum(data$Amount)

var(data$Age)
sd(data$Age)
range(data$Age)
quantile(data$Age)
IQR(data$Amount)

# perform t-testing
# Load necessary libraries
library(dplyr)

# Load the dataset (make sure to set the correct path)
data <- read.csv("path_to_your_file/cleaned_data.csv", stringsAsFactors = FALSE)

# Perform one-sample t-test on the Amount column
# Hypothetical population mean (e.g., $500)
population_mean <- 500

# Calculate the t-test
t_test_result <- t.test(data$Amount, mu = population_mean)

# Print the result
print(t_test_result)

# graph

# Load necessary libraries
library(tidyverse)
data <- read.csv("D://codes//projects//Diwali_Sales_Analysis_Project//cleaned_data.csv")


# Assuming `df` is already defined earlier
ggplot(df, aes(x = Age.Group, y = Amount)) +
  geom_bar(stat = "identity", colour = "blue", fill = "white") + # Corrected 'white' instead of 'while'
  theme_bw() + 
  ggtitle("Bar Chart") 

# Line graph: Age vs Amount
ggplot(data, aes(x = Orders, y = Gender)) +
  geom_line(color = "blue") +
  labs(title = "Amount Spent by Age", x = "Age", y = "Amount") +
  theme_minimal()

# Scatter plot: Orders vs Amount
ggplot(data, aes(x = Age, y = State)) +
  geom_point(color = "red", size = 2) +
  labs(title = "Orders vs Amount Spent", x = "Orders", y = "Amount") +
  theme_minimal()


# advance visual

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the CSV file
dataaa <- read.csv("D://codes//projects//Diwali_Sales_Analysis_Project//cleaned_data.csv")

# Aggregate data by Occupation and Age.Group, summarizing the total Amount
heatmap_data <- dataaa %>%
  group_by(Occupation, Age.Group) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE))

print(heatmap_data)
# Create the heatmap using ggplot2
ggplot(heatmap_data, aes(x = Occupation, y = Age.Group, fill = Total_Amount)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Heatmap of Total Amount by Occupation and Age Group", 
       x = "Occupation", 
       y = "Age Group", 
       fill = "Total Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Install and load necessary libraries
install.packages("ggplot2")
install.packages("treemapify")

library(ggplot2)
library(treemapify)
library(dplyr)


# Aggregate data by Product_Category and State, summarizing the total Amount
treemap_data <- dataaa %>%
  group_by(Product_Category, State) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE))

# Create the treemap using ggplot2 and treemapify
ggplot(treemap_data, aes(area = Total_Amount, fill = Product_Category, 
                         label = paste(State, "\n", Total_Amount))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Treemap of Total Amount by Product Category and State") +
  theme_minimal()

treemapdata <- dataaa %>%
  group_by(State) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE))

# Create the faceted plot
ggplot(treemap_dataa, aes(x = State, y = Total_Amount, fill = State)) +
  geom_bar(stat = "identity") +  # Bar chart for total amount by state
  facet_wrap(~ Product_Category) +  # Create a facet for each product category
  scale_fill_brewer(palette = "Set3") +  # Use a color palette
  labs(title = "Total Sales Amount by State and Product Category", 
       x = "State", 
       y = "Total Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity

# machine learning
# Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(caret)       # For splitting data and model evaluation

# Load the data
data <- read.csv("D://codes//projects//Diwali_Sales_Analysis_Project//cleaned_data.csv", encoding = "ISO-8859-1")

# Check the structure of the data
str(data)

# Convert categorical variables to factors (if applicable)
data$Gender <- as.factor(data$Gender)
data$Age.Group <- as.factor(data$Age.Group)
data$Marital_Status <- as.factor(data$Marital_Status)
data$State <- as.factor(data$State)
data$Zone <- as.factor(data$Zone)
data$Occupation <- as.factor(data$Occupation)
data$Product_Category <- as.factor(data$Product_Category)

# Remove non-predictive columns (e.g., User_ID, Cust_name, Product_ID)
data <- data %>% select(-User_ID, -Cust_name, -Product_ID)

# Split the data into training and test sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$Amount, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train a Linear Regression Model
linear_model <- lm(Amount ~ ., data = trainData)

# Print model summary to see coefficients and model fit statistics
summary(linear_model)

# Predict on the test data
predictions <- predict(linear_model, testData)

# Evaluate the model using Root Mean Squared Error (RMSE)
RMSE <- sqrt(mean((predictions - testData$Amount)^2))
cat("Root Mean Squared Error (RMSE):", RMSE, "\n") 

# Plot actual vs predicted values
ggplot(data = testData, aes(x = predictions, y = Amount)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Amount",
       x = "Predicted Amount", y = "Actual Amount")

# Rshiny

# Required Libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(treemap)
library(DT)

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Advanced Sales Dashboard", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Advanced Visualizations", tabName = "advanced_visuals", icon = icon("chart-bar")),
      menuItem("3D Visualizations", tabName = "3d_visualizations", icon = icon("cube")),
      HTML("<h3>Filter Options</h3>"),
      fileInput("file_upload", "Upload Dataset:", accept = c(".csv")),
      selectInput("state", "Select State:", choices = c("All")),
      selectInput("gender", "Select Gender:", choices = c("All")),
      selectInput("product_category", "Product Category:", choices = c("All")),
      actionButton("apply", "Apply Filters", class = "btn-primary")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { 
          border-radius: 10px; 
          padding: 15px; 
          color: #333;
          font-size: 16px; 
        }
        .small-box h3 {
          font-size: 28px;
        }
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales_box", width = 4),
                valueBoxOutput("avg_sales_box", width = 4),
                valueBoxOutput("num_transactions_box", width = 4)
              ),
              fluidRow(
                box(plotlyOutput("summary_plot"), width = 12, title = "Sales Overview by State", solidHeader = TRUE)
              ),
              fluidRow(
                box(DT::dataTableOutput("data_table"), width = 12, title = "Detailed Data", solidHeader = TRUE)
              ),
              fluidRow(
                box(textOutput("data_summary"), width = 12, title = "Data Summary", solidHeader = TRUE)
              )
      ),
      
      # Advanced Visualizations Tab
      tabItem(tabName = "advanced_visuals",
              fluidRow(
                box(selectInput("advanced_plot", "Select Visualization:", 
                                choices = c("Treemap", "Heatmap")), 
                    width = 12, title = "Select Visualization Type", solidHeader = TRUE),
                uiOutput("dynamic_visualization")
              )
      ),
      
      # 3D Visualizations Tab
      tabItem(tabName = "3d_visualizations",
              fluidRow(
                box(plotlyOutput("scatter3d"), width = 12, title = "3D Sales Visualization", solidHeader = TRUE)
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load Dataset
  data <- reactiveVal(NULL)
  
  observe({
    req(input$file_upload)
    uploaded_file <- input$file_upload$datapath
    df <- read.csv(uploaded_file, encoding = "ISO-8859-1")
    data(df)
    
    # Update dropdown choices dynamically based on uploaded data
    updateSelectInput(session, "state", choices = c("All", unique(df$State)))
    updateSelectInput(session, "gender", choices = c("All", unique(df$Gender)))
    updateSelectInput(session, "product_category", choices = c("All", unique(df$Product_Category)))
  })
  
  # Filtered Data Reactive Function
  filtered_data <- reactive({
    req(data())
    data_filtered <- data()
    if (input$state != "All") {
      data_filtered <- data_filtered %>% filter(State == input$state)
    }
    if (input$gender != "All") {
      data_filtered <- data_filtered %>% filter(Gender == input$gender)
    }
    if (input$product_category != "All") {
      data_filtered <- data_filtered %>% filter(Product_Category == input$product_category)
    }
    data_filtered
  })
  
  # Summary Boxes
  output$total_sales_box <- renderValueBox({
    valueBox(
      value = paste0("$", format(sum(filtered_data()$Amount, na.rm = TRUE), big.mark = ",")),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$avg_sales_box <- renderValueBox({
    valueBox(
      value = paste0("$", round(mean(filtered_data()$Amount, na.rm = TRUE), 2)),
      subtitle = "Average Sale Amount",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$num_transactions_box <- renderValueBox({
    valueBox(
      value = sum(filtered_data()$Orders, na.rm = TRUE),
      subtitle = "Number of Transactions",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  # Sales Summary Plot
  output$summary_plot <- renderPlotly({
    plot_data <- filtered_data() %>% group_by(State) %>% summarise(Total_Sales = sum(Amount, na.rm = TRUE))
    
    plot <- ggplot(plot_data, aes(x = reorder(State, Total_Sales), y = Total_Sales)) +
      geom_bar(stat = "identity", fill = "#4a6b8c") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Total Sales by State", x = "State", y = "Total Sales ($)")
    
    ggplotly(plot)
  })
  
  # Dynamic Visualizations
  output$dynamic_visualization <- renderUI({
    req(input$advanced_plot)
    if (input$advanced_plot == "Treemap") {
      box(plotlyOutput("treemap"), width = 12, title = "Sales Treemap by Category", solidHeader = TRUE)
    } else if (input$advanced_plot == "Heatmap") {
      box(plotlyOutput("heatmap"), width = 12, title = "Sales Heatmap by Age and Gender", solidHeader = TRUE)
    }
  })
  
  output$treemap <- renderPlotly({
    treemap_data <- filtered_data() %>% group_by(Product_Category) %>% summarise(Total_Sales = sum(Amount, na.rm = TRUE))
    
    plot_ly(
      treemap_data,
      labels = ~Product_Category,
      parents = NA,
      values = ~Total_Sales,
      type = 'treemap',
      textinfo = "label+value+percent parent"
    )
  })
  
  output$heatmap <- renderPlotly({
    heatmap_data <- filtered_data() %>% group_by(Age.Group, Gender) %>% summarise(Total_Sales = sum(Amount, na.rm = TRUE))
    
    ggplotly(
      ggplot(heatmap_data, aes(x = Age.Group, y = Gender, fill = Total_Sales)) +
        geom_tile() +
        scale_fill_gradient(low = "#eff3ff", high = "#084594") +
        labs(title = "Heatmap of Sales by Age Group and Gender", x = "Age Group", y = "Gender") +
        theme_minimal()
    )
  })
  
  # 3D Visualization
  output$scatter3d <- renderPlotly({
    plot_data <- filtered_data()
    plot_ly(
      plot_data,
      x = ~Amount,
      y = ~Orders,
      z = ~Age,
      color = ~Gender,
      type = "scatter3d",
      mode = "markers"
    ) %>%
      layout(
        title = "3D Sales Analysis",
        scene = list(
          xaxis = list(title = "Amount"),
          yaxis = list(title = "Orders"),
          zaxis = list(title = "Age")
        )
      )
  })
  
  # Data Table
  output$data_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Summary Text
  output$data_summary <- renderText({
    req(filtered_data())
    summary_data <- filtered_data()
    
    if (nrow(summary_data) > 0) {
      max_sales <- summary_data[which.max(summary_data$Amount), ]
      paste(
        max_sales$Gender, "from", max_sales$State, 
        "working in", max_sales$Occupation, 
        "of age group", max_sales$Age.Group, 
        "has spent $", max_sales$Amount, 
        "on", max_sales$Product_Category, "products."
      )
    } else {
      "No data available for the selected filters."
    }
  })
}

# Run the App
shinyApp(ui, server)


# end of project
