
####################### Catalyst Code ####################

# In the end, will automate the code   


####################### Launching the package ####################### 

?usethis

# Installing and loadin usethis
if(!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}

######## We shall launch this on go-ahead signal#


# Create a new package
usethis::create_package("integrityR")

########## End of Launch ###################################




# Loading necessary libraries and install if not available
if(!require(dplyr))install.packages("dplyr") # Data manipulation
if(!require(ggplot2))install.packages("ggplot2") # Data visualization
if(!require(reshape2))install.packages("reshape2") # Data reshaping
if(!require(audit))install.packages("audit") # Audit trails for data
if(!require(textTinyR))install.packages("textTinyR") # Text processing
if(!require(stringdist))install.packages("stringdist") # String similarity
if(!require(pwr))install.packages("pwr") # Power analysis
if(!require(rmarkdown))install.packages("rmarkdown") # Document generation
if(!require(officer))install.packages("officer") # Word/PPT generation
if(!require(git2r))install.packages("git2r") # Git version control
if(!require(shiny))install.packages("shiny") # Web applications
if(!require(RColorBrewer))install.packages("RColorBrewer") # Color palettes



# Loading sample data
url <- "https://raw.githubusercontent.com/pnjage/integrityR/30d993ba8eb6ee7a525db90f05b2e92ffad8eedd/MutukuData.csv" 

data <- read.csv(url)

########### 1. Data integrity checks

## Validation functions 

# Checking for missing values
missing_values <- sum(is.na(data))
print(paste("Missing values:", missing_values))

# Identifying outliers in specific columns
columns <- c("C1_p.mites", "C2_p.mites", "C3_p.mites")
for (col in columns) {
  outliers <- boxplot.stats(data[[col]])$out
  cat("Outliers in", col, ":\n")
  print(outliers)
  cat("\n")
}

# Checking for numeric consistency
is_numeric <- sapply(data[, 3:5], is.numeric)
print("Are all values numeric in C1, C2, and C3:")
print(is_numeric)

# Checking for duplicated rows
num_duplicates <- sum(duplicated(data))
cat("Number of duplicated rows:", num_duplicates, "\n")


## Visualizations

# Function to create boxplots for numeric columns
create_boxplots <- function(data) {
  numeric_columns <- c("C1_p.mites", "C2_p.mites", "C3_p.mites")
  for (col in numeric_columns) {
    p <- ggplot(data, aes_string(x = "factor(1)", y = col)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", col), x = NULL, y = col) +
      theme_minimal()
    print(p)
  }
}

# Calling the function to create boxplots
create_boxplots(data)

# Function to create histograms for numeric columns
create_histograms <- function(data) {
  numeric_columns <- c("C1_p.mites", "C2_p.mites", "C3_p.mites")
  for (col in numeric_columns) {
    p <- ggplot(data, aes_string(x = col)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
      theme_minimal()
    print(p)
  }
}

# Calling the function to create histograms
create_histograms(data)

# Function to create a heatmap of correlations
create_heatmap <- function(data) {
  numeric_columns <- c("C1_p.mites", "C2_p.mites", "C3_p.mites")
  corr_matrix <- cor(data[, numeric_columns], use = "complete.obs")
  corr_melt <- melt(corr_matrix)
  p <- ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(title = "Heatmap of Correlations", x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

# Calling the function to create a heatmap
create_heatmap(data)


### Visualization after melting

# Melting the data to long format (assuming 'data' is your original dataset)
data_melt <- melt(data, id.vars = "Treatment", measure.vars = c("C1_p.mites", "C2_p.mites", "C3_p.mites"))

# Creating boxplots for each variable
ggplot(data_melt, aes(x = variable, y = value, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Boxplot of p.mites Values by Treatment",
       x = "Measurement",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  # Optional: Use a color palette for better differentiation


# Histograms for each column

# Melting the data to long format
data_melt <- melt(data, id.vars = "Treatment", measure.vars = c("C1_p.mites", "C2_p.mites", "C3_p.mites"))

# Histograms for each column
ggplot(data_melt, aes(x = value, fill = Treatment)) +
  geom_histogram(bins = 10, alpha = 0.7, position = "dodge") +
  facet_wrap(~variable, scales = "free") +
  ggtitle("Histogram of p.mites") +
  theme_minimal()



# Heatmap for each column

# Melting the data to long format
data_melt <- melt(data, id.vars = "Treatment", measure.vars = c("C1_p.mites", "C2_p.mites", "C3_p.mites"))

# Create a heatmap of values
ggplot(data_melt, aes(x = variable, y = Treatment, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(data_melt$value, na.rm = TRUE)) +
  labs(title = "Heatmap of p.mites Values",
       x = "Measurement",
       y = "Treatment",
       fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# this heatmap provides a clear visual representation of the relationship between treatments and measurements of p.mites, making it easy to identify trends and differences at a glance.




####### Audit trail 

############### Generic audit trail #########

# 1. Initializing the Audit Trail
initialize_audit_trail <- function() {
  audit_trail <- data.frame(
    Timestamp = as.POSIXct(character()),
    Action = character(),
    Details = character(),
    stringsAsFactors = FALSE
  )
  return(audit_trail)
}

# Initializing audit trail
audit_trail <- initialize_audit_trail()

# 2. Logging Action Function
log_action <- function(audit_trail, action, details) {
  new_entry <- data.frame(
    Timestamp = format(Sys.time(), "%d-%m-%Y %H:%M:%S"),
    Action = action,
    Details = details,
    stringsAsFactors = FALSE
  )
  audit_trail <- rbind(audit_trail, new_entry)
  return(audit_trail)
}

# 3. Function to add a new column
add_column <- function(data, col_name, col_value, audit_trail) {
  data[[col_name]] <- col_value
  details <- paste("Added column:", col_name, "with default value:", col_value)
  audit_trail <- log_action(audit_trail, "Add Column", details)
  return(list(data = data, audit_trail = audit_trail))
}

# 4. Function to filter Data
filter_data <- function(data, condition, audit_trail) {
  original_row_count <- nrow(data)
  filtered_data <- subset(data, eval(parse(text = condition)))
  new_row_count <- nrow(filtered_data)
  details <- paste("Filtered data with condition:", condition, "- Rows reduced from", original_row_count, "to", new_row_count)
  audit_trail <- log_action(audit_trail, "Filter Data", details)
  return(list(data = filtered_data, audit_trail = audit_trail))
}

# 5. Function to update values
update_values <- function(data, column_name, old_value, new_value, audit_trail) {
  updated_rows <- sum(data[[column_name]] == old_value)
  data[[column_name]][data[[column_name]] == old_value] <- new_value
  details <- paste("Updated column:", column_name, "- Changed", old_value, "to", new_value, "in", updated_rows, "rows")
  audit_trail <- log_action(audit_trail, "Update Values", details)
  return(list(data = data, audit_trail = audit_trail))
}

# 6. Saving the Audit Trail
save_audit_trail <- function(audit_trail, file_name) {
  tryCatch({
    write.csv(audit_trail, file_name, row.names = FALSE)
    message("Audit trail saved successfully.")
  }, error = function(e) {
    message("Error saving audit trail: ", e$message)
  })
}

# 7. Loading the Audit Trail
load_audit_trail <- function(file_name) {
  tryCatch({
    audit_trail <- read.csv(file_name, stringsAsFactors = FALSE)
    message("Audit trail loaded successfully.")
    return(audit_trail)
  }, error = function(e) {
    message("Error loading audit trail: ", e$message)
    return(initialize_audit_trail())  # Return a new audit trail if loading fails
  })
}

# Example usage:
result <- add_column(data, "New_Column", NA, audit_trail)
data <- result$data
audit_trail <- result$audit_trail

result <- filter_data(data, "C1_p.mites > 1000", audit_trail)
data <- result$data
audit_trail <- result$audit_trail

result <- update_values(data, "C1_p.mites", 897, 900, audit_trail)
data <- result$data
audit_trail <- result$audit_trail

# Saving and load the audit trail
save_audit_trail(audit_trail, "audit_trail.csv")
audit_trail <- load_audit_trail("audit_trail.csv")



####### Plagiarism

# Plagiarism detection
text1 <- "The impact of mites on vegetable crops"
text2 <- "A study on the effect of mites on vegetables"
similarity_score <- stringdist(text1, text2, method = "lv")
print(paste("Similarity score between the two texts:", similarity_score))

# A lower score would indicate that the strings are more similar, and a higher score would mean they are more different. To use this for plagiarism detection, you'd look for a low Levenshtein distance, as that indicates higher similarity between the texts. 

######### See how to search online and give % like Turnitin.



# Statistical misconduct detection - - - see if makes sense..
t_test <- t.test(data$C1_p.mites, data$C2_p.mites)
p_value <- t_test$p.value
print(paste("P-value:", p_value))
if (p_value < 0.05) {
  print("Potential p-hacking detected!")
} else {
  print("No p-hacking detected.")
}


# Example

# Loading necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Function to detect potential p-hacking
detect_p_hacking <- function(p_values, alpha = 0.05, method = "bonferroni") {
  # Check if p_values is a numeric vector
  if (!is.numeric(p_values)) {
    stop("p_values must be a numeric vector.")
  }
  
  # Calculating the number of tests
  num_tests <- length(p_values)
  
  # Adjusting p-values for multiple comparisons
  if (method == "bonferroni") {
    adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
  } else if (method == "holm") {
    adjusted_p_values <- p.adjust(p_values, method = "holm")
  } else {
    stop("Unsupported adjustment method. Use 'bonferroni' or 'holm'.")
  }
  
  # Checking for significant p-values
  significant_results <- adjusted_p_values < alpha
  
  # Summary of findings
  report <- data.frame(
    Test_Number = 1:num_tests,
    Original_P_Value = p_values,
    Adjusted_P_Value = adjusted_p_values,
    Significant = significant_results
  )
  
  # Calculating the proportion of significant results
  proportion_significant <- sum(significant_results) / num_tests
  
  # Detecting potential p-hacking
  if (proportion_significant > 0.5) {
    message("Warning: More than 50% of the tests are significant. Potential p-hacking detected!")
  } else {
    message("No p-hacking detected based on the proportion of significant tests.")
  }
  
  return(report)
}

# Loading your dataset
data <- read.csv("MutukuData.csv")  # Adjust the path as necessary

# Initializing a vector for p-values
p_values <- c()

# Performing a t-test for the two groups
t_test <- t.test(data$C1_p.mites, data$C2_p.mites)
p_values <- c(p_values, t_test$p.value)

# If you have more comparisons, you can append more p-values here
# For example, if you have additional comparisons:
# t_test2 <- t.test(data$C1_p.mites, data$C3_p.mites)
# p_values <- c(p_values, t_test2$p.value)

# Calling the function to detect p-hacking
report <- detect_p_hacking(p_values)
print(report)


# Reproducibility assessment
block_means <- sapply(unique(data$Block), function(block) {
  colMeans(subset(data, Block == block)[, 3:5], na.rm = TRUE)
})
reproducibility_check <- as.data.frame(t(block_means))
rownames(reproducibility_check) <- paste("Block", unique(data$Block))
print(reproducibility_check)



# Reporting framework - - to revise

library(officer)
library(ggplot2)

missing_values <- sum(is.na(data))
outliers_c1 <- boxplot.stats(data$C1_p.mites)$out
reproducibility_check <- data.frame(
  C1_p.mites = c(670.75, 677.50, 868.25),
  C2_p.mites = c(426.25, 514.00, 624.75),
  C3_p.mites = c(262.25, 289.00, 348.50)
)
rownames(reproducibility_check) <- c("Block 1", "Block 2", "Block 3")


# Creating a Word document for the report
doc <- read_docx() %>%
  body_add_par("Research Integrity Analysis Report", style = "heading 1") %>%
  body_add_par("1. Introduction", style = "heading 2") %>%
  body_add_par("This report evaluates the integrity of the research data...") %>%
  body_add_par("2. Data Summary", style = "heading 2") %>%
  body_add_par(paste("Number of observations:", nrow(data))) %>%
  body_add_par(paste("Number of variables:", ncol(data))) %>%
  body_add_par("3. Missing Values", style = "heading 2") %>%
  body_add_par(paste("Total missing values:", missing_values)) %>%
  body_add_par("4. Outliers in C1_p.mites", style = "heading 2") %>%
  body_add_par(ifelse(length(outliers_c1) > 0, 
                      paste("Outliers detected:", paste(outliers_c1, collapse = ", ")), 
                      "No outliers detected in C1_p.mites.")) %>%
  body_add_par("5. Reproducibility Analysis", style = "heading 2") %>%
  body_add_table(reproducibility_check, style = "table_template") %>%
  body_add_par("6. Conclusion", style = "heading 2") %>%
  body_add_par("The data appears consistent...") %>%
  body_add_par(paste("Report generated on:", Sys.Date()), style = "Normal")

# Saving the report
print(doc, target = "Comprehensive_Integrity_Report.docx")




# Collaboration

library(shiny)
library(ggplot2)

# Shiny app UI
ui <- fluidPage(
  titlePanel("Research Integrity Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose Variable for Boxplot:", names(data)[3:5]),
      downloadButton("downloadReport", "Download Report")
    ),
    mainPanel(
      plotOutput("boxplot"),
      textOutput("summary"),
      tableOutput("reproducibility")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$boxplot <- renderPlot({
    ggplot(data, aes_string(x = "Treatment", y = input$var)) + 
      geom_boxplot(outlier.colour = "red", outlier.size = 2) +
      labs(title = paste("Boxplot of", input$var), x = "Treatment", y = input$var) +
      theme_minimal()
  })
  
  output$summary <- renderText({
    summary_stats <- summary(data[[input$var]])
    paste("Summary Statistics for", input$var, ":\n", paste(capture.output(summary_stats), collapse = "\n"))
  })
  
  output$reproducibility <- renderTable({
    reproducibility_check
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Integrity_Report_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx() %>%
        body_add_par("Research Integrity Analysis Report", style = "heading 1") %>%
        body_add_par(paste("Generated on:", Sys.Date())) %>%
        body_add_par("Data Summary:", style = "heading 2") %>%
        body_add_par(paste("Missing values:", missing_values)) %>%
        body_add_par("Outliers in C1_p.mites:", style = "heading 2") %>%
        body_add_par(paste("Outliers detected:", paste(outliers_c1, collapse = ", "))) %>%
        body_add_par("Reproducibility Analysis:", style = "heading 2") %>%
        body_add_table(reproducibility_check, style = "table_template")
      
      print(doc, target = file)
    }
  )
}

# Running the Shiny app
shinyApp(ui = ui, server = server)
