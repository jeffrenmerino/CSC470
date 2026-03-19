library(btw)
library(ellmer)
library(shinychat)
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(DT)          # for data table
library(bsicons)     # for value boxes

# Load API key
Sys.setenv(ANTHROPIC_API_KEY = readLines("api-key.txt"))

# ==============================================================================
# CUSTOM CSS (for a polished look)
# ==============================================================================
custom_css <- "
.chat-container {
  background: white;
  border-radius: 20px;
  border: 1px solid rgba(0,0,0,0.06);
  box-shadow: 0 10px 40px rgba(0,0,0,0.1);
  height: 100%;
  display: flex;
  flex-direction: column;
}
.chat-messages {
  flex: 1;
  overflow-y: auto;
  padding: 20px;
}
.chat-input-area {
  padding: 16px;
  border-top: 1px solid #e5e5e5;
}
"

# ==============================================================================
# ENHANCED SYSTEM PROMPT WITH DATA AWARENESS
# ==============================================================================
create_system_prompt <- function(data_summary = NULL) {
  base_prompt <- readLines("system-prompt.Rmd") |> paste(collapse = "\n")
  
  if (!is.null(data_summary)) {
    data_context <- paste0(
      "\n\n## UPLOADED DATA CONTEXT\n\n",
      "The user has uploaded a CSV file with the following characteristics:\n\n",
      "- **File**: ", data_summary$filename, "\n",
      "- **Rows**: ", data_summary$n_rows, "\n",
      "- **Columns**: ", data_summary$n_cols, "\n",
      "- **Column names**: ", paste(data_summary$col_names, collapse = ", "), "\n",
      "- **Numeric columns**: ", paste(data_summary$numeric_cols, collapse = ", "), 
      " (", length(data_summary$numeric_cols), " total)\n",
      "- **Categorical columns**: ", paste(data_summary$categorical_cols, collapse = ", "), 
      " (", length(data_summary$categorical_cols), " total)\n\n",
      "**IMPORTANT**: When providing code, use these actual column names. ",
      "The data is available in R as `uploaded_data`.\n\n",
      "**Example**: Instead of `aes(x = category, y = value)`, ",
      "use `aes(x = ", data_summary$categorical_cols[1], ", y = ", 
      data_summary$numeric_cols[1], ")`\n"
    )
    
    return(paste0(base_prompt, data_context))
  }
  
  return(base_prompt)
}

# ==============================================================================
# DATA ANALYSIS FUNCTION
# ==============================================================================
analyze_uploaded_data <- function(filepath, filename) {
  data <- read_csv(filepath, show_col_types = FALSE)
  assign("uploaded_data", data, envir = .GlobalEnv)
  
  list(
    filename = filename,
    n_rows = nrow(data),
    n_cols = ncol(data),
    col_names = names(data),
    numeric_cols = names(data)[sapply(data, is.numeric)],
    categorical_cols = names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
  )
}

# ==============================================================================
# UI
# ==============================================================================
ui <- page_fillable(
  theme = bs_theme(
    preset = "shiny",
    primary = "#007AFF",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    bg = "#f5f7fa",
    fg = "#1d1d1f"
  ),
  
  tags$head(
    tags$style(HTML(custom_css)),
    tags$script(src = "https://cdn.jsdelivr.net/npm/clipboard@2.0.11/dist/clipboard.min.js")
  ),
  
  navset_card_tab(
    id = "main_tabs",
    
    # TAB 1: AI ADVISOR
    nav_panel(
      "💬 AI Advisor",
      layout_sidebar(
        fillable = TRUE,
        
        sidebar = sidebar(
          width = 350,
          
          div(
            style = "text-align: center; margin-bottom: 24px;",
            div(style = "font-size: 2.5rem; margin-bottom: 8px;", "📊"),
            h4(style = "margin: 0; font-weight: 700;", "Visualization Advisor"),
            p(style = "color: #86868b; font-size: 0.9rem; margin-top: 4px;", 
              "AI-Powered Chart Recommendations")
          ),
          
          hr(style = "border-color: rgba(0, 0, 0, 0.06); margin: 20px 0;"),
          
          # File upload
          div(
            h4(style = "font-size: 1rem; font-weight: 600; margin-bottom: 12px;", "📁 Upload Data"),
            fileInput(
              "data_file",   # ID corrected
              NULL,
              accept = c(".csv", ".tsv", "text/csv", "text/tab-separated-values"),
              buttonLabel = "Choose CSV",
              placeholder = "No file selected"
            ),
            uiOutput("data_upload_status")
          ),
          
          hr(style = "border-color: rgba(0, 0, 0, 0.06); margin: 20px 0;"),
          
          # Quick actions
          div(
            h4(style = "font-size: 1rem; font-weight: 600; margin-bottom: 12px;", "⚡ Quick Actions"),
            actionButton("load_example", "📋 Load Example Dataset", 
                         class = "btn-sm w-100 mb-2", style = "text-align: left;"),
            actionButton("reset_chat", "🔄 Reset Conversation", 
                         class = "btn-sm w-100 mb-2", style = "text-align: left;"),
            downloadButton("export_pdf", "📄 Export as PDF", 
                           class = "btn-sm w-100", style = "text-align: left;")
          ),
          
          hr(style = "border-color: rgba(0, 0, 0, 0.06); margin: 20px 0;"),
          
          div(
            h4(style = "font-size: 1rem; font-weight: 600; margin-bottom: 12px;", "🧠 Science-Based"),
            tags$ul(
              style = "margin-left: 0; padding-left: 20px; font-size: 0.9rem; color: #6e6e73;",
              tags$li("Cleveland & McGill"),
              tags$li("Tufte's Principles"),
              tags$li("Few's Guidelines")
            )
          )
        ),
        
        # Main chat area – use a flex container to fill height
        div(
          class = "chat-container",
          chat_ui("chat", height = "100%")
        )
      )
    ),
    
    # TAB 2: DATA EXPLORER
    nav_panel(
      "📊 Data Explorer",
      layout_columns(
        col_widths = c(12, 12, 4, 4, 4),
        
        # Data stats value boxes
        uiOutput("data_stats_boxes"),
        
        # Data preview table
        card(
          card_header("Data Preview"),
          DTOutput("data_preview_table")
        ),
        
        # Column analysis
        card(
          card_header("Numeric Columns"),
          uiOutput("numeric_cols_info")
        ),
        
        card(
          card_header("Categorical Columns"),
          uiOutput("categorical_cols_info")
        ),
        
        card(
          card_header("Data Quality"),
          uiOutput("data_quality_info")
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data_summary = NULL,
    chat_client = NULL
  )
  
  # Initialize chat client
  observe({
    rv$chat_client <- ellmer::chat_anthropic(
     # model = "claude-3-5-sonnet-20241022",
      system_prompt = create_system_prompt(rv$data_summary)
    )
  })
  
  # Handle CSV upload (note: input ID is "data_file")
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      rv$data_summary <- analyze_uploaded_data(
        input$data_file$datapath,
        input$data_file$name
      )
      # Recreate chat with updated data context
      rv$chat_client <- ellmer::chat_anthropic(
        model = "claude-3-5-sonnet-20241022",
        system_prompt = create_system_prompt(rv$data_summary)
      )
      
      data_msg <- paste0(
        "✅ **Data Loaded!**\n\n",
        "📊 **", rv$data_summary$filename, "**\n",
        "- ", rv$data_summary$n_rows, " rows × ", rv$data_summary$n_cols, " columns\n",
        "- Numeric: ", paste(rv$data_summary$numeric_cols, collapse = ", "), "\n",
        "- Categorical: ", paste(rv$data_summary$categorical_cols, collapse = ", "), "\n\n",
        "I can now give you data-specific recommendations! What would you like to visualize?"
      )
      
      chat_append("chat", list(role = "assistant", content = data_msg))
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Handle example dataset
  observeEvent(input$load_example, {
    set.seed(42)
    example_data <- data.frame(
      Region = rep(c("North", "South", "East", "West", "Central"), each = 50),
      Product = sample(c("Product A", "Product B", "Product C"), 250, replace = TRUE),
      Sales = rnorm(250, mean = 50000, sd = 15000),
      Units = rpois(250, lambda = 100),
      Satisfaction = sample(1:5, 250, replace = TRUE)
    )
    temp_file <- tempfile(fileext = ".csv")
    write_csv(example_data, temp_file)
    rv$data_summary <- analyze_uploaded_data(temp_file, "example_sales.csv")
    rv$chat_client <- ellmer::chat_anthropic(
      model = "claude-3-5-sonnet-20241022",
      system_prompt = create_system_prompt(rv$data_summary)
    )
    chat_append("chat", list(
      role = "assistant",
      content = paste0(
        "📋 **Example Dataset Loaded!**\n\n",
        "Sales data with 250 rows:\n",
        "- **Regions**: North, South, East, West, Central\n",
        "- **Products**: A, B, C\n",
        "- **Metrics**: Sales ($), Units, Satisfaction (1-5)\n\n",
        "Try asking:\n",
        "• 'Compare sales across regions'\n",
        "• 'Show the distribution of satisfaction scores'\n",
        "• 'How do I visualize sales trends by product?'"
      )
    ))
  })
  
  # Reset button (now "reset_chat")
  observeEvent(input$reset_chat, {
    chat_clear("chat")
    rv$data_summary <- NULL
    rv$chat_client <- ellmer::chat_anthropic(
     # model = "claude-3-5-sonnet-20251022",
      system_prompt = create_system_prompt(NULL)
    )
    chat_append(
      "chat",
      list(
        role = "assistant",
        content = paste0(
          "👋 **Welcome to the Visualization Advisor!**\n\n",
          "I help you create effective data visualizations based on perceptual science.\n\n",
          "**Get started:**\n",
          "- Upload your CSV file (sidebar) for personalized recommendations\n",
          "- Or describe your data and I'll guide you\n",
          "- Try the example dataset to see what I can do\n\n",
          "Tell me about your data and I'll recommend the best chart!"
        )
      )
    )
    if (exists("uploaded_data", envir = .GlobalEnv)) {
      rm(uploaded_data, envir = .GlobalEnv)
    }
  })
  
  # Upload status indicator (now "data_upload_status")
  output$data_upload_status <- renderUI({
    if (!is.null(rv$data_summary)) {
      div(
        class = "alert alert-success",
        style = "padding: 8px; margin-top: 10px; font-size: 0.85rem;",
        strong("✓ Loaded"), br(),
        paste0(rv$data_summary$n_rows, " rows × ", rv$data_summary$n_cols, " cols")
      )
    }
  })
  
  # Handle chat messages
  observeEvent(input$chat_user_input, {
    req(rv$chat_client)
    stream <- rv$chat_client$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
  
  # Placeholder outputs for Data Explorer (optional – you can fill them later)
  output$data_stats_boxes <- renderUI({
    # You can create value boxes using bslib::value_box()
    NULL
  })
  
  output$data_preview_table <- renderDT({
    if (!is.null(rv$data_summary)) {
      datatable(uploaded_data, options = list(scrollX = TRUE))
    } else {
      datatable(data.frame(Message = "No data uploaded yet."))
    }
  })
  
  output$numeric_cols_info <- renderUI({
    if (!is.null(rv$data_summary)) {
      tags$ul(lapply(rv$data_summary$numeric_cols, tags$li))
    } else {
      "No numeric columns"
    }
  })
  
  output$categorical_cols_info <- renderUI({
    if (!is.null(rv$data_summary)) {
      tags$ul(lapply(rv$data_summary$categorical_cols, tags$li))
    } else {
      "No categorical columns"
    }
  })
  
  output$data_quality_info <- renderUI({
    if (!is.null(rv$data_summary)) {
      paste("Missing values:", sum(is.na(uploaded_data)))
    } else {
      "No data"
    }
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================
shinyApp(ui, server)