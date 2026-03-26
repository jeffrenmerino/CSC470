# ==============================================================================
# VISUALIZATION ADVISOR BOT - Production Ready Edition (FIXED + ENHANCED)
# ==============================================================================

library(shiny)
library(bslib)
library(shinychat)
library(ellmer)
library(readr)
library(dplyr)
library(DT)
library(bsicons)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)

options(shiny.maxRequestSize = 100 * 1024^2)

key_path <- "/usr/home/georgetowncollege.edu/jmerino/test-bot/api-key.txt"
Sys.setenv(ANTHROPIC_API_KEY = readLines(key_path))

# ==============================================================================
# SYSTEM PROMPT
# ==============================================================================

get_system_prompt <- function() {
  if (file.exists("system-prompt.md")) {
    tryCatch(
      return(readLines("system-prompt.md") |> paste(collapse = "\n")),
      error = function(e) message("Could not read system-prompt.md, using embedded version")
    )
  }
  
  "# Data Visualization Advisor
 
You are an expert data visualization advisor grounded in perceptual science.
 
## Your Expertise
- **Cleveland & McGill's perceptual hierarchy**: Position > Length > Angle > Area
- **Tufte's principles**: Data-ink ratio, integrity, small multiples
- **Few's guidelines**: Clarity for business audiences
 
## Your Process
 
1. **Ask clarifying questions** (2-3 focused questions):
   - Data type (numeric, categorical, time series)
   - Goal (comparison, distribution, relationship, composition)
   - Observations count, audience, constraints
 
2. **Recommend charts** with:
   - **Why it works**: Cite Cleveland & McGill or Tufte
   - **Complete ggplot2 code**: Ready to run, well-commented
   - **Warnings**: Common pitfalls
 
## Decision Rules
 
**Comparisons:**
- Few categories (<=7) -> Vertical bar chart
- Many categories (>7) -> Horizontal bar chart
- Never pie charts with >5 categories
 
**Distributions:**
- Small data (<100) -> Histogram or box plot
- Large data (>1000) -> Density plot
- Groups (<=5) -> Violin plots
- Groups (>5) -> Faceted histograms
 
**Relationships:**
- Two numeric -> Scatter plot
- Large data -> Add transparency or hexbin
- With groups -> Color by category (max 5-7 colors)
 
**Composition:**
- Few parts (<=5) -> Stacked bar or pie
- Many parts (>5) -> Stacked bar or treemap
 
**Time Series:**
- Line chart
- Banking to 45 degrees
- Max 5 lines
 
## Code Standards
 
Always provide complete, executable ggplot2 code.
 
CRITICAL: When user has uploaded data, use the ACTUAL column names from their dataset in all code examples. The data will be available as `uploaded_data`."
}

create_system_prompt <- function(data_summary = NULL) {
  base_prompt <- get_system_prompt()
  if (!is.null(data_summary)) {
    data_context <- paste0(
      "\n\n## UPLOADED DATA CONTEXT\n\n",
      "The user has uploaded: **", data_summary$filename, "**\n\n",
      "**Data Structure:**\n",
      "- Rows: ", data_summary$n_rows, "\n",
      "- Columns: ", data_summary$n_cols, "\n\n",
      "**Column Names:** ", paste(data_summary$col_names, collapse = ", "), "\n\n",
      "**Numeric columns (", length(data_summary$numeric_cols), "):** ",
      paste(data_summary$numeric_cols, collapse = ", "), "\n\n",
      "**Categorical columns (", length(data_summary$categorical_cols), "):** ",
      paste(data_summary$categorical_cols, collapse = ", "), "\n\n",
      "**CRITICAL INSTRUCTIONS:**\n",
      "1. Use these ACTUAL column names in all code (not generic x/y)\n",
      "2. The data is available in R as `uploaded_data`\n",
      "3. All code must be complete and immediately executable\n",
      "4. Example: `ggplot(uploaded_data, aes(x = ",
      if (length(data_summary$categorical_cols) > 0) data_summary$categorical_cols[1] else "column_name",
      ", y = ",
      if (length(data_summary$numeric_cols) > 0) data_summary$numeric_cols[1] else "value",
      "))`\n"
    )
    return(paste0(base_prompt, data_context))
  }
  base_prompt
}

analyze_uploaded_data <- function(filepath, filename) {
  data <- tryCatch(
    read_csv(filepath, show_col_types = FALSE),
    error = function(e) stop(paste("Error reading CSV:", e$message))
  )
  if (nrow(data) == 0) stop("CSV file is empty")
  if (ncol(data) == 0) stop("CSV file has no columns")
  assign("uploaded_data", data, envir = .GlobalEnv)
  numeric_cols    <- names(data)[sapply(data, is.numeric)]
  categorical_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
  list(
    filename         = filename,
    n_rows           = nrow(data),
    n_cols           = ncol(data),
    col_names        = names(data),
    numeric_cols     = numeric_cols,
    categorical_cols = categorical_cols,
    missing_count    = sum(is.na(data)),
    missing_pct      = round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 2)
  )
}

# ==============================================================================
# CSS
# ==============================================================================
# Layout strategy (learned from official shinychat docs):
#
#   The CORRECT way to use shinychat in a sidebar+main layout is:
#   - Use page_sidebar() with chat_ui() IN the sidebar (height="100%"), OR
#   - Use page_fillable() with chat_ui() directly in a card in the MAIN area
#     alongside a separate sidebar built with bslib's sidebar()
#
#   The broken approach was using layout_sidebar() and overriding its internal
#   CSS grid with flexbox — that kills bslib's sidebar toggle mechanism.
#
#   New approach for the AI Advisor tab:
#   - layout_sidebar() is used WITHOUT any flexbox overrides on the grid
#   - The chat goes in the MAIN area inside a card with fill=TRUE
#   - chat_ui() gets height="100%" so shinychat's own fill logic takes over
#   - We only add CSS that styles appearance, NOT layout structure
# ==============================================================================

custom_css <- "
/* ── Google Fonts ──────────────────────────────────────────────────── */
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,400;0,9..40,500;0,9..40,600;0,9..40,700;1,9..40,400&family=DM+Mono:wght@400;500&display=swap');
 
/* ── Design tokens ─────────────────────────────────────────────────── */
:root {
  --va-bg:         #f0f2f7;
  --va-surface:    #ffffff;
  --va-border:     rgba(0,0,0,0.08);
  --va-primary:    #2563eb;
  --va-primary-lt: #eff6ff;
  --va-text:       #111827;
  --va-muted:      #6b7280;
  --va-success:    #16a34a;
  --va-warning:    #d97706;
  --va-radius:     14px;
  --va-shadow:     0 2px 12px rgba(0,0,0,0.07);
  --va-shadow-lg:  0 12px 48px rgba(0,0,0,0.12);
  --font-body:     'DM Sans', sans-serif;
  --font-mono:     'DM Mono', monospace;
}
 
/* ── Global ─────────────────────────────────────────────────────────── */
html, body {
  font-family: var(--font-body) !important;
  background: var(--va-bg) !important;
  color: var(--va-text) !important;
}
* { box-sizing: border-box; }
 
/* ── Nav tabs ────────────────────────────────────────────────────────── */
.nav-tabs { border-bottom: 1px solid var(--va-border) !important; }
.nav-tabs .nav-link {
  font-size: 0.875rem !important;
  font-weight: 500 !important;
  color: var(--va-muted) !important;
  border: none !important;
  padding: 11px 18px !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0 !important;
  transition: color 0.15s;
}
.nav-tabs .nav-link.active {
  color: var(--va-primary) !important;
  background: transparent !important;
  border-bottom-color: var(--va-primary) !important;
  font-weight: 600 !important;
}
.nav-tabs .nav-link:hover:not(.active) { color: var(--va-text) !important; }
 
/* ── Sidebar appearance (no layout overrides!) ───────────────────────── */
.bslib-sidebar-layout > .bslib-sidebar {
  background: #f8fafc !important;
  border-right: 1px solid var(--va-border) !important;
}
 
/* ── Sidebar inner content ───────────────────────────────────────────── */
.sidebar-brand {
  text-align: center;
  padding: 8px 0 14px;
}
.sidebar-brand .brand-icon {
  font-size: 2.2rem;
  display: block;
  margin-bottom: 5px;
}
.sidebar-brand h5 {
  margin: 0;
  font-weight: 700;
  font-size: 0.97rem;
  letter-spacing: -0.2px;
}
.sidebar-brand p {
  color: var(--va-muted);
  font-size: 0.76rem;
  margin: 2px 0 0;
}
 
.sidebar-section {
  background: var(--va-surface);
  border: 1px solid var(--va-border);
  border-radius: 10px;
  margin-bottom: 10px;
  overflow: hidden;
}
.sidebar-section-header {
  padding: 8px 12px;
  font-size: 0.72rem;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--va-muted);
  border-bottom: 1px solid var(--va-border);
  background: #fafbfc;
}
.sidebar-section-body { padding: 10px 12px; }
 
/* Status badges */
.status-badge {
  display: flex;
  align-items: flex-start;
  gap: 7px;
  padding: 7px 10px;
  border-radius: 8px;
  font-size: 0.8rem;
  margin-top: 6px;
}
.status-badge.success {
  background: #f0fdf4; border: 1px solid #bbf7d0; color: #15803d;
}
.status-badge.info {
  background: #eff6ff; border: 1px solid #bfdbfe; color: #1d4ed8;
}
 
/* Buttons */
.btn { font-family: var(--font-body) !important; border-radius: 8px !important; }
.btn-outline-secondary {
  font-size: 0.82rem !important; font-weight: 500 !important;
  color: var(--va-text) !important; border-color: var(--va-border) !important;
}
.btn-outline-secondary:hover {
  background: #f1f5f9 !important; color: var(--va-primary) !important;
  border-color: #c7d7f8 !important;
}
.btn-primary {
  background: var(--va-primary) !important;
  border-color: var(--va-primary) !important;
  font-weight: 600 !important;
}
 
/* ── THE CHAT CARD ───────────────────────────────────────────────────────
   Key insight from shinychat docs: place chat_ui() inside a card() with
   fill = TRUE and give chat_ui() height = '100%'. bslib's card fill system
   then handles all the height propagation natively — we just style it.
   DO NOT override overflow or display on .card or .card-body here.
   ─────────────────────────────────────────────────────────────────────── */
 
/* Style the chat card shell */
#chat-card {
  border: 1px solid var(--va-border) !important;
  border-radius: var(--va-radius) !important;
  box-shadow: var(--va-shadow) !important;
  background: var(--va-surface) !important;
}
 
/* shinychat's own wrapper — only add appearance, not layout changes */
.shiny-chat-container {
  background: var(--va-surface);
}
 
/* Message list: this IS the scrollable area shinychat creates */
.shiny-chat-container .chat-messages {
  padding: 16px 20px !important;
  scroll-behavior: smooth;
}
 
/* Individual messages */
.chat-message {
  margin-bottom: 14px !important;
  line-height: 1.65 !important;
  font-size: 0.915rem !important;
  animation: msgIn 0.18s ease-out;
}
@keyframes msgIn {
  from { opacity: 0; transform: translateY(4px); }
  to   { opacity: 1; transform: translateY(0); }
}
 
/* User messages — right aligned bubble */
.chat-message[data-role='user'] .chat-message-body,
.chat-message.user .chat-message-body {
  background: var(--va-primary-lt) !important;
  border: 1px solid #bfdbfe !important;
  border-radius: 18px 18px 4px 18px !important;
  padding: 10px 15px !important;
  margin-left: 18% !important;
  color: #1e40af !important;
  display: block;
}
 
/* Assistant messages — left aligned bubble */
.chat-message[data-role='assistant'] .chat-message-body,
.chat-message.assistant .chat-message-body {
  background: #f8fafc !important;
  border: 1px solid var(--va-border) !important;
  border-radius: 18px 18px 18px 4px !important;
  padding: 10px 15px !important;
  margin-right: 18% !important;
  display: block;
}
 
/* Code inside messages */
.chat-message pre {
  background: #1e293b !important;
  color: #e2e8f0 !important;
  border-radius: 8px !important;
  padding: 12px 15px !important;
  font-family: var(--font-mono) !important;
  font-size: 0.81rem !important;
  overflow-x: auto !important;
  border: none !important;
  margin: 8px 0 !important;
}
.chat-message code {
  font-family: var(--font-mono) !important;
  font-size: 0.83rem !important;
  background: #f1f5f9 !important;
  padding: 1px 5px !important;
  border-radius: 4px !important;
}
.chat-message pre code {
  background: transparent !important;
  padding: 0 !important;
}
 
/* Chat input area */
.shiny-chat-container .chat-user-input {
  border-top: 1px solid var(--va-border) !important;
  padding: 12px 16px !important;
  background: #fafbfc !important;
}
.shiny-chat-container .chat-user-input textarea {
  font-family: var(--font-body) !important;
  font-size: 0.9rem !important;
  border-radius: 10px !important;
  border: 1.5px solid #e5e7eb !important;
  padding: 9px 13px !important;
  resize: none !important;
  transition: border-color 0.15s, box-shadow 0.15s;
  background: var(--va-surface) !important;
}
.shiny-chat-container .chat-user-input textarea:focus {
  border-color: var(--va-primary) !important;
  box-shadow: 0 0 0 3px rgba(37,99,235,0.1) !important;
  outline: none !important;
}
.shiny-chat-container .chat-user-input button,
.shiny-chat-container .chat-user-input .btn {
  background: var(--va-primary) !important;
  border: none !important;
  border-radius: 8px !important;
  color: #fff !important;
  font-weight: 600 !important;
  transition: background 0.15s, transform 0.1s;
}
.shiny-chat-container .chat-user-input button:hover {
  background: #1d4ed8 !important;
  transform: translateY(-1px);
}
 
/* ── Data Explorer ───────────────────────────────────────────────────── */
.value-box { border-radius: var(--va-radius) !important; }
table.dataTable {
  font-family: var(--font-body) !important;
  font-size: 0.865rem !important;
}
table.dataTable thead th {
  background: #f8fafc !important;
  font-weight: 600 !important;
  color: var(--va-muted) !important;
  font-size: 0.77rem !important;
  text-transform: uppercase !important;
  letter-spacing: 0.05em !important;
  border-bottom: 1px solid var(--va-border) !important;
}
 
/* ── Plot Viewer code input ──────────────────────────────────────────── */
#plot_code_input {
  font-family: var(--font-mono) !important;
  font-size: 0.83rem !important;
  border-radius: 10px !important;
  border: 1.5px solid #334155 !important;
  background: #1e293b !important;
  color: #e2e8f0 !important;
  padding: 13px !important;
}
#plot_code_input:focus {
  border-color: #2563eb !important;
  box-shadow: 0 0 0 3px rgba(37,99,235,0.15) !important;
  outline: none !important;
}
 
/* ── Col info cards ──────────────────────────────────────────────────── */
.col-stat-card {
  margin-bottom: 10px;
  padding: 10px 12px;
  background: #f8fafc;
  border: 1px solid var(--va-border);
  border-radius: 9px;
}
 
/* ── Notifications ───────────────────────────────────────────────────── */
#shiny-notification-panel { bottom: 20px !important; right: 20px !important; top: auto !important; }
.shiny-notification {
  font-family: var(--font-body) !important;
  border-radius: 10px !important;
  box-shadow: var(--va-shadow-lg) !important;
  font-size: 0.875rem !important;
}
 
/* ── Scrollbars ──────────────────────────────────────────────────────── */
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: #d1d5db; border-radius: 99px; }
::-webkit-scrollbar-thumb:hover { background: #9ca3af; }
"

# ==============================================================================
# UI
# ==============================================================================

ui <- page_fillable(
  theme = bs_theme(
    preset       = "shiny",
    primary      = "#2563eb",
    base_font    = font_google("DM Sans"),
    heading_font = font_google("DM Sans"),
    code_font    = font_google("DM Mono"),
    bg           = "#f0f2f7",
    fg           = "#111827"
  ),
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML(custom_css))
  ),
  
  navset_card_tab(
    id = "main_tabs",
    
    # ── TAB 1: AI ADVISOR ─────────────────────────────────────────────────
    # CORRECT PATTERN (per official shinychat docs):
    #   layout_sidebar() with fillable=TRUE, and chat_ui() inside a card()
    #   with fill=TRUE in the MAIN panel. chat_ui() gets height="100%" so
    #   shinychat's own fill logic handles the scrollable messages area.
    #   The sidebar uses bslib's native CSS grid — we do NOT override it.
    nav_panel(
      "💬 AI Advisor",
      layout_sidebar(
        fillable = TRUE,
        border   = FALSE,
        
        sidebar = sidebar(
          width  = 300,
          padding = "12px",
          bg     = "#f8fafc",
          open   = TRUE,
          
          # Brand
          div(
            class = "sidebar-brand",
            tags$span(class = "brand-icon", "📊"),
            h5("Viz Advisor"),
            p(style = "color:var(--va-muted);font-size:0.76rem;margin:2px 0 0;",
              "Science-based chart recommendations")
          ),
          
          tags$hr(style = "border-color:var(--va-border); margin: 4px 0 10px;"),
          
          # Upload section
          div(
            class = "sidebar-section",
            div(class = "sidebar-section-header", "📁 Upload Data"),
            div(
              class = "sidebar-section-body",
              fileInput(
                "data_file", NULL,
                accept      = c(".csv", ".tsv", "text/csv"),
                buttonLabel = "Choose CSV",
                placeholder = "No file selected",
                width       = "100%"
              ),
              uiOutput("upload_status")
            )
          ),
          
          # Quick actions
          div(
            class = "sidebar-section",
            div(class = "sidebar-section-header", "⚡ Quick Actions"),
            div(
              class = "sidebar-section-body",
              style = "display:flex; flex-direction:column; gap:6px;",
              actionButton("load_example", "📋 Load Example Dataset",
                           class = "btn btn-outline-secondary btn-sm w-100"),
              actionButton("reset_chat",   "🔄 Reset Conversation",
                           class = "btn btn-outline-secondary btn-sm w-100"),
              downloadButton("export_chat", "💾 Export Chat",
                             class = "btn btn-outline-secondary btn-sm w-100")
            )
          ),
          
          # Science principles
          div(
            class = "sidebar-section",
            div(class = "sidebar-section-header", "🧠 Grounded In"),
            div(
              class = "sidebar-section-body",
              style = "font-size:0.8rem; color:var(--va-muted); line-height:1.75;",
              tags$ul(
                style = "margin:0; padding-left:16px;",
                tags$li("Cleveland & McGill hierarchy"),
                tags$li("Tufte's data-ink ratio"),
                tags$li("Few's business clarity")
              )
            )
          )
        ),
        
        # ── MAIN: chat in a fill card ──────────────────────────────────────
        # This is the pattern from official docs: card() with chat_ui() inside.
        # bslib's card fill system handles height propagation correctly.
        # Do NOT wrap in withSpinner — it breaks fill. The card itself is instant.
        card(
          id       = "chat-card",
          fill     = TRUE,
          padding  = 0,
          style    = "border-radius: var(--va-radius); overflow: hidden;",
          chat_ui("chat", height = "100%", fill = TRUE)
        )
      )
    ),
    
    # ── TAB 2: DATA EXPLORER ─────────────────────────────────────────────
    nav_panel(
      "📊 Data Explorer",
      div(
        style = "padding: 16px; overflow-y: auto; height: 100%;",
        layout_columns(
          col_widths = c(12, 12, 6, 6),
          
          uiOutput("data_stats_boxes"),
          
          card(
            full_screen = TRUE,
            card_header("📋 Data Preview (first 100 rows)"),
            DTOutput("data_preview_table")
          ),
          
          card(
            card_header("🔢 Numeric Columns"),
            div(style = "overflow-y: auto; max-height: 420px; padding: 4px;",
                uiOutput("numeric_cols_info"))
          ),
          
          card(
            card_header("📑 Categorical Columns"),
            div(style = "overflow-y: auto; max-height: 420px; padding: 4px;",
                uiOutput("categorical_cols_info"))
          )
        )
      )
    ),
    
    # ── TAB 3: PLOT VIEWER ───────────────────────────────────────────────
    nav_panel(
      "📈 Plot Viewer",
      div(
        style = "padding: 16px; overflow-y: auto; height: 100%;",
        card(
          full_screen = TRUE,
          card_header("Generated Plot"),
          div(
            style = "padding: 20px;",
            uiOutput("plot_instructions"),
            tags$br(),
            withSpinner(
              plotOutput("rendered_plot", height = "520px"),
              type  = 8,
              color = "#2563eb",
              size  = 0.5
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER  (unchanged logic, minor robustness tweaks)
# ==============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data_summary     = NULL,
    chat_client      = NULL,
    chat_history     = list(),
    current_plot_code = NULL
  )
  
  # ── Init ──────────────────────────────────────────────────────────────
  observe({
    rv$chat_client <- chat_anthropic(
      system_prompt = create_system_prompt(rv$data_summary)
    )
    
    welcome_msg <- paste0(
      "👋 **Welcome to Viz Advisor!**\n\n",
      "I help you create effective data visualizations grounded in perceptual science.\n\n",
      "**Get started:**\n",
      "- 📁 Upload a CSV in the sidebar for data-specific recommendations\n",
      "- 📋 Or load the built-in example dataset\n",
      "- 💬 Or just describe your data and I'll guide you\n\n",
      "_Ask me anything about charts and I'll tell you what works — and why._"
    )
    
    chat_clear("chat")
    chat_append("chat", list(role = "assistant", content = welcome_msg))
    rv$chat_history <- list(list(role = "assistant", content = welcome_msg))
    
  }) |> bindEvent(session$clientData, once = TRUE, ignoreNULL = FALSE)
  
  # ── File upload ───────────────────────────────────────────────────────
  observeEvent(input$data_file, {
    req(input$data_file)
    progress <- Progress$new(session, min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Reading file…", value = 0.3)
    
    tryCatch({
      progress$set(message = "Analysing columns…", value = 0.6)
      rv$data_summary <- analyze_uploaded_data(
        input$data_file$datapath,
        input$data_file$name
      )
      
      progress$set(message = "Updating AI context…", value = 0.9)
      rv$chat_client <- ellmer::chat_anthropic(
        system_prompt = create_system_prompt(rv$data_summary)
      )
      
      data_msg <- paste0(
        "✅ **Data loaded: ", rv$data_summary$filename, "**\n\n",
        "| | |\n|---|---|\n",
        "| Rows | ", format(rv$data_summary$n_rows, big.mark = ","), " |\n",
        "| Columns | ", rv$data_summary$n_cols, " |\n",
        "| Numeric | ", paste(rv$data_summary$numeric_cols, collapse = ", "), " |\n",
        "| Categorical | ", paste(rv$data_summary$categorical_cols, collapse = ", "), " |\n\n",
        "I now have full context — ask me what you'd like to visualize!"
      )
      
      chat_append("chat", list(role = "assistant", content = data_msg))
      rv$chat_history <- append(rv$chat_history,
                                list(list(role = "assistant", content = data_msg)))
      
      updateTabsetPanel(session, "main_tabs", selected = "📊 Data Explorer")
      progress$set(value = 1)
      showNotification("Data loaded!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # Upload status
  output$upload_status <- renderUI({
    if (!is.null(rv$data_summary)) {
      div(class = "status-badge success",
          bs_icon("check-circle-fill"),
          div(
            div(style = "font-weight: 600;", rv$data_summary$filename),
            div(style = "font-size: 0.77rem; opacity: .8;",
                format(rv$data_summary$n_rows, big.mark = ","),
                " rows ×", rv$data_summary$n_cols, "cols")
          ))
    } else {
      div(class = "status-badge info",
          bs_icon("info-circle"),
          "Upload a CSV for tailored advice")
    }
  })
  
  # ── Example dataset ───────────────────────────────────────────────────
  observeEvent(input$load_example, {
    progress <- Progress$new(session, min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Generating data…", value = 0.5)
    
    tryCatch({
      set.seed(42)
      example_data <- data.frame(
        Region       = rep(c("North","South","East","West","Central"), each = 50),
        Product      = sample(c("Product A","Product B","Product C"), 250, replace = TRUE),
        Sales        = rnorm(250, mean = 50000, sd = 15000),
        Units        = rpois(250, lambda = 100),
        Satisfaction = sample(1:5, 250, replace = TRUE)
      )
      
      temp_file <- tempfile(fileext = ".csv")
      write_csv(example_data, temp_file)
      rv$data_summary <- analyze_uploaded_data(temp_file, "example_sales.csv")
      
      rv$chat_client <- ellmer::chat_anthropic(
        system_prompt = create_system_prompt(rv$data_summary)
      )
      
      example_msg <- paste0(
        "📋 **Example dataset loaded** — 250 rows of regional sales data.\n\n",
        "**Columns:** Region, Product, Sales, Units, Satisfaction\n\n",
        "**Try asking:**\n",
        "- _'Compare average sales across regions'_\n",
        "- _'Show the distribution of satisfaction scores'_\n",
        "- _'Relationship between Units and Sales?'_\n",
        "- _'Sales by product — what chart works best?'_"
      )
      
      chat_append("chat", list(role = "assistant", content = example_msg))
      rv$chat_history <- append(rv$chat_history,
                                list(list(role = "assistant", content = example_msg)))
      
      showNotification("Example dataset loaded!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # ── Chat ──────────────────────────────────────────────────────────────
  observeEvent(input$chat_user_input, {
    req(rv$chat_client, input$chat_user_input)
    
    tryCatch({
      rv$chat_history <- append(
        rv$chat_history,
        list(list(role = "user", content = input$chat_user_input))
      )
      stream <- rv$chat_client$stream_async(input$chat_user_input)
      chat_append("chat", stream)
      
    }, error = function(e) {
      showNotification(paste("AI Error:", e$message), type = "error", duration = 10)
      chat_append("chat", list(
        role    = "assistant",
        content = paste0("❌ **Error:** ", e$message,
                         "\n\nPlease try again or check your API key.")
      ))
    })
  })
  
  # ── Reset ─────────────────────────────────────────────────────────────
  observeEvent(input$reset_chat, {
    rv$data_summary      <- NULL
    rv$chat_history      <- list()
    rv$current_plot_code <- NULL
    
    if (exists("uploaded_data", envir = .GlobalEnv))
      rm(uploaded_data, envir = .GlobalEnv)
    
    reset("data_file")
    
    rv$chat_client <- ellmer::chat_anthropic(
      system_prompt = create_system_prompt(NULL)
    )
    
    welcome_msg <- paste0(
      "👋 **Welcome to Viz Advisor!**\n\n",
      "I help you create effective data visualizations grounded in perceptual science.\n\n",
      "**Get started:**\n",
      "- 📁 Upload a CSV in the sidebar for data-specific recommendations\n",
      "- 📋 Or load the built-in example dataset\n",
      "- 💬 Or just describe your data and I'll guide you\n\n",
      "_Ask me anything about charts and I'll tell you what works — and why._"
    )
    
    chat_clear("chat")
    chat_append("chat", list(role = "assistant", content = welcome_msg))
    rv$chat_history <- list(list(role = "assistant", content = welcome_msg))
    
    showNotification("Conversation reset.", type = "message", duration = 3)
  })
  
  # ── Export ────────────────────────────────────────────────────────────
  output$export_chat <- downloadHandler(
    filename = function() paste0("viz_advisor_", Sys.Date(), ".txt"),
    content = function(file) {
      lines <- paste0(
        "VISUALIZATION ADVISOR CHAT EXPORT\nDate: ", Sys.time(), "\n",
        strrep("=", 72), "\n\n"
      )
      for (msg in rv$chat_history) {
        role   <- if (msg$role == "user") "YOU" else "ADVISOR"
        lines  <- paste0(lines, "[", role, "]\n", msg$content,
                         "\n\n", strrep("-", 72), "\n\n")
      }
      writeLines(lines, file)
    }
  )
  
  # ── Data Explorer ─────────────────────────────────────────────────────
  output$data_stats_boxes <- renderUI({
    if (is.null(rv$data_summary)) {
      div(style = "text-align:center; padding:40px; color: var(--va-muted);",
          h5("No data loaded"),
          p("Upload a CSV or load the example dataset."))
    } else {
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Total Rows",
                  format(rv$data_summary$n_rows, big.mark = ","),
                  showcase = bs_icon("table"), theme = "primary"),
        value_box("Columns",
                  rv$data_summary$n_cols,
                  showcase = bs_icon("columns-gap"), theme = "info"),
        value_box("Numeric Cols",
                  length(rv$data_summary$numeric_cols),
                  showcase = bs_icon("123"), theme = "success"),
        value_box("Missing",
                  paste0(rv$data_summary$missing_pct, "%"),
                  showcase = bs_icon("exclamation-triangle"),
                  theme = if (rv$data_summary$missing_pct > 5) "warning" else "success")
      )
    }
  })
  
  output$data_preview_table <- renderDT({
    if (is.null(rv$data_summary)) {
      datatable(data.frame(Message = "No data uploaded yet."),
                options = list(dom = "t"), rownames = FALSE)
    } else {
      datatable(
        head(uploaded_data, 100),
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        class   = "cell-border stripe hover",
        rownames = FALSE
      )
    }
  })
  
  output$numeric_cols_info <- renderUI({
    if (is.null(rv$data_summary) || length(rv$data_summary$numeric_cols) == 0)
      return(p("No numeric columns.", style = "color: var(--va-muted); padding: 12px;"))
    
    lapply(rv$data_summary$numeric_cols, function(col) {
      d <- uploaded_data[[col]]
      div(style = "margin-bottom:12px; padding:10px 12px; background:#f8fafc;
                   border:1px solid var(--va-border); border-radius:8px;",
          tags$strong(col),
          tags$ul(style = "font-size:0.82rem; color:var(--va-muted); margin:6px 0 0; padding-left:18px;",
                  tags$li(paste("Range:", round(min(d,na.rm=TRUE),2), "–", round(max(d,na.rm=TRUE),2))),
                  tags$li(paste("Mean:",   round(mean(d,na.rm=TRUE),2),
                                "  Median:", round(median(d,na.rm=TRUE),2))),
                  tags$li(paste("SD:",     round(sd(d,na.rm=TRUE),2)))
          ))
    })
  })
  
  output$categorical_cols_info <- renderUI({
    if (is.null(rv$data_summary) || length(rv$data_summary$categorical_cols) == 0)
      return(p("No categorical columns.", style = "color: var(--va-muted); padding: 12px;"))
    
    lapply(rv$data_summary$categorical_cols, function(col) {
      d          <- uploaded_data[[col]]
      n_unique   <- length(unique(d))
      top_vals   <- head(sort(table(d), decreasing = TRUE), 3)
      div(style = "margin-bottom:12px; padding:10px 12px; background:#f8fafc;
                   border:1px solid var(--va-border); border-radius:8px;",
          tags$strong(col),
          div(style = "font-size:0.82rem; color:var(--va-muted); margin-top:6px;",
              paste("Unique values:", n_unique),
              if (length(top_vals) > 0)
                div(style = "margin-top:4px;", "Top values:",
                    tags$ul(style = "margin:4px 0 0; padding-left:18px;",
                            lapply(names(top_vals),
                                   function(v) tags$li(paste0(v, " (n=", top_vals[v], ")")))))
          ))
    })
  })
  
  # ── Plot Viewer ───────────────────────────────────────────────────────
  output$plot_instructions <- renderUI({
    if (is.null(rv$data_summary)) {
      div(class = "status-badge info",
          bs_icon("info-circle"),
          "Upload data first, then paste ggplot2 code from the AI chat below.")
    } else {
      tagList(
        textAreaInput(
          "plot_code_input",
          "Paste ggplot2 code from the chat:",
          value = "",
          rows  = 9,
          placeholder = paste0(
            "# Paste the AI-generated code here\n",
            "library(ggplot2)\n",
            "ggplot(uploaded_data, aes(x = ..., y = ...)) +\n",
            "  geom_col() +\n",
            "  theme_minimal()"
          )
        ),
        actionButton("render_plot_btn", "🎨 Render Plot", class = "btn btn-primary")
      )
    }
  })
  
  output$rendered_plot <- renderPlot({
    req(input$render_plot_btn)
    req(input$plot_code_input)
    isolate({
      if (is.null(rv$data_summary)) {
        plot.new(); text(0.5, 0.5, "No data loaded", cex = 1.5, col = "gray"); return()
      }
      tryCatch(
        eval(parse(text = input$plot_code_input)),
        error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
          showNotification(paste("Plot error:", e$message), type = "error", duration = 10)
        }
      )
    })
  })
  
  observeEvent(input$render_plot_btn, {
    if (nchar(trimws(input$plot_code_input)) < 10) {
      showNotification("Paste some ggplot2 code first.", type = "warning", duration = 4)
      return()
    }
    updateTabsetPanel(session, "main_tabs", selected = "📈 Plot Viewer")
  })
}

# ==============================================================================
shinyApp(ui = ui, server = server)
