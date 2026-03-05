# ==============================================================================
# VISUALIZATION ADVISOR BOT - Shiny App Prototype
# ==============================================================================
# Purpose: Recommend appropriate data visualizations based on user inputs,
#          grounded in perceptual science (Cleveland & McGill) and design 
#          principles (Tufte, Few)
# 
# Author: Jeffren
# Date: February 23, 2026
# For: Class Project Status Report
# Claude chat: https://claude.ai/share/da3f903d-65ac-46ff-bf2f-47dfd4019b3c 
# ==============================================================================

library(shiny)
library(bslib)


# ==============================================================================
# RECOMMENDATION ENGINE
# ==============================================================================

get_recommendation <- function(data_type, goal, n_categories, dataset_size, show_alternative = FALSE) {
  
  # Initialize output structure
  result <- list(
    primary_chart = "",
    primary_explanation = "",
    primary_code = "",
    warning = "",
    alternative_chart = "",
    alternative_explanation = "",
    alternative_code = ""
  )
  
  # ==============================================================================
  # DECISION LOGIC BASED ON PERCEPTUAL PRINCIPLES
  # ==============================================================================
  
  # --------------------------------------------------------------------------
  # CASE 1: Categorical + Comparison
  # --------------------------------------------------------------------------
  if (data_type == "Categorical" && goal == "Comparison") {
    
    if (n_categories > 7) {
      # Many categories - horizontal bars for readability
      result$primary_chart <- "Horizontal Bar Chart (Ordered)"
      result$primary_explanation <- paste0(
        "With ", n_categories, " categories, a horizontal bar chart is recommended. ",
        "<b>Cleveland & McGill's hierarchy</b> shows position along a common scale is the most accurate encoding. ",
        "Horizontal orientation allows long category labels to be read easily, and ordering by value aids comparison. ",
        "<b>Tufte's principle</b>: this maximizes data-ink ratio with minimal chartjunk."
      )
      result$primary_code <- '# Horizontal bar chart (ordered by value)
library(ggplot2)

# Assuming your data is in a data frame "df" with columns "category" and "value"
df <- df %>% arrange(value)  # Order by value
df$category <- factor(df$category, levels = df$category)  # Lock order

ggplot(df, aes(x = value, y = category)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Comparison Across Categories",
       x = "Value", y = "Category") +
  theme(panel.grid.major.y = element_blank())  # Remove horizontal gridlines'
      
      result$warning <- paste0(
        "<span style='color: #d9534f;'><b>⚠ Warning:</b> With ", n_categories, 
        " categories, avoid pie charts! Angle discrimination is poor (Cleveland & McGill), ",
        "and cluttered pies violate Tufte's principle of clarity.</span>"
      )
      
      # Alternative
      result$alternative_chart <- "Dot Plot"
      result$alternative_explanation <- paste0(
        "For many categories, a dot plot is even cleaner than bars. It uses position encoding (most accurate) ",
        "while reducing visual clutter. <b>Tufte's data-ink ratio</b> is maximized since we only show the essential data points."
      )
      result$alternative_code <- '# Dot plot (Cleveland dot plot)
ggplot(df, aes(x = value, y = category)) +
  geom_point(size = 3, color = "steelblue") +
  geom_segment(aes(x = 0, xend = value, y = category, yend = category), 
               color = "gray70") +
  theme_minimal() +
  labs(title = "Comparison Across Categories", x = "Value", y = "") +
  theme(panel.grid.major.y = element_blank())'
      
    } else {
      # Few categories - vertical bars
      result$primary_chart <- "Vertical Bar Chart"
      result$primary_explanation <- paste0(
        "For ", n_categories, " categories, a vertical bar chart is ideal. ",
        "<b>Cleveland & McGill</b> proved that position on a common scale (y-axis) is the most accurate visual encoding for comparison. ",
        "Bar length reinforces the position encoding. Keep baselines at zero to avoid misleading comparisons (<b>Tufte's integrity principle</b>)."
      )
      result$primary_code <- '# Vertical bar chart
library(ggplot2)

ggplot(df, aes(x = category, y = value)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Comparison Across Categories",
       x = "Category", y = "Value") +
  theme(panel.grid.major.x = element_blank())  # Remove vertical gridlines'
      
      if (n_categories <= 5) {
        result$alternative_chart <- "Lollipop Chart"
        result$alternative_explanation <- paste0(
          "With only ", n_categories, " categories, a lollipop chart offers a modern alternative. ",
          "It maintains position encoding accuracy while reducing data-ink (Tufte's principle). ",
          "The visual is cleaner than traditional bars."
        )
        result$alternative_code <- '# Lollipop chart
ggplot(df, aes(x = category, y = value)) +
  geom_segment(aes(x = category, xend = category, y = 0, yend = value), 
               color = "gray70") +
  geom_point(size = 4, color = "steelblue") +
  theme_minimal() +
  labs(title = "Comparison Across Categories", x = "Category", y = "Value")'
      }
    }
  }
  
  # --------------------------------------------------------------------------
  # CASE 2: Numerical + Distribution
  # --------------------------------------------------------------------------
  else if (data_type == "Numerical" && goal == "Distribution") {
    
    if (dataset_size == "Large (>1000)") {
      # Large dataset - density plot
      result$primary_chart <- "Density Plot"
      result$primary_explanation <- paste0(
        "For large datasets (>1000 observations), a density plot effectively shows distribution shape without overplotting. ",
        "<b>Tufte's principle</b>: show the data pattern without overwhelming the viewer. ",
        "Density curves use position and length encoding, which are high on <b>Cleveland & McGill's hierarchy</b>."
      )
      result$primary_code <- '# Density plot
library(ggplot2)

ggplot(df, aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of Values",
       x = "Value", y = "Density") +
  theme(panel.grid.minor = element_blank())'
      
      result$warning <- paste0(
        "<span style='color: #f0ad4e;'><b>💡 Tip:</b> With large datasets, avoid histograms with default binning—they can be misleading. ",
        "If you use a histogram, experiment with bin width. Density plots smooth over this issue.</span>"
      )
      
      result$alternative_chart <- "Hexbin Plot (if showing relationship)"
      result$alternative_explanation <- paste0(
        "If you're actually exploring relationships between two variables in your large dataset, ",
        "a hexbin plot prevents overplotting by aggregating points into hexagonal bins. ",
        "This maintains position encoding (most accurate) while handling volume."
      )
      result$alternative_code <- '# Hexbin plot for relationships in large data
library(ggplot2)

ggplot(df, aes(x = var1, y = var2)) +
  geom_hex() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Relationship Between Variables", x = "Variable 1", y = "Variable 2")'
      
    } else {
      # Small/medium dataset - histogram or boxplot
      result$primary_chart <- "Histogram"
      result$primary_explanation <- paste0(
        "For datasets with fewer than 1000 observations, a histogram clearly shows the distribution shape. ",
        "<b>Position encoding</b> (Cleveland & McGill's most accurate method) is used for both axes. ",
        "Choose bin width carefully to reveal structure without creating spurious patterns (<b>Tufte's integrity</b>)."
      )
      result$primary_code <- '# Histogram
library(ggplot2)

ggplot(df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Values",
       x = "Value", y = "Count") +
  theme(panel.grid.minor = element_blank())

# Try different bin widths to see structure:
# bins = 15 (coarser), bins = 50 (finer)'
      
      result$alternative_chart <- "Box Plot (for outlier detection)"
      result$alternative_explanation <- paste0(
        "If identifying outliers is important, use a box plot. It uses position encoding and shows ",
        "quartiles, median, and outliers in a compact form. However, note that box plots hide the actual ",
        "distribution shape (including bimodality), so they're best as a complement to histograms, not a replacement."
      )
      result$alternative_code <- '# Box plot
ggplot(df, aes(y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution Summary with Outliers", y = "Value") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())'
    }
  }
  
  # --------------------------------------------------------------------------
  # CASE 3: Numerical + Relationship
  # --------------------------------------------------------------------------
  else if (data_type %in% c("Numerical", "Mixed") && goal == "Relationship") {
    
    if (dataset_size == "Large (>1000)") {
      # Large dataset - warn about overplotting
      result$primary_chart <- "Scatter Plot with Transparency"
      result$primary_explanation <- paste0(
        "Scatter plots use <b>position encoding on two dimensions</b> (Cleveland & McGill's most accurate), ",
        "making them ideal for showing relationships. With large datasets, use transparency (alpha) to reveal ",
        "point density and prevent overplotting. This maintains <b>Tufte's principle</b> of showing all data without obscuring patterns."
      )
      result$primary_code <- '# Scatter plot with transparency
library(ggplot2)

ggplot(df, aes(x = var1, y = var2)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +  # Add trend
  theme_minimal() +
  labs(title = "Relationship Between Variables",
       x = "Variable 1", y = "Variable 2")'
      
      result$warning <- paste0(
        "<span style='color: #f0ad4e;'><b>⚠ Overplotting Risk:</b> With ", dataset_size, 
        ", points will overlap heavily. Use alpha transparency (shown in code) or consider alternatives like hexbin or 2D density plots.</span>"
      )
      
      result$alternative_chart <- "2D Density Contour or Hexbin"
      result$alternative_explanation <- paste0(
        "For very large datasets, hexbin or contour plots aggregate points while preserving the relationship pattern. ",
        "This prevents overplotting entirely while maintaining position encoding. <b>Few's principle</b>: show the pattern, not individual noise."
      )
      result$alternative_code <- '# Hexbin alternative
library(ggplot2)
ggplot(df, aes(x = var1, y = var2)) +
  geom_hex(bins = 30) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Relationship (Density)", x = "Variable 1", y = "Variable 2")'
      
    } else {
      # Small/medium dataset
      result$primary_chart <- "Scatter Plot"
      result$primary_explanation <- paste0(
        "Scatter plots are perfect for showing relationships between two numerical variables. ",
        "<b>Cleveland & McGill</b>: position on both x and y axes provides the most accurate perception of relationships. ",
        "With your dataset size (", dataset_size, "), individual points will be visible and informative (<b>Tufte</b>: show the data)."
      )
      result$primary_code <- '# Scatter plot
library(ggplot2)

ggplot(df, aes(x = var1, y = var2)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +  # Add trend line
  theme_minimal() +
  labs(title = "Relationship Between Variables",
       x = "Variable 1", y = "Variable 2")'
      
      result$alternative_chart <- "Scatter with Groups/Colors"
      result$alternative_explanation <- paste0(
        "If you have categorical groups in your data, add color encoding to reveal subgroup patterns. ",
        "<b>Pre-attentive attributes</b>: color hue is processed automatically, allowing immediate pattern detection. ",
        "Use colorblind-friendly palettes (viridis or ColorBrewer)."
      )
      result$alternative_code <- '# Scatter plot with groups
ggplot(df, aes(x = var1, y = var2, color = group)) +
  geom_point(size = 2) +
  scale_color_viridis_d() +  # Colorblind-friendly
  theme_minimal() +
  labs(title = "Relationship by Group", x = "Variable 1", y = "Variable 2")'
    }
  }
  
  # --------------------------------------------------------------------------
  # CASE 4: Composition
  # --------------------------------------------------------------------------
  else if (goal == "Composition") {
    
    if (n_categories > 5) {
      # Many parts - avoid pie
      result$primary_chart <- "Stacked Bar Chart (100%)"
      result$primary_explanation <- paste0(
        "For showing composition with ", n_categories, " parts, use a 100% stacked bar chart. ",
        "<b>Cleveland & McGill</b>: while position is most accurate, aligned lengths are acceptable for part-to-whole. ",
        "This is far superior to pie charts, where <b>angle perception is the least accurate</b> visual encoding."
      )
      result$primary_code <- '# Stacked bar chart (100%)
library(ggplot2)

ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +  # Make horizontal for easier reading
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Composition (Parts to Whole)",
       x = "", y = "Percentage", fill = "Category")'
      
      result$warning <- paste0(
        "<span style='color: #d9534f;'><b>🚫 Avoid Pie Charts:</b> With ", n_categories, 
        " categories, pie charts become unreadable. <b>Cleveland & McGill</b> showed angle perception is the least accurate encoding. ",
        "<b>Tufte</b>: pie charts often violate data integrity when 3D effects are added.</span>"
      )
      
      result$alternative_chart <- "Treemap"
      result$alternative_explanation <- paste0(
        "For many categories, a treemap uses area encoding to show composition. While area is less accurate than position ",
        "(Cleveland & McGill), treemaps handle many parts better than pies or stacked bars. Good for hierarchical data."
      )
      result$alternative_code <- '# Treemap (requires treemapify package)
library(ggplot2)
library(treemapify)

ggplot(df, aes(area = value, fill = category, label = category)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  scale_fill_viridis_d() +
  theme(legend.position = "none")'
      
    } else {
      # Few parts - pie acceptable but bar preferred
      result$primary_chart <- "Stacked Bar Chart"
      result$primary_explanation <- paste0(
        "For ", n_categories, " parts, a stacked bar chart clearly shows composition. ",
        "<b>Length perception</b> (Cleveland & McGill) is more accurate than angles, making this superior to pie charts. ",
        "The clean design adheres to <b>Tufte's data-ink ratio</b> principle."
      )
      result$primary_code <- '# Stacked bar chart
library(ggplot2)

ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Composition (Parts to Whole)",
       x = "", y = "Value", fill = "Category") +
  theme(axis.text.y = element_blank())'
      
      result$alternative_chart <- "Pie Chart (acceptable with caveat)"
      result$alternative_explanation <- paste0(
        "With only ", n_categories, " parts, a pie chart is <em>acceptable</em> but not optimal. ",
        "<b>Cleveland & McGill</b>: angle perception is less accurate than length or position. ",
        "If you use a pie, keep it 2D (no 3D effects—Tufte's integrity) and consider ordering slices by size."
      )
      result$alternative_code <- '# Pie chart (use sparingly!)
library(ggplot2)

ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_col() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  labs(title = "Composition", fill = "Category")

# Note: Only use for 3-5 categories with a general audience'
      
      result$warning <- paste0(
        "<span style='color: #f0ad4e;'><b>💡 Note:</b> While pie charts are common, stacked bars are more accurate. ",
        "Reserve pies for general audiences when exact precision isn't critical.</span>"
      )
    }
  }
  
  # --------------------------------------------------------------------------
  # CASE 5: Mixed Data + Distribution (e.g., comparing distributions across categories)
  # --------------------------------------------------------------------------
  else if (data_type == "Mixed" && goal == "Distribution") {
    
    if (n_categories <= 5) {
      result$primary_chart <- "Violin Plot"
      result$primary_explanation <- paste0(
        "Violin plots show the full distribution shape for each category, combining the benefits of box plots and density curves. ",
        "<b>Cleveland & McGill</b>: position along the y-axis allows accurate comparison between groups. ",
        "<b>Few's principle</b>: show the actual data distribution, not just summary statistics. Great for ", n_categories, " groups."
      )
      result$primary_code <- '# Violin plot
library(ggplot2)

ggplot(df, aes(x = category, y = value, fill = category)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +  # Add boxplot inside for reference
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Distribution Across Categories",
       x = "Category", y = "Value") +
  theme(legend.position = "none")'
      
      result$alternative_chart <- "Ridgeline Plot"
      result$alternative_explanation <- paste0(
        "Ridgeline plots (joyplots) stack density curves vertically, making distribution shapes easy to compare. ",
        "They work well when distributions might overlap in a violin plot. Maintains position encoding accuracy."
      )
      result$alternative_code <- '# Ridgeline plot (requires ggridges)
library(ggplot2)
library(ggridges)

ggplot(df, aes(x = value, y = category, fill = category)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Distribution Across Categories", x = "Value", y = "Category") +
  theme(legend.position = "none")'
      
    } else {
      result$primary_chart <- "Faceted Histograms"
      result$primary_explanation <- paste0(
        "With ", n_categories, " categories, use <b>small multiples</b> (Tufte's principle): faceted histograms with the same scale. ",
        "This allows direct comparison of distribution shapes while avoiding visual clutter. ",
        "<b>Cleveland & McGill</b>: position encoding is preserved in each facet."
      )
      result$primary_code <- '# Faceted histograms (small multiples)
library(ggplot2)

ggplot(df, aes(x = value)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "white") +
  facet_wrap(~category, scales = "free_y") +  # Free y-axis if counts differ
  theme_minimal() +
  labs(title = "Distribution Across Categories",
       x = "Value", y = "Count") +
  theme(panel.grid.minor = element_blank())'
      
      result$warning <- paste0(
        "<span style='color: #f0ad4e;'><b>💡 Tip:</b> With ", n_categories, " groups, avoid overlapping density plots—they become unreadable. ",
        "Small multiples (facets) maintain clarity by giving each group its own panel.</span>"
      )
    }
  }
  
  # --------------------------------------------------------------------------
  # DEFAULT CASE (if no specific match)
  # --------------------------------------------------------------------------
  else {
    result$primary_chart <- "Exploratory Data Analysis Needed"
    result$primary_explanation <- paste0(
      "Based on your inputs (", data_type, " data, ", goal, " goal), I recommend starting with exploratory analysis. ",
      "Try a scatter plot matrix (for numerical relationships) or a summary table (for categorical). ",
      "<b>Tufte's principle</b>: understand your data before choosing a visualization."
    )
    result$primary_code <- '# Exploratory scatter plot matrix
library(GGally)
ggpairs(df, aes(alpha = 0.5)) +
  theme_minimal()

# Or summary statistics
summary(df)'
    
    result$warning <- paste0(
      "<span style='color: #5bc0de;'><b>ℹ Info:</b> The combination of inputs is ambiguous. ",
      "Consider refining your analytical goal or exploring your data structure first.</span>"
    )
  }
  
  # Return results
  if (!show_alternative) {
    result$alternative_chart <- ""
    result$alternative_explanation <- ""
    result$alternative_code <- ""
  }
  
  return(result)
}


# ==============================================================================
# USER INTERFACE
# ==============================================================================

ui <- page_sidebar(
  title = "Visualization Advisor Bot (Prototype)",
  theme = bs_theme(bootswatch = "flatly", 
                   primary = "#2C3E50",
                   base_font = font_google("Source Sans Pro")),
  
  # Sidebar with inputs
  sidebar = sidebar(
    width = 350,
    
    h4("Tell me about your data:"),
    
    selectInput(
      "data_type",
      "Data Type:",
      choices = c("Categorical", "Numerical", "Mixed"),
      selected = "Categorical"
    ),
    
    radioButtons(
      "goal",
      "What do you want to show?",
      choices = c("Comparison", "Distribution", "Relationship", "Composition"),
      selected = "Comparison"
    ),
    
    sliderInput(
      "n_categories",
      "Number of categories/groups:",
      min = 2,
      max = 20,
      value = 5,
      step = 1
    ),
    p(style = "font-size: 0.85em; color: #7f8c8d;",
      "For categorical/mixed data or when comparing groups"),
    
    selectInput(
      "dataset_size",
      "Dataset Size:",
      choices = c("Small (<100 rows)", "Medium (100-1000)", "Large (>1000)"),
      selected = "Medium (100-1000)"
    ),
    
    checkboxInput(
      "show_alternative",
      "Show alternative chart option",
      value = FALSE
    ),
    
    actionButton(
      "recommend",
      "Get Recommendation",
      class = "btn-primary btn-lg",
      width = "100%"
    ),
    
    br(), br(),
    p(style = "font-size: 0.8em; color: #95a5a6;",
      "Built on Cleveland & McGill's perceptual hierarchy and Tufte's design principles.")
  ),
  
  # Main panel with outputs
  div(
    style = "padding: 20px;",
    
    h3("Recommendations", style = "color: #2C3E50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
    
    uiOutput("recommendation_output")
  )
)


# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # Reactive recommendation
  recommendation <- eventReactive(input$recommend, {
    get_recommendation(
      data_type = input$data_type,
      goal = input$goal,
      n_categories = input$n_categories,
      dataset_size = input$dataset_size,
      show_alternative = input$show_alternative
    )
  })
  
  # Render output
  output$recommendation_output <- renderUI({
    req(input$recommend)  # Only show after button click
    
    rec <- recommendation()
    
    tagList(
      # Warning (if exists)
      if (rec$warning != "") {
        div(
          style = "background-color: #fcf8e3; border-left: 4px solid #f0ad4e; padding: 15px; margin-bottom: 20px;",
          HTML(rec$warning)
        )
      },
      
      # Primary recommendation
      div(
        style = "background-color: #ecf0f1; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
        h4(icon("chart-bar"), " Recommended Chart:", style = "color: #2980b9; margin-top: 0;"),
        h3(rec$primary_chart, style = "color: #2C3E50; margin-top: 5px;"),
        
        h5("Why this chart?", style = "margin-top: 20px; color: #34495e;"),
        p(HTML(rec$primary_explanation), style = "line-height: 1.6;"),
        
        h5("R/ggplot2 Code:", style = "margin-top: 20px; color: #34495e;"),
        pre(
          style = "background-color: #2C3E50; color: #ecf0f1; padding: 15px; border-radius: 5px; overflow-x: auto;",
          code(rec$primary_code)
        )
      ),
      
      # Alternative recommendation (if requested)
      if (input$show_alternative && rec$alternative_chart != "") {
        div(
          style = "background-color: #e8f4f8; padding: 20px; border-radius: 5px; border-left: 4px solid #3498db;",
          h4(icon("lightbulb"), " Alternative Option:", style = "color: #2980b9; margin-top: 0;"),
          h4(rec$alternative_chart, style = "color: #2C3E50;"),
          
          h5("Why consider this?", style = "margin-top: 20px; color: #34495e;"),
          p(HTML(rec$alternative_explanation), style = "line-height: 1.6;"),
          
          h5("R/ggplot2 Code:", style = "margin-top: 20px; color: #34495e;"),
          pre(
            style = "background-color: #34495e; color: #ecf0f1; padding: 15px; border-radius: 5px; overflow-x: auto;",
            code(rec$alternative_code)
          )
        )
      },
      
      # Footer
      hr(),
      p(
        style = "font-size: 0.9em; color: #7f8c8d; font-style: italic;",
        icon("info-circle"),
        " Remember: Test your visualization with real data and adjust parameters (colors, scales, labels) as needed. ",
        "These recommendations are starting points based on perceptual science."
      )
    )
  })
}


# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)
