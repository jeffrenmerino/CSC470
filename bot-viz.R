# ============================================================================
# VIZ ADVISOR
# A perceptual-science chart advisor. Modern build.
# ============================================================================

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

options(shiny.maxRequestSize = 200 * 1024^2)

# Null-coalesce helper (defined here so it's available throughout)
`%||%` <- function(a, b) if (is.null(a)) b else a

# ── API key ─────────────────────────────────────────────────────────────────
key_path <- "/usr/home/georgetowncollege.edu/jmerino/test-bot/api-key.txt"
if (file.exists(key_path) && Sys.getenv("ANTHROPIC_API_KEY") == "") {
  Sys.setenv(ANTHROPIC_API_KEY = readLines(key_path, warn = FALSE)[1])
}

# ── Plot resource directory (served as /vaplots/...) ────────────────────────
PLOT_ROOT <- file.path(tempdir(), "viz_advisor_plots")
if (!dir.exists(PLOT_ROOT)) dir.create(PLOT_ROOT, recursive = TRUE)
shiny::addResourcePath("vaplots", PLOT_ROOT)

# ============================================================================
# DATA HELPERS
# ============================================================================

# Scan the working directory for tabular files.
# Restricted to .csv / .tsv. Plain .txt is ambiguous (api keys, READMEs, logs)
# and was previously surfacing api-key.txt as a selectable "dataset".
scan_working_dir <- function(path = getwd()) {
  if (!dir.exists(path)) return(character(0))
  files <- list.files(
    path,
    pattern = "\\.(csv|tsv)$",
    ignore.case = TRUE,
    recursive  = TRUE,
    full.names = FALSE
  )
  # Defensive: never expose anything that looks like a credential, even if
  # the pattern above is later relaxed to include .txt again.
  files <- files[!grepl("(^|/)(api[-_ ]?key|secret|token|credentials)",
                        files, ignore.case = TRUE)]
  if (!length(files)) return(character(0))
  full <- file.path(path, files)
  setNames(full, files)
}

# Find data.frames in the global environment.
scan_global_env <- function() {
  nms <- ls(envir = .GlobalEnv)
  if (!length(nms)) return(character(0))
  ok <- vapply(nms, function(n) {
    obj <- tryCatch(get(n, envir = .GlobalEnv), error = function(e) NULL)
    is.data.frame(obj) && nrow(obj) > 0 && ncol(obj) > 0
  }, logical(1))
  out <- nms[ok]
  if (!length(out)) return(character(0))
  # Build a labelled vector: "name (rows x cols)"
  labels <- vapply(out, function(n) {
    obj <- get(n, envir = .GlobalEnv)
    sprintf("%s   (%s x %d)", n, format(nrow(obj), big.mark = ","), ncol(obj))
  }, character(1))
  setNames(out, labels)
}

# Read a delimited file into a data.frame.
# Validates that the result actually looks tabular (>= 1 row, >= 2 cols).
# Anything else (single-line API keys, prose .txt, etc.) is rejected cleanly
# instead of becoming a one-column "dataframe".
read_tabular <- function(path) {
  ext <- tolower(tools::file_ext(path))
  df <- if (ext == "tsv") {
    read_tsv(path, show_col_types = FALSE)
  } else {
    read_csv(path, show_col_types = FALSE)
  }
  if (!is.data.frame(df) || nrow(df) < 1 || ncol(df) < 2) {
    stop(sprintf(
      "%s does not look like a tabular file (got %d rows x %d cols). Pick a CSV or TSV with a header row and at least two columns.",
      basename(path),
      if (is.data.frame(df)) nrow(df) else 0,
      if (is.data.frame(df)) ncol(df) else 0
    ), call. = FALSE)
  }
  as.data.frame(df)
}

# Build a structural summary for a data.frame.
summarize_data <- function(df, label, source = c("upload", "folder", "global")) {
  source <- match.arg(source)
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  cat_cols <- names(df)[vapply(df, function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }, logical(1))]
  list(
    label            = label,
    source           = source,
    n_rows           = nrow(df),
    n_cols           = ncol(df),
    col_names        = names(df),
    numeric_cols     = numeric_cols,
    categorical_cols = cat_cols,
    missing_count    = sum(is.na(df)),
    missing_pct      = round(100 * sum(is.na(df)) / max(1, nrow(df) * ncol(df)), 2)
  )
}

# ============================================================================
# SYSTEM PROMPT
# ============================================================================

base_prompt <- function() {
  '# Viz Advisor

You are a senior data visualization advisor grounded in perceptual science.

## Perceptual foundations
Cleveland and McGill hierarchy: position at a common baseline beats length beats angle beats area beats saturation beats hue. Always choose the highest-ranked encoding available for the primary comparison.
Tufte: maximize data-ink ratio. Strip every gridline, border, and fill that does not carry information. No chartjunk, no 3D effects, no redundant encodings. Every pixel must earn its place.
Few: a chart for a business or academic audience must land its single message in under three seconds. One chart, one message. Label data directly instead of a legend whenever there are five or fewer groups.

## Voice
Concise. Decisive. Pedagogical. Lead with the recommendation. Then the why, citing a principle by name. Then the code.
Two short paragraphs is plenty. No bullet lists. No em dashes. Use commas, periods, colons, or parentheses.

## Chart selection rules

Comparisons (up to 7 categories): vertical bars, ALWAYS sorted by value descending (reorder(category, value)).
Comparisons (more than 7 categories): horizontal bars, sorted descending by value.
Never pie charts with more than 4 slices. Never 3D charts of any kind.

Distributions (under 200 rows): histogram with bins chosen by the Scott or Sturges rule, or boxplot for cross-group comparison.
Distributions (200-1000 rows): density plot or violin for groups (max 5 groups). Boxplot plus jitter for small group sizes.
Distributions (over 1000 rows): density plot or ridgeline (ggridges). Never use default 30-bin histogram on large data.

Relationships (under 300 rows): scatter plot, ALWAYS include geom_smooth(method = "lm", se = FALSE, color = "#555", linewidth = 0.8) to show the trend. Without a trend line the reader cannot judge the relationship.
Relationships (300-2000 rows): scatter with alpha = 0.25 and geom_smooth.
Relationships (over 2000 rows): geom_hex() or geom_density_2d_filled(). Never plot raw points at this scale. Overplotting destroys the signal entirely.

Time series: line chart. ALWAYS sort by the time column with arrange() before plotting. Without sorting the line looks random. Maximum 5 lines. If there are more series, aggregate or facet. Aspect ratio should bank slopes near 45 degrees.

Composition / part-of-whole: stacked bar for up to 5 categories. Treemap for many small parts. Never stacked area with more than 4 series.

## Mandatory code quality rules (apply to EVERY render_plot call, no exceptions)
1. Theme: always start with theme_minimal(base_size = 13).
2. White background: always add theme(plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA)).
3. Labels: always include labs(x = "<descriptive axis name>", y = "<descriptive axis name>", title = "<one clear insight>"). Use the actual column names and units, not placeholder words.
4. Sorted axes: for any bar or column chart wrap the categorical variable as reorder(category_col, value_col) or reorder(category_col, -value_col) so bars are ordered by magnitude, never alphabetically.
5. Color: for categorical groups use scale_fill_brewer(palette = "Set2") or scale_color_brewer(palette = "Set2"). For continuous use scale_fill_distiller(palette = "Blues", direction = 1). Never use the default ggplot2 rainbow color cycle.
6. Aggregation: for bar or column charts with more than 200 raw rows, ALWAYS aggregate with dplyr (group_by + summarise) before plotting. Never plot one bar per raw row.
7. Overplotting: for scatter or point charts always set alpha explicitly. Start at 0.5 for under 100 rows, 0.3 for 100-500, 0.15 for over 500.
8. Legend pruning: remove legends that duplicate axis labels using guides(fill = "none") or guides(color = "none"). A legend is only needed when the color encoding conveys information not already on an axis.
9. Final expression: assign the plot to p then write p on the last line so the tool can capture it.

## Tools you have
Use all three tools. Do not write code blocks for the user to copy manually.

1. render_plot(code, caption): renders a ggplot inline. Call this every time you recommend a chart. The tool returns a markdown image link. Embed it verbatim on its own line right after your explanation. Pair every rendered plot with one short paragraph of reasoning.
2. summarise_data(code): runs dplyr or base R on the active dataset and returns text. The data is named df. Use for counts, means, quantiles, top-N, group-bys, and any quantitative answer that does not need a chart.
3. get_dataset_info(): returns shape, column names, types, and a 3-row preview. Call this if unsure about column names before writing any code.

If a tool returns an error, read the message, fix the code, and retry silently. Never apologize for tool failures.

## Data binding
Inside render_plot: data is bound as `uploaded_data`.
Inside summarise_data: data is bound as `df`.
Always use the exact column names listed in the Active dataset section below. Never use placeholder names like x or y.
'
}

build_system_prompt <- function(summary = NULL) {
  base <- base_prompt()
  if (is.null(summary)) return(base)
  ctx <- paste0(
    "\n## Active dataset\n",
    "Source: ", summary$source, "\n",
    "Label: ", summary$label, "\n",
    "Shape: ", format(summary$n_rows, big.mark = ","),
    " rows by ", summary$n_cols, " columns.\n",
    "Numeric columns: ",
    if (length(summary$numeric_cols)) paste(summary$numeric_cols, collapse = ", ") else "(none)",
    "\nCategorical columns: ",
    if (length(summary$categorical_cols)) paste(summary$categorical_cols, collapse = ", ") else "(none)",
    "\n\nUse these exact column names in every code example. The data is bound as `uploaded_data`.\n"
  )
  paste0(base, ctx)
}

# ============================================================================
# PLOT RENDERING TOOL  (the model calls this)
# ============================================================================

make_plot_tool <- function(rv) {
  function(code, caption = "Recommended chart") {
    tryCatch({
      # Evaluate in a child of GlobalEnv so `uploaded_data` is reachable.
      env <- new.env(parent = .GlobalEnv)
      val <- eval(parse(text = code), envir = env)
      
      # If the last expression wasn't itself a ggplot, look for one in the env.
      p <- if (inherits(val, "ggplot")) {
        val
      } else {
        ggs <- Filter(function(x) inherits(x, "ggplot"), as.list(env))
        if (length(ggs)) ggs[[length(ggs)]] else NULL
      }
      
      if (is.null(p)) {
        return(paste0(
          "Error: the code did not produce a ggplot object. ",
          "Make the final expression evaluate to the ggplot itself, ",
          "for example: p <- ggplot(uploaded_data, aes(...)) + ... ; p"
        ))
      }
      
      plot_id   <- sprintf("p%s_%05d", format(Sys.time(), "%H%M%S"), sample.int(99999, 1))
      plot_file <- file.path(PLOT_ROOT, paste0(plot_id, ".png"))
      ggsave(plot_file, plot = p, width = 9, height = 5.4, dpi = 110, bg = "white")
      
      # Track for the gallery (reactive update).
      isolate({
        rv$plot_history <- c(
          rv$plot_history,
          list(list(
            id = plot_id, file = plot_file, caption = caption,
            code = code, time = Sys.time()
          ))
        )
      })
      
      url <- paste0("vaplots/", plot_id, ".png")
      paste0(
        "Plot rendered. Embed this exact markdown image link verbatim on its own line ",
        "in your reply, with no edits:\n\n![", caption, "](", url, ")"
      )
    }, error = function(e) {
      paste0(
        "Error rendering plot: ", conditionMessage(e),
        ". Adjust the code and call render_plot again. ",
        "Common fixes: check column names, ensure final expression is the ggplot, ",
        "wrap factors with as.factor() if needed."
      )
    })
  }
}

# Helper: register a tool and tolerate both the new (.name) and old (name)
# ellmer APIs.
register_named_tool <- function(chat, fn, description, args, tool_name) {
  the_tool <- tryCatch(
    do.call(tool, c(list(fn, description), args, list(.name = tool_name))),
    error = function(e) {
      do.call(tool, c(list(fn, description), args, list(name = tool_name)))
    }
  )
  chat$register_tool(the_tool)
  the_tool
}

# Tool: run a dplyr / base R summary on the active dataset and return text.
# Mirrors Tyler's `summarise_data` so the bot can answer "what's the mean of X
# by Y" without rendering a chart for every question.
make_summarise_tool <- function() {
  function(code) {
    if (!exists("uploaded_data", envir = .GlobalEnv)) {
      return("No dataset is currently loaded. Ask the user to pick one first.")
    }
    df <- get("uploaded_data", envir = .GlobalEnv)
    env <- new.env(parent = .GlobalEnv)
    env$df <- df
    suppressPackageStartupMessages({
      env$`%>%`     <- magrittr::`%>%`
      env$n         <- dplyr::n
      env$summarise <- dplyr::summarise
      env$group_by  <- dplyr::group_by
      env$arrange   <- dplyr::arrange
      env$filter    <- dplyr::filter
      env$mutate    <- dplyr::mutate
      env$select    <- dplyr::select
      env$desc      <- dplyr::desc
      env$count     <- dplyr::count
    })
    out <- tryCatch(
      eval(parse(text = code), envir = env),
      error = function(e) paste0("Error: ", conditionMessage(e))
    )
    if (is.character(out) && length(out) == 1) return(out)
    paste(utils::capture.output(print(out)), collapse = "\n")
  }
}

# Tool: structural snapshot of the active dataset.
make_dataset_info_tool <- function() {
  function() {
    if (!exists("uploaded_data", envir = .GlobalEnv)) {
      return("No dataset is currently loaded.")
    }
    df <- get("uploaded_data", envir = .GlobalEnv)
    cols <- vapply(names(df), function(c) {
      sprintf("%s (%s)", c, class(df[[c]])[1])
    }, character(1))
    paste0(
      "Shape: ", nrow(df), " rows x ", ncol(df), " columns\n",
      "Columns: ", paste(cols, collapse = ", "), "\n",
      "First 3 rows:\n",
      paste(utils::capture.output(print(utils::head(df, 3))), collapse = "\n")
    )
  }
}

# Build a fresh chat client with the current system prompt and tools registered.
build_chat_client <- function(rv, summary = NULL) {
  chat <- chat_anthropic(system_prompt = build_system_prompt(summary))
  
  # 1. render_plot — render ggplots inline.
  register_named_tool(
    chat,
    make_plot_tool(rv),
    paste(
      "Render a ggplot2 chart inline in the user's chat. Call this every time you recommend a visualization.",
      "The user sees the rendered image, not the code.",
      "EVERY call must: use theme_minimal(base_size=13), add a white background via theme(plot.background=element_rect(fill='white',color=NA),panel.background=element_rect(fill='white',color=NA)),",
      "include labs() with descriptive x/y/title using real column names,",
      "sort bars with reorder(), use scale_fill_brewer(palette='Set2') for categorical color,",
      "aggregate >200-row data before bar charts, add geom_smooth() to every scatter,",
      "use geom_hex() for scatter with >2000 rows, set explicit alpha for points,",
      "assign the plot to p and end with p on the last line.",
      "Pair every rendered plot with a short paragraph of reasoning."
    ),
    list(
      code = type_string(paste(
        "Complete executable ggplot2 R code. Data is named `uploaded_data`.",
        "Use only real column names from the active dataset.",
        "Apply all mandatory quality rules: theme_minimal, white background, labs(), reorder(), Set2 colors,",
        "aggregate large data, geom_smooth on scatter, geom_hex for >2k rows, explicit alpha.",
        "Last line must be: p"
      )),
      caption = type_string("Short descriptive caption, e.g. 'Sales by region (sorted)'.")
    ),
    "render_plot"
  )
  
  # 2. summarise_data — run dplyr/base R, return text.
  register_named_tool(
    chat,
    make_summarise_tool(),
    paste(
      "Run a statistical summary, count, group-by, or arbitrary computation",
      "on the active dataset and return the result as text.",
      "Use this for averages, top-N, breakdowns, missing-value checks, and",
      "any quantitative answer the user asks for that does not need a chart."
    ),
    list(
      code = type_string(paste(
        "Valid R code that uses a variable named `df` (the active data frame)",
        "and returns a data frame, vector, or scalar. dplyr verbs (filter,",
        "group_by, summarise, arrange, mutate, count) and the pipe `%>%` are",
        "pre-loaded. Do NOT use ggplot here, use render_plot for charts."
      ))
    ),
    "summarise_data"
  )
  
  # 3. get_dataset_info — structural snapshot on demand.
  register_named_tool(
    chat,
    make_dataset_info_tool(),
    paste(
      "Get the shape, column names, types, and a 3-row preview of the active",
      "dataset. Call this if you are unsure about column names or types",
      "before writing code for render_plot or summarise_data."
    ),
    list(),
    "get_dataset_info"
  )
  
  chat
}

# ============================================================================
# CSS  (OKLCH tokens, restrained palette, modern startup vibes)
# ============================================================================

custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Instrument+Serif:ital@0;1&family=JetBrains+Mono:wght@400;500&display=swap');

:root {
  /* Tinted neutrals warm-cool slate */
  --bg:           oklch(0.985 0.004 286);
  --surface-1:    oklch(0.998 0.002 286);
  --surface-2:    oklch(0.965 0.006 286);
  --surface-3:    oklch(0.935 0.008 286);
  --line:         oklch(0.915 0.007 286);
  --line-strong:  oklch(0.86  0.010 286);
  --ink:          oklch(0.20  0.018 286);
  --ink-2:        oklch(0.36  0.018 286);
  --ink-muted:    oklch(0.52  0.014 286);
  --ink-soft:     oklch(0.66  0.012 286);

  /* One confident accent, indigo-violet */
  --accent:       oklch(0.51  0.205 282);
  --accent-2:     oklch(0.45  0.22  282);
  --accent-soft:  oklch(0.965 0.030 282);
  --accent-line:  oklch(0.86  0.080 282);
  --accent-ink:   oklch(0.32  0.20  282);

  /* Semantic */
  --positive:     oklch(0.55  0.14 155);
  --positive-soft:oklch(0.965 0.035 155);
  --warn:         oklch(0.65  0.14  72);
  --warn-soft:    oklch(0.96  0.04  72);
  --danger:       oklch(0.56  0.20  25);
  --danger-soft:  oklch(0.965 0.040 25);

  --r-sm: 8px;
  --r-md: 12px;
  --r-lg: 16px;
  --shadow-1: 0 1px 2px oklch(0.20 0.02 286 / 0.04), 0 0 0 1px oklch(0.20 0.02 286 / 0.04);
  --shadow-2: 0 6px 24px oklch(0.20 0.02 286 / 0.08);

  --font-ui:    'Inter', system-ui, sans-serif;
  --font-serif: 'Instrument Serif', Georgia, serif;
  --font-mono:  'JetBrains Mono', ui-monospace, monospace;
}

html, body {
  font-family: var(--font-ui) !important;
  background: var(--bg) !important;
  color: var(--ink) !important;
  font-feature-settings: 'cv11', 'ss01';
  letter-spacing: -0.005em;
}
* { box-sizing: border-box; }

/* ── Top tab strip ─────────────────────────────────────────────────────── */
.bslib-card .nav-tabs,
.nav-tabs {
  border-bottom: 1px solid var(--line) !important;
  background: var(--surface-1);
  padding: 0 8px;
}
.nav-tabs .nav-link {
  font-size: 0.84rem !important;
  font-weight: 500 !important;
  color: var(--ink-muted) !important;
  border: none !important;
  padding: 13px 16px !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0 !important;
  transition: color .15s, border-color .15s;
  letter-spacing: -0.005em;
}
.nav-tabs .nav-link:hover:not(.active) { color: var(--ink) !important; }
.nav-tabs .nav-link.active {
  color: var(--accent) !important;
  background: transparent !important;
  border-bottom-color: var(--accent) !important;
  font-weight: 600 !important;
}

/* ── Sidebar shell (no layout overrides on the grid) ───────────────────── */
.bslib-sidebar-layout > .bslib-sidebar {
  background: var(--surface-2) !important;
  border-right: 1px solid var(--line) !important;
}

/* ── Brand strip ───────────────────────────────────────────────────────── */
.brand-strip {
  display: flex; align-items: baseline; gap: 8px;
  padding: 6px 4px 14px;
  border-bottom: 1px solid var(--line);
  margin-bottom: 14px;
}
.brand-mark {
  width: 28px; height: 28px;
  border-radius: 7px;
  background:
    radial-gradient(circle at 30% 30%, var(--accent), var(--accent-2));
  flex: 0 0 28px;
  position: relative;
  box-shadow: 0 1px 3px oklch(0.20 0.02 286 / 0.15);
}
.brand-mark::after {
  content: '';
  position: absolute; inset: 6px 6px auto auto;
  width: 8px; height: 8px;
  border-radius: 99px;
  background: oklch(1 0 0 / 0.85);
}
.brand-name {
  font-family: var(--font-serif);
  font-style: italic;
  font-size: 1.6rem;
  line-height: 1;
  color: var(--ink);
  letter-spacing: -0.01em;
}
.brand-tag {
  font-size: 0.71rem;
  color: var(--ink-soft);
  margin-left: auto;
  letter-spacing: 0.04em;
  text-transform: uppercase;
}

/* ── Sidebar sections ──────────────────────────────────────────────────── */
.sb-section { margin-bottom: 16px; }
.sb-label {
  font-size: 0.68rem;
  font-weight: 600;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--ink-soft);
  margin: 0 0 7px 2px;
  display: flex; align-items: center; gap: 6px;
}
.sb-label .sb-dot {
  width: 4px; height: 4px; border-radius: 99px;
  background: var(--accent);
}

/* ── Source mode segmented control ─────────────────────────────────────── */
.source-segmented {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  background: var(--surface-3);
  border: 1px solid var(--line);
  border-radius: 9px;
  padding: 3px;
  gap: 2px;
  margin-bottom: 10px;
}
.source-segmented .seg-btn {
  appearance: none; background: transparent; border: 0;
  padding: 6px 8px;
  font-family: var(--font-ui);
  font-size: 0.78rem;
  font-weight: 500;
  color: var(--ink-muted);
  border-radius: 7px;
  cursor: pointer;
  transition: background .15s, color .15s, box-shadow .15s;
  display: flex; align-items: center; justify-content: center; gap: 5px;
}
.source-segmented .seg-btn:hover {
  color: var(--ink);
}
.source-segmented .seg-btn.active {
  background: var(--surface-1);
  color: var(--ink);
  font-weight: 600;
  box-shadow: 0 1px 2px oklch(0.20 0.02 286 / 0.08);
}

/* ── Source panels ─────────────────────────────────────────────────────── */
.src-panel {
  background: var(--surface-1);
  border: 1px solid var(--line);
  border-radius: 10px;
  padding: 11px 12px;
}
.src-panel .form-control,
.src-panel .selectize-input {
  font-size: 0.83rem !important;
  border-color: var(--line) !important;
  border-radius: 8px !important;
  background: var(--surface-1) !important;
  color: var(--ink) !important;
}
.src-panel .selectize-input:focus,
.src-panel .form-control:focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 3px var(--accent-soft) !important;
}
.src-row { display: flex; gap: 6px; align-items: stretch; margin-top: 6px; }
.src-row .btn { padding: 5px 10px; font-size: 0.78rem; }

/* ── File input restyle ────────────────────────────────────────────────── */
.src-panel .input-group { background: transparent; }
.src-panel input[type='file'] { font-size: 0.78rem; }
.src-panel .btn-file,
.src-panel .input-group-btn .btn {
  background: var(--surface-2) !important;
  color: var(--ink) !important;
  border: 1px solid var(--line) !important;
  font-size: 0.78rem !important;
  font-weight: 500 !important;
}

/* ── Active dataset card ───────────────────────────────────────────────── */
.active-card {
  position: relative;
  background: var(--surface-1);
  border: 1px solid var(--line);
  border-radius: 11px;
  padding: 11px 13px;
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 10px;
  align-items: center;
}
.active-card.is-empty {
  background: var(--surface-2);
  border-style: dashed;
  color: var(--ink-soft);
}
.active-card .ac-name {
  font-weight: 600;
  font-size: 0.86rem;
  color: var(--ink);
  line-height: 1.25;
  word-break: break-word;
}
.active-card .ac-meta {
  font-size: 0.74rem;
  color: var(--ink-muted);
  margin-top: 3px;
  font-variant-numeric: tabular-nums;
}
.active-card .ac-pill {
  font-size: 0.66rem;
  padding: 2px 7px;
  border-radius: 99px;
  background: var(--accent-soft);
  color: var(--accent-ink);
  font-weight: 600;
  letter-spacing: 0.03em;
  text-transform: uppercase;
  align-self: start;
}
.active-card.is-empty .ac-pill {
  background: var(--surface-3);
  color: var(--ink-soft);
}

/* ── Smart prompts (chips) ─────────────────────────────────────────────── */
.chips-wrap {
  display: flex; flex-wrap: wrap; gap: 6px;
}
.chip-btn {
  appearance: none; border: 1px solid var(--line);
  background: var(--surface-1);
  color: var(--ink-2);
  font-family: var(--font-ui);
  font-size: 0.77rem;
  padding: 5px 10px;
  border-radius: 99px;
  cursor: pointer;
  transition: background .15s, border-color .15s, color .15s, transform .15s;
}
.chip-btn:hover {
  background: var(--accent-soft);
  border-color: var(--accent-line);
  color: var(--accent-ink);
}
.chip-btn:active { transform: scale(0.97); }

/* ── Quiet action stack ────────────────────────────────────────────────── */
.action-stack { display: flex; flex-direction: column; gap: 4px; }
.btn-quiet {
  font-family: var(--font-ui) !important;
  font-size: 0.79rem !important;
  font-weight: 500 !important;
  color: var(--ink-2) !important;
  background: transparent !important;
  border: 1px solid var(--line) !important;
  border-radius: 8px !important;
  padding: 7px 10px !important;
  text-align: left !important;
  transition: background .15s, color .15s, border-color .15s;
  display: flex; align-items: center; gap: 8px;
  width: 100%;
}
.btn-quiet:hover {
  background: var(--accent-soft) !important;
  color: var(--accent-ink) !important;
  border-color: var(--accent-line) !important;
}
.btn-quiet:focus { outline: 2px solid var(--accent-line); outline-offset: 1px; }

/* ── Footer principles ─────────────────────────────────────────────────── */
.foundations {
  font-size: 0.74rem;
  color: var(--ink-soft);
  line-height: 1.6;
  padding: 10px 0 4px;
  border-top: 1px solid var(--line);
}
.foundations strong { color: var(--ink-2); font-weight: 600; }
.foundations em { font-family: var(--font-serif); font-style: italic; color: var(--ink-2); font-size: 1.05em; }

/* ── Chat shell ────────────────────────────────────────────────────────── */
#chat-card {
  border: 1px solid var(--line) !important;
  border-radius: var(--r-lg) !important;
  box-shadow: var(--shadow-2) !important;
  background: var(--surface-1) !important;
  overflow: hidden;
}
.shiny-chat-container { background: var(--surface-1); }
.shiny-chat-container .chat-messages {
  padding: 24px 28px !important;
  scroll-behavior: smooth;
}

/* ── Chat messages ─────────────────────────────────────────────────────── */
.chat-message {
  margin-bottom: 16px !important;
  line-height: 1.65 !important;
  font-size: 0.92rem !important;
  animation: msgIn 0.22s cubic-bezier(.21,.99,.39,1);
}
@keyframes msgIn {
  from { opacity: 0; transform: translateY(4px); }
  to   { opacity: 1; transform: translateY(0); }
}

/* User bubble */
.chat-message[data-role='user'] .chat-message-body,
.chat-message.user .chat-message-body {
  background: var(--accent-soft) !important;
  border: 1px solid var(--accent-line) !important;
  border-radius: 16px 16px 4px 16px !important;
  padding: 10px 15px !important;
  margin-left: 22% !important;
  color: var(--accent-ink) !important;
  display: block;
  box-shadow: var(--shadow-1);
}

/* Assistant bubble */
.chat-message[data-role='assistant'] .chat-message-body,
.chat-message.assistant .chat-message-body {
  background: var(--surface-1) !important;
  border: 1px solid var(--line) !important;
  border-radius: 16px 16px 16px 4px !important;
  padding: 12px 16px !important;
  margin-right: 12% !important;
  display: block;
  box-shadow: var(--shadow-1);
}

/* Inline images (rendered plots) */
.chat-message img {
  max-width: 100%;
  height: auto;
  border-radius: 10px;
  border: 1px solid var(--line);
  margin: 8px 0;
  box-shadow: var(--shadow-1);
  background: var(--surface-1);
}

/* Code in chat */
.chat-message pre {
  background: oklch(0.18 0.02 286) !important;
  color: oklch(0.95 0.01 286) !important;
  border-radius: 10px !important;
  padding: 14px 16px !important;
  font-family: var(--font-mono) !important;
  font-size: 0.81rem !important;
  line-height: 1.55 !important;
  overflow-x: auto !important;
  border: none !important;
  margin: 10px 0 !important;
}
.chat-message code {
  font-family: var(--font-mono) !important;
  font-size: 0.83rem !important;
  background: var(--surface-3) !important;
  color: var(--ink-2) !important;
  padding: 1px 6px !important;
  border-radius: 4px !important;
}
.chat-message pre code { background: transparent !important; padding: 0 !important; color: inherit !important; }

.chat-message h1, .chat-message h2, .chat-message h3 {
  font-weight: 600;
  letter-spacing: -0.01em;
  margin: 14px 0 6px;
}
.chat-message h2 { font-size: 1.1rem; }
.chat-message h3 { font-size: 1rem; color: var(--ink-2); }
.chat-message blockquote {
  border-left: 0;
  background: var(--accent-soft);
  border-radius: 8px;
  padding: 10px 14px;
  margin: 10px 0;
  color: var(--accent-ink);
  font-style: italic;
  font-family: var(--font-serif);
  font-size: 1.02rem;
}

/* Chat input */
.shiny-chat-container .chat-user-input {
  border-top: 1px solid var(--line) !important;
  padding: 14px 18px !important;
  background: var(--surface-2) !important;
}
.shiny-chat-container .chat-user-input textarea {
  font-family: var(--font-ui) !important;
  font-size: 0.92rem !important;
  border-radius: 11px !important;
  border: 1.5px solid var(--line-strong) !important;
  padding: 10px 14px !important;
  resize: none !important;
  background: var(--surface-1) !important;
  color: var(--ink) !important;
  transition: border-color .15s, box-shadow .15s;
}
.shiny-chat-container .chat-user-input textarea:focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 3px var(--accent-soft) !important;
  outline: none !important;
}
.shiny-chat-container .chat-user-input button,
.shiny-chat-container .chat-user-input .btn {
  background: var(--accent) !important;
  border: none !important;
  border-radius: 9px !important;
  color: oklch(0.995 0.003 282) !important;
  font-weight: 600 !important;
  transition: background .15s, transform .1s;
}
.shiny-chat-container .chat-user-input button:hover {
  background: var(--accent-2) !important;
  transform: translateY(-1px);
}

/* ── Data Explorer ─────────────────────────────────────────────────────── */
.bslib-value-box {
  border-radius: var(--r-md) !important;
  border: 1px solid var(--line) !important;
  box-shadow: var(--shadow-1) !important;
  background: var(--surface-1) !important;
}

table.dataTable {
  font-family: var(--font-ui) !important;
  font-size: 0.86rem !important;
}
table.dataTable thead th {
  background: var(--surface-2) !important;
  font-weight: 600 !important;
  color: var(--ink-muted) !important;
  font-size: 0.74rem !important;
  text-transform: uppercase !important;
  letter-spacing: 0.06em !important;
  border-bottom: 1px solid var(--line) !important;
  padding: 10px 12px !important;
}
table.dataTable tbody td {
  border-color: var(--line) !important;
  color: var(--ink-2) !important;
}

.col-card {
  margin-bottom: 10px;
  padding: 12px 14px;
  background: var(--surface-1);
  border: 1px solid var(--line);
  border-radius: 11px;
  transition: border-color .15s, transform .15s;
}
.col-card:hover { border-color: var(--accent-line); }
.col-card .col-name {
  font-weight: 600;
  color: var(--ink);
  font-size: 0.92rem;
  letter-spacing: -0.005em;
  display: flex; align-items: center; gap: 8px;
}
.col-card .col-type-pill {
  font-size: 0.65rem;
  padding: 2px 8px;
  border-radius: 99px;
  background: var(--accent-soft);
  color: var(--accent-ink);
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}
.col-card .col-type-pill.cat {
  background: var(--positive-soft);
  color: oklch(0.30 0.14 155);
}
.col-card .col-stats {
  margin-top: 8px;
  font-family: var(--font-mono);
  font-size: 0.78rem;
  color: var(--ink-muted);
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(110px, 1fr));
  gap: 6px 14px;
}
.col-card .col-stats .k { color: var(--ink-soft); font-size: 0.74rem; }
.col-card .col-stats .v { color: var(--ink-2); font-variant-numeric: tabular-nums; }

/* ── Plot Lab ──────────────────────────────────────────────────────────── */
#plot_code_input {
  font-family: var(--font-mono) !important;
  font-size: 0.84rem !important;
  border-radius: 11px !important;
  border: 1.5px solid oklch(0.30 0.02 286) !important;
  background: oklch(0.16 0.02 286) !important;
  color: oklch(0.92 0.01 286) !important;
  padding: 14px !important;
  line-height: 1.55 !important;
}
#plot_code_input:focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 3px var(--accent-soft) !important;
  outline: none !important;
}

.plot-toolbar {
  display: flex; gap: 8px; align-items: center;
  padding: 10px 14px;
  background: var(--surface-2);
  border: 1px solid var(--line);
  border-radius: var(--r-md);
  margin-bottom: 14px;
}
.plot-toolbar .toolbar-spacer { flex: 1; }
.plot-toolbar .toolbar-status {
  font-size: 0.82rem;
  color: var(--ink-muted);
}

.btn-primary {
  background: var(--accent) !important;
  border-color: var(--accent) !important;
  font-weight: 600 !important;
  border-radius: 9px !important;
  font-size: 0.84rem !important;
  padding: 7px 14px !important;
}
.btn-primary:hover { background: var(--accent-2) !important; border-color: var(--accent-2) !important; }

.btn-ghost {
  background: var(--surface-1) !important;
  border: 1px solid var(--line) !important;
  color: var(--ink-2) !important;
  font-weight: 500 !important;
  border-radius: 9px !important;
  font-size: 0.84rem !important;
  padding: 7px 12px !important;
}
.btn-ghost:hover {
  background: var(--accent-soft) !important;
  color: var(--accent-ink) !important;
  border-color: var(--accent-line) !important;
}

/* Gallery */
.gallery-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
  gap: 14px;
  padding: 4px;
}
.gallery-item {
  background: var(--surface-1);
  border: 1px solid var(--line);
  border-radius: var(--r-md);
  overflow: hidden;
  display: flex; flex-direction: column;
  cursor: pointer;
  transition: transform .18s cubic-bezier(.21,.99,.39,1), border-color .18s, box-shadow .18s;
}
.gallery-item:hover {
  border-color: var(--accent-line);
  transform: translateY(-2px);
  box-shadow: var(--shadow-2);
}
.gallery-item img {
  width: 100%; height: 140px; object-fit: cover;
  background: var(--surface-3);
  display: block;
}
.gallery-item .gi-meta {
  padding: 8px 11px;
  font-size: 0.78rem;
  color: var(--ink-muted);
  display: flex; flex-direction: column; gap: 2px;
}
.gallery-item .gi-cap {
  color: var(--ink); font-weight: 500;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}

/* Empty state */
.empty-state {
  text-align: center;
  padding: 60px 24px;
  color: var(--ink-muted);
  background: var(--surface-1);
  border: 1px dashed var(--line-strong);
  border-radius: var(--r-md);
}
.empty-state .es-glyph {
  font-family: var(--font-serif);
  font-style: italic;
  font-size: 2.5rem;
  color: var(--ink-soft);
  margin-bottom: 10px;
}
.empty-state .es-title {
  font-weight: 600;
  color: var(--ink);
  font-size: 1rem;
  margin-bottom: 4px;
}
.empty-state .es-body {
  font-size: 0.86rem;
  color: var(--ink-muted);
  max-width: 380px;
  margin: 0 auto;
  line-height: 1.55;
}

/* Notifications */
#shiny-notification-panel {
  bottom: 20px !important; right: 20px !important; top: auto !important;
}
.shiny-notification {
  font-family: var(--font-ui) !important;
  border-radius: 11px !important;
  box-shadow: var(--shadow-2) !important;
  font-size: 0.86rem !important;
  background: var(--surface-1) !important;
  color: var(--ink) !important;
  border: 1px solid var(--line) !important;
}
.shiny-notification-error { border-left: 3px solid var(--danger) !important; }
.shiny-notification-warning { border-left: 3px solid var(--warn) !important; }
.shiny-notification-message { border-left: 3px solid var(--positive) !important; }

/* Scrollbars */
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: var(--line-strong); border-radius: 99px; }
::-webkit-scrollbar-thumb:hover { background: var(--ink-soft); }

/* hide bsicons stroke that comes from `bs_icon()` when used inline */
.bi { vertical-align: -0.125em; }
"

# Tiny JS to power the segmented control without writing a custom Shiny input
segmented_js <- "
$(document).on('click', '.source-segmented .seg-btn', function() {
  var $btn = $(this);
  var val = $btn.data('value');
  $btn.siblings('.seg-btn').removeClass('active');
  $btn.addClass('active');
  Shiny.setInputValue('source_mode', val, {priority: 'event'});
});
$(document).on('click', '.chip-btn', function() {
  var prompt = $(this).data('prompt');
  Shiny.setInputValue('chip_clicked', { prompt: prompt, ts: Date.now() }, {priority: 'event'});
});
$(document).on('click', '.gallery-item', function() {
  var code = $(this).data('code');
  Shiny.setInputValue('gallery_clicked', { code: code, ts: Date.now() }, {priority: 'event'});
});
"

# ============================================================================
# UI
# ============================================================================

ui <- page_fillable(
  theme = bs_theme(
    preset       = "shiny",
    primary      = "#5b3df5",  # falls back if oklch unsupported
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font    = font_google("JetBrains Mono"),
    bg           = "#f6f5f8",
    fg           = "#1f1d2b"
  ),
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML(custom_css)),
    tags$script(HTML(segmented_js))
  ),
  
  navset_card_tab(
    id = "main_tabs",
    
    # ───────────────────────────── ADVISOR ──────────────────────────────────
    nav_panel(
      "Advisor",
      layout_sidebar(
        fillable = TRUE,
        border   = FALSE,
        
        sidebar = sidebar(
          width   = 320,
          padding = "16px",
          bg      = NULL,  # let CSS handle it
          open    = TRUE,
          
          # Brand strip
          div(
            class = "brand-strip",
            div(class = "brand-mark"),
            div(class = "brand-name", "Viz Advisor"),
            div(class = "brand-tag", "v2")
          ),
          
          # ─── Data source ───
          div(
            class = "sb-section",
            div(class = "sb-label", div(class = "sb-dot"), "Data source"),
            
            # segmented control
            div(
              class = "source-segmented",
              tags$button(class = "seg-btn active", type = "button", `data-value` = "upload",
                          bs_icon("cloud-arrow-up"), "Upload"),
              tags$button(class = "seg-btn", type = "button", `data-value` = "folder",
                          bs_icon("folder"), "Folder"),
              tags$button(class = "seg-btn", type = "button", `data-value` = "global",
                          bs_icon("hexagon"), "R env")
            ),
            
            # Conditional panels for each mode
            uiOutput("source_panel")
          ),
          
          # ─── Active dataset ───
          div(
            class = "sb-section",
            div(class = "sb-label", div(class = "sb-dot"), "Active dataset"),
            uiOutput("active_dataset_card")
          ),
          
          # ─── Smart prompts ───
          div(
            class = "sb-section",
            div(class = "sb-label", div(class = "sb-dot"), "Try asking"),
            uiOutput("smart_prompts")
          ),
          
          # ─── Quiet actions ───
          div(
            class = "sb-section",
            div(class = "sb-label", div(class = "sb-dot"), "Session"),
            div(
              class = "action-stack",
              actionButton("load_example", tagList(bs_icon("magic"), "Load example dataset"),
                           class = "btn-quiet"),
              actionButton("reset_chat", tagList(bs_icon("arrow-counterclockwise"), "Reset conversation"),
                           class = "btn-quiet"),
              downloadButton("export_chat", tagList(bs_icon("download"), "Export chat"),
                             class = "btn-quiet")
            )
          ),
          
          # Foundations
          div(
            class = "foundations",
            tags$em("Grounded in"),
            tags$br(),
            tags$strong("Cleveland and McGill"), " hierarchy. ",
            tags$strong("Tufte"), "'s data-ink. ",
            tags$strong("Few"), "'s clarity."
          )
        ),
        
        # Main: chat in a fill card
        card(
          id      = "chat-card",
          fill    = TRUE,
          padding = 0,
          chat_ui(
            "chat",
            height   = "100%",
            fill     = TRUE,
            messages = list(
              list(
                role    = "assistant",
                content = paste0(
                  "**Viz Advisor.**\n\n",
                  "The right chart for your data, rendered instantly.\n\n",
                  "Load a dataset from the sidebar, then tell me what you want to see. ",
                  "I\u2019ll pick the right chart type, explain the reasoning, and render it right here in the chat, ",
                  "every recommendation grounded in Cleveland and McGill, Tufte, and Few.\n\n",
                  "_No data yet? Click **Load example dataset** in the sidebar and ask away._"
                )
              )
            )
          )
        )
      )
    ),
    
    # ───────────────────────────── DATA ─────────────────────────────────────
    nav_panel(
      "Data",
      div(
        style = "padding: 20px; overflow-y: auto; height: 100%;",
        uiOutput("data_stats_strip"),
        div(style = "height: 14px;"),
        layout_columns(
          col_widths = c(12, 6, 6),
          card(
            full_screen = TRUE,
            card_header("Preview", class = "fw-semibold"),
            DTOutput("data_preview_table")
          ),
          card(
            card_header(tagList(bs_icon("123"), " Numeric columns"), class = "fw-semibold"),
            div(style = "overflow-y: auto; max-height: 480px; padding: 6px 12px 12px;",
                uiOutput("numeric_cols_info"))
          ),
          card(
            card_header(tagList(bs_icon("tag"), " Categorical columns"), class = "fw-semibold"),
            div(style = "overflow-y: auto; max-height: 480px; padding: 6px 12px 12px;",
                uiOutput("categorical_cols_info"))
          )
        )
      )
    ),
    
    # ───────────────────────────── PLOT LAB ─────────────────────────────────
    nav_panel(
      "Plot Lab",
      div(
        style = "padding: 20px; overflow-y: auto; height: 100%;",
        
        # Toolbar
        div(
          class = "plot-toolbar",
          div(class = "toolbar-status", uiOutput("plot_lab_status", inline = TRUE)),
          div(class = "toolbar-spacer"),
          actionButton("render_plot_btn", tagList(bs_icon("play-fill"), " Render"),
                       class = "btn btn-primary"),
          actionButton("refresh_plot_btn", tagList(bs_icon("arrow-clockwise"), " Refresh"),
                       class = "btn btn-ghost"),
          downloadButton("download_plot", tagList(bs_icon("download"), " Save PNG"),
                         class = "btn btn-ghost")
        ),
        
        # Editor + plot, side by side
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("Code", class = "fw-semibold"),
            div(style = "padding: 12px;",
                textAreaInput(
                  "plot_code_input",
                  label = NULL,
                  value = "# Paste or write ggplot2 code\nlibrary(ggplot2)\n\nggplot(uploaded_data, aes(x = ..., y = ...)) +\n  geom_col() +\n  theme_minimal()",
                  rows  = 16,
                  width = "100%",
                  resize = "vertical"
                )
            )
          ),
          card(
            card_header("Output", class = "fw-semibold"),
            div(style = "padding: 16px;",
                withSpinner(
                  plotOutput("rendered_plot", height = "440px"),
                  type = 8, color = "#5b3df5", size = 0.5
                )
            )
          )
        ),
        
        # Gallery
        div(style = "height: 24px;"),
        card(
          card_header(tagList(bs_icon("collection"), " Gallery"), class = "fw-semibold"),
          div(style = "padding: 14px;",
              uiOutput("plot_gallery"))
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive store
  rv <- reactiveValues(
    data_summary = NULL,
    chat_client  = NULL,
    chat_history = list(),
    plot_history = list(),
    source_mode  = "upload",
    folder_path  = getwd(),
    last_render_code = NULL
  )
  
  # ── Source mode tracking ────────────────────────────────────────────────
  observeEvent(input$source_mode, {
    rv$source_mode <- input$source_mode
  })
  
  # ── Conditional source panel ────────────────────────────────────────────
  output$source_panel <- renderUI({
    mode <- rv$source_mode %||% "upload"
    if (mode == "upload") {
      div(class = "src-panel",
          fileInput("data_file", NULL,
                    accept = c(".csv", ".tsv", "text/csv"),
                    buttonLabel = "Choose file",
                    placeholder = "No file selected",
                    width = "100%")
      )
    } else if (mode == "folder") {
      files <- scan_working_dir(rv$folder_path)
      div(class = "src-panel",
          textInput("folder_path_input", NULL, value = rv$folder_path, width = "100%",
                    placeholder = "Working directory"),
          if (length(files)) {
            tagList(
              selectInput("folder_file", NULL, choices = files, width = "100%"),
              div(class = "src-row",
                  actionButton("load_folder_file", "Load", class = "btn btn-primary btn-sm"),
                  actionButton("rescan_folder", tagList(bs_icon("arrow-clockwise")),
                               class = "btn btn-ghost btn-sm",
                               title = "Rescan folder")
              )
            )
          } else {
            tagList(
              div(style = "font-size: 0.79rem; color: var(--ink-soft); margin-top: 8px; line-height: 1.5;",
                  "No CSV or TSV files found in this folder."),
              div(class = "src-row",
                  actionButton("rescan_folder", tagList(bs_icon("arrow-clockwise"), " Rescan"),
                               class = "btn btn-ghost btn-sm")
              )
            )
          }
      )
    } else {  # global
      objs <- scan_global_env()
      div(class = "src-panel",
          if (length(objs)) {
            tagList(
              selectInput("global_obj", NULL, choices = objs, width = "100%"),
              div(class = "src-row",
                  actionButton("load_global_obj", "Use this", class = "btn btn-primary btn-sm"),
                  actionButton("rescan_global", tagList(bs_icon("arrow-clockwise")),
                               class = "btn btn-ghost btn-sm",
                               title = "Rescan global environment")
              )
            )
          } else {
            tagList(
              div(style = "font-size: 0.79rem; color: var(--ink-soft); margin-top: 4px; line-height: 1.5;",
                  "No data frames in the global environment yet."),
              div(style = "font-size: 0.74rem; color: var(--ink-soft); margin-top: 6px; line-height: 1.5;",
                  HTML(paste0(
                    "To test this: in the same R session that is running the app, ",
                    "type something like <code>data(mtcars)</code> or ",
                    "<code>df &lt;- read.csv('your.csv')</code> in the console, ",
                    "then click Rescan."
                  ))),
              div(class = "src-row",
                  actionButton("rescan_global", tagList(bs_icon("arrow-clockwise"), " Rescan"),
                               class = "btn btn-ghost btn-sm")
              )
            )
          }
      )
    }
  })
  
  # Rescan triggers (force renderUI to re-evaluate)
  observeEvent(input$rescan_folder, {
    if (!is.null(input$folder_path_input) && nzchar(input$folder_path_input)) {
      rv$folder_path <- input$folder_path_input
    }
    # Trigger UI refresh by toggling source mode briefly
    cur <- rv$source_mode
    rv$source_mode <- "__"
    rv$source_mode <- cur
    showNotification("Folder rescanned.", type = "message", duration = 2)
  })
  observeEvent(input$rescan_global, {
    cur <- rv$source_mode
    rv$source_mode <- "__"
    rv$source_mode <- cur
    showNotification("Global environment rescanned.", type = "message", duration = 2)
  })
  
  # ── Activate a dataset ──────────────────────────────────────────────────
  activate_dataset <- function(df, label, source) {
    assign("uploaded_data", df, envir = .GlobalEnv)
    rv$data_summary <- summarize_data(df, label, source)
    rv$chat_client  <- build_chat_client(rv, rv$data_summary)
    msg <- paste0(
      "**", rv$data_summary$label, "** is now active.  \n",
      format(rv$data_summary$n_rows, big.mark = ","), " rows by ",
      rv$data_summary$n_cols, " columns. ",
      length(rv$data_summary$numeric_cols), " numeric, ",
      length(rv$data_summary$categorical_cols), " categorical.\n\n",
      "Tell me what you want to see, or pick one of the suggestions on the left."
    )
    chat_append_message("chat", list(role = "assistant", content = msg))
    rv$chat_history <- append(rv$chat_history, list(list(role = "assistant", content = msg)))
  }
  
  # File upload
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      df <- read_tabular(input$data_file$datapath)
      activate_dataset(df, input$data_file$name, "upload")
      updateTabsetPanel(session, "main_tabs", selected = "Data")
      showNotification("Data loaded.", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Read error:", conditionMessage(e)), type = "error", duration = 8)
    })
  })
  
  # Folder file load
  observeEvent(input$load_folder_file, {
    req(input$folder_file)
    tryCatch({
      df <- read_tabular(input$folder_file)
      activate_dataset(df, basename(input$folder_file), "folder")
      updateTabsetPanel(session, "main_tabs", selected = "Data")
      showNotification(paste("Loaded:", basename(input$folder_file)), type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Read error:", conditionMessage(e)), type = "error", duration = 8)
    })
  })
  
  # Global env object load
  observeEvent(input$load_global_obj, {
    req(input$global_obj)
    tryCatch({
      df <- get(input$global_obj, envir = .GlobalEnv)
      if (!is.data.frame(df)) stop("Selected object is not a data frame.")
      activate_dataset(df, input$global_obj, "global")
      updateTabsetPanel(session, "main_tabs", selected = "Data")
      showNotification(paste("Using:", input$global_obj), type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 8)
    })
  })
  
  # ── Active dataset card ─────────────────────────────────────────────────
  output$active_dataset_card <- renderUI({
    if (is.null(rv$data_summary)) {
      div(class = "active-card is-empty",
          div(
            div(class = "ac-name", "No dataset"),
            div(class = "ac-meta", "Pick a source above")
          ),
          div(class = "ac-pill", "Idle")
      )
    } else {
      pill_label <- switch(rv$data_summary$source,
                           upload = "Upload", folder = "Folder", global = "R env")
      div(class = "active-card",
          div(
            div(class = "ac-name", rv$data_summary$label),
            div(class = "ac-meta",
                format(rv$data_summary$n_rows, big.mark = ","), " x ",
                rv$data_summary$n_cols, "  .  ",
                length(rv$data_summary$numeric_cols), " num, ",
                length(rv$data_summary$categorical_cols), " cat")
          ),
          div(class = "ac-pill", pill_label)
      )
    }
  })
  
  # ── Smart prompts ───────────────────────────────────────────────────────
  output$smart_prompts <- renderUI({
    s <- rv$data_summary
    chips <- if (is.null(s)) {
      c("How do I compare two groups?",
        "When should I use a boxplot?",
        "Which chart for time series?")
    } else {
      out <- character(0)
      if (length(s$numeric_cols) >= 2) {
        out <- c(out, sprintf("Relationship between %s and %s",
                              s$numeric_cols[1], s$numeric_cols[2]))
      }
      if (length(s$numeric_cols) && length(s$categorical_cols)) {
        out <- c(out, sprintf("Compare %s across %s",
                              s$numeric_cols[1], s$categorical_cols[1]))
      }
      if (length(s$numeric_cols)) {
        out <- c(out, sprintf("Distribution of %s", s$numeric_cols[1]))
      }
      if (length(s$categorical_cols)) {
        out <- c(out, sprintf("Counts by %s", s$categorical_cols[1]))
      }
      if (!length(out)) out <- "Suggest a chart for this data"
      head(out, 4)
    }
    div(class = "chips-wrap",
        lapply(chips, function(p) {
          tags$button(type = "button", class = "chip-btn", `data-prompt` = p, p)
        })
    )
  })
  
  # Chip click sends message
  observeEvent(input$chip_clicked, {
    req(rv$chat_client)
    msg <- input$chip_clicked$prompt
    if (is.null(msg) || !nzchar(msg)) return()
    rv$chat_history <- append(rv$chat_history, list(list(role = "user", content = msg)))
    chat_append_message("chat", list(role = "user", content = msg))
    stream <- rv$chat_client$stream_async(msg)
    chat_append("chat", stream)
  })
  
  # ── Initialize chat client on session start ──────────────────────────────
  # The greeting is pre-rendered via chat_ui(messages=...) in the UI, so we
  # only need to build the client here. No chat_append_message needed.
  observe({
    rv$chat_client <- build_chat_client(rv, NULL)
    welcome <- paste0(
      "**Viz Advisor.**\n\n",
      "The right chart for your data, rendered instantly.\n\n",
      "Load a dataset from the sidebar, then tell me what you want to see. ",
      "I\u2019ll pick the right chart type, explain the reasoning, and render it right here in the chat, ",
      "every recommendation grounded in Cleveland and McGill, Tufte, and Few.\n\n",
      "_No data yet? Click **Load example dataset** in the sidebar and ask away._"
    )
    rv$chat_history <- list(list(role = "assistant", content = welcome))
  }) |> bindEvent(session$clientData$url_protocol, once = TRUE)
  
  # ── Chat input handling ─────────────────────────────────────────────────
  observeEvent(input$chat_user_input, {
    req(rv$chat_client, input$chat_user_input)
    msg <- input$chat_user_input
    rv$chat_history <- append(rv$chat_history, list(list(role = "user", content = msg)))
    tryCatch({
      stream <- rv$chat_client$stream_async(msg)
      chat_append("chat", stream)
    }, error = function(e) {
      showNotification(paste("AI error:", conditionMessage(e)), type = "error", duration = 8)
      chat_append_message("chat", list(
        role = "assistant",
        content = paste0("Error: ", conditionMessage(e), ". Please try again.")
      ))
    })
  })
  
  # ── Reset ───────────────────────────────────────────────────────────────
  observeEvent(input$reset_chat, {
    rv$data_summary <- NULL
    rv$chat_history <- list()
    rv$plot_history <- list()
    if (exists("uploaded_data", envir = .GlobalEnv)) {
      rm("uploaded_data", envir = .GlobalEnv)
    }
    tryCatch(reset("data_file"), error = function(e) NULL, warning = function(w) NULL)
    rv$chat_client <- build_chat_client(rv, NULL)
    
    welcome <- paste0(
      "**Viz Advisor.**\n\n",
      "Fresh start. Load a dataset from the sidebar, then tell me what you want to see."
    )
    chat_clear("chat")
    chat_append_message("chat", list(role = "assistant", content = welcome))
    rv$chat_history <- list(list(role = "assistant", content = welcome))
    showNotification("Conversation reset.", type = "message", duration = 3)
  })
  
  # ── Example dataset ─────────────────────────────────────────────────────
  observeEvent(input$load_example, {
    set.seed(42)
    df <- data.frame(
      Region       = rep(c("North", "South", "East", "West", "Central"), each = 50),
      Product      = sample(c("Alpha", "Beta", "Gamma"), 250, replace = TRUE),
      Sales        = round(rnorm(250, 50000, 15000), 2),
      Units        = rpois(250, 100),
      Satisfaction = sample(1:5, 250, replace = TRUE),
      Date         = seq(as.Date("2024-01-01"), by = "1 day", length.out = 250)
    )
    activate_dataset(df, "example_sales.csv", "upload")
    updateTabsetPanel(session, "main_tabs", selected = "Data")
    showNotification("Example dataset loaded.", type = "message", duration = 3)
  })
  
  # ── Export chat ─────────────────────────────────────────────────────────
  output$export_chat <- downloadHandler(
    filename = function() paste0("viz_advisor_", Sys.Date(), ".txt"),
    content = function(file) {
      lines <- paste0(
        "Viz Advisor chat export\n",
        "Date: ", Sys.time(), "\n",
        strrep("=", 72), "\n\n"
      )
      for (m in rv$chat_history) {
        role <- if (m$role == "user") "YOU" else "ADVISOR"
        lines <- paste0(lines, "[", role, "]\n", m$content, "\n\n", strrep("-", 72), "\n\n")
      }
      writeLines(lines, file)
    }
  )
  
  # ── DATA TAB ────────────────────────────────────────────────────────────
  output$data_stats_strip <- renderUI({
    if (is.null(rv$data_summary)) {
      div(class = "empty-state",
          div(class = "es-glyph", "no data"),
          div(class = "es-title", "Nothing loaded yet"),
          div(class = "es-body",
              "Pick a CSV from upload, your working folder, or your R session. ",
              "I'll show summary statistics here."))
    } else {
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Rows", format(rv$data_summary$n_rows, big.mark = ","),
                  showcase = bs_icon("rows"), theme = "primary"),
        value_box("Columns", rv$data_summary$n_cols,
                  showcase = bs_icon("layout-three-columns"), theme = "info"),
        value_box("Numeric", length(rv$data_summary$numeric_cols),
                  showcase = bs_icon("123"), theme = "success"),
        value_box("Missing", paste0(rv$data_summary$missing_pct, "%"),
                  showcase = bs_icon("dash-circle"),
                  theme = if (rv$data_summary$missing_pct > 5) "warning" else "secondary")
      )
    }
  })
  
  # Helper: coerce any column type that JSON/DT cannot serialize to a safe string.
  # Order of checks matters: most-specific classes must come before is.atomic().
  sanitize_for_dt <- function(df) {
    as.data.frame(
      lapply(df, function(col) {
        # 1. Multi-dimensional objects (matrix/array/nested data.frame)
        if (!is.null(dim(col))) {
          return(apply(col, 1, function(x) paste(as.character(x), collapse = ", ")))
        }
        # 2. Date/time types — must come before is.atomic() because Date,
        #    POSIXct, difftime, hms are all atomic but need special formatting.
        if (inherits(col, c("POSIXct", "POSIXlt"))) {
          return(format(as.POSIXct(col), "%Y-%m-%d %H:%M:%S"))
        }
        if (inherits(col, "Date")) {
          return(format(col, "%Y-%m-%d"))
        }
        if (inherits(col, c("hms", "difftime"))) {
          return(as.character(col))
        }
        # 3. Factor — as.vector() would give integers; use as.character() instead.
        if (is.factor(col)) {
          return(as.character(col))
        }
        # 4. List-columns (e.g. from tidyr::nest or I()-wrapped lists)
        if (is.list(col)) {
          return(vapply(col, function(x) {
            if (length(x) == 0 || is.null(x)) NA_character_
            else paste(as.character(x), collapse = "; ")
          }, character(1)))
        }
        # 5. Plain atomic vectors (numeric, integer, character, logical) — safe as-is.
        if (is.atomic(col)) {
          return(col)
        }
        # 6. Fallback for S4 objects, environments, etc.
        return(as.character(col))
      }),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  }
  
  output$data_preview_table <- renderDT({
    if (is.null(rv$data_summary)) {
      datatable(
        data.frame(Note = "Load a dataset to preview."),
        options = list(dom = "t"), rownames = FALSE
      )
    } else {
      tryCatch({
        df_raw  <- head(get("uploaded_data", envir = .GlobalEnv), 200)
        df_safe <- sanitize_for_dt(df_raw)
        names(df_safe) <- names(df_raw)
        
        # BRUTAL ASSERTION: Force every column to be a strict 1D atomic vector.
        # If any column slipped through as a list, matrix, or complex object,
        # it will explode DataTables JS. This assertion forces the tryCatch
        # to intercept it cleanly and return the fallback table.
        for (i in seq_along(df_safe)) {
          if (is.list(df_safe[[i]]) || !is.atomic(df_safe[[i]]) || is.matrix(df_safe[[i]])) {
            stop(sprintf("Column '%s' is not a flat atomic vector.", names(df_safe)[i]))
          }
        }
        
        datatable(
          df_safe,
          options = list(pageLength = 10, scrollX = TRUE, dom = "frtip",
                         autoWidth = FALSE),
          class    = "cell-border stripe hover compact",
          rownames = FALSE
        )
      }, error = function(e) {
        datatable(
          data.frame(Error = paste("Preview unavailable:", conditionMessage(e))),
          options = list(dom = "t"),
          rownames = FALSE
        )
      })
    }
  }, server = FALSE)
  
  output$numeric_cols_info <- renderUI({
    s <- rv$data_summary
    if (is.null(s) || !length(s$numeric_cols)) {
      return(div(style = "color: var(--ink-soft); padding: 14px; font-size: 0.86rem;",
                 "No numeric columns."))
    }
    df <- get("uploaded_data", envir = .GlobalEnv)
    lapply(s$numeric_cols, function(col) {
      d <- df[[col]]
      div(class = "col-card",
          div(class = "col-name",
              col,
              tags$span(class = "col-type-pill", "numeric")
          ),
          div(class = "col-stats",
              div(div(class = "k", "min"),    div(class = "v", round(min(d, na.rm = TRUE), 3))),
              div(div(class = "k", "max"),    div(class = "v", round(max(d, na.rm = TRUE), 3))),
              div(div(class = "k", "mean"),   div(class = "v", round(mean(d, na.rm = TRUE), 3))),
              div(div(class = "k", "median"), div(class = "v", round(median(d, na.rm = TRUE), 3))),
              div(div(class = "k", "sd"),     div(class = "v", round(sd(d, na.rm = TRUE), 3))),
              div(div(class = "k", "missing"),div(class = "v", sum(is.na(d))))
          )
      )
    })
  })
  
  output$categorical_cols_info <- renderUI({
    s <- rv$data_summary
    if (is.null(s) || !length(s$categorical_cols)) {
      return(div(style = "color: var(--ink-soft); padding: 14px; font-size: 0.86rem;",
                 "No categorical columns."))
    }
    df <- get("uploaded_data", envir = .GlobalEnv)
    lapply(s$categorical_cols, function(col) {
      d <- df[[col]]
      n_unique <- length(unique(d))
      top_vals <- head(sort(table(d), decreasing = TRUE), 4)
      div(class = "col-card",
          div(class = "col-name",
              col,
              tags$span(class = "col-type-pill cat", "categorical")
          ),
          div(class = "col-stats",
              div(div(class = "k", "unique"),  div(class = "v", n_unique)),
              div(div(class = "k", "missing"), div(class = "v", sum(is.na(d))))
          ),
          if (length(top_vals)) {
            div(style = "margin-top: 8px; font-size: 0.78rem; color: var(--ink-muted);",
                "Top: ",
                paste0(
                  names(top_vals), " (", format(top_vals, big.mark = ","), ")",
                  collapse = ", "
                ))
          }
      )
    })
  })
  
  # ── PLOT LAB ────────────────────────────────────────────────────────────
  output$plot_lab_status <- renderUI({
    if (is.null(rv$data_summary)) {
      tagList(bs_icon("info-circle"), " No active dataset. Code can still run if it doesn't reference data.")
    } else {
      tagList(bs_icon("check-circle-fill", class = "text-success"),
              " Active: ", tags$strong(rv$data_summary$label),
              "  .  ", format(rv$data_summary$n_rows, big.mark = ","), " rows")
    }
  })
  
  # Render the plot when render or refresh is clicked
  rendered_plot_obj <- reactiveVal(NULL)
  
  trigger_render <- function() {
    code <- input$plot_code_input
    if (is.null(code) || nchar(trimws(code)) < 5) {
      showNotification("Add some code first.", type = "warning", duration = 3)
      return()
    }
    rv$last_render_code <- code
    tryCatch({
      env <- new.env(parent = .GlobalEnv)
      val <- eval(parse(text = code), envir = env)
      p <- if (inherits(val, "ggplot")) val else {
        ggs <- Filter(function(x) inherits(x, "ggplot"), as.list(env))
        if (length(ggs)) ggs[[length(ggs)]] else NULL
      }
      if (is.null(p)) {
        showNotification("Code did not produce a ggplot.", type = "error", duration = 6)
        rendered_plot_obj(NULL)
      } else {
        rendered_plot_obj(p)
      }
    }, error = function(e) {
      showNotification(paste("Plot error:", conditionMessage(e)), type = "error", duration = 8)
      rendered_plot_obj(NULL)
    })
  }
  
  observeEvent(input$render_plot_btn,  { trigger_render() })
  observeEvent(input$refresh_plot_btn, { trigger_render() })
  
  output$rendered_plot <- renderPlot({
    p <- rendered_plot_obj()
    if (is.null(p)) {
      ggplot() +
        annotate("text", x = 0, y = 0,
                 label = "Click Render to draw your plot",
                 size = 4.5, color = "#7a7a8a", family = "sans") +
        theme_void() +
        theme(panel.background = element_rect(fill = "white", color = NA),
              plot.background  = element_rect(fill = "white", color = NA))
    } else p
  }, res = 100, bg = "white")
  
  # Download the current plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() paste0("viz_advisor_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      p <- rendered_plot_obj()
      if (is.null(p)) {
        showNotification("Render a plot first.", type = "warning")
        return(NULL)
      }
      ggsave(file, plot = p, width = 9, height = 5.5, dpi = 150, bg = "white")
    }
  )
  
  # Gallery
  output$plot_gallery <- renderUI({
    hist <- rv$plot_history
    if (!length(hist)) {
      return(div(style = "color: var(--ink-soft); padding: 14px; font-size: 0.86rem;",
                 "Plots from the chat will appear here. Click any to load its code into the editor."))
    }
    div(class = "gallery-grid",
        lapply(rev(hist), function(item) {
          url <- paste0("vaplots/", item$id, ".png")
          div(class = "gallery-item",
              `data-code` = item$code,
              tags$img(src = url, alt = item$caption),
              div(class = "gi-meta",
                  div(class = "gi-cap", item$caption),
                  div(format(item$time, "%H:%M:%S"))
              )
          )
        })
    )
  })
  
  # Gallery click loads code into editor and switches to Plot Lab
  observeEvent(input$gallery_clicked, {
    code <- input$gallery_clicked$code
    if (is.null(code) || !nzchar(code)) return()
    updateTextAreaInput(session, "plot_code_input", value = code)
    showNotification("Loaded code into editor.", type = "message", duration = 2)
  })
  
  # Clean up plot files on session end (within reason)
  session$onSessionEnded(function() {
    # Best-effort cleanup of this session's plot files older than the session start
    # Keep it simple: nothing for now, as multiple sessions may share the dir.
    invisible(NULL)
  })
}

shinyApp(ui = ui, server = server)