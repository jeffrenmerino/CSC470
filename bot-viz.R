# ============================================================================
# VIZ ADVISOR  v3
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

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── API key ──────────────────────────────────────────────────────────────────
.load_api_key <- function() {
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) return(TRUE)
  paths <- (
    "api-key.txt"
  )
  for (p in paths) {
    if (file.exists(p)) {
      key <- tryCatch(readLines(p, warn = FALSE)[1], error = function(e) "")
      if (!is.na(key) && nzchar(key)) { Sys.setenv(ANTHROPIC_API_KEY = key); return(TRUE) }
    }
  }
  FALSE
}
API_KEY_OK <- .load_api_key()

# ── Plot resource directory ───────────────────────────────────────────────────
PLOT_ROOT <- file.path(tempdir(), "viz_advisor_plots")
if (!dir.exists(PLOT_ROOT)) dir.create(PLOT_ROOT, recursive = TRUE)
shiny::addResourcePath("vaplots", PLOT_ROOT)

# ============================================================================
# DATA HELPERS
# ============================================================================

scan_working_dir <- function(path = getwd()) {
  if (!dir.exists(path)) return(character(0))
  files <- list.files(path, pattern = "\\.(csv|tsv)$", ignore.case = TRUE,
                      recursive = TRUE, full.names = FALSE)
  files <- files[!grepl("(^|/)(api[-_ ]?key|secret|token|credentials)",
                        files, ignore.case = TRUE)]
  if (!length(files)) return(character(0))
  setNames(file.path(path, files), files)
}

scan_global_env <- function() {
  nms <- ls(envir = .GlobalEnv)
  if (!length(nms)) return(character(0))
  ok <- vapply(nms, function(n) {
    obj <- tryCatch(get(n, envir = .GlobalEnv), error = function(e) NULL)
    is.data.frame(obj) && nrow(obj) > 0 && ncol(obj) > 0
  }, logical(1))
  out <- nms[ok]
  if (!length(out)) return(character(0))
  labels <- vapply(out, function(n) {
    obj <- get(n, envir = .GlobalEnv)
    sprintf("%s   (%s x %d)", n, format(nrow(obj), big.mark = ","), ncol(obj))
  }, character(1))
  setNames(out, labels)
}

read_tabular <- function(path) {
  ext <- tolower(tools::file_ext(path))
  df  <- if (ext == "tsv") read_tsv(path, show_col_types = FALSE) else
    read_csv(path, show_col_types = FALSE)
  if (!is.data.frame(df) || nrow(df) < 1 || ncol(df) < 2)
    stop(sprintf(
      "%s doesn't look tabular (%d rows x %d cols). Need a CSV/TSV with a header and at least two columns.",
      basename(path),
      if (is.data.frame(df)) nrow(df) else 0L,
      if (is.data.frame(df)) ncol(df) else 0L
    ), call. = FALSE)
  as.data.frame(df)
}

summarize_data <- function(df, label, source = c("upload", "folder", "global")) {
  source <- match.arg(source)
  list(
    label            = label,
    source           = source,
    n_rows           = nrow(df),
    n_cols           = ncol(df),
    col_names        = names(df),
    numeric_cols     = names(df)[vapply(df, is.numeric, logical(1))],
    categorical_cols = names(df)[vapply(df, function(x)
      is.character(x) || is.factor(x) || is.logical(x), logical(1))],
    missing_count    = sum(is.na(df)),
    missing_pct      = round(100 * sum(is.na(df)) / max(1L, nrow(df) * ncol(df)), 2)
  )
}

sanitize_for_dt <- function(df) {
  df_out <- data.frame(row.names = seq_len(nrow(df)))
  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.atomic(col) && is.null(dim(col))) {
      if (inherits(col, c("POSIXt", "POSIXct", "POSIXlt", "Date"))) {
        df_out[[nm]] <- as.character(format(col, "%Y-%m-%d %H:%M:%S"))
      } else if (inherits(col, c("hms", "difftime"))) {
        df_out[[nm]] <- as.character(col)
      } else if (is.factor(col)) {
        df_out[[nm]] <- as.character(col)
      } else {
        df_out[[nm]] <- col
      }
    } else {
      stringified <- character(nrow(df))
      for (i in seq_len(nrow(df))) {
        val <- if (is.data.frame(col) || is.matrix(col)) col[i, ] else col[[i]]
        if (is.null(val) || length(val) == 0) {
          stringified[i] <- NA_character_
        } else {
          stringified[i] <- paste(capture.output(dput(val)), collapse = " ")
        }
      }
      df_out[[nm]] <- stringified
    }
  }
  names(df_out) <- names(df)
  df_out
}

# Safe accessor — never throws; returns NULL when data absent or corrupt
get_uploaded_data <- function() {
  if (!exists("uploaded_data", envir = .GlobalEnv)) return(NULL)
  obj <- tryCatch(get("uploaded_data", envir = .GlobalEnv), error = function(e) NULL)
  if (is.data.frame(obj)) obj else NULL
}

# ============================================================================
# SYSTEM PROMPT
# ============================================================================

base_prompt <- function() {
  '# Viz Advisor

You are a senior data visualization advisor grounded in perceptual science.

## Perceptual foundations
Cleveland and McGill hierarchy: position at a common baseline beats length beats angle beats area beats saturation beats hue.
Tufte: maximize data-ink ratio. Strip every gridline, border, and fill that does not carry information.
Few: one chart, one message. Label data directly instead of a legend whenever there are five or fewer groups.

## Voice
Concise. Decisive. Lead with the recommendation, then the why (cite principle by name), then the code.
Two short paragraphs maximum. No bullet lists. No em dashes.

## Chart selection rules
Comparisons (up to 7 categories): vertical bars, sorted descending with reorder().
Comparisons (more than 7): horizontal bars, sorted descending.
Never pie charts with more than 4 slices. Never 3D charts.
Distributions (<200 rows): histogram (Scott/Sturges bins) or boxplot for group comparison.
Distributions (200-1000 rows): density or violin (max 5 groups).
Distributions (>1000 rows): density or ridgeline. Never default 30-bin histogram.
Relationships (<300 rows): scatter with geom_smooth(method="lm", se=FALSE, color="#555", linewidth=0.8).
Relationships (300-2000 rows): scatter with alpha=0.25 and geom_smooth.
Relationships (>2000 rows): geom_hex() or geom_density_2d_filled(). Never raw points.
Time series: line chart, always arrange() by time first. Max 5 lines.
Composition: stacked bar for up to 5 categories. Treemap for many small parts.

## Mandatory code quality rules
1. theme_minimal(base_size = 13)
2. theme(plot.background=element_rect(fill="white",color=NA), panel.background=element_rect(fill="white",color=NA))
3. labs(x="<name>", y="<name>", title="<insight>") with real column names
4. reorder() on categorical axis for all bar/column charts
5. scale_fill_brewer(palette="Set2") for categorical; scale_fill_distiller(palette="Blues",direction=1) for continuous
6. Aggregate >200-row data before bar charts
7. Explicit alpha on scatter/point charts
8. Remove redundant legends with guides(fill="none")
9. Assign plot to p; last line is p

## Tools
Use all three tools. Never write code blocks for the user to copy.
1. render_plot(code, caption): renders ggplot inline — embed the returned markdown image verbatim on its own line.
2. summarise_data(code): runs dplyr/base R, returns text. Data bound as df.
3. get_dataset_info(): shape, columns, types, 3-row preview.

Retry silently on tool errors. Never apologize for failures.
Inside render_plot: data is `uploaded_data`. Inside summarise_data: data is `df`.
'
}

build_system_prompt <- function(summary = NULL) {
  base <- base_prompt()
  if (is.null(summary)) return(base)
  paste0(base,
         "\n## Active dataset\n",
         "Source: ", summary$source, "\n",
         "Label: ", summary$label, "\n",
         "Shape: ", format(summary$n_rows, big.mark = ","), " rows by ", summary$n_cols, " columns.\n",
         "Numeric columns: ",
         if (length(summary$numeric_cols)) paste(summary$numeric_cols, collapse = ", ") else "(none)",
         "\nCategorical columns: ",
         if (length(summary$categorical_cols)) paste(summary$categorical_cols, collapse = ", ") else "(none)",
         "\n\nUse these exact column names. Data is bound as `uploaded_data`.\n"
  )
}

# ============================================================================
# TOOLS
# ============================================================================

make_plot_tool <- function(rv) {
  function(code, caption = "Recommended chart") {
    tryCatch({
      env <- new.env(parent = .GlobalEnv)
      val <- eval(parse(text = code), envir = env)
      p   <- if (inherits(val, "ggplot")) val else {
        ggs <- Filter(function(x) inherits(x, "ggplot"), as.list(env))
        if (length(ggs)) ggs[[length(ggs)]] else NULL
      }
      if (is.null(p)) return(
        "Error: code did not produce a ggplot. Ensure the last line evaluates to the plot (p).")
      
      plot_id   <- sprintf("p%s_%05d", format(Sys.time(), "%H%M%S"), sample.int(99999, 1))
      plot_file <- file.path(PLOT_ROOT, paste0(plot_id, ".png"))
      ggsave(plot_file, plot = p, width = 9, height = 5.4, dpi = 110, bg = "white")
      
      isolate({
        entry <- list(id = plot_id, file = plot_file, caption = caption,
                      code = code, time = Sys.time(), source = "chat")
        rv$gallery <- c(rv$gallery, list(entry))
      })
      
      paste0("Plot rendered. Embed this exact markdown on its own line:\n\n",
             "![", caption, "](vaplots/", plot_id, ".png)")
    }, error = function(e) paste0(
      "Error rendering plot: ", conditionMessage(e),
      ". Fix the code and call render_plot again."))
  }
}

make_summarise_tool <- function() {
  function(code) {
    df <- get_uploaded_data()
    if (is.null(df)) return("No dataset loaded. Ask the user to upload one first.")
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
    out <- tryCatch(eval(parse(text = code), envir = env),
                    error = function(e) paste0("Error: ", conditionMessage(e)))
    if (is.character(out) && length(out) == 1L) return(out)
    paste(utils::capture.output(print(out)), collapse = "\n")
  }
}

make_dataset_info_tool <- function() {
  function() {
    df <- get_uploaded_data()
    if (is.null(df)) return("No dataset loaded.")
    cols <- vapply(names(df), function(c)
      sprintf("%s (%s)", c, class(df[[c]])[1]), character(1))
    paste0("Shape: ", nrow(df), " x ", ncol(df), "\n",
           "Columns: ", paste(cols, collapse = ", "), "\n",
           "First 3 rows:\n",
           paste(utils::capture.output(print(utils::head(df, 3))), collapse = "\n"))
  }
}

register_named_tool <- function(chat, fn, description, args, tool_name) {
  t <- tryCatch(
    do.call(tool, c(list(fn, description), args, list(.name = tool_name))),
    error = function(e) do.call(tool, c(list(fn, description), args, list(name = tool_name)))
  )
  chat$register_tool(t)
}

build_chat_client <- function(rv, summary = NULL) {
  if (!API_KEY_OK) return(NULL)
  tryCatch({
    chat <- chat_anthropic(system_prompt = build_system_prompt(summary))
    register_named_tool(
      chat, make_plot_tool(rv),
      paste("Render a ggplot2 chart inline. Call every time you recommend a visualization.",
            "Apply all mandatory quality rules. Last line of code must be p.",
            "Embed the returned markdown image link verbatim on its own line."),
      list(code    = type_string("Complete ggplot2 R code. Data is `uploaded_data`. Last line: p"),
           caption = type_string("Short descriptive caption.")),
      "render_plot"
    )
    register_named_tool(
      chat, make_summarise_tool(),
      "Run a statistical summary on the active dataset and return text.",
      list(code = type_string("R code using `df`. dplyr verbs pre-loaded.")),
      "summarise_data"
    )
    register_named_tool(
      chat, make_dataset_info_tool(),
      "Get shape, column names, types, and 3-row preview of the active dataset.",
      list(), "get_dataset_info"
    )
    chat
  }, error = function(e) { warning("Chat client error: ", conditionMessage(e)); NULL })
}

# ============================================================================
# CSS
# ============================================================================

custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Instrument+Serif:ital@0;1&family=JetBrains+Mono:wght@400;500&display=swap');

:root {
  --bg:            oklch(0.985 0.004 286);
  --surface-1:     oklch(0.998 0.002 286);
  --surface-2:     oklch(0.965 0.006 286);
  --surface-3:     oklch(0.935 0.008 286);
  --line:          oklch(0.915 0.007 286);
  --line-strong:   oklch(0.86  0.010 286);
  --ink:           oklch(0.20  0.018 286);
  --ink-2:         oklch(0.36  0.018 286);
  --ink-muted:     oklch(0.52  0.014 286);
  --ink-soft:      oklch(0.66  0.012 286);
  --accent:        oklch(0.51  0.205 282);
  --accent-2:      oklch(0.45  0.22  282);
  --accent-soft:   oklch(0.965 0.030 282);
  --accent-line:   oklch(0.86  0.080 282);
  --accent-ink:    oklch(0.32  0.20  282);
  --positive:      oklch(0.55  0.14  155);
  --positive-soft: oklch(0.965 0.035 155);
  --warn:          oklch(0.65  0.14   72);
  --danger:        oklch(0.56  0.20   25);
  --danger-soft:   oklch(0.965 0.040  25);
  --r-sm: 8px; --r-md: 12px; --r-lg: 16px;
  --shadow-1: 0 1px 2px oklch(0.20 0.02 286/.04), 0 0 0 1px oklch(0.20 0.02 286/.04);
  --shadow-2: 0 6px 24px oklch(0.20 0.02 286/.08);
  --font-ui:   'Inter', system-ui, sans-serif;
  --font-serif:'Instrument Serif', Georgia, serif;
  --font-mono: 'JetBrains Mono', ui-monospace, monospace;
}

html, body { font-family:var(--font-ui) !important; background:var(--bg) !important;
  color:var(--ink) !important; font-feature-settings:'cv11','ss01'; letter-spacing:-.005em; }
* { box-sizing:border-box; }

.bslib-card .nav-tabs,.nav-tabs { border-bottom:1px solid var(--line) !important;
  background:var(--surface-1); padding:0 8px; }
.nav-tabs .nav-link { font-size:.84rem !important; font-weight:500 !important;
  color:var(--ink-muted) !important; border:none !important; padding:13px 16px !important;
  border-bottom:2px solid transparent !important; border-radius:0 !important;
  transition:color .15s,border-color .15s; }
.nav-tabs .nav-link:hover:not(.active) { color:var(--ink) !important; }
.nav-tabs .nav-link.active { color:var(--accent) !important; background:transparent !important;
  border-bottom-color:var(--accent) !important; font-weight:600 !important; }

.bslib-sidebar-layout > .bslib-sidebar { background:var(--surface-2) !important;
  border-right:1px solid var(--line) !important; }

.brand-strip { display:flex; align-items:baseline; gap:8px; padding:6px 4px 14px;
  border-bottom:1px solid var(--line); margin-bottom:14px; }
.brand-mark { width:28px; height:28px; border-radius:7px;
  background:radial-gradient(circle at 30% 30%,var(--accent),var(--accent-2));
  flex:0 0 28px; position:relative; box-shadow:0 1px 3px oklch(0.20 0.02 286/.15); }
.brand-mark::after { content:''; position:absolute; inset:6px 6px auto auto; width:8px; height:8px;
  border-radius:99px; background:oklch(1 0 0/.85); }
.brand-name { font-family:var(--font-serif); font-style:italic; font-size:1.6rem;
  line-height:1; color:var(--ink); letter-spacing:-.01em; }
.brand-tag { font-size:.71rem; color:var(--ink-soft); margin-left:auto;
  letter-spacing:.04em; text-transform:uppercase; }

.sb-section { margin-bottom:16px; }
.sb-label { font-size:.68rem; font-weight:600; letter-spacing:.08em; text-transform:uppercase;
  color:var(--ink-soft); margin:0 0 7px 2px; display:flex; align-items:center; gap:6px; }
.sb-label .sb-dot { width:4px; height:4px; border-radius:99px; background:var(--accent); }

.source-segmented { display:grid; grid-template-columns:repeat(3,1fr);
  background:var(--surface-3); border:1px solid var(--line); border-radius:9px;
  padding:3px; gap:2px; margin-bottom:10px; }
.source-segmented .seg-btn { appearance:none; background:transparent; border:0; padding:6px 8px;
  font-family:var(--font-ui); font-size:.78rem; font-weight:500; color:var(--ink-muted);
  border-radius:7px; cursor:pointer; transition:background .15s,color .15s,box-shadow .15s;
  display:flex; align-items:center; justify-content:center; gap:5px; }
.source-segmented .seg-btn:hover { color:var(--ink); }
.source-segmented .seg-btn.active { background:var(--surface-1); color:var(--ink);
  font-weight:600; box-shadow:0 1px 2px oklch(0.20 0.02 286/.08); }

.src-panel { background:var(--surface-1); border:1px solid var(--line);
  border-radius:10px; padding:11px 12px; }
.src-panel .form-control,.src-panel .selectize-input { font-size:.83rem !important;
  border-color:var(--line) !important; border-radius:8px !important;
  background:var(--surface-1) !important; color:var(--ink) !important; }
.src-panel .selectize-input:focus,.src-panel .form-control:focus {
  border-color:var(--accent) !important; box-shadow:0 0 0 3px var(--accent-soft) !important; }
.src-row { display:flex; gap:6px; align-items:stretch; margin-top:6px; }
.src-row .btn { padding:5px 10px; font-size:.78rem; }
.src-panel input[type='file'] { font-size:.78rem; }
.src-panel .btn-file,.src-panel .input-group-btn .btn { background:var(--surface-2) !important;
  color:var(--ink) !important; border:1px solid var(--line) !important;
  font-size:.78rem !important; font-weight:500 !important; }

.active-card { background:var(--surface-1); border:1px solid var(--line); border-radius:11px;
  padding:11px 13px; display:grid; grid-template-columns:1fr auto; gap:10px; align-items:center; }
.active-card.is-empty { background:var(--surface-2); border-style:dashed; color:var(--ink-soft); }
.active-card .ac-name { font-weight:600; font-size:.86rem; color:var(--ink);
  line-height:1.25; word-break:break-word; }
.active-card .ac-meta { font-size:.74rem; color:var(--ink-muted); margin-top:3px;
  font-variant-numeric:tabular-nums; }
.active-card .ac-pill { font-size:.66rem; padding:2px 7px; border-radius:99px;
  background:var(--accent-soft); color:var(--accent-ink); font-weight:600;
  letter-spacing:.03em; text-transform:uppercase; align-self:start; }
.active-card.is-empty .ac-pill { background:var(--surface-3); color:var(--ink-soft); }

.chips-wrap { display:flex; flex-wrap:wrap; gap:6px; }
.chip-btn { appearance:none; border:1px solid var(--line); background:var(--surface-1);
  color:var(--ink-2); font-family:var(--font-ui); font-size:.77rem; padding:5px 10px;
  border-radius:99px; cursor:pointer;
  transition:background .15s,border-color .15s,color .15s,transform .15s; }
.chip-btn:hover { background:var(--accent-soft); border-color:var(--accent-line); color:var(--accent-ink); }
.chip-btn:active { transform:scale(0.97); }

.action-stack { display:flex; flex-direction:column; gap:4px; }
.btn-quiet { font-family:var(--font-ui) !important; font-size:.79rem !important;
  font-weight:500 !important; color:var(--ink-2) !important; background:transparent !important;
  border:1px solid var(--line) !important; border-radius:8px !important;
  padding:7px 10px !important; text-align:left !important;
  transition:background .15s,color .15s,border-color .15s;
  display:flex; align-items:center; gap:8px; width:100%; }
.btn-quiet:hover { background:var(--accent-soft) !important; color:var(--accent-ink) !important;
  border-color:var(--accent-line) !important; }

.foundations { font-size:.74rem; color:var(--ink-soft); line-height:1.6;
  padding:10px 0 4px; border-top:1px solid var(--line); }
.foundations strong { color:var(--ink-2); font-weight:600; }
.foundations em { font-family:var(--font-serif); font-style:italic; color:var(--ink-2); font-size:1.05em; }

#chat-card { border:1px solid var(--line) !important; border-radius:var(--r-lg) !important;
  box-shadow:var(--shadow-2) !important; background:var(--surface-1) !important; overflow:hidden; }
.shiny-chat-container { background:var(--surface-1); }
.shiny-chat-container .chat-messages { padding:24px 28px !important; scroll-behavior:smooth; }

.chat-message { margin-bottom:16px !important; line-height:1.65 !important;
  font-size:.92rem !important; animation:msgIn 0.22s cubic-bezier(.21,.99,.39,1); }
@keyframes msgIn { from{opacity:0;transform:translateY(4px)} to{opacity:1;transform:translateY(0)} }
.chat-message[data-role='user'] .chat-message-body,
.chat-message.user .chat-message-body { background:var(--accent-soft) !important;
  border:1px solid var(--accent-line) !important; border-radius:16px 16px 4px 16px !important;
  padding:10px 15px !important; margin-left:22% !important; color:var(--accent-ink) !important;
  display:block; box-shadow:var(--shadow-1); }
.chat-message[data-role='assistant'] .chat-message-body,
.chat-message.assistant .chat-message-body { background:var(--surface-1) !important;
  border:1px solid var(--line) !important; border-radius:16px 16px 16px 4px !important;
  padding:12px 16px !important; margin-right:12% !important; display:block; box-shadow:var(--shadow-1); }
.chat-message img { max-width:100%; height:auto; border-radius:10px; border:1px solid var(--line);
  margin:8px 0; box-shadow:var(--shadow-1); background:var(--surface-1); }
.chat-message pre { background:oklch(0.18 0.02 286) !important; color:oklch(0.95 0.01 286) !important;
  border-radius:10px !important; padding:14px 16px !important; font-family:var(--font-mono) !important;
  font-size:.81rem !important; line-height:1.55 !important; overflow-x:auto !important;
  border:none !important; margin:10px 0 !important; }
.chat-message code { font-family:var(--font-mono) !important; font-size:.83rem !important;
  background:var(--surface-3) !important; color:var(--ink-2) !important;
  padding:1px 6px !important; border-radius:4px !important; }
.chat-message pre code { background:transparent !important; padding:0 !important; color:inherit !important; }
.chat-message h2 { font-size:1.1rem; font-weight:600; letter-spacing:-.01em; margin:14px 0 6px; }
.chat-message h3 { font-size:1rem; font-weight:600; color:var(--ink-2); margin:12px 0 5px; }
.chat-message blockquote { border-left:0; background:var(--accent-soft); border-radius:8px;
  padding:10px 14px; margin:10px 0; color:var(--accent-ink); font-style:italic;
  font-family:var(--font-serif); font-size:1.02rem; }
.shiny-chat-container .chat-user-input { border-top:1px solid var(--line) !important;
  padding:14px 18px !important; background:var(--surface-2) !important; }
.shiny-chat-container .chat-user-input textarea { font-family:var(--font-ui) !important;
  font-size:.92rem !important; border-radius:11px !important;
  border:1.5px solid var(--line-strong) !important; padding:10px 14px !important;
  resize:none !important; background:var(--surface-1) !important; color:var(--ink) !important;
  transition:border-color .15s,box-shadow .15s; }
.shiny-chat-container .chat-user-input textarea:focus { border-color:var(--accent) !important;
  box-shadow:0 0 0 3px var(--accent-soft) !important; outline:none !important; }
.shiny-chat-container .chat-user-input button,
.shiny-chat-container .chat-user-input .btn { background:var(--accent) !important;
  border:none !important; border-radius:9px !important; color:oklch(0.995 0.003 282) !important;
  font-weight:600 !important; transition:background .15s,transform .1s; }
.shiny-chat-container .chat-user-input button:hover { background:var(--accent-2) !important;
  transform:translateY(-1px); }

.bslib-value-box { border-radius:var(--r-md) !important; border:1px solid var(--line) !important;
  box-shadow:var(--shadow-1) !important; background:var(--surface-1) !important; }
table.dataTable { font-family:var(--font-ui) !important; font-size:.86rem !important; }
table.dataTable thead th { background:var(--surface-2) !important; font-weight:600 !important;
  color:var(--ink-muted) !important; font-size:.74rem !important; text-transform:uppercase !important;
  letter-spacing:.06em !important; border-bottom:1px solid var(--line) !important; padding:10px 12px !important; }
table.dataTable tbody td { border-color:var(--line) !important; color:var(--ink-2) !important; }

.col-card { margin-bottom:10px; padding:12px 14px; background:var(--surface-1);
  border:1px solid var(--line); border-radius:11px; transition:border-color .15s; }
.col-card:hover { border-color:var(--accent-line); }
.col-card .col-name { font-weight:600; color:var(--ink); font-size:.92rem;
  display:flex; align-items:center; gap:8px; }
.col-card .col-type-pill { font-size:.65rem; padding:2px 8px; border-radius:99px;
  background:var(--accent-soft); color:var(--accent-ink); font-weight:600;
  text-transform:uppercase; letter-spacing:.05em; }
.col-card .col-type-pill.cat { background:var(--positive-soft); color:oklch(0.30 0.14 155); }
.col-card .col-stats { margin-top:8px; font-family:var(--font-mono); font-size:.78rem;
  display:grid; grid-template-columns:repeat(auto-fit,minmax(110px,1fr)); gap:6px 14px; }
.col-card .col-stats .k { color:var(--ink-soft); font-size:.74rem; }
.col-card .col-stats .v { color:var(--ink-2); font-variant-numeric:tabular-nums; }

#plot_code_input { font-family:var(--font-mono) !important; font-size:.84rem !important;
  border-radius:11px !important; border:1.5px solid oklch(0.30 0.02 286) !important;
  background:oklch(0.16 0.02 286) !important; color:oklch(0.92 0.01 286) !important;
  padding:14px !important; line-height:1.55 !important; }
#plot_code_input:focus { border-color:var(--accent) !important;
  box-shadow:0 0 0 3px var(--accent-soft) !important; outline:none !important; }

.plot-toolbar { display:flex; gap:8px; align-items:center; padding:10px 14px;
  background:var(--surface-2); border:1px solid var(--line); border-radius:var(--r-md);
  margin-bottom:14px; flex-wrap:wrap; }
.plot-toolbar .toolbar-spacer { flex:1; min-width:8px; }
.plot-toolbar .toolbar-status { font-size:.82rem; color:var(--ink-muted); }

.btn-primary { background:var(--accent) !important; border-color:var(--accent) !important;
  font-weight:600 !important; border-radius:9px !important; font-size:.84rem !important; padding:7px 14px !important; }
.btn-primary:hover { background:var(--accent-2) !important; border-color:var(--accent-2) !important; }
.btn-ghost { background:var(--surface-1) !important; border:1px solid var(--line) !important;
  color:var(--ink-2) !important; font-weight:500 !important; border-radius:9px !important;
  font-size:.84rem !important; padding:7px 12px !important; }
.btn-ghost:hover { background:var(--accent-soft) !important; color:var(--accent-ink) !important;
  border-color:var(--accent-line) !important; }

.gallery-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(210px,1fr));
  gap:14px; padding:4px; }
.gallery-item { background:var(--surface-1); border:1px solid var(--line);
  border-radius:var(--r-md); overflow:hidden; display:flex; flex-direction:column;
  transition:transform .18s cubic-bezier(.21,.99,.39,1),border-color .18s,box-shadow .18s; }
.gallery-item:hover { border-color:var(--accent-line); transform:translateY(-2px); box-shadow:var(--shadow-2); }
.gallery-item img { width:100%; height:130px; object-fit:cover;
  background:var(--surface-3); display:block; cursor:pointer; }
.gallery-item .gi-meta { padding:8px 11px; font-size:.78rem; color:var(--ink-muted);
  display:flex; flex-direction:column; gap:2px; flex:1; }
.gallery-item .gi-cap { color:var(--ink); font-weight:500;
  white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.gallery-item .gi-source { font-size:.66rem; text-transform:uppercase; letter-spacing:.04em; color:var(--ink-soft); }
.gallery-item .gi-actions { display:flex; gap:3px; padding:6px 8px;
  border-top:1px solid var(--line); background:var(--surface-2); }
.gi-btn { flex:1; font-size:.72rem; padding:4px 2px; border-radius:6px;
  border:1px solid var(--line); background:var(--surface-1); color:var(--ink-2);
  cursor:pointer; text-align:center; font-family:var(--font-ui);
  transition:background .12s,color .12s; text-decoration:none; display:inline-block; }
.gi-btn:hover { background:var(--accent-soft); color:var(--accent-ink); border-color:var(--accent-line); }
.gi-btn.danger { color:var(--danger); }
.gi-btn.danger:hover { background:var(--danger-soft); border-color:oklch(0.80 0.10 25); }

.empty-state { text-align:center; padding:60px 24px; color:var(--ink-muted);
  background:var(--surface-1); border:1px dashed var(--line-strong); border-radius:var(--r-md); }
.empty-state .es-glyph { font-family:var(--font-serif); font-style:italic;
  font-size:2.5rem; color:var(--ink-soft); margin-bottom:10px; }
.empty-state .es-title { font-weight:600; color:var(--ink); font-size:1rem; margin-bottom:4px; }
.empty-state .es-body { font-size:.86rem; color:var(--ink-muted);
  max-width:380px; margin:0 auto; line-height:1.55; }

#shiny-notification-panel { bottom:20px !important; right:20px !important; top:auto !important; }
.shiny-notification { font-family:var(--font-ui) !important; border-radius:11px !important;
  box-shadow:var(--shadow-2) !important; font-size:.86rem !important;
  background:var(--surface-1) !important; color:var(--ink) !important; border:1px solid var(--line) !important; }
.shiny-notification-error   { border-left:3px solid var(--danger) !important; }
.shiny-notification-warning { border-left:3px solid var(--warn) !important; }
.shiny-notification-message { border-left:3px solid var(--positive) !important; }

::-webkit-scrollbar { width:6px; height:6px; }
::-webkit-scrollbar-track { background:transparent; }
::-webkit-scrollbar-thumb { background:var(--line-strong); border-radius:99px; }
::-webkit-scrollbar-thumb:hover { background:var(--ink-soft); }
.bi { vertical-align:-.125em; }
.modal-content { border-radius:var(--r-lg) !important; border:1px solid var(--line) !important; }
.modal-header  { border-bottom:1px solid var(--line) !important; }
.modal-footer  { border-top:1px solid var(--line) !important; }
"

app_js <- "
$(document).on('click', '.source-segmented .seg-btn', function() {
  var val = $(this).data('value');
  $(this).siblings('.seg-btn').removeClass('active');
  $(this).addClass('active');
  Shiny.setInputValue('source_mode', val, {priority:'event'});
});
$(document).on('click', '.chip-btn', function() {
  Shiny.setInputValue('chip_clicked',
    {prompt:$(this).data('prompt'), ts:Date.now()}, {priority:'event'});
});
"

# ============================================================================
# UI
# ============================================================================

ui <- page_fillable(
  theme = bs_theme(
    preset       = "shiny",
    primary      = "#5b3df5",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font    = font_google("JetBrains Mono"),
    bg           = "#f6f5f8",
    fg           = "#1f1d2b"
  ),
  useShinyjs(),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$script(HTML(app_js))
  ),
  
  navset_card_tab(
    id = "main_tabs",
    
    # ── ADVISOR ──────────────────────────────────────────────────────────────
    nav_panel("Advisor",
              layout_sidebar(
                fillable = TRUE, border = FALSE,
                sidebar = sidebar(
                  width = 320, padding = "16px", bg = NULL, open = TRUE,
                  
                  div(class="brand-strip",
                      div(class="brand-mark"),
                      div(class="brand-name","Viz Advisor"),
                      div(class="brand-tag","v3")),
                  
                  div(class="sb-section",
                      div(class="sb-label", div(class="sb-dot"), "Data source"),
                      div(class="source-segmented",
                          tags$button(class="seg-btn active",type="button",`data-value`="upload",
                                      bs_icon("cloud-arrow-up"),"Upload"),
                          tags$button(class="seg-btn",type="button",`data-value`="folder",
                                      bs_icon("folder"),"Folder"),
                          tags$button(class="seg-btn",type="button",`data-value`="global",
                                      bs_icon("hexagon"),"R env")),
                      uiOutput("source_panel")),
                  
                  div(class="sb-section",
                      div(class="sb-label",div(class="sb-dot"),"Active dataset"),
                      uiOutput("active_dataset_card")),
                  
                  div(class="sb-section",
                      div(class="sb-label",div(class="sb-dot"),"Try asking"),
                      uiOutput("smart_prompts")),
                  
                  div(class="sb-section",
                      div(class="sb-label",div(class="sb-dot"),"Session"),
                      div(class="action-stack",
                          actionButton("load_example",tagList(bs_icon("magic"),"Load example dataset"),
                                       class="btn-quiet"),
                          actionButton("reset_chat",tagList(bs_icon("arrow-counterclockwise"),"Reset conversation"),
                                       class="btn-quiet"),
                          downloadLink("export_chat", tagList(bs_icon("download"),"Export chat"),
                                         class="btn-quiet"))),
                  
                  div(class="foundations",
                      tags$em("Grounded in"),tags$br(),
                      tags$strong("Cleveland and McGill")," hierarchy. ",
                      tags$strong("Tufte"),"'s data-ink. ",
                      tags$strong("Few"),"'s clarity.")
                ),
                
                card(id="chat-card",fill=TRUE,padding=0,
                     chat_ui("chat",height="100%",fill=TRUE,
                             messages=list(
                               list(role="assistant", content=paste0(
                                 "**Viz Advisor**\n\n",
                                 "The right chart for your data, rendered instantly.\n\n",
                                 "Load a dataset from the sidebar and tell me what you want to explore. ",
                                 "I will pick the right chart, explain why, and render it right here — ",
                                 "every recommendation grounded in Cleveland and McGill, Tufte, and Few.\n\n",
                                 "_No data yet? Click **Load example dataset** in the sidebar._"
                               ))
                             )
                     ))
              )
    ),
    
    # ── DATA ─────────────────────────────────────────────────────────────────
    nav_panel("Data",
              div(style="padding:20px;overflow-y:auto;height:100%;",
                  uiOutput("data_stats_strip"),
                  div(style="height:14px;"),
                  layout_columns(
                    col_widths=c(12,6,6),
                    card(full_screen=TRUE,
                         card_header("Preview",class="fw-semibold"),
                         DTOutput("data_preview_table")),
                    card(card_header(tagList(bs_icon("123")," Numeric columns"),class="fw-semibold"),
                         div(style="overflow-y:auto;max-height:480px;padding:6px 12px 12px;",
                             uiOutput("numeric_cols_info"))),
                    card(card_header(tagList(bs_icon("tag")," Categorical columns"),class="fw-semibold"),
                         div(style="overflow-y:auto;max-height:480px;padding:6px 12px 12px;",
                             uiOutput("categorical_cols_info")))
                  ))
    ),
    
    # ── PLOT LAB ─────────────────────────────────────────────────────────────
    nav_panel("Plot Lab",
              div(style="padding:20px;overflow-y:auto;height:100%;",
                  div(class="plot-toolbar",
                      div(class="toolbar-status",uiOutput("plot_lab_status",inline=TRUE)),
                      div(class="toolbar-spacer"),
                      uiOutput("lab_selector_ui"),
                      actionButton("render_plot_btn",tagList(bs_icon("play-fill")," Render"),
                                   class="btn btn-primary"),
                      actionButton("save_as_new_btn",tagList(bs_icon("plus-circle")," Save as New"),
                                   class="btn btn-ghost"),
                      actionButton("overwrite_btn",tagList(bs_icon("arrow-repeat")," Overwrite"),
                                   class="btn btn-ghost"),
                      downloadButton("download_plot",tagList(bs_icon("download")," PNG"),
                                     class="btn btn-ghost")),
                  
                  layout_columns(
                    col_widths=c(5,7),
                    card(card_header("Code",class="fw-semibold"),
                         div(style="padding:12px;",
                             textAreaInput("plot_code_input",label=NULL,
                                           value="# Paste or write ggplot2 code\n\nggplot(uploaded_data, aes(x=..., y=...)) +\n  geom_col() +\n  theme_minimal()",
                                           rows=16,width="100%",resize="vertical"))),
                    card(card_header("Output",class="fw-semibold"),
                         div(style="padding:16px;",
                             withSpinner(plotOutput("rendered_plot",height="440px"),
                                         type=8,color="#5b3df5",size=0.5)))
                  )
              )
    ),
    
    # ── GALLERY ──────────────────────────────────────────────────────────────
    nav_panel("Gallery",
              div(style="padding:20px;overflow-y:auto;height:100%;",
                  div(style="display:flex;align-items:center;gap:12px;margin-bottom:16px;",
                      h5(style="margin:0;font-weight:600;","Plot Gallery"),
                      uiOutput("gallery_count_badge"),
                      div(style="margin-left:auto;",
                          actionButton("clear_gallery",tagList(bs_icon("trash")," Clear all"),
                                       class="btn btn-ghost btn-sm"))),
                  uiOutput("full_gallery"))
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data_summary  = NULL,
    chat_client   = NULL,
    chat_history  = list(),
    gallery       = list(),
    source_mode   = "upload",
    folder_path   = getwd(),
    lab_plot      = NULL,
    lab_sel_id    = NULL,
    modal_view_id = NULL,
    busy          = FALSE
  )
  
  guard <- function() {
    if (isolate(rv$busy)) {
      showNotification("Please wait for the current operation to finish.",
                       type="warning", duration=2)
      return(FALSE)
    }
    TRUE
  }
  
  # ── Source mode ───────────────────────────────────────────────────────────
  observeEvent(input$source_mode, { rv$source_mode <- input$source_mode })
  
  output$source_panel <- renderUI({
    mode <- rv$source_mode %||% "upload"
    if (mode == "upload") {
      div(class="src-panel",
          fileInput("data_file",NULL,accept=c(".csv",".tsv","text/csv"),
                    buttonLabel="Choose file",placeholder="No file selected",width="100%"))
    } else if (mode == "folder") {
      files <- scan_working_dir(rv$folder_path)
      div(class="src-panel",
          textInput("folder_path_input",NULL,value=rv$folder_path,width="100%"),
          if (length(files)) tagList(
            selectInput("folder_file",NULL,choices=files,width="100%"),
            div(class="src-row",
                actionButton("load_folder_file","Load",class="btn btn-primary btn-sm"),
                actionButton("rescan_folder",bs_icon("arrow-clockwise"),
                             class="btn btn-ghost btn-sm",title="Rescan"))
          ) else tagList(
            div(style="font-size:.79rem;color:var(--ink-soft);margin-top:8px;","No CSV/TSV files found."),
            div(class="src-row",
                actionButton("rescan_folder",tagList(bs_icon("arrow-clockwise")," Rescan"),
                             class="btn btn-ghost btn-sm"))
          ))
    } else {
      objs <- scan_global_env()
      div(class="src-panel",
          if (length(objs)) tagList(
            selectInput("global_obj",NULL,choices=objs,width="100%"),
            div(class="src-row",
                actionButton("load_global_obj","Use this",class="btn btn-primary btn-sm"),
                actionButton("rescan_global",bs_icon("arrow-clockwise"),
                             class="btn btn-ghost btn-sm",title="Rescan"))
          ) else tagList(
            div(style="font-size:.79rem;color:var(--ink-soft);margin-top:4px;",
                "No data frames in global environment."),
            div(class="src-row",
                actionButton("rescan_global",tagList(bs_icon("arrow-clockwise")," Rescan"),
                             class="btn btn-ghost btn-sm"))
          ))
    }
  })
  
  observeEvent(input$rescan_folder, {
    if (nzchar(input$folder_path_input %||% "")) rv$folder_path <- input$folder_path_input
    cur <- rv$source_mode; rv$source_mode <- "__"; rv$source_mode <- cur
    showNotification("Folder rescanned.",type="message",duration=2)
  })
  observeEvent(input$rescan_global, {
    cur <- rv$source_mode; rv$source_mode <- "__"; rv$source_mode <- cur
    showNotification("Global environment rescanned.",type="message",duration=2)
  })
  
  # ── Activate dataset ──────────────────────────────────────────────────────
  activate_dataset <- function(df, label, source) {
    assign("uploaded_data", df, envir = .GlobalEnv)
    rv$data_summary <- summarize_data(df, label, source)
    rv$chat_client  <- build_chat_client(rv, rv$data_summary)
    
    if (is.null(rv$chat_client)) {
      showNotification("API key missing or invalid. Add a valid key to api-key.txt and restart.", type="error", duration=NULL)
      return()
    }
    
    n_num <- length(rv$data_summary$numeric_cols)
    n_cat <- length(rv$data_summary$categorical_cols)
    warn  <- if (nrow(df) < 2)
      "\n\n_Note: very few rows - some chart types may not render meaningfully._"
    else if (n_num == 0)
      "\n\n_Note: no numeric columns detected. I can help with categorical patterns._"
    else ""
    
    # ASCII-only to avoid JSON encoding issues on servers with non-UTF-8 locale
    msg <- paste0(
      "**", rv$data_summary$label, "** is now active.\n",
      format(rv$data_summary$n_rows, big.mark=","), " rows x ",
      rv$data_summary$n_cols, " columns - ",
      n_num, " numeric, ", n_cat, " categorical.", warn, "\n\n",
      "Tell me what you want to see, or pick a suggestion on the left."
    )
    rv$chat_history <- append(rv$chat_history, list(list(role="assistant", content=msg)))
    # Removed chat_append("chat", msg) here to completely eliminate the [object Object] JS error on upload.
    # The active dataset details are already clearly visible in the sidebar UI.
  }
  
  observeEvent(input$data_file, {
    req(input$data_file)
    if (!guard()) return()
    rv$busy <- TRUE
    on.exit(rv$busy <- FALSE)
    tryCatch({
      df <- read_tabular(input$data_file$datapath)
      activate_dataset(df, input$data_file$name, "upload")
      updateTabsetPanel(session,"main_tabs",selected="Data")
      showNotification("Data loaded.",type="message",duration=3)
    }, error = function(e)
      showNotification(paste("Read error:",conditionMessage(e)),type="error",duration=8))
  })
  
  observeEvent(input$load_folder_file, {
    req(input$folder_file)
    tryCatch({
      df <- read_tabular(input$folder_file)
      activate_dataset(df, basename(input$folder_file), "folder")
      updateTabsetPanel(session,"main_tabs",selected="Data")
      showNotification(paste("Loaded:",basename(input$folder_file)),type="message",duration=3)
    }, error = function(e)
      showNotification(paste("Read error:",conditionMessage(e)),type="error",duration=8))
  })
  
  observeEvent(input$load_global_obj, {
    req(input$global_obj)
    tryCatch({
      df <- get(input$global_obj, envir=.GlobalEnv)
      if (!is.data.frame(df)) stop("Selected object is not a data frame.")
      activate_dataset(df, input$global_obj, "global")
      updateTabsetPanel(session,"main_tabs",selected="Data")
      showNotification(paste("Using:",input$global_obj),type="message",duration=3)
    }, error = function(e)
      showNotification(paste("Error:",conditionMessage(e)),type="error",duration=8))
  })
  
  # ── Active dataset card ───────────────────────────────────────────────────
  output$active_dataset_card <- renderUI({
    if (is.null(rv$data_summary)) {
      div(class="active-card is-empty",
          div(div(class="ac-name","No dataset"),div(class="ac-meta","Pick a source above")),
          div(class="ac-pill","Idle"))
    } else {
      s    <- rv$data_summary
      pill <- switch(s$source,upload="Upload",folder="Folder",global="R env")
      div(class="active-card",
          div(div(class="ac-name",s$label),
              div(class="ac-meta",
                  format(s$n_rows,big.mark=",")," × ",s$n_cols,
                  "  ·  ",length(s$numeric_cols)," num, ",length(s$categorical_cols)," cat")),
          div(class="ac-pill",pill))
    }
  })
  
  # ── Smart prompts ─────────────────────────────────────────────────────────
  output$smart_prompts <- renderUI({
    s     <- rv$data_summary
    chips <- if (is.null(s)) {
      c("How do I compare two groups?",
        "When should I use a boxplot?",
        "Which chart for time series?")
    } else {
      out <- character(0)
      if (length(s$numeric_cols) >= 2)
        out <- c(out, sprintf("Relationship between %s and %s",
                              s$numeric_cols[1], s$numeric_cols[2]))
      if (length(s$numeric_cols) && length(s$categorical_cols))
        out <- c(out, sprintf("Compare %s across %s",
                              s$numeric_cols[1], s$categorical_cols[1]))
      if (length(s$numeric_cols))
        out <- c(out, sprintf("Distribution of %s", s$numeric_cols[1]))
      if (length(s$categorical_cols))
        out <- c(out, sprintf("Counts by %s", s$categorical_cols[1]))
      if (!length(out)) out <- "Suggest a chart for this data"
      head(out, 4)
    }
    div(class="chips-wrap",
        lapply(chips, function(p)
          tags$button(type="button",class="chip-btn",`data-prompt`=p, p)))
  })
  
  observeEvent(input$chip_clicked, {
    req(rv$chat_client)
    msg <- input$chip_clicked$prompt
    if (is.null(msg) || !nzchar(msg)) return()
    rv$chat_history <- append(rv$chat_history, list(list(role="user", content=msg)))
    tryCatch({
      stream <- rv$chat_client$stream_async(msg)
      chat_append("chat", stream)
    }, error = function(e) {
      showNotification(paste("AI error:", conditionMessage(e)), type="error", duration=8)
    })
  })
  
  # ── Init chat on first connect ────────────────────────────────────────────
  # ignoreNULL=FALSE ensures the observer fires even when url_protocol starts
  # as NULL on Shiny Server. onFlushed delays the message until after Shiny
  # has finished the initial UI render so shinychat's JS is ready to receive it.
  observe({
    if (!API_KEY_OK) {
      rv$chat_history <- list(list(role="assistant", content="API key not found."))
      session$onFlushed(function() {
        chat_append("chat", paste0(
          "**API key not found.**\n\n",
          "Add your Anthropic key to `api-key.txt` in the app directory and restart."))
      }, once=TRUE)
      return()
    }
    rv$chat_client <- build_chat_client(rv, NULL)
    welcome <- paste0(
      "**Viz Advisor**\n\n",
      "The right chart for your data, rendered instantly.\n\n",
      "Load a dataset from the sidebar and tell me what you want to explore. ",
      "I will pick the right chart, explain why, and render it right here - ",
      "every recommendation grounded in Cleveland and McGill, Tufte, and Few.\n\n",
      "_No data yet? Click **Load example dataset** in the sidebar._"
    )
    rv$chat_history <- list(list(role="assistant", content=welcome))
  }) |> bindEvent(session$clientData$url_protocol, once=TRUE, ignoreNULL=FALSE)
  
  # ── Chat messages ─────────────────────────────────────────────────────────
  observeEvent(input$chat_user_input, {
    req(input$chat_user_input)
    if (is.null(rv$chat_client)) {
      showNotification("No AI client available. Check the API key and reload.", type="error")
      return()
    }
    msg <- input$chat_user_input
    rv$chat_history <- append(rv$chat_history, list(list(role="user",content=msg)))
    tryCatch({
      stream <- rv$chat_client$stream_async(msg)
      chat_append("chat", stream)
    }, error = function(e) {
      err <- conditionMessage(e)
      showNotification(paste("AI error:",err),type="error",duration=8)
    })
  })
  
  # ── Reset ─────────────────────────────────────────────────────────────────
  observeEvent(input$reset_chat, {
    rv$data_summary  <- NULL
    rv$chat_history  <- list()
    rv$gallery       <- list()
    rv$lab_plot      <- NULL
    rv$lab_sel_id    <- NULL
    if (exists("uploaded_data",envir=.GlobalEnv)) rm("uploaded_data",envir=.GlobalEnv)
    tryCatch(reset("data_file"),error=function(e) NULL,warning=function(w) NULL)
    rv$chat_client <- build_chat_client(rv, NULL)
    welcome <- "**Viz Advisor.**\n\nFresh start. Load a dataset from the sidebar, then tell me what you want to see."
    chat_clear("chat")
    rv$chat_history <- list(list(role="assistant",content=welcome))
    showNotification("Conversation reset.",type="message",duration=3)
  })
  
  # ── Example dataset ───────────────────────────────────────────────────────
  observeEvent(input$load_example, {
    if (!guard()) return()
    set.seed(42)
    df <- data.frame(
      Region       = rep(c("North","South","East","West","Central"), each=50),
      Product      = sample(c("Alpha","Beta","Gamma"), 250, replace=TRUE),
      Sales        = round(rnorm(250, 50000, 15000), 2),
      Units        = rpois(250, 100),
      Satisfaction = sample(1:5, 250, replace=TRUE),
      Date         = seq(as.Date("2024-01-01"), by="1 day", length.out=250)
    )
    activate_dataset(df, "example_sales.csv", "upload")
    updateTabsetPanel(session,"main_tabs",selected="Data")
    showNotification("Example dataset loaded.",type="message",duration=3)
  })
  
  # ── Export chat ───────────────────────────────────────────────────────────
  output$export_chat <- downloadHandler(
    filename = function() paste0("viz_advisor_",Sys.Date(),".txt"),
    content  = function(file) {
      lines <- paste0("Viz Advisor export\nDate: ",Sys.time(),"\n",strrep("=",72),"\n\n")
      for (m in rv$chat_history) {
        role  <- if (m$role == "user") "YOU" else "ADVISOR"
        lines <- paste0(lines,"[",role,"]\n",m$content,"\n\n",strrep("-",72),"\n\n")
      }
      writeLines(lines, file)
    }
  )
  
  # ── DATA TAB ──────────────────────────────────────────────────────────────
  output$data_stats_strip <- renderUI({
    if (is.null(rv$data_summary)) {
      div(class="empty-state",
          div(class="es-glyph","no data"),
          div(class="es-title","Nothing loaded yet"),
          div(class="es-body","Pick a CSV from upload, your working folder, or your R session."))
    } else {
      s <- rv$data_summary
      layout_columns(
        col_widths=c(3,3,3,3),
        value_box("Rows",   format(s$n_rows,big.mark=","),
                  showcase=bs_icon("rows"),theme="primary"),
        value_box("Columns",s$n_cols,
                  showcase=bs_icon("layout-three-columns"),theme="info"),
        value_box("Numeric",length(s$numeric_cols),
                  showcase=bs_icon("123"),theme="success"),
        value_box("Missing",paste0(s$missing_pct,"%"),
                  showcase=bs_icon("dash-circle"),
                  theme=if (s$missing_pct > 5) "warning" else "secondary")
      )
    }
  })
  
  output$data_preview_table <- renderDT({
    if (is.null(rv$data_summary))
      return(datatable(data.frame(Note="Load a dataset to preview."),
                       options=list(dom="t"),rownames=FALSE))
    tryCatch({
      df_raw  <- get_uploaded_data()
      if (is.null(df_raw)) stop("Data not available.")
      df_safe <- sanitize_for_dt(head(df_raw, 100))
      names(df_safe) <- names(df_raw)[seq_len(ncol(df_safe))]
      datatable(df_safe,
                options=list(pageLength=10,scrollX=TRUE,dom="frtip",autoWidth=FALSE),
                class="cell-border stripe hover compact",rownames=FALSE)
    }, error = function(e)
      datatable(data.frame(Error=paste("Preview unavailable:",conditionMessage(e))),
                options=list(dom="t"),rownames=FALSE))
  })
  
  output$numeric_cols_info <- renderUI({
    s  <- rv$data_summary
    df <- get_uploaded_data()
    if (is.null(s) || is.null(df) || !length(s$numeric_cols))
      return(div(style="color:var(--ink-soft);padding:14px;font-size:.86rem;","No numeric columns."))
    lapply(s$numeric_cols, function(col) {
      d <- df[[col]]
      if (is.null(d)) return(NULL)
      div(class="col-card",
          div(class="col-name",col,tags$span(class="col-type-pill","numeric")),
          div(class="col-stats",
              div(div(class="k","min"),    div(class="v",round(min(d,   na.rm=TRUE),3))),
              div(div(class="k","max"),    div(class="v",round(max(d,   na.rm=TRUE),3))),
              div(div(class="k","mean"),   div(class="v",round(mean(d,  na.rm=TRUE),3))),
              div(div(class="k","median"), div(class="v",round(median(d,na.rm=TRUE),3))),
              div(div(class="k","sd"),     div(class="v",round(sd(d,    na.rm=TRUE),3))),
              div(div(class="k","NA"),     div(class="v",sum(is.na(d))))))
    })
  })
  
  output$categorical_cols_info <- renderUI({
    s  <- rv$data_summary
    df <- get_uploaded_data()
    if (is.null(s) || is.null(df) || !length(s$categorical_cols))
      return(div(style="color:var(--ink-soft);padding:14px;font-size:.86rem;","No categorical columns."))
    lapply(s$categorical_cols, function(col) {
      d        <- df[[col]]
      if (is.null(d)) return(NULL)
      n_unique <- length(unique(d))
      top_vals <- head(sort(table(d),decreasing=TRUE),4)
      div(class="col-card",
          div(class="col-name",col,tags$span(class="col-type-pill cat","categorical")),
          div(class="col-stats",
              div(div(class="k","unique"), div(class="v",n_unique)),
              div(div(class="k","NA"),     div(class="v",sum(is.na(d))))),
          if (length(top_vals))
            div(style="margin-top:8px;font-size:.78rem;color:var(--ink-muted);",
                "Top: ",paste0(names(top_vals)," (",format(top_vals,big.mark=","),")",collapse=", ")))
    })
  })
  
  # ── PLOT LAB ──────────────────────────────────────────────────────────────
  output$plot_lab_status <- renderUI({
    if (is.null(rv$data_summary))
      tagList(bs_icon("info-circle")," No active dataset.")
    else
      tagList(bs_icon("check-circle-fill",class="text-success"),
              " Active: ",tags$strong(rv$data_summary$label),
              "  ·  ",format(rv$data_summary$n_rows,big.mark=",")," rows")
  })
  
  output$lab_selector_ui <- renderUI({
    g <- rv$gallery
    if (!length(g)) return(NULL)
    choices <- c("(new)"="",
                 setNames(vapply(g,`[[`,character(1),"id"),
                          vapply(g,function(x)
                            paste0(x$caption,"  [",format(x$time,"%H:%M"),"]"),character(1))))
    div(style="min-width:200px;",
        selectInput("lab_gallery_select",NULL,
                    choices=choices,selected=rv$lab_sel_id %||% "",width="100%"))
  })
  
  observeEvent(input$lab_gallery_select, {
    id <- input$lab_gallery_select
    if (is.null(id) || !nzchar(id)) { rv$lab_sel_id <- NULL; return() }
    rv$lab_sel_id <- id
    entry <- Filter(function(x) x$id == id, rv$gallery)
    if (length(entry)) updateTextAreaInput(session,"plot_code_input",value=entry[[1]]$code)
  })
  
  render_lab_code <- function(code) {
    if (is.null(code) || nchar(trimws(code)) < 5) {
      showNotification("Add some code first.",type="warning",duration=3)
      return(NULL)
    }
    if (is.null(get_uploaded_data()) && grepl("uploaded_data", code)) {
      showNotification("No dataset loaded — upload one first.",type="warning",duration=4)
      return(NULL)
    }
    tryCatch({
      env <- new.env(parent=.GlobalEnv)
      val <- eval(parse(text=code), envir=env)
      p   <- if (inherits(val,"ggplot")) val else {
        ggs <- Filter(function(x) inherits(x,"ggplot"), as.list(env))
        if (length(ggs)) ggs[[length(ggs)]] else NULL
      }
      if (is.null(p)) showNotification("Code did not produce a ggplot.",type="error",duration=6)
      p
    }, error = function(e) {
      showNotification(paste("Plot error:",conditionMessage(e)),type="error",duration=8)
      NULL
    })
  }
  
  save_to_gallery <- function(p, code, caption, source="lab") {
    pid   <- sprintf("%s%s_%05d",source,format(Sys.time(),"%H%M%S"),sample.int(99999,1))
    pfile <- file.path(PLOT_ROOT,paste0(pid,".png"))
    ggsave(pfile,plot=p,width=9,height=5.4,dpi=110,bg="white")
    entry <- list(id=pid,file=pfile,caption=caption,code=code,time=Sys.time(),source=source)
    rv$gallery <- c(rv$gallery,list(entry))
    showNotification(paste0('✓ "',caption,'" added to gallery.'),type="message",duration=3)
    pid
  }
  
  lab_plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$render_plot_btn, {
    lab_plot_obj(render_lab_code(input$plot_code_input))
  })
  
  observeEvent(input$save_as_new_btn, {
    p <- render_lab_code(input$plot_code_input)
    if (is.null(p)) return()
    lab_plot_obj(p)
    new_id <- save_to_gallery(p, input$plot_code_input,
                              paste("Lab plot",format(Sys.time(),"%H:%M")))
    rv$lab_sel_id <- new_id
    updateTabsetPanel(session,"main_tabs",selected="Gallery")
  })
  
  observeEvent(input$overwrite_btn, {
    id <- rv$lab_sel_id
    if (is.null(id)) {
      showNotification("Select a gallery entry to overwrite first.",type="warning",duration=3)
      return()
    }
    p <- render_lab_code(input$plot_code_input)
    if (is.null(p)) return()
    lab_plot_obj(p)
    idx <- which(vapply(rv$gallery,function(x) x$id==id,logical(1)))
    if (!length(idx)) return()
    ggsave(rv$gallery[[idx]]$file,plot=p,width=9,height=5.4,dpi=110,bg="white")
    rv$gallery[[idx]]$code <- input$plot_code_input
    rv$gallery[[idx]]$time <- Sys.time()
    showNotification("Gallery entry overwritten.",type="message",duration=3)
  })
  
  output$rendered_plot <- renderPlot({
    p <- lab_plot_obj()
    if (is.null(p))
      ggplot() +
      annotate("text",x=0,y=0,label="Click Render to draw your plot",
               size=4.5,color="#7a7a8a",family="sans") +
      theme_void() +
      theme(panel.background=element_rect(fill="white",color=NA),
            plot.background=element_rect(fill="white",color=NA))
    else p
  }, res=100, bg="white")
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_",format(Sys.time(),"%Y%m%d_%H%M%S"),".png"),
    content  = function(file) {
      p <- lab_plot_obj()
      if (is.null(p)) { showNotification("Render a plot first.",type="warning"); return() }
      ggsave(file,plot=p,width=9,height=5.5,dpi=150,bg="white")
    }
  )
  
  # ── GALLERY TAB ───────────────────────────────────────────────────────────
  output$gallery_count_badge <- renderUI({
    n <- length(rv$gallery)
    if (!n) return(NULL)
    tags$span(style=paste0("font-size:.72rem;padding:2px 9px;border-radius:99px;",
                           "background:var(--accent-soft);color:var(--accent-ink);font-weight:600;"),
              n, if (n==1) " plot" else " plots")
  })
  
  output$full_gallery <- renderUI({
    g <- rv$gallery
    if (!length(g)) {
      return(div(class="empty-state",
                 div(class="es-glyph","gallery"),
                 div(class="es-title","No plots yet"),
                 div(class="es-body",
                     "Chat with the advisor and ask for a chart. ",
                     "Every plot it renders lands here automatically.")))
    }
    div(class="gallery-grid",
        lapply(rev(g), function(item) {
          url <- paste0("vaplots/",item$id,".png")
          div(class="gallery-item",
              tags$img(src=url, alt=item$caption, title="Click to view full size",
                       onclick=sprintf(
                         "Shiny.setInputValue('gallery_view',{id:'%s',ts:Date.now()},{priority:'event'})",
                         item$id)),
              div(class="gi-meta",
                  div(class="gi-cap",item$caption),
                  div(format(item$time,"%H:%M:%S"),
                      tags$span(class="gi-source",item$source %||% "chat"))),
              div(class="gi-actions",
                  tags$button(class="gi-btn",title="View full size",
                              onclick=sprintf(
                                "Shiny.setInputValue('gallery_view',{id:'%s',ts:Date.now()},{priority:'event'})",
                                item$id),
                              bs_icon("arrows-fullscreen")," View"),
                  tags$button(class="gi-btn",title="Edit in Plot Lab",
                              onclick=sprintf(
                                "Shiny.setInputValue('gallery_edit',{id:'%s',ts:Date.now()},{priority:'event'})",
                                item$id),
                              bs_icon("pencil")," Edit"),
                  tags$a(class="gi-btn",href=url,download=paste0(item$id,".png"),
                         title="Download PNG",bs_icon("download")," Save"),
                  tags$button(class="gi-btn danger",title="Delete",
                              onclick=sprintf(
                                "Shiny.setInputValue('gallery_delete',{id:'%s',ts:Date.now()},{priority:'event'})",
                                item$id),
                              bs_icon("trash")))
          )
        }))
  })
  
  # View full-size in modal
  observeEvent(input$gallery_view, {
    id    <- input$gallery_view$id
    entry <- Filter(function(x) x$id==id, rv$gallery)
    if (!length(entry)) return()
    e   <- entry[[1]]
    url <- paste0("vaplots/",e$id,".png")
    rv$modal_view_id <- id
    showModal(modalDialog(
      title     = e$caption,
      tags$img(src=url,style="width:100%;border-radius:8px;border:1px solid var(--line);"),
      div(style="margin-top:12px;",
          tags$pre(style=paste0("font-size:.78rem;background:#1a1a2e;color:#e8e8f0;",
                                "border-radius:8px;padding:12px;overflow-x:auto;"),
                   e$code)),
      footer    = tagList(
        modalButton("Close"),
        actionButton("modal_edit_in_lab",tagList(bs_icon("pencil")," Edit in Plot Lab"),
                     class="btn btn-primary")
      ),
      size      = "xl",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$modal_edit_in_lab, {
    removeModal()
    id <- rv$modal_view_id
    if (is.null(id)) return()
    entry <- Filter(function(x) x$id==id, rv$gallery)
    if (!length(entry)) return()
    rv$lab_sel_id <- id
    updateTextAreaInput(session,"plot_code_input",value=entry[[1]]$code)
    updateTabsetPanel(session,"main_tabs",selected="Plot Lab")
  })
  
  observeEvent(input$gallery_edit, {
    id <- input$gallery_edit$id
    entry <- Filter(function(x) x$id==id, rv$gallery)
    if (!length(entry)) return()
    rv$lab_sel_id <- id
    updateTextAreaInput(session,"plot_code_input",value=entry[[1]]$code)
    updateTabsetPanel(session,"main_tabs",selected="Plot Lab")
    showNotification("Code loaded into Plot Lab.",type="message",duration=2)
  })
  
  observeEvent(input$gallery_delete, {
    id         <- input$gallery_delete$id
    rv$gallery <- Filter(function(x) x$id!=id, rv$gallery)
    if (!is.null(rv$lab_sel_id) && rv$lab_sel_id==id) rv$lab_sel_id <- NULL
    showNotification("Plot removed from gallery.",type="message",duration=2)
  })
  
  observeEvent(input$clear_gallery, {
    rv$gallery    <- list()
    rv$lab_sel_id <- NULL
    showNotification("Gallery cleared.",type="message",duration=2)
  })
  
  session$onSessionEnded(function() invisible(NULL))
}

shinyApp(ui=ui, server=server)
