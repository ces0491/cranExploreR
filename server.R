library(shiny)
library(bslib)
library(plotly)
library(DT)

server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    selected_pkg = NULL,
    metadata = NULL,
    versions = NULL,
    downloads_daily = NULL,
    download_totals = NULL,
    lifetime_downloads = NA_real_,
    rev_deps = NULL,
    health = NULL,
    search_results = NULL,
    browse_results = NULL,
    browse_label = NULL,
    compare_data = NULL,
    compare_search_results = NULL
  )

  # --- Search ---

  observeEvent(input$search_btn, {
    req(nchar(trimws(input$search_query)) > 0)
    query <- trimws(input$search_query)

    rv$search_results <- NULL
    rv$selected_pkg <- NULL

    withProgress(message = "Searching CRAN...", {
      rv$search_results <- search_packages(query)
    })
  })

  output$search_results_ui <- renderUI({
    results <- rv$search_results
    if (is.null(results) || nrow(results) == 0) {
      if (!is.null(rv$search_results)) {
        return(p(
          "No packages found.",
          class = "text-muted"
        ))
      }
      return(NULL)
    }

    tags$div(
      class = "list-group",
      lapply(seq_len(nrow(results)), function(i) {
        pkg <- results$package[i]
        tags$div(
          class = paste(
            "list-group-item",
            "list-group-item-action p-2"
          ),
          style = "cursor: pointer;",
          onclick = sprintf(
            paste0(
              "Shiny.setInputValue('select_pkg',",
              " '%s', {priority: 'event'})"
            ),
            pkg
          ),
          tags$strong(pkg),
          tags$span(
            paste0(" v", results$version[i]),
            class = "text-muted small"
          ),
          tags$br(),
          tags$small(
            results$title[i],
            class = "text-muted"
          )
        )
      })
    )
  })

  observeEvent(input$select_pkg, {
    load_package(input$select_pkg)
  })

  # Clear everything the Explorer tab renders, so a load that fails
  # part way through cannot leave the previous package's chart, stats
  # and score on screen under a blank header.
  reset_package_state <- function() {
    rv$selected_pkg <- NULL
    rv$metadata <- NULL
    rv$versions <- NULL
    rv$downloads_daily <- NULL
    rv$download_totals <- NULL
    rv$lifetime_downloads <- NA_real_
    rv$rev_deps <- NULL
    rv$health <- NULL
  }

  # Core function to load all package data
  load_package <- function(pkg_name) {
    reset_package_state()

    withProgress(
      message = paste("Loading", pkg_name, "..."),
      {
        incProgress(0.1, detail = "Fetching metadata")
        rv$metadata <- fetch_package_metadata(pkg_name)

        if (is.null(rv$metadata)) {
          showNotification(
            paste("Package", pkg_name, "not found."),
            type = "error"
          )
          return()
        }

        rv$selected_pkg <- pkg_name

        incProgress(
          0.15, detail = "Fetching version history"
        )
        rv$versions <- fetch_package_versions(pkg_name)

        # crandb can lag CRAN by days; where they disagree
        # take the current release straight from CRAN
        reconciled <- reconcile_with_cran(
          rv$metadata, rv$versions,
          fetch_cran_description(pkg_name)
        )
        if (reconciled$stale) {
          showNotification(
            paste0(
              "The crandb index is behind CRAN. Showing v",
              reconciled$metadata$Version,
              " from CRAN; download history may lag too."
            ),
            type = "message",
            duration = 8
          )
        }
        rv$metadata <- reconciled$metadata
        rv$versions <- reconciled$versions

        incProgress(
          0.15, detail = "Fetching daily downloads"
        )
        rv$downloads_daily <- fetch_daily_downloads(
          pkg_name
        )

        # The daily series covers the same 365 days as the
        # yearly total, so hand it over instead of asking
        # cranlogs for a number we already have.
        incProgress(
          0.15, detail = "Fetching download stats"
        )
        rv$download_totals <- fetch_download_totals(
          pkg_name, rv$downloads_daily
        )

        incProgress(
          0.15, detail = "Fetching lifetime downloads"
        )
        rv$lifetime_downloads <- NA_real_
        if (!is.null(rv$versions$timeline)) {
          first_pub <- min(as.Date(
            substr(
              unlist(rv$versions$timeline), 1, 10
            )
          ), na.rm = TRUE)
          rv$lifetime_downloads <-
            fetch_lifetime_downloads(
              pkg_name, first_pub
            )
        }

        incProgress(
          0.15, detail = "Fetching reverse dependencies"
        )
        rv$rev_deps <- fetch_reverse_deps(pkg_name)

        # Notify on partial failures. Anything listed here is a
        # factor the viability score was computed without, so the
        # score is normalised over what did arrive.
        failures <- c()
        if (is.null(rv$versions)) {
          failures <- c(failures, "version history")
        }
        if (is.null(rv$downloads_daily)) {
          failures <- c(failures, "daily downloads")
        }
        if (is.na(rv$download_totals$last_month)) {
          failures <- c(failures, "download totals")
        }
        if (is.null(rv$rev_deps)) {
          failures <- c(failures, "reverse dependencies")
        }
        if (length(failures) > 0) {
          showNotification(
            paste(
              "Could not load:",
              paste(failures, collapse = ", ")
            ),
            type = "warning",
            duration = 5
          )
        }

        incProgress(
          0.15, detail = "Calculating health score"
        )
        rv$health <- calculate_health_score(
          rv$metadata, rv$versions,
          rv$download_totals, rv$rev_deps,
          rv$downloads_daily
        )
      }
    )
  }

  # --- Outputs ---

  # Flag for conditional panels
  output$package_loaded <- reactive({
    !is.null(rv$selected_pkg)
  })
  outputOptions(
    output, "package_loaded",
    suspendWhenHidden = FALSE
  )

  # Package header
  output$package_header <- renderUI({
    req(rv$metadata)
    meta <- rv$metadata

    homepage_link <- NULL
    if (!is.null(meta$URL) && nchar(meta$URL) > 0) {
      first_url <- strsplit(meta$URL, "[,\n]+")[[1]][1]
      homepage_link <- tags$a(
        href = first_url,
        icon("link"), "Homepage",
        target = "_blank", class = "small"
      )
    }

    issues_link <- NULL
    if (!is.null(meta$BugReports) &&
          nchar(meta$BugReports) > 0) {
      issues_link <- tags$a(
        href = meta$BugReports,
        icon("bug"), "Issues",
        target = "_blank", class = "small"
      )
    }

    cran_url <- paste0(
      "https://cran.r-project.org/package=",
      meta$Package
    )

    tags$div(
      class = "mb-3",
      h2(
        meta$Package,
        tags$small(
          paste0("v", meta$Version),
          class = "text-muted"
        )
      ),
      p(meta$Title, class = "lead"),
      tags$div(
        class = "d-flex gap-3 flex-wrap",
        tags$span(
          icon("user"),
          meta$Author %||% "Unknown",
          class = "text-muted small"
        ),
        tags$span(
          icon("scale-balanced"),
          meta$License %||% "Unknown",
          class = "text-muted small"
        ),
        homepage_link,
        issues_link,
        tags$a(
          href = cran_url,
          icon("box"), "CRAN",
          target = "_blank", class = "small"
        )
      )
    )
  })

  # Download value boxes
  output$dl_day_title <- renderUI({
    yesterday <- Sys.Date() - 1
    tags$span(
      "Yesterday",
      tags$br(),
      tags$small(
        paste0(
          format(yesterday, "%d %b %Y"),
          " UTC"
        ),
        class = "fw-normal"
      )
    )
  })
  output$dl_day <- renderText({
    req(rv$download_totals)
    format_number(rv$download_totals$last_day)
  })
  output$dl_week <- renderText({
    req(rv$download_totals)
    format_number(rv$download_totals$last_week)
  })
  output$dl_month <- renderText({
    req(rv$download_totals)
    format_number(rv$download_totals$last_month)
  })
  output$dl_year <- renderText({
    # fetch_download_totals() sums the daily series for this rather
    # than calling the cranlogs total endpoint, which can lag.
    req(rv$download_totals)
    format_number(rv$download_totals$last_year)
  })

  # Download trend plot
  output$download_trend_plot <- renderPlotly({
    req(rv$downloads_daily)
    req(input$trend_series)
    df <- rv$downloads_daily
    selected <- input$trend_series

    # Aggregate to weekly
    df$week <- as.Date(cut(df$date, "week"))
    weekly <- aggregate(
      count ~ week, data = df, FUN = sum
    )
    weekly <- weekly[order(weekly$week), ]

    p <- plot_ly()

    show_versions <- "versions" %in% selected &&
      !is.null(rv$versions$timeline)

    if ("weekly" %in% selected && !show_versions) {
      p <- p |>
        add_trace(
          data = weekly,
          x = ~week, y = ~count,
          type = "scatter", mode = "lines",
          name = "Weekly",
          fill = "tozeroy",
          line = list(color = "#2c3e50"),
          fillcolor = "rgba(44, 62, 80, 0.1)"
        )
    }

    if ("weekly" %in% selected && show_versions) {
      # Map each week to the version current when the week began
      timeline <- rv$versions$timeline
      ver_df <- version_release_frame(timeline)
      weekly$version <- weekly_version_map(weekly$week, timeline)

      # Color palette for versions
      ver_colors <- c(
        "#2c3e50", "#e74c3c", "#27ae60",
        "#8e44ad", "#f39c12", "#1abc9c",
        "#d35400", "#2980b9", "#c0392b"
      )

      unique_vers <- unique(weekly$version)

      for (i in seq_along(unique_vers)) {
        v <- unique_vers[i]
        seg <- weekly[weekly$version == v, ]
        col <- ver_colors[
          ((i - 1) %% length(ver_colors)) + 1
        ]

        # Add bridge point from previous version's last
        # data point so lines connect across boundaries
        if (i > 1) {
          prev_v <- unique_vers[i - 1]
          prev_seg <- weekly[weekly$version == prev_v, ]
          if (nrow(prev_seg) > 0) {
            bridge <- prev_seg[nrow(prev_seg), ]
            bridge$version <- v
            seg <- rbind(bridge, seg)
          }
        }

        p <- p |>
          add_trace(
            data = seg,
            x = ~week, y = ~count,
            type = "scatter", mode = "lines",
            name = paste0("v", v),
            fill = "tozeroy",
            line = list(color = col),
            fillcolor = paste0(
              "rgba(",
              paste(
                col2rgb(col), collapse = ", "
              ),
              ", 0.1)"
            )
          )
      }

      # A release only earns a band if it was the current version at
      # the start of some week, so one superseded within a few days,
      # or published since the last full week, gets none. Mark those
      # with a dotted line, otherwise the legend silently reports
      # fewer releases than the version history lists.
      unbanded <- setdiff(ver_df$version, unique_vers)
      for (v in unbanded) {
        d <- ver_df$date[ver_df$version == v]
        if (d < min(weekly$week)) next

        p <- p |>
          add_segments(
            x = d, xend = d,
            y = 0, yend = max(weekly$count, na.rm = TRUE),
            line = list(
              color = "grey", dash = "dot", width = 1
            ),
            name = paste0("v", v),
            showlegend = TRUE
          )
      }
    }

    if (show_versions && !("weekly" %in% selected)) {
      # Show version markers even without weekly
      timeline <- rv$versions$timeline
      ver_dates <- as.Date(
        substr(unlist(timeline), 1, 10)
      )
      min_date <- min(weekly$week)
      max_date <- max(weekly$week)
      ver_in_range <- ver_dates[
        ver_dates >= min_date &
          ver_dates <= max_date
      ]
      ver_names_in <- names(timeline)[
        ver_dates >= min_date &
          ver_dates <= max_date
      ]

      for (i in seq_along(ver_in_range)) {
        p <- p |>
          add_segments(
            x = ver_in_range[i],
            xend = ver_in_range[i],
            y = 0,
            yend = max(weekly$count, na.rm = TRUE),
            line = list(
              color = "grey", dash = "dot",
              width = 1
            ),
            name = paste0("v", ver_names_in[i]),
            showlegend = TRUE
          )
      }
    }

    if ("cumulative" %in% selected) {
      weekly$cumulative <- cumsum(weekly$count)
      p <- p |>
        add_trace(
          data = weekly,
          x = ~week, y = ~cumulative,
          type = "scatter", mode = "lines",
          name = "Cumulative",
          line = list(
            color = "#e74c3c", dash = "dot"
          )
        )
    }

    if ("average" %in% selected) {
      avg_val <- round(mean(weekly$count))
      p <- p |>
        add_trace(
          data = weekly,
          x = ~week,
          y = rep(avg_val, nrow(weekly)),
          hovertemplate = paste0(
            "Avg: ",
            format_number(avg_val),
            "/wk<extra></extra>"
          ),
          type = "scatter", mode = "lines",
          name = paste0(
            "Avg (", format_number(
              round(avg_val)
            ), "/wk)"
          ),
          line = list(
            color = "#27ae60", width = 2,
            dash = "dash"
          )
        )
    }

    p |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Downloads"),
        hovermode = "x unified",
        margin = list(t = 10),
        legend = list(
          orientation = "h", y = -0.15
        )
      ) |>
      config(displayModeBar = FALSE)
  })

  # Health score display
  output$health_score_display <- renderUI({
    req(rv$health)
    score <- rv$health$score
    color <- health_score_color(score)
    label <- health_score_label(score)

    circle_style <- paste0(
      "width: 120px; height: 120px; ",
      "border-radius: 50%; ",
      "border: 8px solid ", color, "; ",
      "display: flex; flex-direction: column; ",
      "align-items: center; ",
      "justify-content: center; ",
      "margin: 0 auto;"
    )

    score_style <- paste0(
      "font-size: 2.5rem; ",
      "font-weight: bold; color: ", color
    )

    label_style <- paste0(
      "color: ", color, "; ",
      "font-weight: 600; font-size: 1.1rem;"
    )

    # Say so when a factor could not be fetched, rather than
    # presenting a partial score as a complete one.
    available <- rv$health$weight_available
    caveat <- if (!is.na(score) && available < 100) {
      tags$div(
        class = "text-muted small mt-1",
        paste0(
          "Scored on ", available,
          " of 100 points of weighting"
        )
      )
    }

    tags$div(
      class = "text-center",
      tags$div(
        style = circle_style,
        tags$span(
          if (is.na(score)) "?" else score,
          style = score_style
        )
      ),
      tags$span(label, style = label_style),
      caveat
    )
  })

  output$health_details <- renderUI({
    req(rv$health)
    details <- rv$health$details
    tags$ul(
      class = "list-unstyled small",
      lapply(names(details), function(key) {
        item <- details[[key]]
        ico_map <- list(
          good = list(
            name = "circle-check",
            cls = "text-success me-1"
          ),
          neutral = list(
            name = "circle-info",
            cls = "text-muted me-1"
          ),
          warn = list(
            name = "triangle-exclamation",
            cls = "text-warning me-1"
          ),
          bad = list(
            name = "circle-xmark",
            cls = "text-danger me-1"
          ),
          unknown = list(
            name = "circle-question",
            cls = "text-muted me-1"
          )
        )
        ico <- ico_map[[
          item$sentiment %||% "neutral"
        ]]
        tags$li(
          icon(ico$name, class = ico$cls),
          item$text
        )
      })
    )
  })

  # Download statistics
  output$download_stats_ui <- renderUI({
    req(rv$downloads_daily)
    req(rv$metadata)
    df <- rv$downloads_daily

    # Lifetime total (pre-fetched in load_package)
    lifetime_dl <- if (!is.na(rv$lifetime_downloads)) {
      format_number(rv$lifetime_downloads)
    } else {
      "N/A"
    }

    # First publication date for Days on CRAN
    first_pub <- NULL
    if (!is.null(rv$versions$timeline)) {
      all_dates <- as.Date(
        substr(unlist(rv$versions$timeline), 1, 10)
      )
      first_pub <- min(all_dates, na.rm = TRUE)
    }

    # Peak day
    peak_idx <- which.max(df$count)
    peak_count <- format_number(df$count[peak_idx])
    peak_date <- format(df$date[peak_idx], "%d %b %Y")

    # Average daily downloads (over available data)
    avg_daily <- format_number(
      round(mean(df$count))
    )

    # Aggregate weekly for weekly stats
    df$week <- as.Date(cut(df$date, "week"))
    weekly <- aggregate(
      count ~ week, data = df, FUN = sum
    )

    # Peak week
    peak_wk_idx <- which.max(weekly$count)
    peak_wk_count <- format_number(
      weekly$count[peak_wk_idx]
    )
    peak_wk_date <- format(
      weekly$week[peak_wk_idx], "%d %b %Y"
    )

    # Average weekly
    avg_weekly <- format_number(
      round(mean(weekly$count))
    )

    # Days on CRAN
    days_on_cran <- if (!is.null(first_pub)) {
      as.numeric(Sys.Date() - first_pub)
    } else {
      NA
    }
    days_label <- if (!is.na(days_on_cran)) {
      paste0(
        format_number(days_on_cran), " days",
        " (since ", format(first_pub, "%d %b %Y"),
        ")"
      )
    } else {
      "Unknown"
    }

    stat_row <- function(label, value) {
      tags$tr(
        tags$td(
          tags$strong(label),
          style = "white-space: nowrap;"
        ),
        tags$td(value)
      )
    }

    layout_columns(
      col_widths = c(6, 6),

      tags$table(
        class = "table table-sm mb-0",
        tags$tbody(
          stat_row(
            "Lifetime Downloads", lifetime_dl
          ),
          stat_row("Days on CRAN", days_label),
          stat_row(
            "Daily Average (past year)", avg_daily
          ),
          stat_row(
            "Weekly Average (past year)",
            avg_weekly
          )
        )
      ),

      tags$table(
        class = "table table-sm mb-0",
        tags$tbody(
          stat_row(
            "Peak Day",
            paste0(peak_count, " (", peak_date, ")")
          ),
          stat_row(
            "Peak Week",
            paste0(
              peak_wk_count,
              " (w/c ", peak_wk_date, ")"
            )
          )
        )
      )
    )
  })

  # Metadata table
  output$metadata_table <- renderUI({
    req(rv$metadata)
    meta <- rv$metadata

    first_published <- NULL
    if (!is.null(rv$versions$timeline)) {
      dates <- as.Date(
        substr(unlist(rv$versions$timeline), 1, 10)
      )
      first_published <- format(
        min(dates, na.rm = TRUE), "%Y-%m-%d"
      )
    }

    last_published <- meta$`Date/Publication`
    if (!is.null(last_published)) {
      last_published <- substr(last_published, 1, 10)
    }

    maintainer <- gsub(
      "<.*>", "", meta$Maintainer %||% "Unknown"
    )

    # Build links list
    links <- list()
    links[["CRAN"]] <- paste0(
      "https://cran.r-project.org/package=",
      meta$Package
    )
    links[["Documentation"]] <- paste0(
      "https://cran.r-project.org/web/packages/",
      meta$Package, "/vignettes/"
    )
    if (!is.null(meta$URL) && nchar(meta$URL) > 0) {
      urls <- trimws(
        strsplit(meta$URL, "[,\n]+")[[1]]
      )
      for (u in urls) {
        if (grepl("github\\.com", u, TRUE)) {
          links[["GitHub"]] <- u
        } else {
          links[["Homepage"]] <- u
        }
      }
    }
    if (!is.null(meta$BugReports) &&
          nchar(meta$BugReports) > 0) {
      links[["Issues"]] <- meta$BugReports
    }

    links_ui <- tags$div(
      class = "d-flex gap-2 flex-wrap",
      lapply(names(links), function(lbl) {
        ico <- switch(
          lbl,
          "CRAN" = "box",
          "Documentation" = "book",
          "GitHub" = "code-branch",
          "Homepage" = "link",
          "Issues" = "bug",
          "link"
        )
        tags$a(
          href = links[[lbl]],
          target = "_blank",
          class = "btn btn-sm btn-outline-secondary",
          icon(ico), lbl
        )
      })
    )

    fields <- list(
      "Description" = meta$Description,
      "Maintainer" = trimws(maintainer),
      "License" = meta$License,
      "First Published" =
        first_published %||% "Unknown",
      "Last Published" =
        last_published %||% "Unknown",
      "R Version Required" =
        meta$Depends$R %||% "Not specified",
      "NeedsCompilation" =
        meta$NeedsCompilation %||% "Unknown",
      "Encoding" =
        meta$Encoding %||% "Unknown"
    )

    tags$div(
      links_ui,
      tags$hr(),
      tags$table(
        class = "table table-sm",
        tags$tbody(
          lapply(names(fields), function(key) {
            tags$tr(
              tags$td(
                tags$strong(key),
                style = paste(
                  "width: 35%;",
                  "white-space: nowrap;"
                )
              ),
              tags$td(fields[[key]])
            )
          })
        )
      )
    )
  })

  # Dependency tables
  render_dep_table <- function(dep_type) {
    renderDT({
      req(rv$metadata)
      deps <- rv$metadata[[dep_type]]
      if (is.null(deps) || length(deps) == 0) {
        return(data.frame(
          Package = "None", Version = "",
          stringsAsFactors = FALSE
        ))
      }
      df <- parse_dependencies(deps)
      names(df) <- c("Package", "Version Constraint")
      df
    },
    options = list(
      pageLength = 10, dom = "tip",
      searching = FALSE
    ),
    rownames = FALSE, class = "compact")
  }

  output$imports_table <- render_dep_table("Imports")
  output$depends_table <- render_dep_table("Depends")
  output$suggests_table <- render_dep_table("Suggests")

  # Version history
  output$version_table <- renderDT({
    req(rv$versions)
    vh <- build_version_history(rv$versions)
    if (is.null(vh)) {
      return(data.frame(
        Version = "N/A", Date = "",
        `Days Ago` = "",
        stringsAsFactors = FALSE
      ))
    }
    vh$date <- format(vh$date, "%Y-%m-%d")
    names(vh) <- c("Version", "Date", "Days Ago")
    vh
  },
  options = list(pageLength = 8, dom = "tip"),
  rownames = FALSE, class = "compact")

  # Reverse dependencies summary
  output$rev_deps_summary <- renderUI({
    req(rv$selected_pkg)

    # NULL means the lookup failed. Showing zero here would read as
    # "nothing depends on this", which is a very different claim.
    if (is.null(rv$rev_deps)) {
      return(tags$p(
        icon("circle-question", class = "text-muted me-1"),
        "Reverse dependency data unavailable.",
        class = "text-muted"
      ))
    }

    rd <- rv$rev_deps

    dep_row <- function(label, value) {
      tags$tr(
        tags$td(label),
        tags$td(tags$strong(format_number(value)))
      )
    }

    tags$div(
      tags$div(
        class = "display-6 text-center mb-3",
        format_number(rd$total),
        tags$small(
          "packages depend on this",
          class = "text-muted d-block fs-6"
        )
      ),
      tags$table(
        class = "table table-sm",
        tags$tbody(
          dep_row("Depends", rd$depends),
          dep_row("Imports", rd$imports),
          dep_row("Suggests", rd$suggests),
          dep_row("LinkingTo", rd$linking_to)
        )
      )
    )
  })

  # --- Browse Tab ---

  output$browse_has_results <- reactive({
    !is.null(rv$browse_results)
  })
  outputOptions(
    output, "browse_has_results",
    suspendWhenHidden = FALSE
  )

  output$browse_results_header <- renderText({
    rv$browse_label
  })

  # Popular packages
  observeEvent(input$browse_popular_btn, {
    withProgress(
      message = "Fetching popular packages...",
      {
        rv$browse_results <- fetch_top_downloads(50)
        rv$browse_label <- paste(
          "Popular Packages (Last Month)"
        )
      }
    )
  })

  # Category selection
  observeEvent(input$browse_category, {
    req(nchar(input$browse_category) > 0)
    category <- input$browse_category
    query <- BROWSE_CATEGORIES[[category]]

    withProgress(
      message = paste(
        "Loading", category, "packages..."
      ),
      {
        rv$browse_results <- search_packages(
          query, limit = 50
        )
        rv$browse_label <- category
      }
    )
  }, ignoreInit = TRUE)

  # A-Z letter buttons
  lapply(LETTERS, function(l) {
    observeEvent(input[[paste0("az_", l)]], {
      withProgress(
        message = paste0(
          "Loading packages starting with ",
          l, "..."
        ),
        {
          results <- fetch_packages_by_letter(l, limit = 50)
          rv$browse_results <- results

          if (is.null(results)) {
            rv$browse_label <- paste(
              "No packages found starting with", l
            )
          } else {
            total <- attr(results, "total") %||% nrow(results)
            rv$browse_label <- if (total > nrow(results)) {
              paste0(
                "Packages starting with ", l,
                " (", nrow(results), " of ",
                format_number(total), ")"
              )
            } else {
              paste("Packages starting with", l)
            }
          }
        }
      )
    })
  })

  # Browse results table
  output$browse_table <- renderDT({
    req(rv$browse_results)
    df <- rv$browse_results

    if ("downloads" %in% names(df)) {
      pkg_names <- df$package
      df$downloads <- vapply(
        df$downloads, format_number, character(1)
      )
      df$Links <- vapply(
        pkg_names, package_links_html,
        character(1)
      )
      names(df) <- c("Package", "Downloads", "Links")
    } else if ("package" %in% names(df)) {
      df$Links <- vapply(
        df$package, package_links_html,
        character(1)
      )
      df <- df[, c("package", "title", "version", "Links")]
      names(df) <- c(
        "Package", "Title", "Version", "Links"
      )
    }

    # Only the Links column holds markup we built ourselves. Titles
    # and names come from the search index, so leave them escaped.
    datatable(
      df,
      selection = "single",
      rownames = FALSE,
      escape = setdiff(
        seq_len(ncol(df)),
        which(names(df) == "Links")
      ),
      options = list(pageLength = 20, dom = "frtip"),
      class = "compact hover"
    )
  })

  # Click a row to load in Explorer
  observeEvent(input$browse_table_rows_selected, {
    row <- input$browse_table_rows_selected
    req(row)
    df <- rv$browse_results
    pkg <- if ("package" %in% names(df)) {
      df$package[row]
    } else {
      df$Package[row]
    }
    req(pkg)
    load_package(pkg)
    nav_select("main_nav", selected = "Explorer")
  })

  # --- Compare Tab ---

  observeEvent(input$compare_search_btn, {
    req(nchar(trimws(input$compare_search)) > 0)
    query <- trimws(input$compare_search)
    withProgress(message = "Searching...", {
      rv$compare_search_results <- search_packages(
        query, limit = 10
      )
    })
  })

  output$compare_search_results_ui <- renderUI({
    results <- rv$compare_search_results
    if (is.null(results) || nrow(results) == 0) {
      if (!is.null(rv$compare_search_results)) {
        return(p(
          "No packages found.",
          class = "text-muted small"
        ))
      }
      return(NULL)
    }

    tags$div(
      class = "list-group mb-2",
      lapply(seq_len(nrow(results)), function(i) {
        tags$div(
          class = paste(
            "list-group-item",
            "list-group-item-action py-1 px-2"
          ),
          style = "cursor: pointer;",
          onclick = sprintf(
            paste0(
              "Shiny.setInputValue(",
              "'compare_pick_pkg', '%s',",
              " {priority: 'event'})"
            ),
            results$package[i]
          ),
          tags$strong(
            results$package[i],
            class = "small"
          ),
          tags$span(
            paste0(" v", results$version[i]),
            class = "text-muted small"
          )
        )
      })
    )
  })

  observeEvent(input$compare_pick_pkg, {
    pkg <- input$compare_pick_pkg
    # Fill next empty slot
    if (nchar(trimws(input$compare_pkg1)) == 0) {
      updateTextInput(
        session, "compare_pkg1", value = pkg
      )
    } else if (nchar(trimws(input$compare_pkg2)) == 0) {
      updateTextInput(
        session, "compare_pkg2", value = pkg
      )
    } else {
      updateTextInput(
        session, "compare_pkg3", value = pkg
      )
    }
    rv$compare_search_results <- NULL
  })

  output$comparison_loaded <- reactive({
    !is.null(rv$compare_data)
  })
  outputOptions(
    output, "comparison_loaded",
    suspendWhenHidden = FALSE
  )

  observeEvent(input$compare_btn, {
    pkgs <- c(
      trimws(input$compare_pkg1),
      trimws(input$compare_pkg2)
    )
    if (nchar(trimws(input$compare_pkg3)) > 0) {
      pkgs <- c(pkgs, trimws(input$compare_pkg3))
    }
    pkgs <- pkgs[nchar(pkgs) > 0]

    if (length(pkgs) < 2) {
      showNotification(
        "Enter at least 2 package names.",
        type = "warning"
      )
      return()
    }

    withProgress(message = "Comparing packages...", {
      compare <- lapply(pkgs, function(pkg) {
        incProgress(1 / length(pkgs), detail = pkg)
        meta <- fetch_package_metadata(pkg)
        if (is.null(meta)) return(NULL)

        daily <- fetch_daily_downloads(pkg)
        totals <- fetch_download_totals(pkg, daily)
        versions <- fetch_package_versions(pkg)

        reconciled <- reconcile_with_cran(
          meta, versions, fetch_cran_description(pkg)
        )
        meta <- reconciled$metadata
        versions <- reconciled$versions

        rdeps <- fetch_reverse_deps(pkg)
        health <- calculate_health_score(
          meta, versions, totals, rdeps, daily
        )

        list(
          package = pkg,
          metadata = meta,
          totals = totals,
          daily = daily,
          versions = versions,
          rev_deps = rdeps,
          health = health
        )
      })

      compare <- Filter(Negate(is.null), compare)

      if (length(compare) < 2) {
        showNotification(
          "Could not find enough valid packages.",
          type = "error"
        )
        return()
      }

      rv$compare_data <- compare
    })
  })

  output$compare_trend_plot <- renderPlotly({
    req(rv$compare_data)
    compare <- rv$compare_data

    colors <- c(
      "#2c3e50", "#e74c3c", "#27ae60", "#8e44ad"
    )

    p <- plot_ly()

    for (i in seq_along(compare)) {
      df <- compare[[i]]$daily
      if (!is.null(df)) {
        df$week <- as.Date(cut(df$date, "week"))
        weekly <- aggregate(
          count ~ week, data = df, FUN = sum
        )

        p <- p |>
          add_trace(
            data = weekly, x = ~week, y = ~count,
            type = "scatter", mode = "lines",
            name = compare[[i]]$package,
            line = list(
              color = colors[i], width = 2
            )
          )
      }
    }

    p |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Weekly Downloads"),
        hovermode = "x unified",
        margin = list(t = 10),
        legend = list(orientation = "h", y = -0.1)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$compare_table <- renderDT({
    req(rv$compare_data)
    compare <- rv$compare_data

    build_row <- function(item) {
      meta <- item$metadata
      totals <- item$totals
      versions <- item$versions

      n_ver <- if (!is.null(versions$versions)) {
        length(versions$versions)
      } else {
        NA
      }

      last_pub <- meta$`Date/Publication`
      if (!is.null(last_pub)) {
        last_pub <- substr(last_pub, 1, 10)
      }

      data.frame(
        Package = item$package,
        Version = meta$Version %||% "",
        # Two scores sit side by side here, so a score computed
        # over less than the full weighting has to say so.
        `Viability Score` = if (is.na(item$health$score)) {
          "N/A"
        } else if (item$health$weight_available < 100) {
          paste0(item$health$score, "/100 (partial)")
        } else {
          paste0(item$health$score, "/100")
        },
        `Monthly Downloads` = format_number(
          totals$last_month
        ),
        `Yearly Downloads` = format_number(
          totals$last_year %||% NA_real_
        ),
        `Reverse Deps` = format_number(
          item$rev_deps$total %||% NA_real_
        ),
        Releases = if (!is.na(n_ver)) {
          as.character(n_ver)
        } else {
          "N/A"
        },
        `Last Published` = last_pub %||% "N/A",
        License = meta$License %||% "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    df <- do.call(
      rbind, lapply(compare, build_row)
    )
    df
  },
  options = list(
    dom = "t", ordering = FALSE, paging = FALSE
  ),
  rownames = FALSE, class = "compact")
}
