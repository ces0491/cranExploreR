library(shiny)
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
    rev_deps = NULL,
    health = NULL,
    search_results = NULL,
    compare_data = NULL
  )

  # --- Search ---
  observeEvent(input$search_btn, {
    req(nchar(trimws(input$search_query)) > 0)
    query <- trimws(input$search_query)

    rv$search_results <- NULL
    rv$selected_pkg <- NULL

    withProgress(message = "Searching CRAN...", {
      results <- search_packages(query)
      rv$search_results <- results
    })
  })

  # Allow Enter key to trigger search
  observeEvent(input$search_query, {
    # This is handled by the actionButton binding
  }, ignoreInit = TRUE)

  # Render search results as clickable list
  output$search_results_ui <- renderUI({
    results <- rv$search_results
    if (is.null(results) || nrow(results) == 0) {
      if (!is.null(rv$search_results)) {
        return(p("No packages found.", class = "text-muted"))
      }
      return(NULL)
    }

    tags$div(
      class = "list-group",
      lapply(seq_len(nrow(results)), function(i) {
        pkg <- results$package[i]
        actionLink(
          inputId = paste0("select_pkg_", i),
          label = tags$div(
            class = "list-group-item list-group-item-action p-2",
            tags$strong(pkg),
            tags$span(paste0(" v", results$version[i]), class = "text-muted small"),
            tags$br(),
            tags$small(results$title[i], class = "text-muted")
          )
        )
      })
    )
  })

  # Handle package selection from search results
  observe({
    results <- rv$search_results
    req(results)

    lapply(seq_len(nrow(results)), function(i) {
      observeEvent(input[[paste0("select_pkg_", i)]], {
        load_package(results$package[i])
      }, ignoreInit = TRUE)
    })
  })

  # Core function to load all package data
  load_package <- function(pkg_name) {
    withProgress(message = paste("Loading", pkg_name, "..."), {
      incProgress(0.1, detail = "Fetching metadata")
      rv$metadata <- fetch_package_metadata(pkg_name)

      if (is.null(rv$metadata)) {
        showNotification(paste("Package", pkg_name, "not found on CRAN."),
                          type = "error")
        return()
      }

      rv$selected_pkg <- pkg_name

      incProgress(0.2, detail = "Fetching version history")
      rv$versions <- fetch_package_versions(pkg_name)

      incProgress(0.2, detail = "Fetching download stats")
      rv$download_totals <- fetch_download_totals(pkg_name)

      incProgress(0.2, detail = "Fetching daily downloads")
      rv$downloads_daily <- fetch_daily_downloads(pkg_name)

      incProgress(0.2, detail = "Fetching reverse dependencies")
      rv$rev_deps <- fetch_reverse_deps(pkg_name)

      incProgress(0.1, detail = "Calculating health score")
      rv$health <- calculate_health_score(
        rv$metadata, rv$versions, rv$download_totals, rv$rev_deps
      )
    })
  }

  # --- Outputs ---

  # Flag for conditional panels
  output$package_loaded <- reactive({ !is.null(rv$selected_pkg) })
  outputOptions(output, "package_loaded", suspendWhenHidden = FALSE)

  # Package header
  output$package_header <- renderUI({
    req(rv$metadata)
    meta <- rv$metadata
    tags$div(
      class = "mb-3",
      h2(meta$Package, tags$small(paste0("v", meta$Version), class = "text-muted")),
      p(meta$Title, class = "lead"),
      tags$div(
        class = "d-flex gap-3 flex-wrap",
        tags$span(icon("user"), meta$Author %||% "Unknown", class = "text-muted small"),
        tags$span(icon("scale-balanced"), meta$License %||% "Unknown", class = "text-muted small"),
        if (!is.null(meta$URL) && nchar(meta$URL) > 0)
          tags$a(href = strsplit(meta$URL, "[,\\s]+")[[1]][1],
                 icon("link"), "Homepage", target = "_blank", class = "small"),
        if (!is.null(meta$BugReports) && nchar(meta$BugReports) > 0)
          tags$a(href = meta$BugReports,
                 icon("bug"), "Issues", target = "_blank", class = "small"),
        tags$a(href = paste0("https://cran.r-project.org/package=", meta$Package),
               icon("box"), "CRAN", target = "_blank", class = "small")
      )
    )
  })

  # Download value boxes
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
    req(rv$download_totals)
    format_number(rv$download_totals$last_year)
  })

  # Download trend plot
  output$download_trend_plot <- renderPlotly({
    req(rv$downloads_daily)
    df <- rv$downloads_daily

    # Aggregate to weekly for smoother trend
    df$week <- as.Date(cut(df$date, "week"))
    weekly <- aggregate(count ~ week, data = df, FUN = sum)

    plot_ly(weekly, x = ~week, y = ~count, type = "scatter", mode = "lines",
            fill = "tozeroy",
            line = list(color = "#2c3e50"),
            fillcolor = "rgba(44, 62, 80, 0.1)") |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Weekly Downloads"),
        hovermode = "x unified",
        margin = list(t = 10)
      ) |>
      config(displayModeBar = FALSE)
  })

  # Health score display
  output$health_score_display <- renderUI({
    req(rv$health)
    score <- rv$health$score
    color <- health_score_color(score)
    label <- health_score_label(score)

    tags$div(
      class = "text-center",
      tags$div(
        style = paste0(
          "width: 120px; height: 120px; border-radius: 50%; ",
          "border: 8px solid ", color, "; ",
          "display: flex; flex-direction: column; align-items: center; justify-content: center; ",
          "margin: 0 auto;"
        ),
        tags$span(score, style = paste0("font-size: 2.5rem; font-weight: bold; color: ", color)),
      ),
      tags$span(label, style = paste0("color: ", color, "; font-weight: 600; font-size: 1.1rem;"))
    )
  })

  output$health_details <- renderUI({
    req(rv$health)
    details <- rv$health$details
    tags$ul(
      class = "list-unstyled small",
      lapply(names(details), function(key) {
        tags$li(
          icon("circle-check", class = "text-muted me-1"),
          details[[key]]
        )
      })
    )
  })

  # Metadata table
  output$metadata_table <- renderUI({
    req(rv$metadata)
    meta <- rv$metadata

    first_published <- NULL
    if (!is.null(rv$versions$timeline)) {
      dates <- as.Date(substr(unlist(rv$versions$timeline), 1, 10))
      first_published <- format(min(dates, na.rm = TRUE), "%Y-%m-%d")
    }

    last_published <- meta$`Date/Publication`
    if (!is.null(last_published)) {
      last_published <- substr(last_published, 1, 10)
    }

    maintainer <- gsub("<.*>", "", meta$Maintainer %||% "Unknown")

    fields <- list(
      "Description" = meta$Description,
      "Maintainer" = trimws(maintainer),
      "License" = meta$License,
      "First Published" = first_published %||% "Unknown",
      "Last Published" = last_published %||% "Unknown",
      "R Version Required" = meta$Depends$R %||% "Not specified",
      "NeedsCompilation" = meta$NeedsCompilation %||% "Unknown",
      "Encoding" = meta$Encoding %||% "Unknown"
    )

    tags$table(
      class = "table table-sm",
      tags$tbody(
        lapply(names(fields), function(key) {
          tags$tr(
            tags$td(tags$strong(key), style = "width: 35%; white-space: nowrap;"),
            tags$td(fields[[key]])
          )
        })
      )
    )
  })

  # Dependency tables
  render_dep_table <- function(dep_type) {
    renderDT({
      req(rv$metadata)
      deps <- rv$metadata[[dep_type]]
      if (is.null(deps) || length(deps) == 0) {
        return(data.frame(Package = "None", Version = "", stringsAsFactors = FALSE))
      }
      df <- parse_dependencies(deps)
      names(df) <- c("Package", "Version Constraint")
      df
    }, options = list(pageLength = 10, dom = "tip", searching = FALSE),
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
      return(data.frame(Version = "N/A", Date = "", `Days Ago` = "", stringsAsFactors = FALSE))
    }
    vh$date <- format(vh$date, "%Y-%m-%d")
    names(vh) <- c("Version", "Date", "Days Ago")
    vh
  }, options = list(pageLength = 8, dom = "tip"), rownames = FALSE, class = "compact")

  # Reverse dependencies summary
  output$rev_deps_summary <- renderUI({
    req(rv$rev_deps)
    rd <- rv$rev_deps

    tags$div(
      tags$div(
        class = "display-6 text-center mb-3",
        format_number(rd$total),
        tags$small("packages depend on this", class = "text-muted d-block fs-6")
      ),
      tags$table(
        class = "table table-sm",
        tags$tbody(
          tags$tr(tags$td("Depends"), tags$td(tags$strong(format_number(rd$depends)))),
          tags$tr(tags$td("Imports"), tags$td(tags$strong(format_number(rd$imports)))),
          tags$tr(tags$td("Suggests"), tags$td(tags$strong(format_number(rd$suggests)))),
          tags$tr(tags$td("LinkingTo"), tags$td(tags$strong(format_number(rd$linking_to))))
        )
      )
    )
  })

  # --- Compare Tab ---

  output$comparison_loaded <- reactive({ !is.null(rv$compare_data) })
  outputOptions(output, "comparison_loaded", suspendWhenHidden = FALSE)

  observeEvent(input$compare_btn, {
    pkgs <- c(trimws(input$compare_pkg1), trimws(input$compare_pkg2))
    if (nchar(trimws(input$compare_pkg3)) > 0) {
      pkgs <- c(pkgs, trimws(input$compare_pkg3))
    }
    pkgs <- pkgs[nchar(pkgs) > 0]

    if (length(pkgs) < 2) {
      showNotification("Enter at least 2 package names.", type = "warning")
      return()
    }

    withProgress(message = "Comparing packages...", {
      compare <- lapply(pkgs, function(pkg) {
        incProgress(1 / length(pkgs), detail = pkg)
        meta <- fetch_package_metadata(pkg)
        if (is.null(meta)) return(NULL)

        totals <- fetch_download_totals(pkg)
        daily <- fetch_daily_downloads(pkg)
        versions <- fetch_package_versions(pkg)
        rev_deps <- fetch_reverse_deps(pkg)
        health <- calculate_health_score(meta, versions, totals, rev_deps)

        list(
          package = pkg,
          metadata = meta,
          totals = totals,
          daily = daily,
          versions = versions,
          rev_deps = rev_deps,
          health = health
        )
      })

      compare <- Filter(Negate(is.null), compare)

      if (length(compare) < 2) {
        showNotification("Could not find enough valid packages.", type = "error")
        return()
      }

      rv$compare_data <- compare
    })
  })

  output$compare_trend_plot <- renderPlotly({
    req(rv$compare_data)
    compare <- rv$compare_data

    colors <- c("#2c3e50", "#e74c3c", "#27ae60", "#8e44ad")

    p <- plot_ly()

    for (i in seq_along(compare)) {
      df <- compare[[i]]$daily
      if (!is.null(df)) {
        df$week <- as.Date(cut(df$date, "week"))
        weekly <- aggregate(count ~ week, data = df, FUN = sum)

        p <- p |> add_trace(
          data = weekly, x = ~week, y = ~count,
          type = "scatter", mode = "lines",
          name = compare[[i]]$package,
          line = list(color = colors[i], width = 2)
        )
      }
    }

    p |> layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Weekly Downloads"),
      hovermode = "x unified",
      margin = list(t = 10),
      legend = list(orientation = "h", y = -0.1)
    ) |> config(displayModeBar = FALSE)
  })

  output$compare_table <- renderDT({
    req(rv$compare_data)
    compare <- rv$compare_data

    build_row <- function(item) {
      meta <- item$metadata
      totals <- item$totals
      versions <- item$versions

      n_versions <- if (!is.null(versions$versions)) length(versions$versions) else NA

      last_pub <- meta$`Date/Publication`
      if (!is.null(last_pub)) last_pub <- substr(last_pub, 1, 10)

      data.frame(
        Package = item$package,
        Version = meta$Version %||% "",
        `Viability Score` = paste0(item$health$score, "/100"),
        `Monthly Downloads` = format_number(totals$last_month),
        `Yearly Downloads` = format_number(totals$last_year),
        `Reverse Deps` = format_number(item$rev_deps$total),
        Releases = if (!is.na(n_versions)) as.character(n_versions) else "N/A",
        `Last Published` = last_pub %||% "N/A",
        License = meta$License %||% "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    df <- do.call(rbind, lapply(compare, build_row))
    df
  }, options = list(dom = "t", ordering = FALSE, paging = FALSE),
  rownames = FALSE, class = "compact")
}
