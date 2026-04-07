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
    compare_data = NULL
  )

  # --- Search ---
  rv$search_results <- NULL

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
        actionLink(
          inputId = paste0("select_pkg_", i),
          label = tags$div(
            class = paste(
              "list-group-item",
              "list-group-item-action p-2"
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
        )
      })
    )
  })

  observe({
    results <- rv$search_results
    req(results)

    lapply(seq_len(nrow(results)), function(i) {
      observeEvent(
        input[[paste0("select_pkg_", i)]],
        {
          load_package(results$package[i])
        },
        ignoreInit = TRUE
      )
    })
  })

  # Core function to load all package data
  load_package <- function(pkg_name) {
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
          0.2, detail = "Fetching version history"
        )
        rv$versions <- fetch_package_versions(pkg_name)

        incProgress(
          0.2, detail = "Fetching download stats"
        )
        rv$download_totals <- fetch_download_totals(
          pkg_name
        )

        incProgress(
          0.2, detail = "Fetching daily downloads"
        )
        rv$downloads_daily <- fetch_daily_downloads(
          pkg_name
        )

        incProgress(
          0.2, detail = "Fetching reverse dependencies"
        )
        rv$rev_deps <- fetch_reverse_deps(pkg_name)

        incProgress(
          0.1, detail = "Calculating health score"
        )
        rv$health <- calculate_health_score(
          rv$metadata, rv$versions,
          rv$download_totals, rv$rev_deps
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
      first_url <- strsplit(meta$URL, "[,\\s]+")[[1]][1]
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

    p <- plot_ly()

    if ("weekly" %in% selected) {
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

    tags$div(
      class = "text-center",
      tags$div(
        style = circle_style,
        tags$span(score, style = score_style)
      ),
      tags$span(label, style = label_style)
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

    # Fetch lifetime total downloads
    lifetime_dl <- "N/A"
    if (!is.null(first_published)) {
      lifetime <- tryCatch({
        url <- paste0(
          "https://cranlogs.r-pkg.org/",
          "downloads/total/",
          first_published, ":",
          Sys.Date() - 1, "/", meta$Package
        )
        resp <- httr2::request(url) |>
          httr2::req_timeout(10) |>
          httr2::req_perform()
        data <- jsonlite::fromJSON(
          httr2::resp_body_string(resp),
          simplifyVector = TRUE
        )
        dl <- data$downloads
        if (is.list(dl) && !is.data.frame(dl)) {
          dl <- dl[[1]]
        }
        if (is.numeric(dl)) dl else as.numeric(dl)
      }, error = function(e) NA)
      if (!is.na(lifetime)) {
        lifetime_dl <- format_number(lifetime)
      }
    }

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
        strsplit(meta$URL, "[,\\s]+")[[1]]
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
      "Lifetime Downloads" = lifetime_dl,
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
    req(rv$rev_deps)
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

  rv$browse_results <- NULL
  rv$browse_label <- NULL

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
          rv$browse_results <- search_packages(
            paste0("package:", l, "*"), limit = 50
          )
          rv$browse_label <- paste(
            "Packages starting with", l
          )
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
    df
  },
  selection = "single", rownames = FALSE,
  escape = FALSE,
  options = list(pageLength = 20, dom = "frtip"),
  class = "compact hover")

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

  rv$compare_search_results <- NULL

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
        actionLink(
          inputId = paste0("compare_pick_", i),
          label = tags$div(
            class = paste(
              "list-group-item",
              "list-group-item-action py-1 px-2"
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
        )
      })
    )
  })

  observe({
    results <- rv$compare_search_results
    req(results)

    lapply(seq_len(nrow(results)), function(i) {
      observeEvent(
        input[[paste0("compare_pick_", i)]],
        {
          pkg <- results$package[i]
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
        },
        ignoreInit = TRUE
      )
    })
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

        totals <- fetch_download_totals(pkg)
        daily <- fetch_daily_downloads(pkg)
        versions <- fetch_package_versions(pkg)
        rdeps <- fetch_reverse_deps(pkg)
        health <- calculate_health_score(
          meta, versions, totals, rdeps
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
        `Viability Score` = paste0(
          item$health$score, "/100"
        ),
        `Monthly Downloads` = format_number(
          totals$last_month
        ),
        `Yearly Downloads` = format_number(
          totals$last_year
        ),
        `Reverse Deps` = format_number(
          item$rev_deps$total
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
