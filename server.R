library(shiny)
library(ggplot2)
library(dplyr)
library(scholar)
library(DT)
library(ggiraph)

shinyServer(function(input, output) {
  
  rv  <- reactiveValues(n_researcher = 1,
                        scholar_ids = c())
  
  output[["selection_boxes"]] <- renderUI({
    lapply(1L:rv[["n_researcher"]], function(ith_id) {
      textInput(inputId = paste0("researcher", ith_id),
                label = "Scholar ID")
    })
  })
  
  observeEvent(input[["n_researcher"]], {
    rv[["n_researcher"]] <- input[["n_researcher"]]
  })
  
  observeEvent(input[["run_btn"]], {
    raw_ids <- sapply(paste0("researcher", 1L:rv[["n_researcher"]]),
                      function(ith_researcher) {
                        input[[ith_researcher]]
                      })
    
    rv[["scholar_ids"]] <- raw_ids[raw_ids != ""]
  })
  
  scholar_dat <- reactive({
    input[["run_btn"]]
    
    ids <- rv[["scholar_ids"]]
    
    validate(
      need(ids, "Provide at least single Scholar ID")
    )
    
    cite_dat <- compare_scholar_careers(ids, career = TRUE)
    
    validate(
      need(nrow(cite_dat) != 0, "Invalid scholar id(s)")
    )
    
    pubs_raw <- lapply(ids, function(ith_id) {
      get_publications(ith_id, pagesize = 2000)
    })
    
    pubs_tab <- do.call(rbind, lapply(1L:length(pubs_raw), function(ith_author_id) {
      res <- data.frame(id = ids[ith_author_id],
                        table(year = pubs_raw[[ith_author_id]][["year"]]))
      res[["year"]] <- as.numeric(as.character(res[["year"]]))
      res
    }))
    
    
    rbind(select(cite_dat, id, year, value = cites) %>%
            mutate(type = "cites"),
          select(pubs_tab, id, year, value = Freq) %>%
            mutate(type = "publications")) %>%
      tidyr::complete(year, id, type, fill = list(value = 0)) %>%
      inner_join(unique(select(cite_dat, id, name)), by = c("id" = "id")) %>%
      arrange(year) %>%
      group_by(type, id) %>%
      mutate(cum_value = cumsum(value)) %>%
      ungroup()
    
  })
  
  
  output[["comp_plot"]] <- renderGirafe({
    
    plot_dat <- if(input[["cum_logical"]]) {
      scholar_dat() %>%
        select(-value) %>%
        rename(value = cum_value)
    } else {
      scholar_dat()
    }
    
    p <- ggplot(plot_dat, aes(x = year,
                              y = value,
                              group = name,
                              color = name,
                              tooltip = paste0(type, ": ", value))) +
      geom_line() +
      geom_point_interactive() +
      facet_wrap(~ type, scales = "free_y", ncol = 1) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(breaks= scales::pretty_breaks()) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    girafe(ggobj = p, width_svg = 12)
  })
  
  output[["cites_per_pub_df"]] <- renderDataTable({
    totals <- group_by(scholar_dat(), type, name) %>%
      summarise(total = sum(value)) %>%
      ungroup()
    
    pubs <- filter(totals, type == "publications") %>%
      pull(total)
    
    filter(totals, type == "cites") %>%
      mutate(publications = pubs) %>%
      rename(cites = total) %>%
      mutate(cpp = round(cites/pubs, 2)) %>%
      select(-type) %>%
      datatable(colnames = c("Name", "Cites", "Publications", "CPP"),
                class = "table-bordered table-condensed",
                extensions = "Buttons",
                options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "pdf")),
                filter = "top",
                rownames = FALSE)
    
  })
  
  
  
  output[["comp_df"]] <- renderDataTable({
    mutate(scholar_dat(), type = as.factor(type), name = as.factor(name)) %>%
      select(type, name, value, year) %>%
      datatable(colnames = c("Type", "Name", "Value", "Year"),
                class = "table-bordered table-condensed",
                extensions = "Buttons",
                options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "pdf")),
                filter = "top",
                rownames = FALSE)
  })
})
