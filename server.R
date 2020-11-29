library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
    
    rv  <- reactiveValues(n_researcher = 1)
    
    output[["selection_boxes"]] <- renderUI({
        lapply(1L:rv[["n_researcher"]], function(ith_id) {
            textInput(inputId = paste0("researcher", ith_id), 
                      label = "Scholar ID")
        })
    })
    
    observeEvent(input[["n_researcher"]], {
        rv[["n_researcher"]] <- input[["n_researcher"]]
    })
    
    scholar_dat <- reactive({
        raw_ids <- sapply(paste0("researcher", 1L:rv[["n_researcher"]]),
                          function(ith_researcher) {
                              input[[ith_researcher]]
                          })
        
        ids <- raw_ids[raw_ids != "raw_ids"]
        
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
            inner_join(unique(select(cite_dat, id, name)), by = c("id" = "id")) 
        
    })
    
    output[["comp_plot"]] <- renderPlot({
        ggplot(scholar_dat(), aes(x = year, y = value, fill = name)) +
            geom_col(position = position_dodge(width = 0.75), width = 0.5) +
            #geom_text(position = position_dodge(width = 0.75), color = "black", 
            #          vjust = 1) +
            facet_wrap(~ type, scales = "free_y", ncol = 1) +
            scale_x_continuous(breaks = scales::pretty_breaks()) +
            scale_y_continuous(breaks= scales::pretty_breaks()) +
            theme_bw() +
            theme(legend.position = "bottom")
    })
    
})
