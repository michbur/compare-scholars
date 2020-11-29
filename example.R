library(scholar)
library(ggplot2)
library(dplyr)

ids <- c("riuFKDkAAAAJ", "DJULtJUAAAAJ", "q7cfoz8AAAAJ")

cite_dat <- compare_scholar_careers(ids, career = TRUE)

pubs_raw <- lapply(ids, function(ith_id) {
  get_publications(ith_id, pagesize = 2000)
})

pubs_tab <- do.call(rbind, lapply(1L:length(pubs_raw), function(ith_author_id) {
  res <- data.frame(id = ids[ith_author_id],
                    table(year = pubs_raw[[ith_author_id]][["year"]]))
  res[["year"]] <- as.numeric(as.character(res[["year"]]))
  res
}))

scholar_dat <- rbind(select(cite_dat, id, year, value = cites) %>% 
        mutate(type = "cites"),
      select(pubs_tab, id, year, value = Freq) %>% 
        mutate(type = "publications")) %>% 
  tidyr::complete(year, id, type, fill = list(value = 0)) %>% 
  inner_join(unique(select(cite_dat, id, name)), by = c("id" = "id")) 

ggplot(scholar_dat, aes(x = year, y = value, fill = name)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.5) +
  #geom_text(position = position_dodge(width = 0.75), color = "black", 
  #          vjust = 1) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(scholar_dat, aes(x = year, y = value, label = value)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.5) +
  facet_grid(type ~ name, scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  theme_bw() 
  
