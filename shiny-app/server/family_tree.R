observe({
  req(db_table())  # ensure data is available
  
  local_table <- db_table()
  if (nrow(local_table) < 1) {
    output$o_tree <- renderPlot(NULL)
    return()
  }
  
  cat("DEBUG tree rows:", nrow(local_table), "\n")
  
  ## Build edgelist
  edgelist <- local_table %>%
    distinct(suborder) %>%
    rename(to = suborder) %>%
    mutate(from = "Lepidoptera", .before = to)
  
  edgelist_1 <- local_table %>%
    distinct(suborder, family) %>%
    rename(from = suborder, to = family)
  
  edgelist_2 <- local_table %>%
    distinct(family, genus) %>%
    rename(from = family, to = genus)
  
  edgelist_3 <- local_table %>%
    distinct(genus, genus_and_species) %>%
    rename(from = genus, to = genus_and_species)
  
  edgelist <- bind_rows(edgelist, edgelist_1, edgelist_2, edgelist_3) %>%
    distinct(from, to)
  
  ## Build vertices dataframe
  nodelist <- data.frame(name = "Lepidoptera")
  nodelist <- bind_rows(
    nodelist,
    local_table %>% distinct(suborder) %>% rename(name = suborder),
    local_table %>% distinct(family) %>% rename(name = family),
    local_table %>% distinct(genus) %>% rename(name = genus),
    local_table %>% distinct(genus_and_species) %>% rename(name = genus_and_species)
  ) %>% distinct()
  
  ## Build graph
  tree <- graph_from_data_frame(d = edgelist, vertices = nodelist, directed = TRUE)
  
  plot_height <- if (nrow(nodelist) < 30) {
    150 + 10 * nrow(nodelist)
  } else {
    10 * nrow(nodelist)
  }
  
  ## Render tree
  output$o_tree <- renderPlot(height = plot_height, {
    ggraph(tree, "dendrogram") +
      coord_flip() +
      scale_y_reverse() +
      scale_x_reverse() +
      geom_edge_diagonal(show.legend = FALSE, colour = "black", edge_width = 1) +
      geom_node_label(
        aes(label = name),  # ✅ use `name`
        show.legend = FALSE,
        repel = FALSE,
        colour = "black",
        size = 3,
        fill = "orange",
        label.padding = unit(0.15, "lines")
      )
  })
})
