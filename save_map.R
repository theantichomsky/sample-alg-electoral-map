if (!exists("combined_map")) {
  stop("Please run sample_electoral_map.R first to generate the map.")
}

ggsave("samnam_electoral_map.png", 
       combined_map, 
       width = 16, 
       height = 12, 
       dpi = 300,
       bg = "white")

cat("Map saved as 'samnam_electoral_map.png'\n")

