## debug_map.R
##
## Diagnostics runner for sample_electoral_map.R
## - Keeps sample_electoral_map.R clean
## - Produces debug_coverage_gap.png showing missing coverage in red
##
## Usage (R console / RStudio):
##   source("debug_map.R")
##

RUN_MAIN <- FALSE
DEBUG_MAP <- TRUE

source("sample_electoral_map.R")

cat("\n=== DEBUG RUN START ===\n")

key_counties <- c("San Francisco", "Santa Clara", "Sacramento")

cat("\n=== DEBUG: do key counties exist in spatial_data$tracts? ===\n")
print(
  spatial_data$tracts %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(COUNTY %in% key_counties) %>%
    dplyr::count(COUNTY, sort = TRUE)
)

cat("\n=== DEBUG: do key counties exist in allocations$stats? ===\n")
print(
  allocations$stats %>%
    dplyr::as_tibble() %>%
    dplyr::filter(COUNTY %in% key_counties) %>%
    dplyr::select(COUNTY, POPULATION, N_TRACTS, TARGET_DISTRICTS)
)

county_run_log <- data.frame(
  COUNTY = character(0),
  produced_rows = integer(0),
  produced_district_ids = integer(0),
  stringsAsFactors = FALSE
)

all_county_districts <- list()
district_counter <- 1

for (i in 1:nrow(allocations$stats)) {
  county_name <- allocations$stats$COUNTY[i]
  result <- process_county_with_city(
    county_name, spatial_data$tracts, allocations, spatial_data$tracts,
    allocations$ideal_pop, district_counter
  )

  if (!is.null(result$result)) {
    all_county_districts[[length(all_county_districts) + 1]] <- result$result
    county_run_log <- rbind(
      county_run_log,
      data.frame(
        COUNTY = county_name,
        produced_rows = nrow(result$result),
        produced_district_ids = length(unique(result$result$DISTRICT)),
        stringsAsFactors = FALSE
      )
    )
  } else {
    county_run_log <- rbind(
      county_run_log,
      data.frame(
        COUNTY = county_name,
        produced_rows = 0L,
        produced_district_ids = 0L,
        stringsAsFactors = FALSE
      )
    )
  }
  district_counter <- result$counter
}

cat("\n=== DEBUG: key counties in county_run_log ===\n")
print(
  county_run_log %>%
    dplyr::as_tibble() %>%
    dplyr::filter(COUNTY %in% key_counties)
)

all_tract_districts <- dplyr::bind_rows(all_county_districts)

cat("\n=== DEBUG: key counties in all_tract_districts (tract rows) ===\n")
print(
  all_tract_districts %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(COUNTY %in% key_counties) %>%
    dplyr::count(COUNTY, sort = TRUE)
)

final_districts <- all_tract_districts %>%
  dplyr::group_by(DISTRICT) %>%
  dplyr::summarise(
    POPULATION = sum(POPULATION, na.rm = TRUE),
    COUNTY = dplyr::first(COUNTY),
    .groups = "drop"
  ) %>%
  dplyr::mutate(DISTRICT_NUM = dplyr::row_number()) %>%
  sf::st_make_valid()

county_union <- spatial_data$counties %>% sf::st_union() %>% sf::st_make_valid()

final_districts <- final_districts %>%
  sf::st_intersection(county_union) %>%
  sf::st_make_valid() %>%
  dplyr::filter(sf::st_is_valid(geometry) & as.numeric(sf::st_area(geometry)) > 0)

cat("\n=== DEBUG: final_districts counts for key counties ===\n")
print(
  final_districts %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(COUNTY %in% key_counties) %>%
    dplyr::count(COUNTY, sort = TRUE)
)

districts_union <- tryCatch(sf::st_union(final_districts) %>% sf::st_make_valid(), error = function(e) NULL)
if (!is.null(districts_union)) {
  gap <- tryCatch(sf::st_difference(county_union, districts_union) %>% sf::st_make_valid(), error = function(e) NULL)
  if (!is.null(gap)) {
    gap_area <- as.numeric(sf::st_area(gap))
    cat(sprintf("\n=== DEBUG: uncovered area (sq meters) inside county_union: %.0f ===\n", gap_area))

    debug_plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = county_union, fill = "grey92", color = "grey60", linewidth = 0.2) +
      ggplot2::geom_sf(data = final_districts, fill = "grey75", color = NA, alpha = 0.9) +
      ggplot2::geom_sf(data = gap, fill = "red", color = NA, alpha = 0.6) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))

    ggplot2::ggsave("debug_coverage_gap.png", debug_plot, width = 10, height = 10, dpi = 200, bg = "white")
    cat("DEBUG: Wrote debug_coverage_gap.png (red = missing coverage)\n")
  }
}

cat("\n=== DEBUG RUN COMPLETE ===\n")


