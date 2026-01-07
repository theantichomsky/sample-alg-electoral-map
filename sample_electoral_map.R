required_packages <- c("sf", "tigris", "tidycensus", "dplyr", "ggplot2", "cowplot", "classInt")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  tryCatch({
    install.packages(missing_packages, dependencies = TRUE, repos = "https://cloud.r-project.org")
  }, error = function(e) {
    stop("Required packages not installed.")
  })
}

library(sf)
library(tigris)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(cowplot)
library(classInt)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

target_counties <- c(
  "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", 
  "San Mateo", "Santa Clara", "Solano", "Sonoma",
  "Sacramento", "San Joaquin", "Yolo", "Placer", "El Dorado", 
  "Yuba", "Sutter", "Butte", "Glenn", "Colusa", "Lake",
  "Santa Cruz", "San Benito", "Stanislaus", "Calaveras", 
  "Amador", "Sierra", "Plumas",
  "Mendocino", "Tehama", "Nevada"
)

load_spatial_data <- function() {
  ca_counties_boundaries <- counties(state = "CA", cb = TRUE, year = 2020) %>%
    filter(NAME %in% target_counties) %>%
    st_transform(crs = 3310)
  
  ca_tracts <- tracts(state = "CA", cb = FALSE, year = 2020) %>%
    st_transform(crs = 3310)
  
  samnam_tracts <- ca_tracts %>%
    st_filter(ca_counties_boundaries, .predicate = st_intersects) %>%
    st_make_valid()
  
  tryCatch({
    ca_pop <- get_acs(
      geography = "tract",
      variables = "B01003_001",
      state = "CA",
      year = 2020,
      geometry = FALSE
    )
    
    samnam_tracts <- samnam_tracts %>%
      left_join(
        ca_pop %>% select(GEOID, estimate) %>% rename(POPULATION = estimate),
        by = "GEOID"
      )
  }, error = function(e) {
    stop("Could not fetch population data. Please set your Census API key.")
  })
  
  samnam_tracts <- samnam_tracts %>%
    filter(!is.na(POPULATION) & POPULATION > 0) %>%
    st_transform(crs = 3310) %>%
    st_join(
      ca_counties_boundaries %>% select(NAME) %>% rename(COUNTY = NAME),
      left = FALSE,
      largest = TRUE
    )
  
  ca_places <- places(state = "CA", cb = FALSE, year = 2020) %>%
    st_transform(crs = 3310)
  
  sf_city_boundary <- ca_places %>% filter(NAME == "San Francisco")
  sac_city_boundary <- ca_places %>% filter(NAME == "Sacramento")
  sj_city_boundary <- ca_places %>% filter(grepl("^San Jose", NAME, ignore.case = TRUE))
  
  if (nrow(sj_city_boundary) == 0) {
    sj_city_boundary <- ca_places %>%
      filter(NAME == "San Jose" | NAME == "San José")
  }
  
  samnam_tracts <- samnam_tracts %>%
    mutate(
      IN_SF_CITY = st_intersects(geometry, sf_city_boundary, sparse = FALSE)[,1],
      IN_SAC_CITY = st_intersects(geometry, sac_city_boundary, sparse = FALSE)[,1],
      IN_SJ_CITY = st_intersects(geometry, sj_city_boundary, sparse = FALSE)[,1]
    )
  
  list(
    tracts = samnam_tracts,
    counties = ca_counties_boundaries,
    cities = list(sf = sf_city_boundary, sac = sac_city_boundary, sj = sj_city_boundary)
  )
}

calculate_district_allocation <- function(tracts, target_districts) {
  total_pop <- sum(tracts$POPULATION, na.rm = TRUE)
  ideal_pop_per_district <- total_pop / target_districts
  
  county_stats <- tracts %>%
    st_drop_geometry() %>%
    group_by(COUNTY) %>%
    summarise(POPULATION = sum(POPULATION, na.rm = TRUE), N_TRACTS = n(), .groups = "drop") %>%
    mutate(TARGET_DISTRICTS = round(POPULATION / ideal_pop_per_district))
  
  allocate_city_districts <- function(city_pop, city_target, county_pop, ideal_pop) {
    rest_pop <- county_pop - city_pop
    rest_target <- if (rest_pop > 0) max(1, round(rest_pop / ideal_pop)) else 0
    list(city = city_target, rest = rest_target, total = city_target + rest_target)
  }
  
  sf_alloc <- allocate_city_districts(
    sum(tracts$POPULATION[tracts$IN_SF_CITY], na.rm = TRUE), 8,
    county_stats$POPULATION[county_stats$COUNTY == "San Francisco"],
    ideal_pop_per_district
  )
  
  sac_alloc <- allocate_city_districts(
    sum(tracts$POPULATION[tracts$IN_SAC_CITY], na.rm = TRUE), 8,
    county_stats$POPULATION[county_stats$COUNTY == "Sacramento"],
    ideal_pop_per_district
  )
  
  sj_alloc <- allocate_city_districts(
    sum(tracts$POPULATION[tracts$IN_SJ_CITY], na.rm = TRUE),
    round(sum(tracts$POPULATION[tracts$IN_SJ_CITY], na.rm = TRUE) / ideal_pop_per_district),
    county_stats$POPULATION[county_stats$COUNTY == "Santa Clara"],
    ideal_pop_per_district
  )
  
  county_stats$TARGET_DISTRICTS[county_stats$COUNTY == "San Francisco"] <- sf_alloc$total
  county_stats$TARGET_DISTRICTS[county_stats$COUNTY == "Sacramento"] <- sac_alloc$total
  county_stats$TARGET_DISTRICTS[county_stats$COUNTY == "Santa Clara"] <- sj_alloc$total
  
  difference <- target_districts - sum(county_stats$TARGET_DISTRICTS)
  
  if (difference != 0) {
    other_counties <- county_stats %>%
      filter(COUNTY != "San Francisco" & COUNTY != "Sacramento") %>%
      arrange(if (difference > 0) desc(POPULATION) else POPULATION)
    
    for (i in 1:min(abs(difference), nrow(other_counties))) {
      current <- county_stats$TARGET_DISTRICTS[county_stats$COUNTY == other_counties$COUNTY[i]]
      if (difference > 0 || current > 1) {
        county_stats$TARGET_DISTRICTS[county_stats$COUNTY == other_counties$COUNTY[i]] <- 
          current + sign(difference)
      }
    }
  }
  
  list(
    stats = county_stats,
    allocations = list(sf = sf_alloc, sac = sac_alloc, sj = sj_alloc),
    ideal_pop = ideal_pop_per_district
  )
}

district_within_county <- function(county_name, target_districts_county, all_tracts, ideal_pop, pre_filtered_tracts = NULL) {
  cat(sprintf("\nDistricting %s (target: %d districts)...\n", county_name, target_districts_county))
  
  county_tracts <- if (!is.null(pre_filtered_tracts)) pre_filtered_tracts else all_tracts %>% filter(COUNTY == county_name)
  if (nrow(county_tracts) == 0) return(NULL)
  
  district_assignment <- 1:nrow(county_tracts)
  ideal_pop_county <- sum(county_tracts$POPULATION, na.rm = TRUE) / target_districts_county
  adjacency_sparse <- st_touches(county_tracts, sparse = TRUE)
  
  find_adjacent_districts <- function(dist_id, assignment) {
    tracts_in_dist <- which(assignment == dist_id)
    adjacent_tract_ids <- unique(unlist(adjacency_sparse[tracts_in_dist]))
    if (length(adjacent_tract_ids) > 0) unique(assignment[adjacent_tract_ids]) else integer(0)
  }
  
  calc_district_pops <- function(assignment) {
    county_tracts %>%
      st_drop_geometry() %>%
      mutate(DISTRICT = assignment) %>%
      group_by(DISTRICT) %>%
      summarise(POP = sum(POPULATION, na.rm = TRUE), .groups = "drop") %>%
      pull(POP)
  }
  
  current_districts <- length(unique(district_assignment))
  iteration <- 0
  
  while (current_districts > target_districts_county && iteration < 10000) {
    iteration <- iteration + 1
    if (iteration %% 200 == 0) {
      cat(sprintf("  Iteration %d: %d districts remaining\n", iteration, current_districts))
    }
    
    district_pops <- calc_district_pops(district_assignment)
    unique_districts <- unique(district_assignment)
    
    district_df <- data.frame(DISTRICT = unique_districts, POP = district_pops) %>%
      filter(POP < ideal_pop_county * 1.2) %>%
      arrange(POP)
    
    if (nrow(district_df) == 0) break
    
    best_merge <- NULL
    best_score <- Inf
    
    for (i in 1:min(200, nrow(district_df))) {
      dist1_id <- district_df$DISTRICT[i]
      dist1_pop <- district_df$POP[i]
      if (dist1_pop >= ideal_pop_county * 0.9) next
      
      adj_districts <- find_adjacent_districts(dist1_id, district_assignment)
      adj_districts <- adj_districts[adj_districts != dist1_id]
      
      if (length(adj_districts) > 0) {
        adj_pops <- data.frame(DISTRICT = unique_districts, POP = district_pops) %>%
          filter(DISTRICT %in% adj_districts) %>%
          arrange(POP)
        
        for (j in 1:nrow(adj_pops)) {
          dist2_pop <- adj_pops$POP[j]
          combined_pop <- dist1_pop + dist2_pop
          
          if (combined_pop <= ideal_pop_county * 2.0) {
            score <- abs(combined_pop - ideal_pop_county) + ((dist1_pop + dist2_pop) / ideal_pop_county * 0.1)
            if (score < best_score) {
              best_score <- score
              best_merge <- list(dist1 = dist1_id, dist2 = adj_pops$DISTRICT[j])
            }
          }
        }
      }
    }
    
    if (is.null(best_merge)) {
      smallest_dist <- district_df$DISTRICT[1]
      adj_districts <- find_adjacent_districts(smallest_dist, district_assignment)
      adj_districts <- adj_districts[adj_districts != smallest_dist]
      
      if (length(adj_districts) > 0) {
        adj_pops <- district_df %>% filter(DISTRICT %in% adj_districts) %>% arrange(POP)
        if (nrow(adj_pops) > 0) {
          best_merge <- list(dist1 = smallest_dist, dist2 = adj_pops$DISTRICT[1])
        } else {
          break
        }
      } else {
        break
      }
    }
    
    district_assignment[district_assignment == best_merge$dist2] <- best_merge$dist1
    current_districts <- length(unique(district_assignment))
  }
  
  while (current_districts > target_districts_county) {
    district_pops <- calc_district_pops(district_assignment)
    unique_districts <- unique(district_assignment)
    district_df <- data.frame(DISTRICT = unique_districts, POP = district_pops) %>% arrange(POP)
    
    smallest_dist <- district_df$DISTRICT[1]
    adj_districts <- find_adjacent_districts(smallest_dist, district_assignment)
    adj_districts <- adj_districts[adj_districts != smallest_dist]
    
    if (length(adj_districts) == 0) break
    
    adj_pops <- district_df %>% filter(DISTRICT %in% adj_districts) %>% arrange(POP)
    if (nrow(adj_pops) == 0) break
    
    district_assignment[district_assignment == adj_pops$DISTRICT[1]] <- smallest_dist
    current_districts <- length(unique(district_assignment))
    
    if (current_districts %% 5 == 0) {
      cat(sprintf("  Final merge: %d districts remaining\n", current_districts))
    }
  }
  
  county_tracts %>% mutate(DISTRICT = district_assignment)
}

process_county_with_city <- function(county_name, tracts, allocations, all_tracts, ideal_pop, district_counter) {
  county_tracts <- tracts %>% filter(COUNTY == county_name)
  
  if (!county_name %in% c("San Francisco", "Sacramento", "Santa Clara")) {
    result <- district_within_county(county_name, allocations$stats$TARGET_DISTRICTS[allocations$stats$COUNTY == county_name], 
                                     all_tracts, ideal_pop)
    if (!is.null(result)) {
      unique_districts <- unique(result$DISTRICT)
      for (dist_id in unique_districts) {
        result$DISTRICT[result$DISTRICT == dist_id] <- district_counter
        district_counter <- district_counter + 1
      }
      return(list(result = result, counter = district_counter))
    }
    return(list(result = NULL, counter = district_counter))
  }
  
  alloc <- if (county_name == "San Francisco") allocations$sf
    else if (county_name == "Sacramento") allocations$sac
    else allocations$sj
  
  if (county_name == "San Francisco") {
    city_tracts <- county_tracts %>% filter(IN_SF_CITY == TRUE)
    rest_tracts <- county_tracts %>% filter(IN_SF_CITY == FALSE)
  } else if (county_name == "Sacramento") {
    city_tracts <- county_tracts %>% filter(IN_SAC_CITY == TRUE)
    rest_tracts <- county_tracts %>% filter(IN_SAC_CITY == FALSE)
  } else {
    city_tracts <- county_tracts %>% filter(IN_SJ_CITY == TRUE)
    rest_tracts <- county_tracts %>% filter(IN_SJ_CITY == FALSE)
  }
  
  results <- list()
  
  if (nrow(city_tracts) > 0) {
    city_result <- district_within_county(
      paste(county_name, "City"), alloc$city, all_tracts, ideal_pop, pre_filtered_tracts = city_tracts
    )
    if (!is.null(city_result)) {
      unique_districts <- unique(city_result$DISTRICT)
      for (dist_id in unique_districts) {
        city_result$DISTRICT[city_result$DISTRICT == dist_id] <- district_counter
        district_counter <- district_counter + 1
      }
      results[[length(results) + 1]] <- city_result
    }
  }
  
  if (nrow(rest_tracts) > 0 && alloc$rest > 0) {
    rest_result <- district_within_county(
      paste(county_name, "County Rest"), alloc$rest, all_tracts, ideal_pop, pre_filtered_tracts = rest_tracts
    )
    if (!is.null(rest_result)) {
      unique_districts <- unique(rest_result$DISTRICT)
      for (dist_id in unique_districts) {
        rest_result$DISTRICT[rest_result$DISTRICT == dist_id] <- district_counter
        district_counter <- district_counter + 1
      }
      results[[length(results) + 1]] <- rest_result
    }
  }
  
  list(result = if (length(results) > 0) bind_rows(results) else NULL, counter = district_counter)
}

create_visualization <- function(final_districts, counties, city_boundaries) {
  bbox <- st_bbox(final_districts)
  
  sac_city_districts <- st_filter(final_districts, city_boundaries$sac, .predicate = st_intersects)
  sj_city_districts <- st_filter(final_districts, city_boundaries$sj, .predicate = st_intersects)
  other_districts <- final_districts %>%
    filter(!DISTRICT_NUM %in% c(sac_city_districts$DISTRICT_NUM, sj_city_districts$DISTRICT_NUM))
  
  main_map <- ggplot() +
    geom_sf(data = sj_city_districts, fill = "#EFEFEF", color = "white", linewidth = 0.1) +
    geom_sf(data = sac_city_districts, fill = "#EFEFEF", color = "white", linewidth = 0.1) +
    geom_sf(data = other_districts, fill = "#EFEFEF", color = "white", linewidth = 0.1) +
    geom_sf(data = counties, fill = NA, color = "white", linewidth = 0.3) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    coord_sf(expand = FALSE, xlim = c(bbox[["xmin"]], bbox[["xmax"]]), ylim = c(bbox[["ymin"]], bbox[["ymax"]]))
  
  create_inset <- function(city_boundary, city_name) {
    city_bbox <- st_bbox(city_boundary)
    city_districts <- st_filter(final_districts, city_boundary, .predicate = st_intersects)
    
    ggplot() +
      geom_sf(data = city_districts, fill = "#EFEFEF", color = "white", linewidth = 0.15) +
      geom_sf(data = city_boundary, fill = NA, color = "white", linewidth = 0.4) +
      coord_sf(xlim = c(city_bbox["xmin"], city_bbox["xmax"]),
               ylim = c(city_bbox["ymin"], city_bbox["ymax"]), expand = FALSE) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
            panel.background = element_rect(fill = "white", color = NA))
  }
  
  sf_inset <- create_inset(city_boundaries$sf, "San Francisco")
  sj_inset <- create_inset(city_boundaries$sj, "San Jose")
  sac_inset <- create_inset(city_boundaries$sac, "Sacramento")
  
  ggdraw() +
    draw_plot(main_map, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(sf_inset, x = 0.02, y = 0.70, width = 0.22, height = 0.22) +
    draw_plot(sj_inset, x = 0.02, y = 0.47, width = 0.22, height = 0.22) +
    draw_plot(sac_inset, x = 0.02, y = 0.24, width = 0.22, height = 0.22)
}

spatial_data <- load_spatial_data()
target_districts <- 269
allocations <- calculate_district_allocation(spatial_data$tracts, target_districts)

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
  }
  district_counter <- result$counter
}

final_districts <- bind_rows(all_county_districts) %>%
  group_by(DISTRICT) %>%
  summarise(POPULATION = sum(POPULATION, na.rm = TRUE), .groups = "drop") %>%
  mutate(DISTRICT_NUM = row_number()) %>%
  st_make_valid()

county_union <- spatial_data$counties %>% st_union() %>% st_make_valid()

final_districts <- final_districts %>%
  st_intersection(county_union) %>%
  st_make_valid() %>%
  filter(st_is_valid(geometry) & as.numeric(st_area(geometry)) > 0)

cat(sprintf("\n=== DISTRICTING COMPLETE ===\n"))
cat(sprintf("Final district count: %d (target: %d)\n", nrow(final_districts), target_districts))

combined_map <- create_visualization(final_districts, spatial_data$counties, spatial_data$cities)

cat("\n=== MAP GENERATION COMPLETE ===\n")
print(combined_map)
