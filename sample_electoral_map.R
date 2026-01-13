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

# Allow debug/driver scripts to control behavior without modifying this file.
# - Set RUN_MAIN <- FALSE before sourcing this file to avoid auto-execution.
# - Set DEBUG_MAP <- TRUE before sourcing to enable any optional debug hooks (none by default).
if (!exists("RUN_MAIN")) RUN_MAIN <- TRUE
if (!exists("DEBUG_MAP")) DEBUG_MAP <- FALSE

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
  # Use full-resolution county boundaries for more reliable spatial joins
  ca_counties_boundaries <- counties(state = "CA", cb = FALSE, year = 2020) %>%
    filter(NAME %in% target_counties) %>%
    st_transform(crs = 3310) %>%
    st_make_valid()
  
  ca_tracts <- tracts(state = "CA", cb = FALSE, year = 2020) %>%
    st_transform(crs = 3310)
  
  # IMPORTANT: avoid spatial filtering here (it can silently drop whole counties due to edge/validity issues).
  # Tract shapes already include COUNTYFP, so filter deterministically by county FIPS.
  target_countyfp <- unique(ca_counties_boundaries$COUNTYFP)
  samnam_tracts <- ca_tracts %>%
    filter(COUNTYFP %in% target_countyfp) %>%
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
  
  # Assign COUNTY name from COUNTYFP (no spatial join required)
  samnam_tracts <- samnam_tracts %>%
    filter(!is.na(POPULATION) & POPULATION > 0) %>%
    left_join(
      ca_counties_boundaries %>%
        st_drop_geometry() %>%
        distinct(COUNTYFP, NAME) %>%
        transmute(COUNTYFP, COUNTY = NAME),
      by = "COUNTYFP"
    ) %>%
    filter(!is.na(COUNTY) & COUNTY %in% target_counties)

  # Optional sanity check kept as a warning (use debug_map.R for full diagnostics)
  key_counties <- c("San Francisco", "Santa Clara", "Sacramento")
  missing_key <- setdiff(key_counties, unique(samnam_tracts$COUNTY))
  if (length(missing_key) > 0) {
    warning(sprintf(
      "Some key counties are missing from tract data after county assignment: %s",
      paste(missing_key, collapse = ", ")
    ))
  }
  
  ca_places <- places(state = "CA", cb = FALSE, year = 2020) %>%
    st_transform(crs = 3310)
  
  sf_city_boundary <- ca_places %>% filter(NAME == "San Francisco") %>% st_make_valid()
  sac_city_boundary <- ca_places %>% filter(NAME == "Sacramento") %>% st_make_valid()
  sj_city_boundary <- ca_places %>% filter(grepl("^San Jose", NAME, ignore.case = TRUE)) %>% st_make_valid()
  
  if (nrow(sj_city_boundary) == 0) {
    sj_city_boundary <- ca_places %>%
      filter(NAME == "San Jose" | NAME == "San José") %>%
      st_make_valid()
  }

  # Union boundaries to single features so intersection logic is consistent
  if (nrow(sf_city_boundary) > 0) sf_city_boundary <- st_union(sf_city_boundary) %>% st_as_sf() %>% st_make_valid()
  if (nrow(sac_city_boundary) > 0) sac_city_boundary <- st_union(sac_city_boundary) %>% st_as_sf() %>% st_make_valid()
  if (nrow(sj_city_boundary) > 0) sj_city_boundary <- st_union(sj_city_boundary) %>% st_as_sf() %>% st_make_valid()
  
  samnam_tracts <- samnam_tracts %>%
    mutate(
      # Use sparse intersections and lengths() so multi-part boundaries can't break via [,1]
      IN_SF_CITY = lengths(st_intersects(geometry, sf_city_boundary)) > 0,
      IN_SAC_CITY = lengths(st_intersects(geometry, sac_city_boundary)) > 0,
      IN_SJ_CITY = lengths(st_intersects(geometry, sj_city_boundary)) > 0
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
    mutate(TARGET_DISTRICTS = ifelse(is.finite(POPULATION / ideal_pop_per_district), 
                                     round(POPULATION / ideal_pop_per_district), 0)) %>%
    mutate(TARGET_DISTRICTS = ifelse(is.na(TARGET_DISTRICTS) | TARGET_DISTRICTS < 1, 1, TARGET_DISTRICTS))
  
  allocate_city_districts <- function(city_pop, city_target, county_pop, ideal_pop) {
    if (is.na(city_pop) || is.na(county_pop) || is.na(ideal_pop) || ideal_pop <= 0) {
      return(list(city = 0, rest = 0, total = 0))
    }
    rest_pop <- county_pop - city_pop
    rest_target <- if (rest_pop > 0 && is.finite(ideal_pop)) max(1, round(rest_pop / ideal_pop)) else 0
    total <- city_target + rest_target
    if (is.na(total)) total <- 0
    list(city = city_target, rest = rest_target, total = total)
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

relabel_district_ids <- function(district_sf, district_counter) {
  old_ids <- sort(unique(district_sf$DISTRICT))
  original_ids <- district_sf$DISTRICT
  for (id in old_ids) {
    district_sf$DISTRICT[original_ids == id] <- district_counter
    district_counter <- district_counter + 1
  }
  list(result = district_sf, counter = district_counter)
}

district_within_county <- function(county_name, target_districts_county, all_tracts, ideal_pop, pre_filtered_tracts = NULL) {
  if (is.na(target_districts_county) || target_districts_county <= 0) {
    cat(sprintf("\nSkipping %s: invalid target districts (%s)\n", county_name, target_districts_county))
    return(NULL)
  }
  
  cat(sprintf("\nDistricting %s (target: %d districts)...\n", county_name, target_districts_county))
  
  county_tracts <- if (!is.null(pre_filtered_tracts)) pre_filtered_tracts else all_tracts %>% filter(COUNTY == county_name)
  if (nrow(county_tracts) == 0) {
    return(NULL)
  }
  
  district_assignment <- 1:nrow(county_tracts)
  total_pop_county <- sum(county_tracts$POPULATION, na.rm = TRUE)
  ideal_pop_county <- total_pop_county / target_districts_county
  
  if (is.na(ideal_pop_county) || !is.finite(ideal_pop_county) || ideal_pop_county <= 0) {
    cat(sprintf("Skipping %s: invalid ideal population calculation\n", county_name))
    return(NULL)
  }
  
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
  
  if (is.na(current_districts) || is.na(target_districts_county)) {
    cat(sprintf("Skipping %s: invalid district count calculation\n", county_name))
    return(NULL)
  }
  
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
    target_dist <- allocations$stats$TARGET_DISTRICTS[allocations$stats$COUNTY == county_name]
    if (length(target_dist) == 0 || is.na(target_dist) || target_dist <= 0) {
      return(list(result = NULL, counter = district_counter))
    }
    result <- district_within_county(county_name, target_dist, all_tracts, ideal_pop)
    if (!is.null(result)) {
      relabeled <- relabel_district_ids(result, district_counter)
      return(list(result = relabeled$result, counter = relabeled$counter))
    }
    return(list(result = NULL, counter = district_counter))
  }
  
  # NOTE: calculate_district_allocation() returns allocations under allocations$allocations
  alloc <- if (county_name == "San Francisco") allocations$allocations$sf
    else if (county_name == "Sacramento") allocations$allocations$sac
    else allocations$allocations$sj
  
  if (is.null(alloc) || is.na(alloc$city) || is.na(alloc$rest)) {
    cat(sprintf("Skipping %s: invalid allocation values\n", county_name))
    return(list(result = NULL, counter = district_counter))
  }
  
  if (county_name == "San Francisco") {
    city_tracts <- county_tracts %>% filter(dplyr::coalesce(IN_SF_CITY, FALSE))
    rest_tracts <- county_tracts %>% filter(!dplyr::coalesce(IN_SF_CITY, FALSE))
  } else if (county_name == "Sacramento") {
    city_tracts <- county_tracts %>% filter(dplyr::coalesce(IN_SAC_CITY, FALSE))
    rest_tracts <- county_tracts %>% filter(!dplyr::coalesce(IN_SAC_CITY, FALSE))
  } else {
    city_tracts <- county_tracts %>% filter(dplyr::coalesce(IN_SJ_CITY, FALSE))
    rest_tracts <- county_tracts %>% filter(!dplyr::coalesce(IN_SJ_CITY, FALSE))
  }
  
  results <- list()
  
  if (nrow(city_tracts) > 0 && !is.na(alloc$city) && alloc$city > 0) {
    city_result <- district_within_county(
      paste(county_name, "City"), alloc$city, all_tracts, ideal_pop, pre_filtered_tracts = city_tracts
    )
    if (!is.null(city_result)) {
      relabeled <- relabel_district_ids(city_result, district_counter)
      district_counter <- relabeled$counter
      results[[length(results) + 1]] <- relabeled$result
    }
  }
  
  if (nrow(rest_tracts) > 0 && !is.na(alloc$rest) && alloc$rest > 0) {
    rest_result <- district_within_county(
      paste(county_name, "County Rest"), alloc$rest, all_tracts, ideal_pop, pre_filtered_tracts = rest_tracts
    )
    if (!is.null(rest_result)) {
      relabeled <- relabel_district_ids(rest_result, district_counter)
      district_counter <- relabeled$counter
      results[[length(results) + 1]] <- relabeled$result
    }
  }
  
  # Fallback: if city/rest split produced nothing (e.g., boundary intersection failed),
  # district the whole county using the total allocation for that county.
  if (length(results) == 0) {
    target_total <- allocations$stats$TARGET_DISTRICTS[allocations$stats$COUNTY == county_name]
    if (length(target_total) == 0 || is.na(target_total) || target_total <= 0) {
      return(list(result = NULL, counter = district_counter))
    }
    fallback <- district_within_county(county_name, target_total, all_tracts, ideal_pop, pre_filtered_tracts = county_tracts)
    if (!is.null(fallback)) {
      relabeled <- relabel_district_ids(fallback, district_counter)
      return(list(result = relabeled$result, counter = relabeled$counter))
    }
    return(list(result = NULL, counter = district_counter))
  }

  list(result = bind_rows(results), counter = district_counter)
}

create_visualization <- function(final_districts, counties, city_boundaries) {
  bbox <- st_bbox(final_districts)
  
  # cowplot versions differ: some don't support clip= in draw_plot()
  draw_plot_safe <- function(plot, x, y, width, height) {
    if ("clip" %in% names(formals(cowplot::draw_plot))) {
      cowplot::draw_plot(plot, x = x, y = y, width = width, height = height, clip = "off")
    } else {
      cowplot::draw_plot(plot, x = x, y = y, width = width, height = height)
    }
  }

  # Show ALL districts on the main map - don't filter out city districts
  main_map <- ggplot() +
    geom_sf(data = final_districts, fill = "#EFEFEF", color = "white", linewidth = 0.1) +
    geom_sf(data = counties, fill = NA, color = "white", linewidth = 0.3) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    coord_sf(expand = FALSE, xlim = c(bbox[["xmin"]], bbox[["xmax"]]), ylim = c(bbox[["ymin"]], bbox[["ymax"]]))
  
  create_inset <- function(city_boundary, city_name) {
    # Union + validate so bbox/buffering is stable
    city_boundary <- st_union(city_boundary) %>% st_make_valid()

    city_districts <- st_filter(final_districts, city_boundary, .predicate = st_intersects)

    # Buffer in projected meters (EPSG:3310) so the city outline / strokes never get clipped
    buffered_bbox <- st_bbox(st_buffer(city_boundary, dist = 2000))

    ggplot() +
      geom_sf(data = city_districts, fill = "#EFEFEF", color = "white", linewidth = 0.15) +
      geom_sf(data = city_boundary, fill = NA, color = "white", linewidth = 0.4) +
      coord_sf(
        xlim = c(buffered_bbox[["xmin"]], buffered_bbox[["xmax"]]),
        ylim = c(buffered_bbox[["ymin"]], buffered_bbox[["ymax"]]),
        expand = FALSE
      ) +
      theme_void() +
      theme(
        # Avoid drawing borders that can get clipped differently between devices
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0, 0, 0, 0)
      )
  }
  
  sf_inset <- create_inset(city_boundaries$sf, "San Francisco")
  sj_inset <- create_inset(city_boundaries$sj, "San Jose")
  sac_inset <- create_inset(city_boundaries$sac, "Sacramento")
  
  # Place insets with a bit more breathing room from the canvas edge to avoid device clipping
  ggdraw() +
    draw_plot_safe(main_map, x = 0, y = 0, width = 1, height = 1) +
    draw_plot_safe(sf_inset, x = 0.03, y = 0.70, width = 0.22, height = 0.22) +
    draw_plot_safe(sj_inset, x = 0.03, y = 0.47, width = 0.22, height = 0.22) +
    draw_plot_safe(sac_inset, x = 0.03, y = 0.24, width = 0.22, height = 0.22)
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
  summarise(
    POPULATION = sum(POPULATION, na.rm = TRUE),
    COUNTY = dplyr::first(COUNTY),
    .groups = "drop"
  ) %>%
  mutate(DISTRICT_NUM = row_number()) %>%
  st_make_valid()

county_union <- spatial_data$counties %>% st_union() %>% st_make_valid()

final_districts <- final_districts %>%
  st_intersection(county_union) %>%
  st_make_valid() %>%
  filter(st_is_valid(geometry) & as.numeric(st_area(geometry)) > 0)

if (isTRUE(RUN_MAIN)) {
  cat(sprintf("\n=== DISTRICTING COMPLETE ===\n"))
  cat(sprintf("Final district count: %d (target: %d)\n", nrow(final_districts), target_districts))

  combined_map <- create_visualization(final_districts, spatial_data$counties, spatial_data$cities)

  cat("\n=== MAP GENERATION COMPLETE ===\n")
  print(combined_map)
}
