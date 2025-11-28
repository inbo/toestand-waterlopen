library(sf)
library(dplyr) # For data manipulation if needed

# Define EPSG code for Lambert 72
lambert72_epsg <- 31370

# 1. Create a sample polyline and two points in Lambert 72 coordinates
# These coordinates are illustrative and are in meters.
# For example, values around 100,000 - 200,000 for X (Eastings) and Y (Northings)
# are typical for Belgium in Lambert 72.
line_coords_lambert72 <- matrix(c(
  150000, 160000, # Start X, Y (meters)
  155000, 165000,
  160000, 162000,
  165000, 168000,
  170000, 166000  # End X, Y (meters)
), ncol = 2, byrow = TRUE)

# Create the polyline as an sf object, specifying the Lambert 72 CRS
polyline_lambert72 <- st_sfc(st_linestring(line_coords_lambert72), crs = lambert72_epsg)

# Sample points (not necessarily on the polyline vertices)
# These points are also explicitly defined in Lambert 72 coordinates.
point1_lambert72 <- st_sfc(st_point(c(152000, 161500)), crs = lambert72_epsg)
point2_lambert72 <- st_sfc(st_point(c(167000, 167500)), crs = lambert72_epsg)

# Combine points into an sf object for easier processing
points_sf_lambert72 <- st_sf(id = c("A", "B"), geometry = c(point1_lambert72, point2_lambert72))

# Plot to visualize (optional)
# Crucially, we use 'axes = TRUE' to show the projected X/Y coordinates (meters)
# and omit 'graticule = TRUE' to prevent the display of latitude/longitude in degrees.
plot(polyline_lambert72, main = "Polyline and Points (Lambert 72 - EPSG:31370)",
     axes = TRUE) # This will show axes in meters (e.g., 1.5e+05 for 150,000m)
plot(points_sf_lambert72, add = TRUE, col = c("red", "blue"), pch = 19, cex = 1.5)
text(st_coordinates(points_sf_lambert72), labels = points_sf_lambert72$id, pos = 4)

# 2. Ensure consistent CRS (this step is always good practice for real data)
# In this example, we've explicitly set both to EPSG:31370, so no transformation is needed.
# if (st_crs(polyline_lambert72) != st_crs(points_sf_lambert72)) {
#   points_sf_lambert72 <- st_transform(points_sf_lambert72, st_crs(polyline_lambert72))
# }

# 3. Snap points to polyline and get their measure along the line
# Since the CRS is projected in meters, the result will be in meters.
measure1_lambert72 <- st_line_project(polyline_lambert72, point1_lambert72)
cat("\nMeasure of Point A along polyline:", round(as.numeric(measure1_lambert72), 2), "meters\n")

measure2_lambert72 <- st_line_project(polyline_lambert72, point2_lambert72)
cat("Measure of Point B along polyline:", round(as.numeric(measure2_lambert72), 2), "meters\n")

# To get the actual projected points on the line (optional, but good for visualization)
proj_point1_lambert72 <- st_line_interpolate(polyline_lambert72, measure1_lambert72)
proj_point2_lambert72 <- st_line_interpolate(polyline_lambert72, measure2_lambert72)

# Plot the projected points on the map
plot(proj_point1_lambert72, add = TRUE, col = "purple", pch = 8, cex = 1.5)
plot(proj_point2_lambert72, add = TRUE, col = "green", pch = 8, cex = 1.5)
text(st_coordinates(proj_point1_lambert72), labels = "Proj A", pos = 1, col = "purple")
text(st_coordinates(proj_point2_lambert72), labels = "Proj B", pos = 1, col = "green")


# 4. Calculate the distance between the two points along the line
# This will also be in meters, consistent with the CRS.
distance_along_line_lambert72 <- abs(as.numeric(measure1_lambert72) - as.numeric(measure2_lambert72))

cat("Distance between points along the polyline:", round(distance_along_line_lambert72, 2), "meters\n")

# --- Function to extract the segment for visualization ---
# This function is designed to be CRS-agnostic; it works correctly as long as
# the input 'sf' objects have a defined CRS (which they do here, as Lambert 72).
get_segment_between_projected_points_corrected <- function(line_sf, proj_p1_sf, proj_p2_sf) {
  m1 <- as.numeric(st_line_project(line_sf, proj_p1_sf))
  m2 <- as.numeric(st_line_project(line_sf, proj_p2_sf))

  seg_start_m <- min(m1, m2)
  seg_end_m <- max(m1, m2)

  original_vertices_sf <- st_cast(line_sf, "POINT")
  measures_of_vertices <- as.numeric(st_line_project(line_sf, original_vertices_sf))

  proj_p1_coords <- st_coordinates(proj_p1_sf)[, 1:2]
  proj_p2_coords <- st_coordinates(proj_p2_sf)[, 1:2]

  segment_coords_list <- list()
  segment_coords_list[[length(segment_coords_list) + 1]] <- proj_p1_coords

  for (i in seq_along(measures_of_vertices)) {
    if (measures_of_vertices[i] > seg_start_m && measures_of_vertices[i] < seg_end_m) {
      segment_coords_list[[length(segment_coords_list) + 1]] <- st_coordinates(original_vertices_sf[i, ])[, 1:2]
    }
  }

  segment_coords_list[[length(segment_coords_list) + 1]] <- proj_p2_coords

  segment_coords_matrix <- do.call(rbind, segment_coords_list)
  segment_coords_matrix <- unique(segment_coords_matrix)

  temp_segment_points <- st_sfc(lapply(1:nrow(segment_coords_matrix), function(i) st_point(segment_coords_matrix[i, ])), crs = st_crs(line_sf))
  temp_measures <- as.numeric(st_line_project(line_sf, temp_segment_points))

  ordered_indices <- order(temp_measures)
  ordered_segment_coords_matrix <- segment_coords_matrix[ordered_indices, ]

  if (nrow(ordered_segment_coords_matrix) >= 2) {
    segment_linestring <- st_sfc(st_linestring(ordered_segment_coords_matrix), crs = st_crs(line_sf))
  } else {
    warning("Could not create a valid linestring segment between the points. Possibly too few unique points or points are too close.")
    segment_linestring <- st_sfc(st_linestring(), crs = st_crs(line_sf))
  }
  return(segment_linestring)
}

# Example of using the custom function to get the segment geometry
segment_between_points_geom_lambert72 <- get_segment_between_projected_points_corrected(
  polyline_lambert72, proj_point1_lambert72, proj_point2_lambert72
)

# Plot the segment on the map
plot(segment_between_points_geom_lambert72, col = "darkorange", lwd = 3, add = TRUE)
# Verify its length matches the direct calculation
cat("Length of the segment calculated from geometry (should match direct measure diff):",
    round(as.numeric(st_length(segment_between_points_geom_lambert72)), 2), "meters\n")
