# --- STEP 0: ENSURE PACKAGES ARE INSTALLED AND LOADED ---
# If you haven't installed these packages before, uncomment and run:
# install.packages("sf")
# install.packages("dplyr")
# install.packages("sfnetworks") # This package is essential for network analysis

# --- IMPORTANT: If you encounter errors like 'unused argument (tolerance)' or
# --- 'formal argument "output" matched by multiple actual arguments', it indicates
# --- your sfnetworks package is an older version. This code is adjusted to work
# --- around such versions, but updating is always recommended for full functionality:
# --- install.packages("sfnetworks", dependencies = TRUE)
# --- install.packages("igraph", dependencies = TRUE)
# --- Then restart your R session.

# Load the necessary libraries for your current R session
library(sf, quietly = TRUE)      # 'quietly = TRUE' suppresses startup messages
library(dplyr, quietly = TRUE)   # Used for %>% and data manipulation
library(sfnetworks, quietly = TRUE) # Essential for network analysis


# Define EPSG code for Lambert 72 (Belgian National Projection)
lambert72_epsg <- 31370

# 1. Create a network of multiple connected segments
# These coordinates are illustrative of Lambert 72 (in meters)
seg1_coords <- matrix(c(
  150000, 160000, # Start point (Node A)
  155000, 165000,
  160000, 160000  # Intersection Node X
), ncol = 2, byrow = TRUE)

seg2_coords <- matrix(c(
  160000, 160000, # Intersection Node X
  165000, 165000,
  170000, 160000  # End point (Node B)
), ncol = 2, byrow = TRUE)

seg3_coords <- matrix(c(
  160000, 160000, # Intersection Node X
  160000, 155000,
  155000, 150000  # End point (Node C)
), ncol = 2, byrow = TRUE)

network_lines_sfc <- st_sfc(
  st_linestring(seg1_coords),
  st_linestring(seg2_coords),
  st_linestring(seg3_coords),
  crs = lambert72_epsg
)
network_sf_edges <- st_sf(id = 1:length(network_lines_sfc), geometry = network_lines_sfc)


# 2. Convert to an sfnetwork object and add weights using edge_length()
# Using 'directed = FALSE' for a bi-directional network.
# 'edge_length()' is an sfnetworks function to calculate length of edges.
net <- as_sfnetwork(network_sf_edges, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight = edge_length()) # This automatically calculates lengths in meters


# 3. Define the two points for which we want to find the path
# These points are precisely located on the network lines (not necessarily nodes).
point1_lambert72 <- st_sfc(st_point(c(152500, 162500)), crs = lambert72_epsg) # On Segment 1
point2_lambert72 <- st_sfc(st_point(c(167500, 162500)), crs = lambert72_epsg) # On Segment 2

points_sf <- st_sf(id = c("A", "B"), geometry = c(point1_lambert72, point2_lambert72))

# --- Plotting to visualize the network and points ---
# Set graphical parameters (like mar) to ensure plots look good.
oldpar <- par(no.readonly = TRUE) # Save current par settings
par(mar = c(1,1,1,1)) # Set margins (bottom, left, top, right)

# Calculate a combined bounding box to ensure all features are visible.
combined_bbox <- st_bbox(c(st_bbox(st_as_sf(net, "edges")), st_bbox(points_sf)))

plot(net, main = "Network Path (Points on Lines) - Lambert 72",
     axes = TRUE, col = "grey", lwd = 2, # Plot edges in grey
     xlim = combined_bbox[c("xmin", "xmax")], # Set X-axis limits
     ylim = combined_bbox[c("ymin", "ymax")]) # Set Y-axis limits

plot(points_sf, col = c("red", "blue"), pch = 19, cex = 1.5, add = TRUE)
text(st_coordinates(points_sf), labels = points_sf$id, pos = 4, col = c("red", "blue"), cex = 0.9)

# Plot all network nodes (intersections and endpoints)
plot(st_as_sf(net, "nodes"), col = "black", pch = 16, cex = 0.8, add = TRUE)


# 4. Find the shortest path using st_network_paths
# 'weights = "weight"' uses the edge lengths calculated earlier.
# We are NOT using 'output = "paths"' nor 'tolerance' due to potential version issues.
shortest_path_result <- st_network_paths(net, from = point1_lambert72, to = point2_lambert72,
                                         weights = "weight")

# --- DIAGNOSTIC PRINTS FOR PATHFINDING RESULT ---
cat("\n--- Debugging shortest_path_result ---\n")
print(shortest_path_result) # Should now be a tibble with node_paths and edge_paths
cat("Number of paths found:", nrow(shortest_path_result), "\n")
cat("----------------------------------\n")


# Check if a path was found and has valid edge_paths data
if (nrow(shortest_path_result) > 0 &&
    !is.null(shortest_path_result$edge_paths) &&
    length(shortest_path_result$edge_paths[[1]]) > 0) { # Ensure there are edges in the path

  # --- CRITICAL STEP: Manually reconstruct the path geometry ---
  # This part is necessary because 'st_network_paths' (in your version)
  # returns edge IDs, not the combined LINESTRING geometry directly.

  # Extract the edge IDs of the first (and usually only) path
  path_edge_ids <- shortest_path_result$edge_paths[[1]]

  # Get the geometries of these specific edges from the network
  # and combine them into a single LINESTRING representing the path.
  path_geometry <- net %>%
    activate("edges") %>%
    slice(path_edge_ids) %>%     # Select the edges that form the path using their IDs
    st_geometry() %>%            # Extract their geometries (sfg objects within an sfc list)
    st_union() %>%               # Union them to combine into a single (possibly MULTILINESTRING) geometry
    st_cast("LINESTRING")        # Ensure the result is a single LINESTRING (if it's a MULTILINESTRING,
                                 # this will attempt to combine segments if possible, or take the first part)


  # 5. Calculate the total length of this shortest path
  total_path_length <- st_length(path_geometry)

  cat("\nShortest distance along the network between points:", round(as.numeric(total_path_length), 2), "meters\n")

  # Plot the calculated shortest path on the map
  plot(path_geometry, col = "darkorange", lwd = 3, add = TRUE)
} else {
  cat("\n!!!! No path found between the points or path data is incomplete. !!!!\n")
  cat("  This might be due to:\n")
  cat("  - Points not being close enough to the network (sfnetworks' default snapping tolerance might be too small).\n")
  cat("  - The network segments not being truly connected (check shared coordinates at intersections).\n")
  cat("  - The path being genuinely not traversable within the network.\n")
  cat("  Please examine the 'shortest_path_result' output above for more details.\n")
  cat("  Consider updating 'sfnetworks' and 'igraph' for improved snapping functionality (using 'tolerance').\n")
}

# Restore original par settings
par(oldpar)
