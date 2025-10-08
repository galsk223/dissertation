k <- 3
outlier_trim <- 0.01
library(kernlab)   # for specc
library(factoextra)
library(cluster)

fleet_summary_emb <- function(gy, outlier_trim = 0.01, k = 5) {
  # emb: n x 2 embedding (rows = vessels)
  emb <- embed_laplacian_matrix(gy, no = 2, type = "dad")
  coords <- as.data.frame(emb$X)
  n <- nrow(coords)
  
  # centroid & distances
  centroid <- colMeans(coords)
  d_to_centroid <- sqrt(rowSums((coords - 
                                   matrix(centroid, n, ncol(coords), 
                                          byrow = TRUE))^2))
  median_d_centroid <- median(d_to_centroid, na.rm = TRUE)
  mean_d_centroid   <- mean(d_to_centroid, na.rm = TRUE)
  
  # convex hull area (2D volume)
  hull_area <- NA
  hull_area_trimmed <- NA
  if (n >= 3) {
    hull_area <- tryCatch({
      ch <- geometry::convhulln(coords, options = "FA")
      ch$area
    }, error = function(e) 0)
    
    # trimmed hull: drop top outlier fraction by distance
    keep <- order(d_to_centroid)[1:floor((1 - outlier_trim) * n)]
    hull_area_trimmed <- if (length(keep) > 2) {
      tryCatch({
        geometry::convhulln(coords[keep, , drop = FALSE], options = "FA")$area
      }, error = function(e) 0)
    } else {
      0
    }
  } else {
    hull_area <- 0
    hull_area_trimmed <- 0
  }
  
  list(
    hull_area = hull_area,
    hull_area_trimmed = hull_area_trimmed,
    median_dist_to_centroid = median_d_centroid,
    mean_dist_to_centroid = mean_d_centroid
    # n = n
  )
}

fleet_summary_emb_safe <- function(gy, outlier_trim = 0.01, k = 5) {
  tryCatch(
    fleet_summary_emb(gy, outlier_trim = outlier_trim, k = k),
    error = function(e) {
      message("fleet_summary_emb failed: ", e$message)
      # Return safe defaults
      list(
        hull_area = 0,
        hull_area_trimmed = 0,
        median_dist_to_centroid = NA,
        mean_dist_to_centroid = NA
      )
    }
  )
}

# 
# df <- as.data.frame(coords)
# colnames(df) <- c("X1", "X2")
# 
# sil <- sapply(2:8, function(k) {
#   sc <- specc(as.matrix(coords), centers = k)
#   sil_vals <- silhouette(as.numeric(sc), dist(coords))
#   mean(sil_vals[!table(sc)[sc] == 1, 3])
#   # mean(silhouette(as.numeric(sc), dist(coords))[, 3])
# })
# 
# best_k <- which.max(sil) + 1
# 
# # Run spectral clustering
# set.seed(123)
# sc <- specc(as.matrix(coords), centers = best_k)  # best_k from silhouette search
# 
# df$cluster <- factor(sc)
# 
# # Plot with ellipses
# ggplot(df, aes(x = X1, y = X2, color = cluster, fill = cluster)) +
#   geom_point(size = 2, alpha = 0.7) +
#   stat_ellipse(type = "norm", alpha = 0.2, geom = "polygon") +
#   theme_minimal() +
#   theme(legend.position = "right") +
#   labs(title = "Spectral Clustering on Embedded Network",
#        x = "Embedding 1", y = "Embedding 2")
# 
# 
# 
# # median k-NN distance (local density)
# knn <- FNN::get.knn(coords, k = k)
# median_knn <- median(rowMeans(knn$nn.dist), na.rm = TRUE)
