#' Longitudinal Health Trajectory Clustering (CEDM Proposition 3)
#'
#' Identifies developmental health phenotypes (trajectory classes) using
#' k-means or hierarchical clustering on person-level longitudinal health
#' profiles. Replicates the BMI trajectory analysis from Hait (2025) which
#' identified stable-average, persistently-low, and high-rising BMI classes.
#' Supports CEDM Proposition 3 (Developmental Recursion) by identifying
#' children on cumulative health trajectories.
#'
#' @param data A data.frame in LONG format.
#' @param health_var Character string: health variable (e.g., BMI).
#' @param id_var Character string: person-level ID variable.
#' @param time_var Character string: wave/time variable.
#' @param k Integer: number of trajectory clusters. Default 3 (reflecting
#'   the CEDM's three-class structure: stable-average, low, high-rising).
#' @param method Character: clustering method, one of \code{"kmeans"} (default)
#'   or \code{"hierarchical"}.
#' @param outcome_var Character string (optional): if provided, mean outcome
#'   is computed by cluster for interpretation.
#' @param ses_var Character string (optional): if provided, mean SES is
#'   computed by cluster.
#' @param seed Integer: random seed. Default 123.
#' @param plot Logical: if TRUE (default), generate trajectory plot.
#'
#' @return A list of class \code{"cedm_trajectory"} with:
#'   \itemize{
#'     \item \code{cluster_assignment}: data.frame with id and cluster label.
#'     \item \code{cluster_profiles}: mean health by wave and cluster.
#'     \item \code{cluster_summary}: mean outcome and SES by cluster (if provided).
#'     \item \code{plot}: ggplot2 trajectory plot (if \code{plot = TRUE}).
#'     \item \code{k}: number of clusters.
#'   }
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' df <- data.frame(
#'   id   = rep(1:200, each = 5),
#'   wave = rep(1:5, times = 200),
#'   bmi  = c(rnorm(200 * 5, 25, 3)),
#'   math = rnorm(200 * 5, 500, 100),
#'   ses  = rep(rnorm(200), each = 5)
#' )
#' result <- cedm_trajectory(df, health_var = "bmi", id_var = "id",
#'                            time_var = "wave", outcome_var = "math",
#'                            ses_var = "ses")
#' print(result)
#' }
#'
#' @export
cedm_trajectory <- function(data,
                            health_var,
                            id_var,
                            time_var,
                            k           = 3,
                            method      = c("kmeans", "hierarchical"),
                            outcome_var = NULL,
                            ses_var     = NULL,
                            seed        = 123,
                            plot        = TRUE) {

  set.seed(seed)
  method <- match.arg(method)

  req_vars <- c(health_var, id_var, time_var, outcome_var, ses_var)
  req_vars <- req_vars[!is.null(req_vars)]
  missing  <- req_vars[!req_vars %in% names(data)]
  if (length(missing) > 0) {
    stop(paste("Missing variables:", paste(missing, collapse = ", ")))
  }

  # Reshape to wide for clustering
  data_wide <- data %>%
    dplyr::select(dplyr::all_of(c(id_var, time_var, health_var))) %>%
    tidyr::pivot_wider(
      id_cols      = dplyr::all_of(id_var),
      names_from   = dplyr::all_of(time_var),
      values_from  = dplyr::all_of(health_var),
      names_prefix = "wave_"
    )

  # Extract numeric matrix for clustering
  wave_cols  <- grep("^wave_", names(data_wide), value = TRUE)
  health_mat <- as.matrix(data_wide[, wave_cols])

  # Impute row means for missing waves
  for (i in seq_len(nrow(health_mat))) {
    na_cols <- which(is.na(health_mat[i, ]))
    if (length(na_cols) > 0 && length(na_cols) < ncol(health_mat)) {
      health_mat[i, na_cols] <- mean(health_mat[i, -na_cols], na.rm = TRUE)
    }
  }

  complete_rows <- stats::complete.cases(health_mat)
  health_mat_cc <- health_mat[complete_rows, ]

  if (method == "kmeans") {
    clust_result <- stats::kmeans(health_mat_cc, centers = k, nstart = 25)
    cluster_vec  <- clust_result$cluster
  } else {
    dist_mat    <- stats::dist(health_mat_cc)
    hclust_obj  <- stats::hclust(dist_mat, method = "ward.D2")
    cluster_vec <- stats::cutree(hclust_obj, k = k)
  }

  # Label clusters by ascending mean health level
  cluster_means  <- tapply(rowMeans(health_mat_cc), cluster_vec, mean)
  cluster_order  <- order(cluster_means)
  cluster_labels <- character(length(cluster_vec))

  label_map <- if (k == 3) {
    c("persistently-low", "stable-average", "high-rising")
  } else {
    c("low", "average", "high")[seq_len(k)]
  }

  for (ci in seq_len(k)) {
    cluster_labels[cluster_vec == cluster_order[ci]] <- label_map[ci]
  }

  id_vec     <- data_wide[[id_var]][complete_rows]
  cluster_df <- data.frame(id = id_vec, cluster = factor(cluster_labels),
                           stringsAsFactors = FALSE)
  names(cluster_df)[1] <- id_var

  # Merge cluster labels back into long data
  data_long_clustered <- dplyr::inner_join(data, cluster_df, by = id_var)

  # Mean health profile by cluster and wave
  profiles <- data_long_clustered %>%
    dplyr::group_by(.data[["cluster"]], .data[[time_var]]) %>%
    dplyr::summarise(
      mean_health = mean(.data[[health_var]], na.rm = TRUE),
      .groups     = "drop"
    )

  # Cluster-level summary table
  cluster_summary <- data_long_clustered %>%
    dplyr::group_by(.data[["cluster"]]) %>%
    dplyr::summarise(
      n           = dplyr::n_distinct(.data[[id_var]]),
      mean_health = mean(.data[[health_var]], na.rm = TRUE),
      sd_health   = stats::sd(.data[[health_var]], na.rm = TRUE),
      .groups     = "drop"
    )

  if (!is.null(outcome_var)) {
    out_means <- data_long_clustered %>%
      dplyr::group_by(.data[["cluster"]]) %>%
      dplyr::summarise(
        mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
        .groups      = "drop"
      )
    cluster_summary <- dplyr::left_join(cluster_summary, out_means,
                                        by = "cluster")
  }

  if (!is.null(ses_var)) {
    ses_means <- data_long_clustered %>%
      dplyr::group_by(.data[["cluster"]]) %>%
      dplyr::summarise(
        mean_ses = mean(.data[[ses_var]], na.rm = TRUE),
        .groups  = "drop"
      )
    cluster_summary <- dplyr::left_join(cluster_summary, ses_means,
                                        by = "cluster")
  }

  # Trajectory plot
  p <- NULL
  if (plot) {
    p <- ggplot2::ggplot(
      profiles,
      ggplot2::aes(
        x     = .data[[time_var]],
        y     = mean_health,
        group = cluster,
        color = cluster
      )
    ) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(
        title    = paste0("CEDM Health Trajectory Classes (k = ", k, ")"),
        subtitle = paste("Health variable:", health_var),
        x        = time_var,
        y        = paste("Mean", health_var),
        color    = "Trajectory Class",
        caption  = paste(
          "CEDM Proposition 3: trajectory classes reflect",
          "developmental capability constraints"
        )
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom")
  }

  out <- list(
    cluster_assignment = cluster_df,
    cluster_profiles   = profiles,
    cluster_summary    = cluster_summary,
    data_clustered     = data_long_clustered,
    plot               = p,
    k                  = k,
    health_var         = health_var,
    method             = method
  )
  class(out) <- "cedm_trajectory"
  return(out)
}

#' @export
print.cedm_trajectory <- function(x, ...) {
  cat("=== CEDM Health Trajectory Clustering ===\n")
  cat("Health variable:", x$health_var,
      "| Clusters (k):", x$k,
      "| Method:", x$method, "\n\n")
  cat("Cluster Summary:\n")
  print(x$cluster_summary)
  invisible(x)
}
