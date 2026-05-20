# QMap - VisualEasier 2.0.9

APP_VERSION <- "2.0-9"

required_pkgs <- c(
  "shiny", "shinythemes", "data.table", "dplyr", "tidyr", "stringr",
  "ggplot2", "patchwork", "Cairo", "imager", "pals", "ggpubr",
  "shinycssloaders", "scales", "png", "colourpicker", "RColorBrewer",
  "plotly", "corrplot", "htmlwidgets", "viridisLite", "GGally"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    sprintf("Please install the following packages before running the app: %s",
            paste(missing_pkgs, collapse = ", ")),
    call. = FALSE
  )
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

options(shiny.maxRequestSize = 3072 * 3072 * 3072) # increases the maximum upload size
options(shiny.launch.browser = TRUE)
options(warn = -1) # suppress warning messages by default in the Shiny session

# =============================================================================
# Helpers
# =============================================================================

"%||%" <- function(x, y) if (is.null(x)) y else x

parse_elements <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character(0))
  elems <- str_split(text, pattern = ",\\s*")[[1]]
  elems[nzchar(trimws(elems))]
}

read_matrix_file <- function(file_input) {
  validate(need(!is.null(file_input), "Please upload all required files."))
  mat <- as.matrix(fread(file_input$datapath, header = FALSE))
  storage.mode(mat) <- "numeric"
  mat
}

normalize_minmax_matrix <- function(mat) {
  rng <- range(mat, na.rm = TRUE)
  out <- if (!all(is.finite(rng)) || diff(rng) == 0) {
    matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  } else {
    (mat - rng[1]) / diff(rng)
  }
  out <- t(out)
  out[!is.finite(out)] <- 0
  out
}

matrix_to_df <- function(mat, value_name = "value") {
  if (inherits(mat, "cimg")) stop("matrix_to_df expects a 2D numeric matrix, not a cimg object.")
  mat <- as.matrix(mat)
  d <- dim(mat)
  if (length(d) != 2 || any(!is.finite(d)) || any(d < 1))
    stop("matrix_to_df received an object that cannot be interpreted as a 2D matrix.")
  nr <- as.integer(d[1]); nc <- as.integer(d[2])
  df <- expand.grid(x = seq_len(nc), y = seq_len(nr))
  df[[value_name]] <- as.vector(t(mat[nr:1, , drop = FALSE]))
  df
}

maybe_invert_df <- function(df, invert = FALSE) {
  if (!isTRUE(invert)) return(df)
  out <- df; out$x <- df$y; out$y <- df$x
  out
}

compute_aspect_ratio <- function(nr, nc, invert = FALSE) {
  if (isTRUE(invert)) nc / nr else nr / nc
}

matrix_list_to_merged_df <- function(mats, element_names) {
  stopifnot(length(mats) == length(element_names))
  mat1 <- as.matrix(mats[[1]])
  nr <- as.integer(nrow(mat1)); nc <- as.integer(ncol(mat1))
  df <- expand.grid(x = seq_len(nc), y = seq_len(nr))
  for (i in seq_along(mats)) {
    m <- as.matrix(mats[[i]])
    df[[element_names[[i]]]] <- as.vector(t(m[nr:1, , drop = FALSE]))
  }
  df
}

resolve_palette <- function(name) {
  name <- as.character(name[1] %||% "turbo")
  switch(name,
    turbo   = pals::turbo(256),
    viridis = pals::viridis(256),
    plasma  = pals::plasma(256),
    magma   = pals::magma(256),
    inferno = pals::inferno(256),
    pals::turbo(256)
  )
}

apply_global_normalization <- function(mats, norm_values) {
  Map(function(mat, norm_value) {
    if (is.null(norm_value) || is.na(norm_value) || norm_value <= 0) return(mat)
    total <- sum(mat, na.rm = TRUE)
    if (!is.finite(total) || total == 0) return(mat)
    (mat * norm_value) / total
  }, mats, norm_values)
}

compute_display_matrix <- function(index, mats, norm_values) {
  if (is.null(mats) || length(mats) == 0 || is.null(index) || !is.finite(index) ||
      index < 1 || index > length(mats))
    stop("compute_display_matrix received an invalid matrix index.", call. = FALSE)

  if (is.null(norm_values) || length(norm_values) < length(mats)) {
    tmp <- as.list(rep(0, length(mats)))
    if (!is.null(norm_values) && length(norm_values) > 0) tmp[seq_along(norm_values)] <- norm_values
    norm_values <- tmp
  }

  selected_norm <- suppressWarnings(as.numeric(norm_values[[index]] %||% 0)[1])
  has_valid_norm <- vapply(norm_values[seq_along(mats)], function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    isTRUE(is.finite(x) && !is.na(x) && x > 0)
  }, logical(1))

  if (!isTRUE(is.finite(selected_norm) && !is.na(selected_norm) && selected_norm > 0 && has_valid_norm[index]))
    return(mats[[index]])

  valid_idx <- which(has_valid_norm)
  globally_scaled <- apply_global_normalization(mats[valid_idx], norm_values[valid_idx])
  stacked <- simplify2array(globally_scaled)
  px_sum <- apply(stacked, c(1, 2), sum, na.rm = TRUE)
  px_sum[!is.finite(px_sum) | px_sum == 0] <- NA_real_

  selected_pos <- match(index, valid_idx)
  if (is.na(selected_pos) || selected_pos < 1) return(mats[[index]])

  out <- (globally_scaled[[selected_pos]] / px_sum) * 100
  out[!is.finite(out)] <- NA_real_
  out
}

pixel_normalize_matrix_list <- function(mats, eps = 1e-12) {
  if (length(mats) == 0) return(mats)
  arr <- simplify2array(mats)
  px_sum <- apply(arr, c(1, 2), sum, na.rm = TRUE)
  px_sum[!is.finite(px_sum) | px_sum <= 0] <- NA_real_
  out <- lapply(mats, function(mat) { val <- mat / px_sum; val[!is.finite(val)] <- NA_real_; val })
  names(out) <- names(mats)
  out
}

clr_transform_matrix_list <- function(mats, pseudocount = 1e-6) {
  if (length(mats) == 0) return(mats)
  arr <- simplify2array(mats)
  arr[!is.finite(arr)] <- NA_real_
  arr <- pmax(arr, 0, na.rm = TRUE) + pseudocount
  gm_log <- apply(log(arr), c(1, 2), mean, na.rm = TRUE)
  out <- lapply(seq_len(dim(arr)[3]), function(i) {
    val <- log(arr[, , i]) - gm_log; val[!is.finite(val)] <- NA_real_; val
  })
  names(out) <- names(mats)
  out
}

minmax_transform_matrix_list <- function(mats) {
  if (length(mats) == 0) return(mats)
  out <- lapply(mats, function(mat) {
    mat <- as.matrix(mat); rng <- range(mat, na.rm = TRUE)
    val <- if (!all(is.finite(rng)) || diff(rng) == 0) matrix(0, nrow(mat), ncol(mat))
           else (mat - rng[1]) / diff(rng)
    val[!is.finite(val)] <- NA_real_; val
  })
  names(out) <- names(mats); out
}

ln_transform_matrix_list <- function(mats, pseudocount = 1e-6) {
  if (length(mats) == 0) return(mats)
  out <- lapply(mats, function(mat) {
    mat <- as.matrix(mat)
    val <- log(pmax(mat, 0) + pseudocount); val[!is.finite(val)] <- NA_real_; val
  })
  names(out) <- names(mats); out
}

sd_scale_matrix_list <- function(mats) {
  if (length(mats) == 0) return(mats)
  out <- lapply(mats, function(mat) {
    mat <- as.matrix(mat); s <- stats::sd(as.numeric(mat), na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(mat)
    val <- mat / s; val[!is.finite(val)] <- NA_real_; val
  })
  names(out) <- names(mats); out
}

cps_to_wt_matrix_list <- function(mats, norm_values) {
  if (length(mats) == 0) return(mats)
  has_norm <- vapply(norm_values, function(x) isTRUE(is.finite(x) && !is.na(x) && x > 0), logical(1))
  if (!any(has_norm)) return(mats)
  frac <- pixel_normalize_matrix_list(apply_global_normalization(mats, norm_values))
  out <- lapply(frac, function(mat) { val <- mat * 100; val[!is.finite(val)] <- NA_real_; val })
  names(out) <- names(mats); out
}

default_processing_method <- function(core) {
  if (is.null(core) || is.null(core$norms)) return("raw")
  has_norm <- vapply(core$norms, function(x) isTRUE(is.finite(x) && !is.na(x) && x > 0), logical(1))
  if (any(has_norm)) "cps_wt" else "raw"
}

apply_data_processing_method <- function(mats, method = "raw", norm_values = NULL) {
  method <- method %||% "raw"
  switch(method,
    cps_wt  = cps_to_wt_matrix_list(mats, norm_values %||% as.list(rep(0, length(mats)))),
    clr     = clr_transform_matrix_list(mats),
    ln      = ln_transform_matrix_list(mats),
    minmax  = minmax_transform_matrix_list(mats),
    sdscale = sd_scale_matrix_list(mats),
    mats
  )
}

clip_matrix_limits <- function(mat, limits = NULL) {
  if (is.null(limits) || length(limits) != 2 || any(!is.finite(limits))) return(mat)
  lo <- min(limits); hi <- max(limits)
  out <- as.matrix(mat)
  out[out < lo] <- 0; out[out > hi] <- hi
  out
}

apply_clip_state_to_matrices <- function(mats, clip_state = NULL) {
  if (length(mats) == 0 || is.null(clip_state) || length(clip_state) == 0) return(mats)
  out <- mats
  for (nm in intersect(names(out), names(clip_state)))
    out[[nm]] <- clip_matrix_limits(out[[nm]], clip_state[[nm]])
  out
}

build_pca_maps <- function(core, rescale_all = FALSE) {
  mats <- core$matrices %||% core$mats
  elems <- core$elements
  validate(need(length(elems) >= 2, "At least two variables are required to compute PCA maps."))
  df <- matrix_list_to_merged_df(unname(mats[elems]), elems)
  X <- as.matrix(df[, elems, drop = FALSE])
  cc <- stats::complete.cases(X)
  validate(need(sum(cc) > 2, "Not enough complete pixels are available to compute PCA maps."))

  pca <- stats::prcomp(X[cc, , drop = FALSE], center = TRUE, scale. = TRUE)
  ncomp <- ncol(pca$x)
  score_df <- df[, c("x", "y"), drop = FALSE]
  pc_names <- paste0("PC", seq_len(ncomp))
  for (pc in pc_names) score_df[[pc]] <- NA_real_
  score_df[cc, pc_names] <- pca$x

  if (isTRUE(rescale_all)) {
    for (pc in pc_names) {
      rng <- range(score_df[[pc]], na.rm = TRUE)
      if (all(is.finite(rng)) && diff(rng) != 0)
        score_df[[pc]] <- (score_df[[pc]] - rng[1]) / diff(rng)
    }
  }

  list(pca = pca, score_df = score_df, pc_names = pc_names,
       pc_mats = score_df_to_matrix_list(score_df, pc_names, core$dims))
}

score_df_to_matrix_list <- function(score_df, pc_names, dims) {
  nr <- as.integer(dims[1]); nc <- as.integer(dims[2])
  out <- lapply(pc_names, function(pc) {
    mat <- matrix(NA_real_, nrow = nr, ncol = nc)
    x <- as.integer(score_df$x); y <- as.integer(score_df$y)
    ok <- is.finite(x) & is.finite(y) & x >= 1 & x <= nc & y >= 1 & y <= nr
    mat[nr + 1L - y[ok], x[ok]] <- score_df[[pc]][ok]
    mat
  })
  names(out) <- pc_names; out
}

cluster_filter_coords <- function(cluster_state, selected_labels = NULL) {
  if (is.null(cluster_state) || is.null(cluster_state$result_df)) return(NULL)
  df <- cluster_state$result_df %>% dplyr::select(x, y, cluster_label)
  if (!is.null(selected_labels) && length(selected_labels) > 0)
    df <- df %>% dplyr::filter(cluster_label %in% selected_labels)
  df
}

apply_cluster_filter_df <- function(df, cluster_state, selected_labels = NULL) {
  coords <- cluster_filter_coords(cluster_state, selected_labels)
  if (is.null(coords)) return(df)
  dplyr::inner_join(df, coords, by = c("x", "y"))
}

convolve2d <- function(mat, kernel) {
  mat <- as.matrix(mat); kernel <- as.matrix(kernel)
  img <- imager::as.cimg(t(mat)); kern <- imager::as.cimg(t(kernel))
  res <- imager::convolve(img, kern)
  mat_out <- t(as.matrix(res)); storage.mode(mat_out) <- "double"; mat_out
}

median_filter_3x3 <- function(mat) {
  mat <- as.matrix(mat)
  img <- imager::as.cimg(t(mat))
  res <- imager::medianblur(img, n = 1L)
  mat_out <- t(as.matrix(res)); storage.mode(mat_out) <- "double"; mat_out
}

compute_gradient_magnitude <- function(mat) {
  mat <- as.matrix(mat)
  nr <- nrow(mat); nc <- ncol(mat)
  dx <- matrix(0, nrow = nr, ncol = nc)
  dy <- matrix(0, nrow = nr, ncol = nc)

  if (nc > 1) {
    dx[, 1]  <- mat[, 2] - mat[, 1]
    dx[, nc] <- mat[, nc] - mat[, nc - 1]
    if (nc > 2) {
      dx[, 2:(nc - 1)] <- (mat[, 3:nc, drop = FALSE] - mat[, 1:(nc - 2), drop = FALSE]) / 2
    }
  }

  if (nr > 1) {
    dy[1, ]  <- mat[2, ] - mat[1, ]
    dy[nr, ] <- mat[nr, ] - mat[nr - 1, ]
    if (nr > 2) {
      dy[2:(nr - 1), ] <- (mat[3:nr, , drop = FALSE] - mat[1:(nr - 2), , drop = FALSE]) / 2
    }
  }

  sqrt(dx^2 + dy^2)
}

filter_label_for_title <- function(filter_name) {
  filter_name <- filter_name %||% "None"
  if (identical(filter_name, "None")) "" else paste0("\n", filter_name, " filter applied")
}

filter_label_for_filename <- function(filter_name) {
  filter_name <- filter_name %||% "None"
  if (identical(filter_name, "None")) return("")
  clean <- gsub("[^A-Za-z0-9]+", "_", filter_name)
  clean <- gsub("^_+|_+$", "", clean)
  if (!nzchar(clean)) "" else paste0("_", clean)
}

apply_filter_to_matrix <- function(mat, filter_name = "None") {
  mat <- as.matrix(mat)
  filter_name <- filter_name %||% "None"
  if (identical(filter_name, "None")) return(mat)
  rng <- range(mat, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(mat)
  scaled <- (mat - rng[1]) / diff(rng)
  scaled[!is.finite(scaled)] <- 0
  filtered <- switch(filter_name,
    "Median"        = median_filter_3x3(scaled),
    "Gaussian blur" = convolve2d(scaled, matrix(c(1,2,1,2,4,2,1,2,1), nrow=3, byrow=TRUE) / 16),
    "Gradient"      = compute_gradient_magnitude(scaled),
    scaled
  )
  pmax(pmin(filtered, 1), 0) * diff(rng) + rng[1]
}

compose_multicolor_df <- function(mats, colors, labels = names(mats)) {
  stopifnot(length(mats) == length(colors))
  if (length(mats) == 0) return(data.frame())

  norm_mats <- lapply(mats, function(mat) {
    mm <- normalize_minmax_matrix(mat); mm <- as.matrix(mm)
    d <- dim(mm)
    if (length(d) != 2 || any(d < 1))
      stop("Each selected layer for the multi-color composite must be a valid 2D matrix.")
    mm
  })

  dims <- vapply(norm_mats, function(mm) paste(dim(mm), collapse = "x"), character(1))
  if (length(unique(dims)) != 1)
    stop("All selected layers for the multi-color composite must have identical dimensions.")

  color_rgb <- lapply(colors, function(cl) hex_to_rgb(cl) / 255)
  nr <- as.integer(nrow(norm_mats[[1]])); nc <- as.integer(ncol(norm_mats[[1]]))
  r <- g <- b <- intensity_sum <- matrix(0, nrow = nr, ncol = nc)

  for (i in seq_along(norm_mats)) {
    mat <- norm_mats[[i]]; rgbv <- unname(color_rgb[[i]])
    r <- r + mat * rgbv[1]; g <- g + mat * rgbv[2]; b <- b + mat * rgbv[3]
    intensity_sum <- intensity_sum + mat
  }

  scale_den <- pmax(intensity_sum, 1e-9)
  r <- pmin(1, r / scale_den); g <- pmin(1, g / scale_den); b <- pmin(1, b / scale_den)

  df <- matrix_to_df(r, "r") %>%
    dplyr::left_join(matrix_to_df(g, "g"), by = c("x", "y")) %>%
    dplyr::left_join(matrix_to_df(b, "b"), by = c("x", "y")) %>%
    dplyr::mutate(
      r   = pmax(pmin(ifelse(is.finite(r), r, 0), 1), 0),
      g   = pmax(pmin(ifelse(is.finite(g), g, 0), 1), 0),
      b   = pmax(pmin(ifelse(is.finite(b), b, 0), 1), 0),
      hex = grDevices::rgb(r, g, b)
    )

  attr(df, "legend_df") <- data.frame(Element = labels, Color = colors, stringsAsFactors = FALSE)
  df
}

make_composite_plot <- function(df, title, legend_df = NULL,
                                map_width_mm = NULL, map_height_mm = NULL,
                                invert_coords = FALSE, legend_orientation = "horizontal") {
  df <- maybe_invert_df(df, invert_coords)
  p <- ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = hex)) +
    scale_fill_identity() +
    coord_equal() +
    theme_minimal() +
    ggtitle(title) +
    theme(
      plot.title      = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title      = element_blank(),
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      panel.grid      = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  if (!is.null(legend_df) && nrow(legend_df) > 0) {
    x_mid <- max(df$x, na.rm = TRUE) / 2
    y_lab <- max(df$y, na.rm = TRUE) + 20
    n <- nrow(legend_df)
    if (identical(tolower(legend_orientation), "vertical")) {
      x_lab <- max(df$x, na.rm = TRUE) + 20
      y_offsets <- seq(-(n-1)/2, (n-1)/2, length.out = n) * 20
      for (i in seq_len(n))
        p <- p + annotate("label", x=x_lab, y=y_lab-y_offsets[i], label=legend_df$Element[i],
                          fill=legend_df$Color[i], color="white", fontface="bold", size=4.5)
    } else {
      offsets <- seq(-(n-1)/2, (n-1)/2, length.out = n) * 55
      for (i in seq_len(n))
        p <- p + annotate("label", x=x_mid+offsets[i], y=y_lab, label=legend_df$Element[i],
                          fill=legend_df$Color[i], color="white", fontface="bold", size=4.5)
    }
  }
  scale_width_mm  <- if (isTRUE(invert_coords)) map_height_mm else map_width_mm
  scale_height_mm <- if (isTRUE(invert_coords)) map_width_mm  else map_height_mm
  add_scale_bar(p, build_scale_annotation(df, scale_width_mm, scale_height_mm, y_reversed = FALSE))
}



build_scale_annotation <- function(df, map_width_mm, map_height_mm, target_fraction = 0.20, y_reversed = FALSE) {
  if (is.null(map_width_mm) || !is.finite(map_width_mm) || map_width_mm <= 0 ||
      is.null(map_height_mm) || !is.finite(map_height_mm) || map_height_mm <= 0) {
    return(NULL)
  }

  x_range <- range(df$x, na.rm = TRUE)
  y_range <- range(df$y, na.rm = TRUE)

  x_span <- diff(x_range)
  y_span <- diff(y_range)

  if (!is.finite(x_span) || x_span <= 0 ||
      !is.finite(y_span) || y_span <= 0) {
    return(NULL)
  }

  # Desired real scale bar length, approximately 20% of the physical map width.
  # map_width_mm must represent the real dimension currently plotted on the x-axis.
  raw_bar_mm <- map_width_mm * target_fraction

  # Round to a readable number: 1, 2, 5, 10, 20, 50...
  pow10 <- 10^floor(log10(raw_bar_mm))
  candidates <- c(1, 2, 5, 10) * pow10
  bar_mm <- candidates[which.min(abs(candidates - raw_bar_mm))]

  # Convert mm to plot units/pixels along the visible x-axis.
  bar_plot_units <- (bar_mm / map_width_mm) * x_span

  x_start <- x_range[1] + 0.06 * x_span
  x_end   <- x_start + bar_plot_units
  text_x  <- x_start + bar_plot_units / 2

  # Keep the scale visually at the lower-left corner.
  # For plots with scale_y_reverse(), larger y-values are visually lower.
  if (isTRUE(y_reversed)) {
    y_bar  <- y_range[2] - 0.06 * y_span
    text_y <- y_range[2] - 0.10 * y_span
  } else {
    y_bar  <- y_range[1] + 0.06 * y_span
    text_y <- y_range[1] + 0.10 * y_span
  }

  # Background rectangle around both scale bar and label
  x_pad <- 0.025 * x_span
  y_pad <- 0.025 * y_span

  list(
    x_start = x_start,
    x_end   = x_end,
    y       = y_bar,
    text_x  = text_x,
    text_y  = text_y,
    label   = paste0(bar_mm, " mm"),

    rect_xmin = x_start - x_pad,
    rect_xmax = x_end + x_pad,
    rect_ymin = min(y_bar, text_y) - y_pad,
    rect_ymax = max(y_bar, text_y) + y_pad
  )
}

add_scale_bar <- function(plot_obj, scale_info) {
  if (is.null(scale_info)) return(plot_obj)
  
  plot_obj +
    annotate(
      "rect",
      xmin = scale_info$rect_xmin,
      xmax = scale_info$rect_xmax,
      ymin = scale_info$rect_ymin,
      ymax = scale_info$rect_ymax,
      fill = "white",
      color = "black",
      alpha = 0.6
    ) +
    annotate(
      "segment",
      x = scale_info$x_start,
      xend = scale_info$x_end,
      y = scale_info$y,
      yend = scale_info$y,
      linewidth = 1.6,
      color = "black"
    ) +
    annotate(
      "text",
      x = scale_info$text_x,
      y = scale_info$text_y,
      label = scale_info$label,
      size = 4.6,
      fontface = "bold",
      color = "black"
    )
}

draw_ggplot_raster_page <- function(plot_obj, width = 8, height = 10, dpi = 130, bg = "white") {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ggplot2::ggsave(tmp, plot=plot_obj, width=width, height=height, dpi=dpi,
                  bg=bg, device="png", limitsize=FALSE)
  img <- png::readPNG(tmp)
  grid::grid.newpage()
  grid::grid.raster(img, x=0.5, y=0.5, width=grid::unit(0.94,"npc"),
                    height=grid::unit(0.94,"npc"), interpolate=TRUE)
}

apply_axis_scaling <- function(plot_obj, df, map_width_mm = NULL, map_height_mm = NULL, show_axes = TRUE) {
  x_range <- range(df$x, na.rm = TRUE); y_range <- range(df$y, na.rm = TRUE)
  width_mm  <- if (is.null(map_width_mm)  || is.na(map_width_mm)  || map_width_mm  <= 0) diff(x_range) else map_width_mm
  height_mm <- if (is.null(map_height_mm) || is.na(map_height_mm) || map_height_mm <= 0) diff(y_range) else map_height_mm
  plot_obj <- plot_obj +
    scale_x_continuous(name=NULL, breaks=pretty(x_range),
      labels=label_number(accuracy=0.1)(rescale(pretty(x_range), to=c(0, width_mm)))) +
    scale_y_continuous(name=NULL, breaks=pretty(y_range),
      labels=label_number(accuracy=0.1)(rescale(pretty(y_range), to=c(0, height_mm))))
  if (!show_axes)
    plot_obj <- plot_obj + theme(axis.title=element_blank(), axis.text=element_blank(),
                                 axis.ticks=element_blank(), axis.line=element_blank(),
                                 panel.grid=element_blank())
  plot_obj
}

cimg_to_df <- function(img) {
  df <- as.data.frame(img, wide = "c")
  ch_names <- grep("^c\\.", names(df), value = TRUE)
  df$hex <- if (length(ch_names) == 1) rgb(df[[ch_names]], df[[ch_names]], df[[ch_names]])
             else rgb(df[[ch_names[1]]], df[[ch_names[2]]], df[[ch_names[3]]])
  df
}

make_rgb_plot <- function(df, title, labels = NULL, map_width_mm = NULL, map_height_mm = NULL, invert_coords = FALSE) {
  df <- maybe_invert_df(df, invert_coords)
  p <- ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = hex)) +
    scale_fill_identity() +
    coord_equal(clip = "off") +
    scale_y_reverse() +
    theme_minimal() +
    ggtitle(title) +
    theme(
      plot.title      = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title      = element_blank(),
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      panel.grid      = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  if (!is.null(labels) && length(labels) == 3) {
    x_range <- range(df$x, na.rm = TRUE)
    y_range <- range(df$y, na.rm = TRUE)
    x_span <- diff(x_range)
    y_span <- diff(y_range)

    if (isTRUE(invert_coords) && is.finite(x_span) && is.finite(y_span) &&
        x_span > 0 && y_span > 0) {
      # In inverted coordinates, x represents the original y dimension and y represents
      # the original x dimension. Use the swapped aspect ratio to preserve the same
      # visual spacing that worked well in the normal RGB orientation.
      aspect_correction <- max(1, y_span / x_span)
      x_mid <- x_range[1] + 0.50 * x_span
      x_offset <- 50 * aspect_correction
      y_lab <- y_range[1] - 20 * aspect_correction
      p <- p + expand_limits(y = y_lab)
    } else {
      # Original v1.0.0.22 placement, retained because it gives the best spacing
      # for the normal RGB orientation.
      x_mid <- max(df$x, na.rm = TRUE) / 2
      x_offset <- 50
      y_lab <- min(df$y, na.rm = TRUE) - 20
    }

    p <- p +
      annotate("label", x=x_mid-x_offset, y=y_lab, label=labels[1], fill="red",   color="white", fontface="bold", size=5) +
      annotate("label", x=x_mid,          y=y_lab, label=labels[2], fill="green", color="white", fontface="bold", size=5) +
      annotate("label", x=x_mid+x_offset, y=y_lab, label=labels[3], fill="blue",  color="white", fontface="bold", size=5)
  }
  scale_width_mm  <- if (isTRUE(invert_coords)) map_height_mm else map_width_mm
  scale_height_mm <- if (isTRUE(invert_coords)) map_width_mm  else map_height_mm
  add_scale_bar(p, build_scale_annotation(df, scale_width_mm, scale_height_mm, y_reversed = TRUE))
}

cluster_palette <- function(k, palette_name = "Okabe-Ito") {
  k <- max(1, as.integer(k))
  palette_name <- palette_name %||% "Okabe-Ito"
  okabe20 <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
    "#D55E00", "#CC79A7", "#999999", "#332288", "#88CCEE",
    "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677",
    "#882255", "#AA4499", "#6699CC", "#661100", "#AA4466"
  )
  switch(palette_name,
    "Okabe-Ito" = rep_len(okabe20, k),
    "Set 2"     = rep_len(RColorBrewer::brewer.pal(8,  "Set2"),   k),
    "Set 3"     = rep_len(RColorBrewer::brewer.pal(12, "Set3"),   k),
    "Paired"    = rep_len(RColorBrewer::brewer.pal(12, "Paired"), k),
    "Dark 2"    = rep_len(RColorBrewer::brewer.pal(8,  "Dark2"),  k),
    "Accent"    = rep_len(RColorBrewer::brewer.pal(8,  "Accent"), k),
    rep_len(grDevices::hcl.colors(max(k, 20), palette="Dark 3", rev=FALSE), k)
  )
}

hex_to_rgb <- function(hex) {
  vals <- tryCatch(grDevices::col2rgb(hex)[, 1], error = function(e) c(0, 0, 0))
  c(r = unname(vals[1]), g = unname(vals[2]), b = unname(vals[3]))
}

clamp_rgb <- function(x) {
  x <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (is.na(x)) x <- 0L
  max(0L, min(255L, x))
}

rgb_to_hex_safe <- function(r, g, b) {
  grDevices::rgb(clamp_rgb(r), clamp_rgb(g), clamp_rgb(b), maxColorValue = 255)
}

save_plot_as_raster_pdf <- function(plot_obj, file, width, height, dpi = 200, bg = "white") {
  width  <- suppressWarnings(as.numeric(width)[1])
  height <- suppressWarnings(as.numeric(height)[1])
  validate(need(is.finite(width) && width > 0 && is.finite(height) && height > 0,
                "Invalid export size. Please generate the map again before downloading."))

  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)

  ggsave(filename=tmp_png, plot=plot_obj, width=width, height=height,
         dpi=dpi, bg=bg, device="png", limitsize=FALSE)
  img <- png::readPNG(tmp_png)

  grDevices::cairo_pdf(file=file, width=width, height=height, bg=bg)
  dev_opened <- TRUE
  on.exit(if (isTRUE(dev_opened) && grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::grid.raster(img, width=grid::unit(1,"npc"), height=grid::unit(1,"npc"), interpolate=FALSE)
}

save_plot_as_tiff <- function(plot_obj, file, width, height, dpi = 300, bg = "white") {
  width  <- suppressWarnings(as.numeric(width)[1])
  height <- suppressWarnings(as.numeric(height)[1])
  validate(need(is.finite(width) && width > 0 && is.finite(height) && height > 0,
                "Invalid export size. Please generate the map again before downloading."))

  grDevices::tiff(filename=file, width=width, height=height, units="in", res=dpi,
                  compression="lzw", bg=bg)
  dev_opened <- TRUE
  on.exit(if (isTRUE(dev_opened) && grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)

  print(plot_obj)
}

compute_cluster_summary_tables <- function(result_df, box_long, cluster_colors, elems, ambiguity_df = NULL) {
  active_ids <- unique(as.character(result_df$cluster_id))
  active_ids <- active_ids[order(suppressWarnings(as.numeric(active_ids)), active_ids)]

  label_map <- result_df %>% distinct(cluster_id, cluster_label) %>%
    mutate(cluster_id = as.character(cluster_id))

  total_rows <- nrow(result_df)

  mean_wide <- box_long %>% group_by(cluster_id, cluster_label, Element) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Element, values_from = Value)

  sd_wide <- box_long %>% group_by(cluster_id, cluster_label, Element) %>%
    summarise(Value = sd(Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Element, values_from = Value)

  combined_table <- result_df %>% count(cluster_id, name = "Count") %>%
    mutate(Percentage = round(100 * Count / total_rows, 1)) %>%
    select(cluster_id, Percentage)

  if (!is.null(ambiguity_df) && nrow(ambiguity_df) > 0) {
    entropy_tbl <- ambiguity_df %>% group_by(cluster_id) %>%
      summarise(Entropy = mean(ambiguity, na.rm = TRUE),
                Entropy_SD = sd(ambiguity, na.rm = TRUE), .groups = "drop")
    combined_table <- combined_table %>% left_join(entropy_tbl, by = "cluster_id")
  } else {
    combined_table <- combined_table %>% mutate(Entropy = NA_real_, Entropy_SD = NA_real_)
  }

  combined_table <- combined_table %>%
    left_join(label_map, by = "cluster_id") %>%
    left_join(mean_wide, by = c("cluster_id", "cluster_label")) %>%
    left_join(sd_wide,   by = c("cluster_id", "cluster_label"), suffix = c("", "_SD")) %>%
    arrange(match(cluster_id, active_ids)) %>%
    rename(Cluster = cluster_label)

  final_cols <- c("Cluster", "Percentage", "Entropy", "Entropy_SD")
  for (el in elems) {
    sd_name <- paste0(el, "_SD")
    if (!sd_name %in% names(combined_table)) combined_table[[sd_name]] <- NA_real_
    final_cols <- c(final_cols, el, sd_name)
  }
  combined_table <- combined_table %>% select(all_of(final_cols))

  ord_lab <- order(label_map$cluster_label)
  label_map <- label_map[ord_lab, , drop = FALSE]
  label_levels <- label_map$cluster_label[match(active_ids, label_map$cluster_id)]
  names(label_levels) <- active_ids
  colors_out <- cluster_colors[active_ids]
  names(colors_out) <- label_levels
  colors_out <- colors_out[order(names(colors_out))]

  list(summary_table = combined_table, cluster_colors_named = colors_out,
       active_ids = active_ids, label_map = label_map)
}

apply_original_percentage_to_summary <- function(summary_table, result_df) {
  if (is.null(summary_table) || !is.data.frame(summary_table) || nrow(summary_table) == 0 ||
      is.null(result_df) || !is.data.frame(result_df) || nrow(result_df) == 0 ||
      !("cluster_label" %in% names(result_df)) || !("Cluster" %in% names(summary_table))) {
    return(summary_table)
  }
  pct_tbl <- result_df %>%
    dplyr::count(cluster_label, name = "Count") %>%
    dplyr::mutate(Percentage_original = round(100 * Count / sum(Count), 1)) %>%
    dplyr::select(Cluster = cluster_label, Percentage_original)
  out <- summary_table %>%
    dplyr::left_join(pct_tbl, by = "Cluster") %>%
    dplyr::mutate(Percentage = dplyr::if_else(is.finite(Percentage_original), Percentage_original, Percentage)) %>%
    dplyr::select(-Percentage_original) %>%
    dplyr::arrange(dplyr::desc(Percentage), Cluster)
  out
}

rescale_01_safe <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  rng <- range(x[ok], na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    out[ok] <- 1
  } else {
    out[ok] <- (x[ok] - rng[1]) / diff(rng)
  }
  out
}

compute_bulk_composition_index <- function(bulk_vec, recon_vec) {
  bulk_vec <- suppressWarnings(as.numeric(bulk_vec))
  recon_vec <- suppressWarnings(as.numeric(recon_vec))
  ok <- is.finite(bulk_vec) & is.finite(recon_vec) & bulk_vec >= 0 & recon_vec >= 0
  if (sum(ok) < 2) return(NA_real_)
  bulk <- bulk_vec[ok]
  recon <- recon_vec[ok]
  if (sum(bulk, na.rm = TRUE) <= 0 || sum(recon, na.rm = TRUE) <= 0) return(NA_real_)
  bulk <- 100 * bulk / sum(bulk, na.rm = TRUE)
  recon <- 100 * recon / sum(recon, na.rm = TRUE)
  bci <- 100 * (1 - sum(abs(bulk - recon), na.rm = TRUE) / sum(bulk, na.rm = TRUE))
  max(0, min(100, bci))
}

compute_reconstructed_bulk_from_clusters <- function(result_df, box_wide, elems) {
  if (is.null(result_df) || is.null(box_wide) || nrow(result_df) == 0 || nrow(box_wide) == 0)
    return(setNames(rep(NA_real_, length(elems)), elems))

  # BCI must not be computed from pixel-wise closed values, otherwise the
  # weighted mean of cluster means becomes mathematically equivalent to the
  # global mean and the index is nearly invariant with k. Here, box_wide should
  # contain concentration-compatible values. We first calculate each cluster
  # mean, then close the mean composition at the cluster level, and only then
  # weight pseudo-mineral compositions by their areal abundance.
  dat <- dplyr::bind_cols(
    result_df %>% dplyr::select(cluster_id),
    box_wide %>% dplyr::select(dplyr::all_of(elems))
  )

  means <- dat %>%
    dplyr::group_by(cluster_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(elems), ~mean(.x, na.rm = TRUE)),
      Count = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(means) == 0 || sum(means$Count, na.rm = TRUE) <= 0)
    return(setNames(rep(NA_real_, length(elems)), elems))

  comp <- as.matrix(means[, elems, drop = FALSE])
  comp[!is.finite(comp) | comp < 0] <- 0
  row_tot <- rowSums(comp, na.rm = TRUE)
  valid <- is.finite(row_tot) & row_tot > 0 & is.finite(means$Count) & means$Count > 0
  if (!any(valid)) return(setNames(rep(NA_real_, length(elems)), elems))

  comp_closed <- sweep(comp[valid, , drop = FALSE], 1, row_tot[valid], FUN = "/") * 100
  weights <- means$Count[valid] / sum(means$Count[valid], na.rm = TRUE)
  recon <- as.numeric(crossprod(weights, comp_closed))
  names(recon) <- elems
  recon
}


compute_reconstructed_bulk_from_clusters_pure <- function(result_df_all, box_wide_all, pure_idx, elems) {
  if (is.null(result_df_all) || is.null(box_wide_all) || nrow(result_df_all) == 0 || nrow(box_wide_all) == 0)
    return(setNames(rep(NA_real_, length(elems)), elems))

  pure_idx <- pure_idx[is.finite(pure_idx) & pure_idx >= 1 & pure_idx <= nrow(result_df_all)]
  if (length(pure_idx) == 0) pure_idx <- seq_len(nrow(result_df_all))

  dat_all <- dplyr::bind_cols(
    result_df_all %>% dplyr::select(cluster_id),
    box_wide_all %>% dplyr::select(dplyr::all_of(elems))
  )

  weights_tbl <- dat_all %>%
    dplyr::count(cluster_id, name = "Count") %>%
    dplyr::mutate(Weight = Count / sum(Count, na.rm = TRUE))

  means_all <- dat_all %>%
    dplyr::group_by(cluster_id) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(elems), ~mean(.x, na.rm = TRUE)), .groups = "drop")

  means_pure <- dat_all[pure_idx, , drop = FALSE] %>%
    dplyr::group_by(cluster_id) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(elems), ~mean(.x, na.rm = TRUE)), .groups = "drop")

  # Use the 30% purest pixels to estimate each pseudo-mineral composition, but
  # use all sampled pixels to estimate modal abundance. If a rare cluster has no
  # pure pixels after filtering, fall back to its all-pixel mean composition.
  comp_tbl <- means_all
  for (cid in means_pure$cluster_id) {
    hit_all <- which(comp_tbl$cluster_id == cid)
    hit_pure <- which(means_pure$cluster_id == cid)
    if (length(hit_all) == 1 && length(hit_pure) == 1)
      comp_tbl[hit_all, elems] <- means_pure[hit_pure, elems]
  }

  comp_tbl <- comp_tbl %>% dplyr::inner_join(weights_tbl, by = "cluster_id")
  if (nrow(comp_tbl) == 0 || sum(comp_tbl$Weight, na.rm = TRUE) <= 0)
    return(setNames(rep(NA_real_, length(elems)), elems))

  comp <- as.matrix(comp_tbl[, elems, drop = FALSE])
  comp[!is.finite(comp) | comp < 0] <- 0
  row_tot <- rowSums(comp, na.rm = TRUE)
  valid <- is.finite(row_tot) & row_tot > 0 & is.finite(comp_tbl$Weight) & comp_tbl$Weight > 0
  if (!any(valid)) return(setNames(rep(NA_real_, length(elems)), elems))

  comp_closed <- sweep(comp[valid, , drop = FALSE], 1, row_tot[valid], FUN = "/") * 100
  weights <- comp_tbl$Weight[valid] / sum(comp_tbl$Weight[valid], na.rm = TRUE)
  recon <- as.numeric(crossprod(weights, comp_closed))
  names(recon) <- elems
  recon
}

adjusted_rand_index <- function(labels_a, labels_b) {
  labels_a <- as.character(labels_a)
  labels_b <- as.character(labels_b)
  ok <- !is.na(labels_a) & !is.na(labels_b)
  labels_a <- labels_a[ok]
  labels_b <- labels_b[ok]
  n <- length(labels_a)
  if (n < 2) return(NA_real_)

  tab <- table(labels_a, labels_b)
  choose2 <- function(x) x * (x - 1) / 2
  sum_ij <- sum(choose2(as.numeric(tab)), na.rm = TRUE)
  sum_i  <- sum(choose2(rowSums(tab)), na.rm = TRUE)
  sum_j  <- sum(choose2(colSums(tab)), na.rm = TRUE)
  total  <- choose2(n)
  if (!is.finite(total) || total <= 0) return(NA_real_)

  expected <- (sum_i * sum_j) / total
  max_idx  <- 0.5 * (sum_i + sum_j)
  denom <- max_idx - expected
  if (!is.finite(denom) || abs(denom) < .Machine$double.eps) return(1)
  ari <- (sum_ij - expected) / denom
  max(-1, min(1, ari))
}

mean_pairwise_ari <- function(cluster_runs) {
  cluster_runs <- cluster_runs[!vapply(cluster_runs, is.null, logical(1))]
  if (length(cluster_runs) < 2) return(NA_real_)
  vals <- c()
  for (i in seq_len(length(cluster_runs) - 1L)) {
    for (j in (i + 1L):length(cluster_runs))
      vals <- c(vals, adjusted_rand_index(cluster_runs[[i]], cluster_runs[[j]]))
  }
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NA_real_)
  max(0, min(1, mean(vals, na.rm = TRUE)))
}

compute_auto_detect_composite_scores <- function(diag_df) {
  if (is.null(diag_df) || nrow(diag_df) == 0) return(diag_df)

  diag_df$BCI_score <- sqrt(pmax(0, pmin(1, rescale_01_safe(diag_df$BCI))))
  diag_df$Purity_score <- 1 - pmax(0, pmin(1, diag_df$WeightedEntropy))
  diag_df$Purity_score[!is.finite(diag_df$Purity_score)] <- 0.5
  diag_df$Stability_score <- pmax(0, pmin(1, diag_df$Stability))
  diag_df$Stability_score[!is.finite(diag_df$Stability_score)] <- 0.5
  diag_df$Parsimony_score <- exp(-0.03 * pmax(0, diag_df$k - 2))

  eps <- 1e-6
  log_score <-
    0.30 * log(pmax(diag_df$BCI_score, eps)) +
    0.35 * log(pmax(diag_df$Purity_score, eps)) +
    0.25 * log(pmax(diag_df$Stability_score, eps)) +
    0.10 * log(pmax(diag_df$Parsimony_score, eps))

  diag_df$CompositeScore <- exp(log_score)
  diag_df$CompositeScore[!is.finite(diag_df$CompositeScore)] <- NA_real_
  diag_df$Composite_relative <- rescale_01_safe(diag_df$CompositeScore)
  diag_df
}


compute_cluster_ambiguity <- function(result_df, box_wide, elems) {
  if (is.null(result_df) || is.null(box_wide) || nrow(result_df) == 0 || nrow(box_wide) == 0)
    return(data.frame())

  dat <- dplyr::bind_cols(
    result_df %>% dplyr::select(x, y, cluster_id, cluster_label),
    box_wide %>% dplyr::select(dplyr::all_of(elems))
  )
  active_ids <- unique(as.character(dat$cluster_id))
  active_ids <- active_ids[order(suppressWarnings(as.numeric(active_ids)), active_ids)]

  if (length(active_ids) == 1)
    return(dat %>% dplyr::mutate(purity=1, ambiguity=0,
                                 nearest_cluster_id=cluster_id, nearest_cluster_label=cluster_label,
                                 entropy_kernel="idw"))

  centroids <- dat %>% dplyr::group_by(cluster_id, cluster_label) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(elems), ~mean(.x, na.rm=TRUE)), .groups="drop")

  centroid_mat <- as.matrix(centroids[, elems, drop = FALSE])
  pix_mat      <- as.matrix(dat[, elems, drop = FALSE])
  pix_sq  <- rowSums(pix_mat^2,      na.rm = TRUE)
  cen_sq  <- rowSums(centroid_mat^2, na.rm = TRUE)
  d2 <- outer(pix_sq, cen_sq, "+") - 2 * (pix_mat %*% t(centroid_mat))
  d2[d2 < 0] <- 0
  dist_mat <- sqrt(d2)

  eps <- 1e-10
  w_mat <- 1 / (dist_mat + eps)
  row_sums <- rowSums(w_mat, na.rm = TRUE)
  row_sums[!is.finite(row_sums) | row_sums <= 0] <- 1
  prob_mat <- w_mat / row_sums

  n_cent     <- nrow(centroid_mat)
  winner_idx <- apply(prob_mat, 1, which.max)
  shannon_norm <- -rowSums(prob_mat * log(pmax(prob_mat, 1e-12)), na.rm = TRUE) / log(n_cent)
  shannon_norm[!is.finite(shannon_norm)] <- 0
  shannon_norm <- pmin(pmax(shannon_norm, 0), 1)

  dat$purity                <- apply(prob_mat, 1, max, na.rm = TRUE)
  dat$ambiguity             <- shannon_norm
  dat$nearest_cluster_id    <- centroids$cluster_id[winner_idx]
  dat$nearest_cluster_label <- centroids$cluster_label[winner_idx]
  dat$entropy_kernel        <- "idw"
  dat
}

cluster_result_to_matrix <- function(result_df, value_col = "cluster_id") {
  if (is.null(result_df) || nrow(result_df) == 0 || !(value_col %in% names(result_df))) return(NULL)
  nr <- max(result_df$y, na.rm = TRUE); nc <- max(result_df$x, na.rm = TRUE)
  mat <- matrix(NA_character_, nrow = nr, ncol = nc)
  idx <- cbind(as.integer(result_df$y), as.integer(result_df$x))
  mat[idx] <- as.character(result_df[[value_col]])
  mat
}

mode_character <- function(x) {
  x <- as.character(x); x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

apply_cluster_mode_filter <- function(result_df, window_size = 3) {
  mat <- cluster_result_to_matrix(result_df, "cluster_id")
  if (is.null(mat)) return(result_df)
  w <- suppressWarnings(as.integer(window_size))
  if (!is.finite(w) || w < 3) w <- 3L
  pad_before <- floor(w / 2); pad_after <- w - pad_before - 1L
  out <- mat; nr <- nrow(mat); nc <- ncol(mat)
  for (r in seq_len(nr)) {
    r1 <- max(1L, r - pad_before); r2 <- min(nr, r + pad_after)
    for (c in seq_len(nc)) {
      c1 <- max(1L, c - pad_before); c2 <- min(nc, c + pad_after)
      out[r, c] <- mode_character(mat[r1:r2, c1:c2])
    }
  }
  idx <- cbind(as.integer(result_df$y), as.integer(result_df$x))
  res <- result_df; res$cluster_id <- as.character(out[idx])
  label_lookup <- result_df %>% dplyr::distinct(cluster_id, cluster_label)
  res <- res %>% dplyr::select(-cluster_label) %>% dplyr::left_join(label_lookup, by = "cluster_id")
  res$cluster_label[is.na(res$cluster_label)] <- paste0("Cluster ", res$cluster_id[is.na(res$cluster_label)])
  res
}

apply_cluster_border_erosion <- function(result_df, box_wide, erosion_pixels = 5) {
  mat <- cluster_result_to_matrix(result_df, "cluster_id")
  if (is.null(mat)) return(list(result_df = result_df, box_wide = box_wide))
  d <- suppressWarnings(as.integer(erosion_pixels))
  if (!is.finite(d) || d <= 0) return(list(result_df = result_df, box_wide = box_wide))
  nr <- nrow(mat); nc <- ncol(mat)
  keep <- rep(TRUE, nrow(result_df))
  for (i in seq_len(nrow(result_df))) {
    r <- as.integer(result_df$y[i]); c <- as.integer(result_df$x[i])
    cid <- as.character(result_df$cluster_id[i])
    r1 <- max(1L, r-d); r2 <- min(nr, r+d); c1 <- max(1L, c-d); c2 <- min(nc, c+d)
    vals <- as.character(mat[r1:r2, c1:c2]); vals <- vals[!is.na(vals)]
    keep[i] <- length(vals) > 0 && all(vals == cid)
  }
  list(result_df = result_df[keep, , drop=FALSE], box_wide = box_wide[keep, , drop=FALSE])
}

build_stats_from_filtered <- function(result_df_map, box_wide_map, elems, cluster_colors, entropy_rank_percent = NULL) {
  amb_df <- compute_cluster_ambiguity(result_df_map, box_wide_map, elems)

  if (!is.null(entropy_rank_percent) && is.finite(entropy_rank_percent) && nrow(amb_df) > 0) {
    keep_prop <- max(0.30, min(1.00, entropy_rank_percent / 100))
    keep <- rep(FALSE, nrow(amb_df))
    for (idx in split(seq_len(nrow(amb_df)), as.character(amb_df$cluster_id))) {
      ord <- order(amb_df$ambiguity[idx], decreasing = FALSE, na.last = TRUE)
      n_keep <- max(1L, ceiling(length(idx) * keep_prop))
      keep[idx[ord[seq_len(min(n_keep, length(ord)))]]] <- TRUE
    }
  } else {
    keep <- rep(TRUE, nrow(amb_df))
  }

  stats_result_df <- result_df_map[keep, , drop = FALSE]
  stats_box_wide  <- box_wide_map[keep, , drop = FALSE]
  stats_box_long  <- dplyr::bind_cols(stats_result_df, stats_box_wide) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(elems), names_to = "Element", values_to = "Value")

  if (nrow(stats_result_df) > 0) {
    summary_parts <- compute_cluster_summary_tables(stats_result_df, stats_box_long, cluster_colors, elems,
                                                    ambiguity_df = amb_df[keep, , drop = FALSE])
    summary_table        <- summary_parts$summary_table
    cluster_colors_named <- summary_parts$cluster_colors_named
    active_ids           <- summary_parts$active_ids
    label_map            <- summary_parts$label_map
  } else {
    base_ids  <- sort(unique(as.character(result_df_map$cluster_id)))
    label_map <- result_df_map %>% dplyr::distinct(cluster_id, cluster_label) %>%
      dplyr::mutate(cluster_id = as.character(cluster_id))
    label_levels <- label_map$cluster_label[match(base_ids, label_map$cluster_id)]
    cluster_colors_named <- setNames(cluster_colors[base_ids], label_levels)
    summary_table <- data.frame(Cluster = character()); active_ids <- base_ids
  }

  list(ambiguity_df = amb_df, stats_result_df = stats_result_df,
       stats_box_wide = stats_box_wide, stats_box_long = stats_box_long,
       summary_table = summary_table, cluster_colors_named = cluster_colors_named,
       active_ids = active_ids, label_map = label_map,
       filtered_n = nrow(stats_result_df), total_n = nrow(result_df_map))
}

loaded_data_ready <- function(core) {
  !is.null(core) && is.list(core) && length(core$elements) > 0
}

render_corrplot <- function(cor_mat, orientation = "vertical", title = NULL) {
  col_fun <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  title_txt <- title %||% ""
  corrplot::corrplot(
    cor_mat,
    method = "shade",
    shade.col = NA,
    tl.col = "black",
    tl.srt = 45,
    col = col_fun(200),
    addCoef.col = "black",
    cl.pos = if (identical(orientation, "vertical")) "r" else "b",
    order = "AOE",
    title = title_txt,
    mar = c(1.2, 1.2, if (nzchar(title_txt)) 5.8 else 1.2, 1.2)
  )
}

parse_decimal_value <- function(x, decimal_sep = ".") {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- trimws(as.character(x)); if (!nzchar(x)) return(NA_real_)
  if (decimal_sep == ",") { x <- gsub("\\.", "", x); x <- gsub(",", ".", x, fixed=TRUE) }
  else x <- gsub(",", "", x, fixed=TRUE)
  out <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(out), out, NA_real_)
}

format_decimal_value <- function(x, decimal_sep = ".", digits = 4) {
  if (is.null(x) || length(x) == 0 || !is.finite(x)) return("")
  out <- format(round(x, digits), trim=TRUE, scientific=FALSE, nsmall=0)
  if (decimal_sep == ",") out <- gsub("\\.", ",", out)
  out
}

resolve_entropy_rank_percent <- function(enabled, value, default = 100) {
  if (!isTRUE(enabled)) return(NA_real_)
  x <- suppressWarnings(as.numeric(value))
  if (length(x) == 0 || !is.finite(x[1])) return(as.numeric(default))
  max(30, min(100, x[1]))
}

build_analysis_dataframe <- function(active_core, cluster_state = NULL, use_raw = FALSE) {
  if (is.null(active_core) || length(active_core$elements) == 0) return(data.frame())
  mats <- if (isTRUE(use_raw)) (active_core$mats_raw %||% active_core$matrices %||% active_core$mats)
          else (active_core$matrices %||% active_core$mats)
  df <- matrix_list_to_merged_df(unname(mats[active_core$elements]), active_core$elements)
  if (!is.null(cluster_state) && !is.null(cluster_state$result_df)) {
    if (isTRUE(cluster_state$invert_cluster_coords)) df <- maybe_invert_df(df, TRUE)
    df <- dplyr::left_join(df,
      cluster_state$result_df %>% dplyr::select(x, y, cluster_id, cluster_label),
      by = c("x", "y"))
  }
  df
}

sample_rows_for_preview <- function(df, frac = 0.15, max_n = 1500) {
  if (is.null(df) || nrow(df) == 0) return(df)
  n_target <- min(max_n, max(1L, floor(nrow(df) * frac)))
  if (n_target >= nrow(df)) return(df)
  df[sample.int(nrow(df), n_target, replace = FALSE), , drop = FALSE]
}

sample_rows_analysis <- function(df, frac = 0.30, max_n = 2000, cluster_col = "cluster_label") {
  if (is.null(df) || nrow(df) == 0) return(df)
  n_target <- min(max_n, max(1L, floor(nrow(df) * frac)))
  if (n_target >= nrow(df)) return(df)
  if (!(cluster_col %in% names(df)) || all(is.na(df[[cluster_col]])))
    return(df[sample.int(nrow(df), n_target, replace=FALSE), , drop=FALSE])
  grp <- as.character(df[[cluster_col]]); grp[is.na(grp) | !nzchar(grp)] <- "__NA__"
  idx_split <- split(seq_len(nrow(df)), grp)
  group_sizes <- vapply(idx_split, length, integer(1))
  alloc <- floor(group_sizes / sum(group_sizes) * n_target)
  remainder <- n_target - sum(alloc)
  if (remainder > 0) {
    ord <- order((group_sizes / sum(group_sizes) * n_target) - alloc, decreasing=TRUE)
    alloc[ord[seq_len(remainder)]] <- alloc[ord[seq_len(remainder)]] + 1L
  }
  sampled_idx <- unlist(Map(function(idx, n_take) {
    if (n_take <= 0) integer(0) else sample(idx, size=min(length(idx), n_take), replace=FALSE)
  }, idx_split, alloc), use.names=FALSE)
  if (length(sampled_idx) == 0) sampled_idx <- sample.int(nrow(df), min(n_target, nrow(df)), replace=FALSE)
  df[sampled_idx, , drop=FALSE]
}

summary_stats_for_matrix <- function(x) {
  x_all <- as.numeric(x); n_total <- length(x_all)
  miss_n <- sum(!is.finite(x_all) | is.na(x_all))
  x <- x_all[is.finite(x_all)]
  if (length(x) == 0) return(data.frame(N=0L, MissingPct=NA_real_, OutlierPct=NA_real_, Min=NA_real_,
    Q1=NA_real_, Median=NA_real_, Average=NA_real_, Q3=NA_real_, IQR=NA_real_,
    StandardDeviation=NA_real_, StandardError=NA_real_, Max=NA_real_))
  qs <- stats::quantile(x, probs=c(0.25, 0.5, 0.75), na.rm=TRUE, names=FALSE)
  iqr <- qs[3] - qs[1]
  data.frame(N=length(x), MissingPct=100*miss_n/max(1L,n_total),
    OutlierPct=100*mean(x > qs[3]+1.5*iqr, na.rm=TRUE),
    Min=min(x,na.rm=TRUE), Q1=qs[1], Median=qs[2], Average=mean(x,na.rm=TRUE),
    Q3=qs[3], IQR=iqr, StandardDeviation=stats::sd(x,na.rm=TRUE),
    StandardError=stats::sd(x,na.rm=TRUE)/sqrt(length(x)), Max=max(x,na.rm=TRUE))
}

table_df_to_lines <- function(df, max_rows = 40) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return("No data available.")
  df2 <- utils::head(df, max_rows)
  txt <- utils::capture.output(print(df2, row.names=FALSE, right=FALSE))
  if (nrow(df) > max_rows) txt <- c(txt, paste0("... truncated to first ", max_rows, " rows"))
  txt
}

processing_method_catalog <- function(has_norm = TRUE) {
  tbl <- data.frame(
    code  = c("cps_wt","raw","clr","ln","minmax","sdscale"),
    label = c("Pixel-wise CPS to Wt%","Raw value","Centered Log-Ratio",
              "Natural logarithm","MinMax","Data Scaling (SD)"),
    description = c(
      "Converts pixel-wise CPS values to concentration-like values by rescaling each elemental matrix with the normalization percentages informed in Input Data.",
      "Keeps the original numeric input values exactly as loaded, without additional transformation.",
      "Applies the centered log-ratio transform to emphasize relative compositional relationships while reducing closure effects.",
      "Applies the natural logarithm to reduce right skewness and compress large values.",
      "Rescales each variable independently to the 0 to 1 range using its minimum and maximum values.",
      "Centers and scales each variable by its standard deviation so that variables become more comparable in spread."
    ),
    stringsAsFactors = FALSE
  )
  if (!isTRUE(has_norm)) tbl <- tbl[tbl$code != "cps_wt", , drop=FALSE]
  tbl
}

processing_method_label <- function(code, has_norm = TRUE) {
  tbl <- processing_method_catalog(has_norm)
  hit <- tbl$label[match(code, tbl$code)]
  ifelse(is.na(hit), code, hit)
}

processing_method_description <- function(code, has_norm = TRUE) {
  tbl <- processing_method_catalog(has_norm)
  hit <- tbl$description[match(code, tbl$code)]
  ifelse(is.na(hit), "", hit)
}

report_format_df <- function(df, digits = 2) {
  if (is.null(df) || !is.data.frame(df)) return(data.frame())
  out <- df
  for (nm in names(out)) {
    if (is.numeric(out[[nm]])) {
      out[[nm]] <- ifelse(is.na(out[[nm]]), "",
                          format(round(out[[nm]], digits), nsmall=0, trim=TRUE, scientific=FALSE))
    } else {
      out[[nm]] <- as.character(out[[nm]]); out[[nm]][is.na(out[[nm]])] <- ""
    }
  }
  out
}

draw_report_text_page <- function(title, paragraphs, subtitle = NULL) {
  grid::grid.newpage()
  grid::grid.text(title, x=0.04, y=0.97, just=c("left","top"),
                  gp=grid::gpar(fontsize=16, fontface="bold", fontfamily="Arial"))
  y <- 0.93
  if (!is.null(subtitle) && nzchar(subtitle)) {
    grid::grid.text(subtitle, x=0.04, y=y, just=c("left","top"),
                    gp=grid::gpar(fontsize=10, col="grey35", fontfamily="Arial"))
    y <- y - 0.045
  }
  left_x <- 0.06; chars_per_line <- 102; line_h <- 0.028; para_gap <- 0.05
  for (p in paragraphs) {
    txt <- paste(strwrap(p, width=chars_per_line), collapse="\n")
    grid::grid.text(txt, x=left_x, y=y, just=c("left","top"),
                    gp=grid::gpar(fontsize=11, fontfamily="Arial"))
    n_lines <- max(1L, length(strsplit(txt, "\n")[[1]]))
    y <- y - (line_h * n_lines + para_gap)
    if (y < 0.08) break
  }
}

draw_report_table_pages <- function(title, df, subtitle = NULL, rows_per_page = 24, digits = 2) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    draw_report_text_page(title, "No data available for this section.", subtitle=subtitle)
    return(invisible(NULL))
  }
  df_fmt <- report_format_df(df, digits=digits)
  n_pages <- ceiling(nrow(df_fmt) / rows_per_page)
  for (pg in seq_len(n_pages)) {
    idx <- ((pg-1L)*rows_per_page+1L):min(pg*rows_per_page, nrow(df_fmt))
    page_df <- df_fmt[idx, , drop=FALSE]
    cols <- names(page_df); cell_mat <- as.matrix(page_df)
    header_chars <- nchar(cols, type="width") + 4
    col_chars <- vapply(seq_along(cols), function(j)
      max(c(header_chars[j], nchar(cell_mat[,j], type="width")+2), na.rm=TRUE), numeric(1))
    widths <- col_chars / sum(col_chars)
    x_left <- 0.06 + c(0, cumsum(widths[-length(widths)]) * 0.88)
    x_width <- widths * 0.88; x_mid <- x_left + x_width / 2
    grid::grid.newpage()
    grid::grid.text(title, x=0.06, y=0.94, just=c("left","top"),
                    gp=grid::gpar(fontsize=14, fontface="bold", fontfamily="Arial"))
    y_title <- 0.90
    if (!is.null(subtitle) && nzchar(subtitle)) {
      grid::grid.text(subtitle, x=0.06, y=y_title, just=c("left","top"),
                      gp=grid::gpar(fontsize=9, col="grey35", fontfamily="Arial"))
      y_title <- y_title - 0.03
    }
    if (n_pages > 1L)
      grid::grid.text(sprintf("Page %d of %d", pg, n_pages), x=0.94, y=0.94,
                      just=c("right","top"), gp=grid::gpar(fontsize=8, col="grey35", fontfamily="Arial"))
    row_h <- min(0.030, 0.80 / (nrow(page_df)+1)); y_top <- y_title - 0.02
    for (j in seq_along(cols)) {
      grid::grid.rect(x=x_mid[j], y=y_top, width=x_width[j], height=row_h, just="center",
                      gp=grid::gpar(fill="grey85", col="grey55"))
      grid::grid.text(cols[j], x=x_mid[j], y=y_top, just=c("center","center"),
                      gp=grid::gpar(fontsize=8.5, fontface="bold", fontfamily="Arial"))
    }
    for (i in seq_len(nrow(page_df))) {
      y_i <- y_top - i * row_h
      fill_i <- if (i %% 2 == 1) "white" else "grey97"
      for (j in seq_along(cols)) {
        grid::grid.rect(x=x_mid[j], y=y_i, width=x_width[j], height=row_h, just="center",
                        gp=grid::gpar(fill=fill_i, col="grey80"))
        grid::grid.text(cell_mat[i,j], x=x_mid[j], y=y_i, just=c("center","center"),
                        gp=grid::gpar(fontsize=8, fontfamily="Arial"))
      }
    }
  }
  invisible(NULL)
}

extract_cluster_mean_table <- function(summary_table) {
  if (is.null(summary_table) || !is.data.frame(summary_table) || nrow(summary_table) == 0) return(data.frame())
  summary_table[, !grepl("_SD$", names(summary_table)), drop=FALSE]
}

extract_cluster_sd_table <- function(summary_table) {
  if (is.null(summary_table) || !is.data.frame(summary_table) || nrow(summary_table) == 0) return(data.frame())
  base_cols <- intersect(c("Cluster","Percentage","Entropy","Entropy_SD"), names(summary_table))
  sd_cols <- grep("_SD$", names(summary_table), value=TRUE)
  out <- summary_table[, c(base_cols, sd_cols), drop=FALSE]
  names(out) <- sub("_SD$", "", names(out)); out
}

build_processing_pairs_plot <- function(core, max_n = 1500, report_mode = FALSE) {
  req(!is.null(core), loaded_data_ready(core))
  df <- matrix_list_to_merged_df(unname(core$matrices[core$elements]), core$elements) %>%
    dplyr::select(dplyr::all_of(core$elements))
  df <- df[stats::complete.cases(df), , drop=FALSE]
  validate(need(nrow(df) > 1, "Not enough complete rows are available for the relationships preview."))
  df <- sample_rows_for_preview(df, frac=0.15, max_n=max_n)

  n_vars <- length(core$elements)
  cor_size  <- if (report_mode) 2 else max(2.2, min(4.2, 13 / sqrt(max(1, n_vars))))
  strip_size <- if (report_mode) 9 else max(6.0, min(9.0, 18 / sqrt(max(1, n_vars))))
  axis_size  <- if (report_mode) 6 else max(4.5, min(7.0, 14 / sqrt(max(1, n_vars))))
  point_size <- if (report_mode) 0.35 else 0.45

  p <- GGally::ggpairs(df, columns=seq_along(core$elements),
    lower = list(continuous = GGally::wrap("points", alpha=0.7, size=point_size)),
    diag  = list(continuous = GGally::wrap("densityDiag", alpha=0.7)),
    upper = list(continuous = GGally::wrap("cor", size=cor_size, alignPercent=1,
                                           justify_labels="center", stars=FALSE))) +
    theme_bw(base_size=if (report_mode) 8 else 9) +
    theme(strip.placement="outside", strip.background=element_blank(),
          strip.text=element_text(size=strip_size, face="bold"),
          strip.text.x.top=element_text(size=strip_size, face="bold"),
          strip.text.y.right=element_text(size=strip_size, face="bold", angle=270),
          axis.text.x=element_text(size=axis_size, angle=45, hjust=1),
          axis.text.y=element_text(size=axis_size),
          axis.title=element_blank(), panel.grid=element_blank(),
          panel.spacing=grid::unit(if (report_mode) 0.12 else 0.05,"lines"),
          plot.margin=margin(5,5,5,5), aspect.ratio=0.85)

  for (i in seq_len(n_vars)) for (j in seq_len(n_vars)) {
    if (i >= j)
      p[i,j] <- p[i,j] + theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
                                 axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                                 axis.title.x=element_blank(), axis.title.y=element_blank())
  }
  p
}

make_cluster_entropy_plot <- function(df, fill_var, title, aspect_ratio, fill_scale,
                                      map_width_mm = NULL, map_height_mm = NULL,
                                      invert_coords = FALSE) {
  p <- ggplot(df, aes(x=x, y=y, fill=.data[[fill_var]])) +
    geom_raster() + coord_equal() + fill_scale +
    theme_minimal() + labs(title=title) +
    theme(
      plot.title      = element_text(hjust=0.5, face="bold"),
      axis.title      = element_blank(),
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      panel.grid      = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "vertical",
      plot.background = element_rect(fill = "white", colour = NA)
    )
  scale_width_mm  <- if (isTRUE(invert_coords)) map_height_mm else map_width_mm
  scale_height_mm <- if (isTRUE(invert_coords)) map_width_mm  else map_height_mm
  add_scale_bar(p, build_scale_annotation(df, scale_width_mm, scale_height_mm, y_reversed = FALSE))
}

make_static_ternary_plot <- function(plot_df, avar, bvar, cvar, color_mode="cluster",
                                     color_var=NULL, cluster_colors_named=NULL) {
  tri <- data.frame(x=c(0,1,0.5,0), y=c(0,0,sqrt(3)/2,0))
  plot_df$x_tern <- plot_df$b + plot_df$c / 2
  plot_df$y_tern <- plot_df$c * sqrt(3) / 2
  base <- ggplot() +
    geom_path(data=tri, aes(x=x,y=y), linewidth=0.6, color="black") +
    annotate("text", x=0,   y=-0.03,          label=avar, hjust=0, size=3.6) +
    annotate("text", x=1,   y=-0.03,          label=bvar, hjust=1, size=3.6) +
    annotate("text", x=0.5, y=sqrt(3)/2+0.03, label=cvar, vjust=0, size=3.6) +
    coord_equal() + theme_void() +
    theme(plot.title=element_text(hjust=0.5), legend.position="right") +
    labs(title="Ternary Plot")
  switch(color_mode,
    cluster = base + geom_point(data=plot_df, aes(x=x_tern, y=y_tern, color=cluster_label),
                                alpha=0.7, size=1.4) +
              scale_color_manual(values=cluster_colors_named, drop=FALSE) + labs(color="Cluster"),
    entropy = base + geom_point(data=plot_df, aes(x=x_tern, y=y_tern, color=ambiguity),
                                alpha=0.7, size=1.4) +
              scale_color_viridis_c(option="magma") + labs(color="Entropy"),
    base + geom_point(data=plot_df, aes(x=x_tern, y=y_tern, color=.data[[color_var]]),
                      alpha=0.7, size=1.4) +
    scale_color_viridis_c(option="viridis") + labs(color=color_var)
  )
}

resolve_ternary_axis <- function(var_name, plot_df, core, dat) {
  if (var_name %in% names(plot_df)) return(plot_df[[var_name]])
  if (var_name %in% core$elements) {
    base_df <- build_analysis_dataframe(core, dat, use_raw=FALSE)
    base_df <- dplyr::left_join(plot_df %>% dplyr::select(x, y),
                                base_df %>% dplyr::select(x, y, dplyr::all_of(var_name)),
                                by=c("x","y"))
    return(base_df[[var_name]])
  }
  rep(NA_real_, nrow(plot_df))
}

build_map_plot <- function(df, elements, map_width_mm = NULL, map_height_mm = NULL,
                           invert_coords = FALSE, legend_orientation = "horizontal") {
  legend_df <- attr(df, "legend_df")
  make_composite_plot(df, title="RGB Map", legend_df=legend_df,
                      map_width_mm=map_width_mm, map_height_mm=map_height_mm,
                      invert_coords=invert_coords, legend_orientation=legend_orientation)
}

analysis_pc_names <- function(dat) {
  if (is.null(dat) || is.null(dat$elems) || length(dat$elems) < 2 || is.null(dat$box_wide))
    return(character(0))
  n_pc <- min(length(dat$elems), max(0L, nrow(dat$box_wide) - 1L))
  if (n_pc < 1) character(0) else paste0("PC", seq_len(n_pc))
}

build_cluster_analysis_scores <- function(dat, core) {
  if (is.null(dat) || is.null(core) || is.null(dat$result_df) || is.null(dat$box_wide))
    return(data.frame())

  base <- dat$result_df %>% dplyr::select(x, y, cluster_id, cluster_label)
  elems <- intersect(dat$elems %||% character(0), names(dat$box_wide))
  out <- base

  if (length(elems) >= 2 && nrow(dat$box_wide) > 1) {
    X <- as.matrix(dat$box_wide[, elems, drop = FALSE])
    X[!is.finite(X)] <- NA_real_
    for (j in seq_len(ncol(X))) {
      med_j <- stats::median(X[, j], na.rm = TRUE)
      if (!is.finite(med_j)) med_j <- 0
      X[!is.finite(X[, j]), j] <- med_j
    }
    X_scaled <- scale(X, center = TRUE, scale = TRUE)
    X_scaled[!is.finite(X_scaled)] <- 0
    pca_model <- stats::prcomp(X_scaled, center = FALSE, scale. = FALSE)
    pc_scores <- as.data.frame(pca_model$x)
    names(pc_scores) <- paste0("PC", seq_len(ncol(pc_scores)))
    out <- dplyr::bind_cols(out, pc_scores)
  }

  out <- dplyr::bind_cols(out, dat$box_wide)

  missing_core_elements <- setdiff(core$elements %||% character(0), names(out))
  if (length(missing_core_elements) > 0) {
    base_df <- build_analysis_dataframe(core, dat, use_raw = FALSE)
    out <- dplyr::left_join(
      out,
      base_df %>% dplyr::select(x, y, dplyr::all_of(missing_core_elements)),
      by = c("x", "y")
    )
  }

  out
}

analysis_axis_choices <- function(dat, core) {
  unique(c(analysis_pc_names(dat), core$elements %||% character(0)))
}

# =============================================================================
# UI
# =============================================================================

ui <- tagList(
  tags$head(
    tags$style(HTML("
      .ve-main-panel { width: 100%; }
      .ve-element-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(420px, 1fr));
        gap: 16px;
        align-items: start;
      }
      .ve-plot-card {
        background: #ffffff;
        border: 1px solid #d9e2ec;
        border-radius: 8px;
        padding: 10px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      }
      .ve-note { font-size: 12px; color: #5c6b7a; margin-top: 6px; }
      .ve-box {
        background: #ffffff;
        border: 1px solid #d9e2ec;
        border-radius: 8px;
        padding: 14px;
        margin-bottom: 16px;
      }
      .ve-navbar-title { display: flex; align-items: center; gap: 10px; }
      .ve-navbar-save .btn { padding: 6px 10px; margin: 8px 4px 8px 0; }
      .ve-cluster-color-row {
        display: flex;
        align-items: flex-end;
        gap: 8px;
        flex-wrap: nowrap;
        margin-bottom: 10px;
        padding-bottom: 2px;
        overflow: visible;
        position: relative;
        z-index: 1;
      }
      .ve-cluster-color-row:hover,
      .ve-cluster-color-row:focus-within {
        z-index: 10000;
      }
      .ve-cluster-color-row .form-group {
        margin-bottom: 0;
      }
      .ve-cluster-color-row,
      .ve-cluster-color-row * {
        box-shadow: none;
      }
      .ve-cluster-color-label {
        width: 92px;
        min-width: 92px;
        margin-bottom: 8px;
        font-weight: 600;
        white-space: nowrap;
      }
      .ve-cluster-color-picker {
        width: 40px;
        min-width: 40px;
        max-width: 40px;
        overflow: visible;
      }
      .ve-cluster-color-picker .shiny-input-container,
      .ve-cluster-color-picker .form-group,
      .ve-cluster-color-picker .colourpicker-input-container {
        width: 40px !important;
        min-width: 40px !important;
        max-width: 40px !important;
        margin-bottom: 0 !important;
        overflow: visible !important;
      }
      .ve-cluster-color-picker .input-group {
        width: 40px !important;
        min-width: 40px !important;
        max-width: 40px !important;
        display: block !important;
        overflow: visible !important;
      }
      .ve-cluster-color-picker input.form-control,
      .ve-cluster-color-picker .input-group input {
        display: block !important;
        width: 40px !important;
        min-width: 40px !important;
        max-width: 40px !important;
        height: 34px !important;
        padding: 0 !important;
        border-radius: 4px !important;
        cursor: pointer !important;
        color: transparent !important;
        text-shadow: none !important;
        caret-color: transparent !important;
      }
      .ve-cluster-color-picker input.form-control::selection,
      .ve-cluster-color-picker .input-group input::selection {
        color: transparent !important;
        background: transparent !important;
      }
      .ve-cluster-color-picker .input-group-addon,
      .ve-cluster-color-picker .colourpicker-input-addon,
      .ve-cluster-color-picker .colourpicker-input-container span {
        display: none !important;
      }
      .ve-cluster-color-picker,
      .ve-cluster-color-picker .shiny-input-container,
      .ve-cluster-color-picker .form-group,
      .ve-cluster-color-picker .colourpicker-input-container,
      .ve-cluster-color-picker .input-group {
        position: relative;
        z-index: 10001;
      }
      .colorpicker,
      .colourpicker,
      .colourpicker-dropdown,
      .colourpicker-menu,
      .dropdown-menu.colorpicker,
      .bootstrap-colorpicker-widget,
      .sp-container,
      .sp-replacer,
      .shiny-bound-input + .colorpicker {
        z-index: 999999 !important;
      }

      .ve-post-cluster-row {
        display: flex;
        align-items: flex-end;
        gap: 10px;
        margin-bottom: 8px;
        overflow: visible;
        position: relative;
      }
      .ve-post-cluster-row:focus-within {
        z-index: 1000000;
      }
      .ve-post-cluster-row .shiny-input-container {
        margin-bottom: 0 !important;
      }
      .ve-post-cluster-name {
        width: 500px;
        max-width: 500px;
        min-width: 220px;
      }
      .ve-post-cluster-name input[type='text'] {
        width: 500px !important;
        max-width: 500px !important;
      }
      .ve-post-cluster-picker {
        width: 40px;
        min-width: 40px;
        max-width: 40px;
        overflow: visible;
        align-self: flex-end;
        position: relative;
        z-index: 10001;
      }
      .ve-post-cluster-picker:focus-within {
        z-index: 1000001;
      }
      .ve-box, .well, .wellPanel, .tab-content, .tab-pane {
        overflow: visible !important;
      }
      .ve-post-cluster-picker .shiny-input-container,
      .ve-post-cluster-picker .form-group,
      .ve-post-cluster-picker .colourpicker-input-container,
      .ve-post-cluster-picker .input-group {
        width: 40px !important;
        min-width: 40px !important;
        max-width: 40px !important;
        margin-bottom: 0 !important;
        overflow: visible !important;
        position: relative;
        z-index: 10001;
      }
      .ve-post-cluster-picker input.form-control,
      .ve-post-cluster-picker .input-group input {
        display: block !important;
        width: 40px !important;
        min-width: 40px !important;
        max-width: 40px !important;
        height: 34px !important;
        padding: 0 !important;
        border-radius: 4px !important;
        cursor: pointer !important;
        color: transparent !important;
        caret-color: transparent !important;
      }
      .ve-post-cluster-picker .input-group-addon,
      .ve-post-cluster-picker .colourpicker-input-addon,
      .ve-post-cluster-picker .colourpicker-input-container span {
        display: none !important;
      }
      .ve-auto-detect-row {
        display: flex;
        align-items: flex-end;
        gap: 14px;
        flex-wrap: nowrap;
        margin-bottom: 6px;
      }
      .ve-ncluster-box {
        width: 120px;
        min-width: 120px;
      }
      .ve-auto-detect-button-box {
        width: 140px;
        min-width: 140px;
        padding-bottom: 15px;
      }
      .ve-auto-sample-box {
        width: 400px;
        max-width: 400px;
        min-width: 260px;
        margin-bottom: 10px;
      }
      .ve-auto-sample-box .shiny-input-container {
        width: 100% !important;
        max-width: 400px !important;
        margin-bottom: 0 !important;
      }
      .ve-auto-sample-box .irs {
        margin-top: -4px;
      }
      .ve-post-control-500,
      .ve-post-control-500 .shiny-input-container {
        width: 500px !important;
        max-width: 500px !important;
      }
      .ve-post-control-500 select,
      .ve-post-control-500 input[type='number'] {
        max-width: 500px !important;
      }
      .ve-post-button-row {
        display: flex;
        gap: 10px;
        align-items: center;
        margin-top: 6px;
        margin-bottom: 8px;
      }
      .ve-post-button-row .btn {
        width: 150px !important;
        max-width: 150px !important;
      }
      .ve-rgb-field {
        width: 60px;
        min-width: 60px;
      }
      .ve-hex-field {
        width: 80px;
        min-width: 80px;
      }
      .ve-rgb-field input[type='number'],
      .ve-hex-field input[type='text'] {
        width: 100% !important;
        min-width: 100% !important;
        box-sizing: border-box;
        padding-left: 5px;
        padding-right: 3px;
        text-align: left;
      }
      .ve-rgb-field input[type='number']::-webkit-outer-spin-button,
      .ve-rgb-field input[type='number']::-webkit-inner-spin-button {
        -webkit-appearance: none;
        margin: 0;
      }
      .ve-rgb-field input[type='number'] {
        -moz-appearance: textfield;
      }

      /* Global visual language, shared by all tabs */
      body, .navbar, .tab-content, .form-control, .btn, label, .control-label {
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }
      body { background: #f5f7fb; }
      .navbar-default {
        background: #263746;
        border-color: #1f2f3d;
        box-shadow: 0 5px 18px rgba(15, 23, 42, 0.16);
        margin: 10px 12px 16px 12px;
        border-radius: 14px;
      }
      .navbar-default .navbar-brand,
      .navbar-default .ve-navbar-title,
      .navbar-default .ve-navbar-title span {
        color: #ffffff !important;
        font-weight: 800;
      }
      .navbar-default .navbar-nav > li > a {
        margin: 7px 3px;
        padding: 9px 13px;
        border-radius: 11px;
        background: #e7edf4;
        color: #102a43 !important;
        font-weight: 650;
        box-shadow: 0 1px 4px rgba(15, 23, 42, 0.08);
        transition: background 0.15s ease, box-shadow 0.15s ease, transform 0.15s ease;
      }

      .ve-global-actions {
        display: inline-flex;
        gap: 6px;
        align-items: center;
        margin-right: 8px;
      }
      .ve-global-action-btn {
        width: 30px;
        height: 30px;
        padding: 2px 0 !important;
        border-radius: 9px !important;
        border: 1px solid rgba(255,255,255,0.35) !important;
        background: rgba(255,255,255,0.12) !important;
        color: #ffffff !important;
        font-size: 16px !important;
        line-height: 1 !important;
        box-shadow: 0 2px 7px rgba(15, 23, 42, 0.20);
      }
      .ve-global-action-btn:hover,
      .ve-global-action-btn:focus {
        background: rgba(255,255,255,0.24) !important;
        color: #ffffff !important;
      }
      .ve-hidden-download {
        position: absolute !important;
        left: -10000px !important;
        top: auto !important;
        width: 1px !important;
        height: 1px !important;
        overflow: hidden !important;
        opacity: 0 !important;
        pointer-events: none !important;
      }
      .ve-global-action-btn i,
      .ve-global-action-btn .glyphicon,
      .ve-global-action-btn .fa {
        display: none !important;
      }
      .ve-global-actions .shiny-download-link.ve-global-action-btn {
        display: inline-flex !important;
        justify-content: center !important;
        align-items: center !important;
        text-decoration: none !important;
      }

      .ve-master-panel .shiny-input-container .input-group-btn,
      .ve-box .shiny-input-container .input-group-btn {
        padding-right: 8px;
      }
      .ve-master-panel .shiny-input-container .progress,
      .ve-box .shiny-input-container .progress {
        margin-top: 8px;
      }

      .navbar-default .navbar-nav > li > a:hover {
        background: #f3f6fa !important;
        color: #102a43 !important;
        transform: translateY(-1px);
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background: #ffffff !important;
        color: #102a43 !important;
        box-shadow: 0 4px 12px rgba(15, 23, 42, 0.22);
      }
      .navbar-default .navbar-brand {
        color: #ffffff !important;
      }
      .input-group .input-group-btn {
        padding-right: 8px;
      }
      .input-group .form-control {
        margin-left: 8px;
      }
      .ve-box, .well, .wellPanel, .ve-main-panel,
      .sidebarPanel, .mainPanel {
        background: #ffffff !important;
        border: 1px solid #d9e2ec !important;
        border-radius: 10px !important;
        box-shadow: 0 4px 14px rgba(15, 23, 42, 0.07) !important;
      }
      .ve-box h2, .ve-box h3, .ve-box h4,
      .ve-main-panel h2, .ve-main-panel h3, .ve-main-panel h4,
      .well h2, .well h3, .well h4 {
        color: #243b53;
        letter-spacing: 0.01em;
      }
      .btn {
        border-radius: 8px !important;
        font-weight: 650 !important;
        border-color: #bcccdc !important;
      }
      .btn-default {
        background: #f8fafc !important;
        color: #243b53 !important;
      }
      .btn-default:hover {
        background: #eef2f7 !important;
      }
      .form-control {
        border-radius: 7px !important;
        border-color: #bcccdc !important;
        box-shadow: none !important;
      }
      .ve-note { color: #627d98 !important; }

      .ve-cluster-layout {
        display: flex;
        gap: 18px;
        align-items: flex-start;
        width: 100%;
      }
      .ve-cluster-sidebar {
        flex: 0 0 650px;
        max-width: 650px;
        min-width: 360px;
      }
      .ve-cluster-preview {
        flex: 1 1 auto;
        min-width: 0;
      }
      .ve-collapse-box {
        background: #ffffff;
        border: 1px solid #d9e2ec;
        border-radius: 10px;
        padding: 0;
        margin-bottom: 14px;
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.11);
        overflow: visible !important;
      }
      .ve-collapse-box > summary {
        cursor: pointer;
        font-weight: 700;
        font-size: 18px;
        padding: 12px 14px;
        border-bottom: 1px solid #eef2f7;
        list-style: none;
        color: #243b53;
        background: #f8fafc;
        border-radius: 10px 10px 0 0;
      }
      .ve-collapse-box > summary::-webkit-details-marker {
        display: none;
      }
      .ve-collapse-box > summary::before {
        content: '▸';
        display: inline-block;
        width: 1.2em;
        color: #486581;
        font-size: 15px;
        transition: transform 0.15s ease;
      }
      .ve-collapse-box[open] > summary::before {
        content: '▾';
      }
      .ve-collapse-content {
        padding: 14px;
        overflow: visible !important;
      }
      .ve-cluster-download-box,
      .ve-preview-action-box {
        background: #ffffff;
        border: 1px solid #d9e2ec;
        border-radius: 10px;
        padding: 10px 12px;
        margin-bottom: 12px;
        box-shadow: 0 4px 14px rgba(15, 23, 42, 0.08);
      }
      .ve-cluster-download-row,
      .ve-preview-action-row {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        align-items: flex-end;
      }
      .ve-cluster-download-row .shiny-input-container,
      .ve-preview-action-row .shiny-input-container {
        margin-bottom: 0 !important;
      }
      .ve-preview-topbar {
        display: flex;
        gap: 12px;
        align-items: flex-start;
        justify-content: space-between;
        flex-wrap: wrap;
        margin-bottom: 12px;
      }
      .ve-preview-tabs {
        background: #ffffff;
        border: 1px solid #d9e2ec;
        border-radius: 10px;
        padding: 12px;
        box-shadow: 0 4px 14px rgba(15, 23, 42, 0.07);
      }
      .ve-preview-tabs .nav {
        margin-bottom: 12px;
      }
      .ve-preview-tabs .nav > li > a {
        padding: 7px 12px;
        font-weight: 650;
        color: #243b53;
        border-radius: 8px;
        background: #eef2f7;
        border: 1px solid #d9e2ec;
        box-shadow: 0 1px 3px rgba(15, 23, 42, 0.05);
        transition: background 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease;
      }
      .ve-preview-tabs .nav > li > a:hover,
      .ve-preview-tabs .nav > li > a:focus {
        background: #e2e8f0;
        color: #102a43;
        border-color: #b6c5d6;
        box-shadow: 0 2px 6px rgba(15, 23, 42, 0.08);
      }
      .ve-preview-tabs .nav-pills > li.active > a,
      .ve-preview-tabs .nav-pills > li.active > a:hover,
      .ve-preview-tabs .nav-pills > li.active > a:focus {
        background: #486581;
        color: #ffffff;
        border-color: #486581;
        box-shadow: 0 3px 8px rgba(72, 101, 129, 0.25);
      }

      .ve-report-section-selected {
        background: #eef2f7;
        border: 1px solid #cbd5e1;
        border-radius: 10px;
        padding: 10px 12px;
        margin-bottom: 12px;
      }
      .ve-report-section-selected .checkbox {
        margin-top: 6px;
        margin-bottom: 6px;
      }

      .ve-app-layout {
        display: flex;
        gap: 18px;
        align-items: flex-start;
        width: 100%;
      }
      .ve-app-sidebar {
        flex: 0 0 650px;
        max-width: 650px;
        min-width: 360px;
      }
      .ve-app-preview {
        flex: 1 1 auto;
        min-width: 0;
      }
      .ve-input-data-left {
        flex: 0 0 520px;
        max-width: 520px;
        min-width: 320px;
      }
      .ve-input-data-main {
        flex: 1 1 auto;
        min-width: 0;
      }
      .ve-map-preview-tabs .nav {
        margin-bottom: 12px;
      }
      .ve-map-preview-tabs .tab-content {
        width: 100%;
      }
      .ve-map-tab-close {
        float: right;
        margin-left: 8px;
        padding: 1px 6px !important;
        font-size: 11px !important;
        line-height: 1.2 !important;
      }
      .ve-tab-title-with-remove {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        max-width: 100%;
      }
      .ve-tab-title-with-remove .btn {
        flex: 0 0 auto;
      }
      .ve-remove-map-btn,
      .ve-remove-analysis-btn {
        width: 19px !important;
        height: 19px !important;
        min-width: 19px !important;
        padding: 0 !important;
        margin: 0 0 0 6px !important;
        border-radius: 4px !important;
        border: 1px solid #b6c5d6 !important;
        background: #ffffff !important;
        color: #334e68 !important;
        font-size: 14px !important;
        font-weight: 800 !important;
        line-height: 15px !important;
        text-align: center !important;
        box-shadow: 0 1px 2px rgba(15, 23, 42, 0.08) !important;
      }
      .ve-remove-map-btn:hover,
      .ve-remove-analysis-btn:hover {
        background: #dbe4ee !important;
        color: #102a43 !important;
        border-color: #829ab1 !important;
      }
      .ve-map-type-grid {
        display: grid;
        gap: 14px;
      }
      .ve-map-element-filter-box,
      .ve-map-rgb-box {
        max-width: 500px;
      }
      .ve-map-tab-header {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        margin-bottom: 8px;
      }
      .ve-processing-pairs-card {
        overflow-x: auto;
        width: 100%;
      }
      .ve-processing-pairs-card .shiny-plot-output {
        width: 100% !important;
        min-height: 680px;
      }

      .ve-analysis-layout {
        display: flex;
        gap: 18px;
        align-items: flex-start;
        width: 100%;
        transition: all 0.25s ease;
      }
      .ve-analysis-sidebar {
        flex: 0 0 650px;
        max-width: 650px;
        min-width: 360px;
        transition: flex-basis 0.25s ease, max-width 0.25s ease, min-width 0.25s ease;
        overflow: visible;
      }
      .ve-analysis-preview {
        flex: 1 1 auto;
        min-width: 0;
      }
      .ve-analysis-sidebar-toggle {
        width: 100%;
        margin-bottom: 10px;
      }
      .ve-analysis-layout.ve-analysis-collapsed .ve-analysis-sidebar {
        flex-basis: 52px;
        max-width: 52px;
        min-width: 52px;
        overflow: hidden;
      }
      .ve-analysis-layout.ve-analysis-collapsed .ve-analysis-sidebar .ve-collapse-box,
      .ve-analysis-layout.ve-analysis-collapsed .ve-analysis-sidebar .ve-analysis-sidebar-content {
        display: none;
      }
      .ve-analysis-close-row {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        margin-bottom: 8px;
      }
      .ve-analysis-tab-close {
        padding: 2px 8px !important;
        font-size: 12px !important;
        line-height: 1.2 !important;
      }
      .ve-cluster-view-controls .shiny-input-container {
        max-width: 500px;
      }

      /* Reusable master sidebar layout with lateral collapse */
      .ve-side-layout {
        display: flex;
        gap: 18px;
        align-items: flex-start;
        width: 100%;
        transition: all 0.25s ease;
      }
      .ve-side-panel {
        flex: 0 0 650px;
        max-width: 650px;
        min-width: 360px;
        transition: flex-basis 0.25s ease, max-width 0.25s ease, min-width 0.25s ease;
        overflow: visible;
      }
      .ve-side-main {
        flex: 1 1 auto;
        min-width: 0;
      }
      .ve-master-panel {
        background: #ffffff;
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.11);
        overflow: visible;
        position: relative;
      }
      .ve-master-header {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 12px 14px;
        background: #f8fafc;
        border-bottom: 1px solid #eef2f7;
        border-radius: 12px 12px 0 0;
      }
      .ve-master-title {
        font-size: 19px;
        font-weight: 800;
        color: #243b53;
        letter-spacing: 0.01em;
      }
      .ve-side-toggle {
        width: 34px;
        height: 30px;
        padding: 0 !important;
        border-radius: 8px !important;
        border: 1px solid #bcccdc !important;
        background: #eef2f7 !important;
        color: #243b53 !important;
        font-weight: 800 !important;
        line-height: 1 !important;
      }
      .ve-master-content {
        padding: 14px;
        overflow: visible;
      }
      .ve-master-collapsed-label {
        display: none;
      }
      .ve-side-layout.ve-side-collapsed .ve-side-panel {
        flex-basis: 54px;
        max-width: 54px;
        min-width: 54px;
        overflow: hidden;
      }
      .ve-side-layout.ve-side-collapsed .ve-master-panel {
        min-height: 280px;
        overflow: hidden !important;
      }
      .ve-side-layout.ve-side-collapsed .ve-master-header,
      .ve-side-layout.ve-side-collapsed .ve-master-content {
        display: none;
      }
      .ve-side-layout.ve-side-collapsed .ve-master-collapsed-label {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: flex-start;
        gap: 12px;
        padding: 10px 6px;
        height: 100%;
      }
      .ve-side-layout.ve-side-collapsed .ve-master-collapsed-label .ve-side-toggle {
        transform: rotate(180deg);
      }
      .ve-vertical-title {
        writing-mode: vertical-rl;
        transform: rotate(180deg);
        white-space: nowrap;
        font-weight: 800;
        font-size: 15px;
        color: #243b53;
        letter-spacing: 0.04em;
      }

      @media (max-width: 1100px) {
        .ve-app-layout, .ve-analysis-layout, .ve-side-layout { flex-direction: column; }
        .ve-app-sidebar, .ve-input-data-left, .ve-analysis-sidebar, .ve-side-panel { flex: 1 1 auto; max-width: 100%; width: 100%; }
      }
      @media (max-width: 1100px) {
        .ve-cluster-layout { flex-direction: column; }
        .ve-cluster-sidebar { flex: 1 1 auto; max-width: 100%; width: 100%; }
      }
    ")),

    tags$script(HTML("
      document.title = 'QMap - VisualEasier (v' + '2.0.2' + ')';
      Shiny.addCustomMessageHandler('veToggleClusterPanels', function(message) {
        var parameters = document.getElementById('ve_cluster_parameters_details');
        var rename = document.getElementById('ve_cluster_rename_details');
        var post = document.getElementById('ve_cluster_post_details');
        if (parameters) parameters.open = !!message.settings_open;
        if (post) post.open = !!message.post_open;
        if (rename) rename.open = !!message.edit_open;
      });
      Shiny.addCustomMessageHandler('veToggleProcessingPanels', function(message) {
        var variables = document.getElementById('ve_processing_variables_details');
        var clip = document.getElementById('ve_processing_clip_details');
        if (variables) variables.open = !!message.settings_open;
        if (clip) clip.open = !!message.clip_open;
      });
      Shiny.addCustomMessageHandler('veToggleAnalysisSidebar', function(message) {
        var layout = document.getElementById('ve_analysis_layout');
        if (layout) layout.classList.toggle('ve-analysis-collapsed', !!message.collapsed);
      });
      Shiny.addCustomMessageHandler('veSetSideCollapsed', function(message) {
        var layout = document.getElementById(message.id);
        if (layout) layout.classList.toggle('ve-side-collapsed', !!message.collapsed);
      });
      Shiny.addCustomMessageHandler('veClickElement', function(message) {
        var el = document.getElementById(message.id);
        if (el) el.click();
      });
      Shiny.addCustomMessageHandler('veSaveThenReload', function(message) {
        var el = document.getElementById(message.id);
        if (el) el.click();
        window.setTimeout(function() {
          window.location.reload();
        }, message.delay || 1500);
      });
    "))
  ),
  navbarPage(
    id = "main_navbar",
    theme = shinytheme("yeti"),
    title = div(class="ve-navbar-title",
      tags$div(class="ve-global-actions",
        downloadButton("global_save_project_download", "💾", class="ve-global-action-btn", title="Save current project"),
        actionButton("global_new_project_btn", "📄", class="ve-global-action-btn", title="Start new project"),
        tags$div(class="ve-hidden-download",
          downloadButton("save_before_new_project", "Save before new project")
        )
      ),
      span(paste0("QMap - VisualEasier (v", APP_VERSION, ")"))
    ),

    # ---- Input Data ----
    tabPanel("Input Data",
      fluidPage(
        tags$div(id="ve_input_layout", class="ve-side-layout",
          tags$div(class="ve-side-panel",
            tags$div(class="ve-master-panel",
              tags$div(class="ve-master-header",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_input_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-master-title", "Central Input Data")
              ),
              tags$div(class="ve-master-content",
                fileInput("load_project_file", "Load Project", accept=c(".rds",".RDS",".veproj.rds")),
                uiOutput("input_data_project_creation_ui")
              ),
              tags$div(class="ve-master-collapsed-label",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_input_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-vertical-title", "Central Input Data")
              )
            )
          ),
          tags$div(class="ve-side-main",
            div(class="ve-preview-tabs",
              h3(tags$b("Loaded data summary"), align="center"),
              div(class="ve-box", uiOutput("loaded_data_status")),
              div(class="ve-box", tableOutput("loaded_data_table"), uiOutput("save_project_ui"))
            )
          )
        )
      )
    ),

    # ---- Data Processing ----
    tabPanel("Data Processing",
      fluidPage(
        tags$div(id="ve_processing_layout", class="ve-side-layout",
          tags$div(class="ve-side-panel",
            tags$div(class="ve-master-panel",
              tags$div(class="ve-master-header",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_processing_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-master-title", "Processing settings")
              ),
              tags$div(class="ve-master-content",
                tags$details(id="ve_processing_variables_details", class="ve-collapse-box", open="open",
                  tags$summary("Variables and Transformation"),
                  tags$div(class="ve-collapse-content",
                    uiOutput("processing_variable_selector_ui"),
                    uiOutput("processing_method_ui"),
                    checkboxInput("enable_processing_pairs_preview", "Enable Variables Relationship Preview", value=TRUE),
                    actionButton("apply_processing", "Apply"),
                    actionButton("reset_processing", "Reset"),
                    hr(),
                    tags$p("The selected variables and transformation will be used by Map Processing, Cluster Analysis, and Data Analysis after applying.", class="ve-note")
                  )
                ),
                tags$details(id="ve_processing_clip_details", class="ve-collapse-box",
                  tags$summary("Element clipping editor"),
                  tags$div(class="ve-collapse-content",
                    uiOutput("element_clip_editor_ui")
                  )
                )
              ),
              tags$div(class="ve-master-collapsed-label",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_processing_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-vertical-title", "Processing settings")
              )
            )
          ),
          tags$div(class="ve-side-main",
            div(class="ve-preview-tabs",
              tabsetPanel(id="processing_preview_tabs", type="pills",
                tabPanel("Summary",
                  h3(tags$b("Summary table"), align="center"),
                  uiOutput("processing_status_ui"),
                  tableOutput("missing_summary_table")
                ),
                tabPanel("Relationships",
                  div(class="ve-processing-pairs-card",
                    h4("Sampled relationships preview", align="center"),
                    tags$p("The preview uses a random sample of 15% of the pixels, capped at 1,500 rows, and is shown only after Apply when Variables Relationship Preview is enabled.", class="ve-note"),
                    withSpinner(plotOutput("processing_pairs_plot", width="100%", height="86vh"), type=1, color="black")
                  )
                )
              )
            )
          )
        )
      )
    ),

    # ---- Map Processing ----
    tabPanel("Map Processing",
      fluidPage(
        tags$div(id="ve_map_layout", class="ve-side-layout",
          tags$div(class="ve-side-panel",
            tags$div(class="ve-master-panel",
              tags$div(class="ve-master-header",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_map_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-master-title", "Map settings")
              ),
              tags$div(class="ve-master-content",
                uiOutput("map_processing_controls_ui"),
                tags$details(class="ve-collapse-box", open="open",
                  tags$summary("Cluster-based map filtering"),
                  tags$div(class="ve-collapse-content",
                    uiOutput("map_cluster_filter_ui")
                  )
                )
              ),
              tags$div(class="ve-master-collapsed-label",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_map_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-vertical-title", "Map settings")
              )
            )
          ),
          tags$div(class="ve-side-main",
            div(class="ve-preview-tabs ve-map-preview-tabs",
              uiOutput("map_processing_preview_ui")
            ),
            div(class="ve-preview-action-box ve-preview-action-box-bottom",
              tags$div(class="ve-preview-action-row",
                uiOutput("map_processing_download_ui")
              )
            )
          )
        )
      )
    ),

    # ---- Cluster Analysis ----
    tabPanel("Cluster Analysis",
      fluidPage(
        tags$div(id="ve_cluster_layout", class="ve-side-layout",
          tags$div(class="ve-side-panel",
            tags$div(class="ve-master-panel",
              tags$div(class="ve-master-header",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_cluster_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-master-title", "Cluster settings")
              ),
              tags$div(class="ve-master-content",
                tags$details(id="ve_cluster_parameters_details", class="ve-collapse-box", open="open",
                  tags$summary("Parameters"),
                  tags$div(class="ve-collapse-content",
                    tags$div(class="ve-auto-detect-row",
                      tags$div(class="ve-ncluster-box",
                        numericInput("ncluster", "Number of phases", value=2, min=2, step=1, width="120px")
                      ),
                      tags$div(class="ve-auto-detect-button-box",
                        actionButton("auto_detect_clusters", "Auto Detect", width="140px")
                      )
                    ),
                    tags$div(class="ve-auto-sample-box",
                      sliderInput(
                        "auto_detect_sample_percent",
                        "Auto Detect sample size (% of valid pixels; maximum 500,000 points):",
                        min = 1,
                        max = 100,
                        value = 100,
                        step = 1,
                        post = "%"
                      )
                    ),
                    tags$p(
                      "Available only when concentration values have been entered in Input Data and used for sample normalization. ",
                      tags$a("See Help for more.", href="#auto-detect-help"),
                      class="ve-note"
                    ),
                    uiOutput("cluster_color_controls_ui"),
                    uiOutput("cluster_selector_ui"),
                    checkboxInput("invert_cluster_coords", "Invert map coordinates?", value=FALSE),
                    checkboxInput("use_pca_cluster", "Use PCA instead of variables", value=FALSE),
                    conditionalPanel(
                      condition = "input.use_pca_cluster == true",
                      tags$div(class="ve-conditional-slider",
                        sliderInput("cluster_pca_variance", "Minimum cumulative variance for component selection",
                          min=0.5, max=0.99, value=0.99, step=0.01)
                      )
                    ),
                    checkboxInput("enable_ambiguity_filter", "Exclude highly ambiguous pixels from statistics", value=FALSE),
                    conditionalPanel(
                      condition = "input.enable_ambiguity_filter == true",
                      tags$div(class="ve-conditional-slider",
                        sliderInput("entropy_rank_percent", "Maximum Entropy rank retained in statistics (%)",
                          min=30, max=100, value=30, step=1, post="%")
                      )
                    ),
                    actionButton("cluster", "Generate map")
                  )
                ),
                tags$details(id="ve_cluster_post_details", class="ve-collapse-box",
                  tags$summary("Post-processing"),
                  tags$div(class="ve-collapse-content",
                    tags$p("Apply merge, delete, erosion, and moving-mode operations over the current clustering result.", class="ve-note"),
                    uiOutput("cluster_postprocess_ui")
                  )
                ),
                tags$details(id="ve_cluster_rename_details", class="ve-collapse-box",
                  tags$summary("Rename"),
                  tags$div(class="ve-collapse-content",
                    tags$p("Rename clusters and adjust display colors. Changes are applied only after clicking Apply and do not rerun k-means or statistics.", class="ve-note"),
                    uiOutput("cluster_edit_clusters_ui")
                  )
                )
              ),
              tags$div(class="ve-master-collapsed-label",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_cluster_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-vertical-title", "Cluster settings")
              )
            )
          ),
          tags$div(class="ve-side-main",
            div(class="ve-preview-tabs",
              tabsetPanel(id="cluster_preview_tabs", type="pills",
                tabPanel("Table", uiOutput("table_cluster_ui")),
                tabPanel("Map", uiOutput("cluster_visible_ui"), withSpinner(plotOutput("plot_cluster", width="100%", height="72vh"), type=1, color="black")),
                tabPanel("Entropy", withSpinner(plotOutput("plot_cluster_ambiguity", width="100%", height="72vh"), type=1, color="black")),
                tabPanel("Auto Detect", uiOutput("cluster_auto_detect_plot_ui"))
              )
            ),
            div(class="ve-preview-action-box ve-preview-action-box-bottom",
              tags$div(class="ve-preview-action-row",
                selectInput("cluster_download_format", "Download map format:", choices=c("PDF"="pdf", "TIFF"="tiff"), selected="pdf", width="150px"),
                downloadButton("download_cluster_map", "Download Map"),
                downloadButton("download_cluster_table", "Download Table")
              )
            )
          )
        )
      )
    ),

    # ---- Data Analysis ----
    tabPanel("Data Analysis",
      fluidPage(
        tags$div(id="ve_analysis_layout", class="ve-side-layout",
          tags$div(class="ve-side-panel",
            tags$div(class="ve-master-panel",
              tags$div(class="ve-master-header",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_analysis_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-master-title", "Data settings")
              ),
              tags$div(class="ve-master-content",
                tags$details(class="ve-collapse-box", open="open",
                  tags$summary("Scatter Plot"),
                  tags$div(class="ve-collapse-content", uiOutput("cluster_pc_scatter_controls_ui"))
                ),
                tags$details(class="ve-collapse-box",
                  tags$summary("Ternary Plot"),
                  tags$div(class="ve-collapse-content", uiOutput("cluster_pc_ternary_controls_ui"))
                ),
                tags$details(class="ve-collapse-box",
                  tags$summary("Cluster View"),
                  tags$div(class="ve-collapse-content", uiOutput("cluster_view_controls_ui"))
                ),
                tags$details(class="ve-collapse-box",
                  tags$summary("Correlogram"),
                  tags$div(class="ve-collapse-content", uiOutput("correlogram_controls_ui"))
                )
              ),
              tags$div(class="ve-master-collapsed-label",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_analysis_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-vertical-title", "Data settings")
              )
            )
          ),
          tags$div(class="ve-side-main",
            div(class="ve-preview-tabs", uiOutput("data_analysis_preview_ui")),
            div(class="ve-preview-action-box ve-preview-action-box-bottom",
              tags$div(class="ve-preview-action-row",
                downloadButton("download_cluster_pca_plot", "Download Scatter"),
                downloadButton("download_cluster_ternary_plot", "Download Ternary"),
                downloadButton("download_correlogram_plot", "Download Correlogram")
              )
            )
          )
        )
      )
    ),

    # ---- Create Report ----
    tabPanel("Create Report",
      fluidPage(
        tags$div(id="ve_report_layout", class="ve-side-layout",
          tags$div(class="ve-side-panel",
            tags$div(class="ve-master-panel",
              tags$div(class="ve-master-header",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_report_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-master-title", "Report settings")
              ),
              tags$div(class="ve-master-content",
                checkboxInput("report_select_all", "Select All", value = TRUE),
                uiOutput("report_sections_ui"),
                tags$p("All sections are selected by default. The report is generated in A4 PDF format and uses the current application settings whenever possible.", class="ve-note"),
                tags$div(class="ve-download-row",
                  actionButton("generate_pdf_report_preview", "Generate PDF Report"),
                  downloadButton("save_pdf_report", "Save Report")
                )
              ),
              tags$div(class="ve-master-collapsed-label",
                tags$button(type="button", class="ve-side-toggle", onclick="document.getElementById('ve_report_layout').classList.toggle('ve-side-collapsed');", "◀"),
                tags$span(class="ve-vertical-title", "Report settings")
              )
            )
          ),
          tags$div(class="ve-side-main",
            div(class="ve-preview-tabs",
              h3(tags$b("Report preview")),
              uiOutput("report_preview_ui")
            )
          )
        )
      )
    ),

    # ---- Help ----
    tabPanel("Help",
      fluidPage(
        tags$style(HTML("
          .help-section { margin-bottom: 28px; }
          .help-section h3 { border-bottom: 2px solid #d9e2ec; padding-bottom: 6px; margin-top: 24px; }
          .help-section h4 { color: #2c4a6e; margin-top: 16px; margin-bottom: 4px; }
          .help-note  { background:#f0f4f8; border-left:4px solid #4a90d9; padding:10px 14px;
                        border-radius:4px; font-size:13px; margin:10px 0; }
          .help-warn  { background:#fff8e1; border-left:4px solid #f0a500; padding:10px 14px;
                        border-radius:4px; font-size:13px; margin:10px 0; }
          .help-tag   { display:inline-block; background:#e3edf7; color:#2c4a6e; border-radius:4px;
                        padding:1px 7px; font-size:12px; font-weight:600; margin-right:4px; }
        ")),

        div(class="ve-box help-section",
          h2("QMap \u2014 VisualEasier", align="center"),
          tags$p(style="text-align:center; color:#5c6b7a; font-size:15px;",
            paste0("Version ", APP_VERSION, " \u2014 Interactive geochemical map analysis")),
          p("VisualEasier is designed for the processing, visualization, and quantitative interpretation of spatially resolved geochemical and mineral chemistry datasets — particularly those acquired by electron microprobe (EPMA), energy-dispersive X-ray spectroscopy (EDS/SEM), or laser ablation ICP-MS mapping. The application operates entirely in-session: no data are sent to external servers."),
          div(class="help-note",
            tags$b("Recommended workflow:"),
            tags$ol(
              tags$li("Load matrices in ", tags$b("Input Data")),
              tags$li("Choose a data transform in ", tags$b("Data Processing"), " and click Apply"),
              tags$li("Preview elemental and RGB maps in ", tags$b("Map Processing")),
              tags$li("Segment mineral phases in ", tags$b("Cluster Analysis")),
              tags$li("Explore compositional relationships in ", tags$b("Data Analysis")),
              tags$li("Export the full session summary in ", tags$b("Create Report"))
            )
          )
        ),

        div(class="ve-box help-section",
          h3("Tab 1 \u2014 Input Data"),
          p("This is the entry point for every session. All other tabs consume the data loaded here."),

          h4("Creating a new project"),
          p("Click ", tags$b("Create New Project"), " and select one CSV or TXT file per element. Each file must contain a plain numeric matrix (no headers) with identical row and column counts across all elements. Row order corresponds to the spatial Y axis of the map (top to bottom) and column order to the X axis (left to right)."),
          tags$ul(
            tags$li(tags$b("Element name:"), " editable label used throughout the application. Keep names short and unique (e.g. Si, Ca, Fe, Al)."),
            tags$li(tags$b("Normalization percentage:"), " the known or theoretical weight percent of that element in a reference standard used during acquisition. Leave as 0 if unavailable — this disables the CPS\u2192Wt% transform but all other methods remain available."),
            tags$li(tags$b("Sample ID:"), " free text identifier included in exported file names and the PDF report."),
            tags$li(tags$b("Map width / height [mm]:"), " physical dimensions of the mapped area. When provided, elemental maps will display a calibrated scale bar and axis labels in millimetres.")
          ),

          h4("Loading a saved project"),
          p("Click ", tags$b("Load Project"), " and select a ", tags$code(".veproj.rds"), " file previously saved from this application. The project file stores matrices, metadata, normalization values, the active processing method and selected variables, and element clipping settings. Rendered maps and cluster results are not stored and must be regenerated after loading."),

          h4("Saving a project"),
          p("Once data are loaded, a ", tags$b("Save Project"), " button appears. The file can be reloaded in any future session to continue exactly where you left off without re-uploading the original CSV files."),

          h4("Decimal separator"),
          p("Choose the decimal separator that matches your CSV files (point or comma). This setting affects both matrix reading and normalization value parsing.")
        ),

        div(class="ve-box help-section",
          h3("Tab 2 \u2014 Data Processing"),
          p("Applies a mathematical transformation to the loaded matrices before they are used in mapping, clustering, and analysis. The transformation is applied uniformly to all selected variables."),

          h4("Variable selection"),
          p("You can restrict the active variable set by deselecting elements. Removed elements will not appear in maps, cluster inputs, or analytical plots for the current session. Click ", tags$b("Apply"), " to commit the selection."),

          h4("Available transforms"),

          tags$div(class="help-note",
            tags$b("Pixel-wise CPS \u2192 Wt% (requires normalization values)"), tags$br(),
            "Converts raw count rates (CPS) into an approximation of weight percent. For each pixel, all element matrices are first scaled by their respective normalization percentages and then normalized so that the pixel sum equals 100. This is the recommended starting point when acquisition normalization values are available, as it places all elements on a compositionally comparable scale."
          ),
          tags$div(class="help-note",
            tags$b("Raw value"), tags$br(),
            "No transformation is applied. The original numeric values from the input files are used as-is. Appropriate when data are already in a processed or calibrated form."
          ),
          tags$div(class="help-note",
            tags$b("Centered Log-Ratio (CLR)"), tags$br(),
            "Each pixel\u2019s element vector is log-transformed and then centred by subtracting the geometric mean of all elements at that pixel. CLR removes the compositional closure constraint (the spurious negative correlations introduced when values sum to a constant) and is the standard transform for compositional data analysis (Aitchison, 1986). Recommended for EPMA and EDS datasets when you want clustering and correlations to reflect true geochemical relationships."
          ),
          tags$div(class="help-note",
            tags$b("Natural logarithm (ln)"), tags$br(),
            "Applies log(x + \u03b5) independently to each element matrix. Compresses large dynamic ranges and reduces the influence of high-count outliers. Useful when element distributions are strongly right-skewed but a full CLR is not appropriate."
          ),
          tags$div(class="help-note",
            tags$b("MinMax"), tags$br(),
            "Rescales each element independently to the [0, 1] interval using its observed minimum and maximum. Useful for visual comparison when the absolute scale of each element is not meaningful, but note that it does not correct for closure effects."
          ),
          tags$div(class="help-note",
            tags$b("Data Scaling (SD)"), tags$br(),
            "Divides each element matrix by its standard deviation (without mean-centring). Brings all variables to a comparable spread, which can help k-means clustering when element dynamic ranges differ greatly."
          ),

          h4("Relationships preview (ggpairs)"),
          p("After clicking Apply, a scatterplot matrix appears showing pairwise correlations and density distributions for a random sample of pixels. Use this to assess collinearities, skewness, and outlier structure before clustering."),

          div(class="help-warn",
            tags$b("Important:"), " the Apply button commits the transform and variable selection to all downstream tabs. If you change settings without clicking Apply, the maps and cluster results will still use the previously committed state."
          )
        ),

        div(class="ve-box help-section",
          h3("Tab 3 \u2014 Map Processing"),
          p("Generates visual maps from the active (transformed) dataset. Two map types are available."),

          h4("Element Map"),
          p("Renders one false-colour map per selected element. Each map uses the display matrix computed from the active transform: if CPS\u2192Wt% is active, the colour scale represents the pixel-wise weight percent of that element; otherwise it represents the transformed value directly."),
          tags$ul(
            tags$li(tags$b("Colour palette:"), " Turbo, Viridis, Plasma, Magma, or Inferno. Turbo provides the highest perceptual discriminability across the full value range. Viridis and Magma are preferred for figures intended for greyscale printing or colour-blind readers."),
            tags$li(tags$b("Image filter:"), " optional spatial smoothing applied before display only (does not alter the stored matrices). Median removes salt-and-pepper noise while preserving edges; Gaussian blur applies a 3\u00d73 weighted average; Gradient highlights spatial transitions and phase boundaries."),
            tags$li(tags$b("Invert map coordinates:"), " transposes the X and Y axes. Use when the acquisition raster direction differs from the expected map orientation."),
            tags$li(tags$b("Legend orientation:"), " places the colour bar horizontally (below) or vertically (right).")
          ),

          h4("Ternary Map (RGB)"),
          p("Composites exactly three elements into a single RGB image. Each element is independently normalised to [0, 1] and mapped to the red, green, or blue channel respectively. The resulting colour at each pixel encodes the relative intensity of all three elements simultaneously, allowing rapid visual identification of compositional domains and gradients."),
          div(class="help-note",
            "The RGB composite uses additive colour mixing weighted by local intensity. Pixels dominated by one element appear in the corresponding primary colour; pixels with equal contributions of two elements appear in their additive mixture (e.g. red + green = yellow)."
          ),

          h4("Element clipping editor"),
          p("Allows you to define lower and upper detection limits for any individual element. Values below the lower limit are set to zero; values above the upper limit are clamped to the upper limit. Clipping is applied in-place to the active processed variable — it overwrites the stored matrix for that element and affects all downstream operations (clustering, analysis, report) until reset."),

          h4("Cluster-based map filter"),
          p("Once a cluster solution has been generated, you can restrict the displayed map to pixels belonging to selected clusters. This is useful for inspecting the spatial distribution of a single phase without regenerating the map.")
        ),

        div(class="ve-box help-section",
          h3("Tab 4 \u2014 Cluster Analysis"),
          p("Segments the map into compositionally distinct groups using k-means clustering. Each pixel becomes an observation and each active element becomes a feature."),

          h4("Cluster settings"),
          tags$ul(
            tags$li(tags$b("Number of clusters (k):"), " the number of mineral phases or compositional groups to identify. You can set k manually or use Auto Detect when valid concentration values are available."),
            tags$li(tags$b("Auto Detect:"), tags$span(id="auto-detect-help"), " estimates the optimum number of pseudo-mineral phases from k = 2 to 15. It is available only when concentration values have been entered in Input Data and used for sample normalization. The routine samples the selected percentage of valid pixels, capped at 500,000 points, runs PCA, tests multiple k-means solutions, and ranks k using a composite score combining BCI, entropy-based purity, clustering stability, and weak parsimony. BCI is estimated from the 30% purest pixels, while phase abundance uses all sampled pixels."),
            tags$li(tags$b("Cluster colours:"), " customisable per cluster via hex code, RGB sliders, or colour picker. Colours are used consistently in the cluster map, scatter plot, ternary plot, and summary table."),
            tags$li(tags$b("Elements for clustering:"), " subset of active elements to include as clustering features. Excluding an element removes it from distance calculations but it remains available in analytical plots."),
            tags$li(tags$b("Invert map coordinates:"), " same convention as in Map Processing. Must match if you want cluster results to overlay correctly on element maps."),
            tags$li(tags$b("Use PCA instead of variables:"), " replaces the raw element features with principal components before running k-means. The number of PCs is chosen automatically to explain at least the specified cumulative variance (default 99%). PCA can improve clustering when elements are highly collinear or when dimensionality reduction is desirable to reduce noise.")
          ),

          h4("Entropy map and ambiguity filtering"),
          p("After clustering, a normalised Shannon entropy value is computed for each pixel. Entropy measures how confidently a pixel belongs to a single cluster: it is 0 when the pixel is unambiguously closest to one centroid, and approaches 1 when it is equidistant from all centroids. The entropy map is displayed below the cluster map."),
          p(tags$b("Exclude highly ambiguous pixels from statistics:"), " when enabled, only the lowest-entropy pixels within each cluster (up to the specified percentile rank) are used to compute means and standard deviations in the summary table. This reduces the influence of mixed or transitional pixels on phase composition estimates. The cluster map and all plots always show all pixels regardless of this filter."),

          h4("Post-processing operations"),
          tags$ul(
            tags$li(tags$b("Rename:"), " assign a geological or mineralogical label to each cluster (e.g. \u2018Plagioclase\u2019, \u2018Pyroxene\u2019). Labels appear in all plots, the summary table, and the PDF report."),
            tags$li(tags$b("Merge:"), " combine two or more clusters into one. The merged cluster inherits the colour of the target cluster. Use when k-means has split a single phase across multiple clusters."),
            tags$li(tags$b("Delete:"), " remove clusters entirely from the dataset. Deleted pixels are excluded from all subsequent statistics and plots."),
            tags$li(tags$b("Border erosion (statistics only):"), " masks pixels within a specified distance (in pixels) of a cluster boundary before computing summary statistics. This suppresses contamination from mixed-phase beam volumes at grain boundaries. The displayed cluster map is not affected."),
            tags$li(tags$b("Moving mode filter:"), " replaces each pixel\u2019s cluster assignment with the most frequent assignment in its neighbourhood window (3\u00d73, 5\u00d75, or 8\u00d78). Reduces salt-and-pepper noise in the cluster map. Apply before finalising labels and statistics.")
          ),

          div(class="help-warn",
            tags$b("Note on k-means:"), " results depend on the random initialisation. The application uses nstart\u202f=\u202f10 (ten independent restarts) and retains the solution with the lowest total within-cluster sum of squares. Set a fixed seed is used internally (123) so results are reproducible within the same session and dataset."
          )
        ),

        div(class="ve-box help-section",
          h3("Tab 5 \u2014 Data Analysis"),
          p("Provides three interactive analytical plots to explore compositional relationships across and within clusters. All plots operate on the committed processing state and the current cluster solution."),

          h4("Sampling slider"),
          p("All plots in this tab use a stratified random sample of pixels (preserving cluster proportions) to maintain interactive responsiveness. Increase the percentage for publication-quality exports; lower values are sufficient for exploratory work. The maximum rendered point count is capped at 100,000."),

          h4("Scatter Plot"),
          p("Displays any two variables (elements or principal components) on orthogonal axes. Points can be coloured by cluster label, normalised Shannon entropy, or the value of any input variable. Rendered as an interactive Plotly chart: hover over points to see exact values and cluster assignments, and use the toolbar to zoom, pan, and export as PNG."),

          h4("Ternary Plot"),
          p("Projects three variables onto a ternary diagram, where each vertex represents 100% of one variable and each point\u2019s position encodes the relative proportions of all three. The three axes are normalised so that their row-wise sum equals 1 at every pixel. Variables can be elements or principal components. Colouring options are the same as the scatter plot. Rendered as an interactive Plotly chart and exportable as a self-contained HTML file."),
          div(class="help-note",
            "Ternary plots are particularly powerful for distinguishing solid-solution series (points fall along a line between two end-members) from mechanical mixing (points scatter across the diagram) and for identifying discrete phase populations (tight point clusters)."
          ),

          h4("Correlogram"),
          p("Displays a pairwise correlation matrix for the selected variables, computed either on all pixels or on a single cluster subset. The Spearman rank correlation is recommended for geochemical data because it is robust to outliers and does not assume linearity; Pearson correlation is available when a linear model is appropriate. Correlations are rendered as a colour-shaded matrix with numeric coefficients. Variables are reordered by angular order of eigenvectors (AOE) to group correlated elements together.")
        ),

        div(class="ve-box help-section",
          h3("Tab 6 \u2014 Create Report"),
          p("Exports a multi-page A4 PDF report summarising the full analysis session. Each section is optional and can be toggled independently."),
          tags$ul(
            tags$li(tags$b("Processing report:"), " narrative description of the active data transform, cluster configuration (k, PCA usage, entropy filtering), and methodological notes."),
            tags$li(tags$b("Input values table:"), " lists the loaded files, element names, normalization values, and matrix dimensions."),
            tags$li(tags$b("Summary statistics table:"), " descriptive statistics (N, missing %, outlier %, min, Q1, median, mean, Q3, IQR, SD, SE, max) for each active variable."),
            tags$li(tags$b("Data relationships preview:"), " the ggpairs scatterplot matrix, rendered at report resolution. Only included when processing has been applied."),
            tags$li(tags$b("Element maps:"), " one page per element, using the current palette and filter settings."),
            tags$li(tags$b("RGB map:"), " the current ternary RGB composite, using the elements selected in Map Processing."),
            tags$li(tags$b("Cluster map + entropy map:"), " the k-means segmentation map and the corresponding normalised Shannon entropy map."),
            tags$li(tags$b("Cluster mean-composition table:"), " mean value per element per cluster (excluding SD columns)."),
            tags$li(tags$b("Cluster standard-deviation table:"), " standard deviation per element per cluster."),
            tags$li(tags$b("Scatter plot, Ternary plot, Correlogram:"), " static renders of the current Data Analysis plots.")
          ),
          div(class="help-note",
            "The report uses the current application state at the moment of export. If you have not yet generated a cluster solution or run specific plots, the corresponding sections will either be omitted or contain a \u2018no data available\u2019 message."
          )
        ),

        div(class="ve-box help-section",
          h3("Data formats and file requirements"),
          tags$ul(
            tags$li(tags$b("Input matrices:"), " plain text CSV or TXT, no header row, numeric values only, consistent decimal separator (configurable). Each file must have the same number of rows and columns."),
            tags$li(tags$b("Project file (.veproj.rds):"), " R serialised object containing matrices and metadata. Not intended for external editing."),
            tags$li(tags$b("Map downloads:"), " PDF (raster-embedded for maximum compatibility) and TIFF for RGB maps. The cluster ternary plot is exported as a self-contained HTML file to preserve interactivity."),
            tags$li(tags$b("Cluster summary table:"), " CSV with semicolon separator (CSV2 format, compatible with European locale spreadsheet software).")
          )
        ),

        div(class="ve-box help-section",
          h3("Limitations and known constraints"),
          tags$ul(
            tags$li("K-means assumes convex, roughly spherical clusters in feature space. It may underperform for phases with strongly elongated or curved compositional trends. In such cases, consider using PCA before clustering to decorrelate the features."),
            tags$li("The CPS\u2192Wt% transform assumes that the normalization percentages entered in Input Data are accurate reference values for the acquisition conditions. Errors in these values will propagate to all weight percent estimates."),
            tags$li("Closure effects (the constraint that compositional parts sum to a constant) can produce spurious negative correlations between elements. The CLR transform is specifically designed to address this; other transforms do not."),
            tags$li("The entropy map uses inverse-distance weighting (IDW): each pixel receives a probability proportional to the inverse of its distance to each cluster centroid. It is a relative, not absolute, measure of compositional ambiguity and should be interpreted comparatively within a session."),
            tags$li("Border erosion and moving mode filter operations modify the cluster assignment data in memory. Use the Reset buttons in Post-processing to revert to the original k-means output."),
            tags$li("Very large maps (> 10 million pixels) may cause slow rendering or memory pressure. Consider downsampling or splitting the map before loading if performance is inadequate.")
          )
        ),

        div(class="ve-box help-section",
          h3("Contributors"),
          tags$ul(
            tags$li("Guilherme Ferreira da Silva — guilherme.ferreira@sgb.gov.br"),
            tags$li("Lucas Abud de Mesquita — lucas.mesquita@sgb.gov.br"),
            tags$li("Marcos Vinícius Ferreira — marcos.ferreira@sgb.gov.br")
          ),
          h3("Citation"),
          p("If you use QMap — VisualEasier in published work, please cite:"),
          div(style="background:#f7f9fb; border:1px solid #d9e2ec; border-radius:6px; padding:12px 16px; font-style:italic;",
            "Mesquita, L.A.; Ferreira da Silva, G.; Rodrigues, J.B. 2025. VisualEasier: An interactive tool for geochemical and mineral chemistry data visualization and analysis. Journal of the Geological Survey of Brazil. DOI: https://10.29396/jgsb.2025.v8.n3.2/"
          )
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  decimal_sep        <- reactive({ input$decimal_separator %||% "." })
  file_input_version <- reactiveVal(1)
  create_project_mode <- reactiveVal(FALSE)
  input_data_core    <- reactiveVal(NULL)
  processing_state   <- reactiveVal(list(applied=FALSE, enabled=FALSE, method="raw", selected_elements=NULL))
  map_clip_state     <- reactiveVal(list())
  cluster_post_state  <- reactiveVal(NULL)
  cluster_visible_state <- reactiveVal(character())
  merge_sources_state <- reactiveVal(character())
  merge_target_state  <- reactiveVal("")
  cluster_color_values <- reactiveVal(character())
  cluster_auto_detect_state <- reactiveVal(NULL)


  reset_current_session <- function() {
    input_data_core(NULL)
    processing_state(list(applied=FALSE, enabled=FALSE, method="raw", selected_elements=NULL))
    map_clip_state(list())
    cluster_post_state(NULL)
    cluster_visible_state(character())
    merge_sources_state(character())
    merge_target_state("")
    cluster_color_values(character())
    cluster_auto_detect_state(NULL)
    try(map_saved_maps(list()), silent = TRUE)
    try(rgb_saved_maps(list()), silent = TRUE)
    try(last_rgb_plot_data(NULL), silent = TRUE)
    try(correlogram_saved_plots(list()), silent = TRUE)
    try(report_preview_file(NULL), silent = TRUE)
    try({
      data_analysis_visible$scatter <- FALSE
      data_analysis_visible$ternary <- FALSE
      data_analysis_visible$cluster <- FALSE
      data_analysis_visible$correlogram <- FALSE
    }, silent = TRUE)
    try(updateTextInput(session, "sample_id", value = ""), silent = TRUE)
    try(updateTextInput(session, "global_map_width", value = ""), silent = TRUE)
    try(updateTextInput(session, "global_map_height", value = ""), silent = TRUE)
    try(updateRadioButtons(session, "decimal_separator", selected = "."), silent = TRUE)
    try(updateTabsetPanel(session, "map_processing_preview_tabs", selected = character(0)), silent = TRUE)
    try(updateTabsetPanel(session, "cluster_preview_tabs", selected = character(0)), silent = TRUE)
    try(updateTabsetPanel(session, "data_analysis_preview_tabs", selected = character(0)), silent = TRUE)
    create_project_mode(TRUE)
    file_input_version(file_input_version() + 1L)
    gc(full = TRUE, reset = TRUE)
    updateTabsetPanel(session, "main_navbar", selected = "Input Data")
  }

  build_project_payload <- function() {
    core <- input_data_core()
    validate(need(loaded_data_ready(core), "Please load data before saving a project."))

    active_core_light <- tryCatch(active_input_core(), error = function(e) NULL)
    transformed_mats <- if (!is.null(active_core_light)) {
      active_core_light$matrices %||% active_core_light$mats
    } else {
      NULL
    }

    compact_cluster <- function(dat) {
      if (is.null(dat) || !is.list(dat)) return(NULL)
      keep_names <- intersect(names(dat), c(
        "result_df", "box_wide", "summary_table", "ambiguity_df",
        "entropy_rank_percent", "ambiguity_enabled", "aspect_ratio", "pca_info",
        "k", "cluster_colors", "cluster_colors_named", "active_ids", "label_map",
        "elems", "filtered_n", "total_n", "map_width", "map_height", "invert_coords",
        "erosion_enabled", "erosion_pixels", "erosion_applied"
      ))
      out <- dat[keep_names]
      out$note <- "Compact cluster state saved without rendered RGB/element/correlogram objects to keep the project file lightweight."
      out
    }

    dat <- tryCatch(cluster_post_state(), error = function(e) NULL)

    list(
      version = APP_VERSION,
      saved_at = as.character(Sys.time()),
      sample_id = core$sample_id %||% "",
      decimal_separator = core$decimal_separator %||% decimal_sep(),
      map_width = core$map_width,
      map_height = core$map_height,
      elements = core$elements,
      file_names = core$summary$File,
      input_paths = core$input_paths %||% core$summary$DataPath,
      norms = core$norms,
      matrices = core$mats %||% core$matrices,
      transformed_matrices = transformed_mats,
      processing_method = processing_state()$method %||% "raw",
      selected_elements = processing_state()$selected_elements %||% core$elements,
      processing_state = processing_state(),
      clip_state = map_clip_state(),
      cluster_state = compact_cluster(dat),
      cluster_visible = cluster_visible_state(),
      cluster_colors = cluster_color_values(),
      auto_detect_state = cluster_auto_detect_state(),
      cluster_summary_table = if (!is.null(dat)) dat$summary_table else NULL,
      cluster_result_dataframe = if (!is.null(dat)) dat$result_df else NULL,
      cluster_entropy_dataframe = if (!is.null(dat)) dat$ambiguity_df else NULL,
      note = "Lightweight project file: stores loaded matrices, active transformed matrices, processing/clipping settings, compact cluster state, Auto Detect diagnostics, cluster/entropy data frames, and the cluster summary table. Rendered Element/RGB maps and correlogram objects are intentionally not stored."
    )
  }

  # ---------------------------------------------------------------------------
  # Shared helper: build active core from state (eliminates duplicate reactive)
  # ---------------------------------------------------------------------------
  .build_active_core <- function(core, method, selected_elements) {
    if (is.null(selected_elements) || length(selected_elements) == 0) selected_elements <- core$elements
    selected_elements <- intersect(selected_elements, core$elements)
    validate(need(length(selected_elements) >= 1, "Please keep at least one variable selected for processing."))
    mats_raw <- (core$matrices %||% core$mats)[selected_elements]
    norms    <- (core$norms %||% as.list(rep(0, length(core$elements))))[selected_elements]
    mats_proc <- apply_clip_state_to_matrices(
      apply_data_processing_method(mats_raw, method, norm_values = norms),
      map_clip_state()
    )
    out <- core
    out$elements          <- names(mats_proc)
    out$mats_raw          <- mats_raw
    out$mats              <- mats_proc
    out$matrices          <- mats_proc
    out$norms             <- norms[names(mats_proc)]
    out$processing_method <- method
    out$processing_enabled <- !identical(method, "raw")
    out
  }

  # preview core: reads live UI inputs (before Apply)
  processing_preview_core <- reactive({
    core <- input_data_core(); req(loaded_data_ready(core))
    method <- input$processing_method %||% default_processing_method(core)
    .build_active_core(core, method, input$processing_elements_selected)
  })

  # active core: reads committed processing_state (after Apply)
  active_input_core <- reactive({
    core <- input_data_core(); req(loaded_data_ready(core))
    st <- processing_state()
    .build_active_core(core, st$method %||% default_processing_method(core), st$selected_elements)
  })

  applied_processing_core <- reactive({
    st <- processing_state()
    if (isTRUE(st$applied)) return(active_input_core())
    core <- input_data_core(); req(loaded_data_ready(core))
    .build_active_core(core, default_processing_method(core), core$elements)
  })

  # ---------------------------------------------------------------------------
  # Input Data
  # ---------------------------------------------------------------------------
  output$input_data_project_creation_ui <- renderUI({
    core <- input_data_core()
    tagList(
      if (!isTRUE(create_project_mode()) && !loaded_data_ready(core))
        actionButton("create_new_project", "Create New Project"),
      if (isTRUE(create_project_mode()) && !loaded_data_ready(core))
        tagList(
          tags$p("Create a new project by loading the elemental matrices and shared metadata below.", class="ve-note"),
          p("Load the elemental matrices and shared metadata once. The other tabs will use these loaded data."),
          radioButtons("decimal_separator", "Decimal separator:",
            choices=c("Point (.)"=".","Comma (,)"=","), selected=".", inline=TRUE),
          uiOutput("global_files_input_ui"),
          textInput("sample_id", "Sample ID", value="", placeholder="Enter a sample identifier"),
          textInput("global_map_width", "Mapping width [mm]:", value="", placeholder="Ex: 10.5 or 10,5"),
          textInput("global_map_height", "Mapping height [mm]:", value="", placeholder="Ex: 8.2 or 8,2"),
          uiOutput("global_files_table_ui"),
          actionButton("load_input_data", "Load data"),
          actionButton("reset_input_data", "Restart"),
          hr(),
          p("Element names can be edited after file selection. Normalization percentages entered here will also be reused in Map Processing and Cluster Analysis.", class="ve-note")
        ),
      if (loaded_data_ready(core))
        tagList(
          tags$p("A project is currently loaded. Use Create New Project to clear the current session and start again, or Restart to return to the initial state.", class="ve-note"),
          actionButton("create_new_project", "Create New Project"),
          actionButton("reset_input_data", "Restart")
        )
    )
  })

  .reset_session <- function(clear_mode = FALSE) {
    updateTextInput(session, "sample_id", value="")
    updateTextInput(session, "global_map_width", value="")
    updateTextInput(session, "global_map_height", value="")
    updateRadioButtons(session, "decimal_separator", selected=".")
    file_input_version(file_input_version() + 1)
    create_project_mode(if (clear_mode) TRUE else FALSE)
    input_data_core(NULL)
    processing_state(list(applied=FALSE, enabled=FALSE, method="raw", selected_elements=NULL))
    updateSelectInput(session, "processing_method", selected="raw")
    cluster_post_state(NULL); cluster_visible_state(character())
    merge_sources_state(character()); merge_target_state("")
    map_clip_state(list())
    invisible(gc(full = TRUE, reset = TRUE))
  }

  observeEvent(input$create_new_project, { .reset_session(clear_mode=TRUE) }, ignoreInit=TRUE)
  observeEvent(input$reset_input_data,   { .reset_session(clear_mode=FALSE) })

  output$global_files_input_ui <- renderUI({
    fileInput(inputId=paste0("global_files_", file_input_version()),
              label="Select elements", multiple=TRUE, accept=c(".csv",".txt"))
  })

  selected_global_files <- reactive({ input[[paste0("global_files_", file_input_version())]] })

  suggested_element_names <- reactive({
    files <- selected_global_files()
    if (is.null(files) || nrow(files) == 0) return(character(0))
    tools::file_path_sans_ext(files$name)
  })

  output$global_files_table_ui <- renderUI({
    files <- selected_global_files(); req(!is.null(files), nrow(files) > 0)
    suggestions <- suggested_element_names()
    tagList(
      tags$h4("Selected files", style="margin-top:12px;"),
      div(style="display:grid;grid-template-columns:1.3fr 1fr 1fr;gap:8px;align-items:center;font-weight:bold;margin-bottom:6px;",
        div("File name"), div("Element name"), div("Normalization percentage")),
      lapply(seq_len(nrow(files)), function(i)
        div(style="display:grid;grid-template-columns:1.3fr 1fr 1fr;gap:8px;align-items:center;margin-bottom:8px;",
          div(files$name[i], style="overflow-wrap:anywhere;"),
          textInput(paste0("global_element_name_", i), NULL, value=suggestions[i], width="100%"),
          textInput(paste0("global_norm_", i), NULL, value="0", width="100%")
        )
      )
    )
  })

  sync_loaded_selectors <- function(core, selected_elements=NULL, method_selected=NULL) {
    req(loaded_data_ready(core))
    elems <- core$elements
    selected_elements <- if (is.null(selected_elements) || length(selected_elements) == 0) elems
                         else intersect(selected_elements, elems)
    if (length(selected_elements) == 0) selected_elements <- elems
    updateSelectInput(session, "processing_elements_selected", choices=elems, selected=selected_elements)
    updateSelectInput(session, "processing_method", selected=method_selected %||% default_processing_method(core))
    updateSelectInput(session, "filtered_single_element", choices=selected_elements, selected=selected_elements[1])
    updateSelectInput(session, "filtered_rgb_elements", choices=selected_elements,
                      selected=head(selected_elements, min(3, length(selected_elements))))
    updateSelectInput(session, "elemental_elements_selected", choices=selected_elements, selected=selected_elements)
    updateSelectInput(session, "cluster_elements_selected", choices=selected_elements,
                      selected=head(selected_elements, min(3, length(selected_elements))))
  }

  observeEvent(input$load_input_data, {
    files_df <- selected_global_files()
    validate(need(!is.null(files_df) && nrow(files_df) > 0, "Please select at least one file."))
    elems <- vapply(seq_len(nrow(files_df)), function(i) trimws(input[[paste0("global_element_name_", i)]] %||% ""), character(1))
    validate(need(all(nzchar(elems)), "Please provide an element name for every selected file."))
    validate(need(length(unique(elems)) == length(elems), "Element names must be unique."))

    files <- lapply(seq_len(nrow(files_df)), function(i)
      list(datapath=files_df$datapath[i], name=files_df$name[i]))
    mats <- lapply(files, read_matrix_file)
    dims_chr <- vapply(mats, function(x) paste(dim(x), collapse=" x "), character(1))
    validate(need(length(unique(dims_chr)) == 1, "All uploaded matrices must have identical dimensions."))

    norms <- lapply(seq_len(nrow(files_df)), function(i) parse_decimal_value(input[[paste0("global_norm_", i)]], decimal_sep()))
    names(mats) <- elems; names(norms) <- elems

    sample_id <- trimws(input$sample_id %||% "")
    if (!nzchar(sample_id)) sample_id <- paste0("sample_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    map_width  <- parse_decimal_value(input$global_map_width,  decimal_sep())
    map_height <- parse_decimal_value(input$global_map_height, decimal_sep())

    core <- list(
      sample_id = sample_id, elements = elems, mats = mats, matrices = mats,
      norms = norms, map_width = map_width, map_height = map_height,
      dims = dim(mats[[1]]), decimal_separator = decimal_sep(),
      source_type = "manual_upload",
      summary = data.frame(
        File=files_df$name, Element=elems, Normalization=unlist(norms),
        Rows=vapply(mats, nrow, integer(1)), Columns=vapply(mats, ncol, integer(1)),
        stringsAsFactors=FALSE
      )
    )

    input_data_core(core)
    default_method <- default_processing_method(core)
    processing_state(list(applied=FALSE, enabled=!identical(default_method,"raw"),
                          method=default_method, selected_elements=core$elements))
    sync_loaded_selectors(core, selected_elements=core$elements, method_selected=default_method)
    session$sendCustomMessage('veSetSideCollapsed', list(id = 've_input_layout', collapsed = TRUE))
  }, ignoreInit=TRUE)

  observeEvent(input$load_project_file, {
    req(input$load_project_file)
    proj <- tryCatch(readRDS(input$load_project_file$datapath), error=function(e) NULL)
    validate(need(!is.null(proj) && is.list(proj), "Invalid project file."))
    validate(need(!is.null(proj$elements) && !is.null(proj$matrices) && length(proj$elements) > 0,
                  "Project file is missing matrices or element information."))

    mats <- proj$matrices; elems <- proj$elements
    norms <- proj$norms %||% as.list(rep(0, length(elems)))
    names(mats) <- elems; names(norms) <- elems
    validate(need(length(unique(vapply(mats, function(x) paste(dim(x), collapse=" x "), character(1)))) == 1,
                  "Project matrices must have identical dimensions."))

    dec_sep_loaded     <- proj$decimal_separator %||% "."
    sample_id_loaded   <- proj$sample_id %||% tools::file_path_sans_ext(input$load_project_file$name)
    map_width_loaded   <- proj$map_width  %||% NA_real_
    map_height_loaded  <- proj$map_height %||% NA_real_

    summary_df <- proj$summary
    if (is.null(summary_df) || !is.data.frame(summary_df)) {
      summary_df <- data.frame(
        File=proj$file_names %||% paste0(elems, ".csv"),
        DataPath=proj$input_paths %||% rep(NA_character_, length(elems)),
        Element=elems, Normalization=unlist(norms),
        Rows=vapply(mats, nrow, integer(1)), Columns=vapply(mats, ncol, integer(1)),
        stringsAsFactors=FALSE)
    } else if (!"DataPath" %in% names(summary_df)) {
      summary_df$DataPath <- proj$input_paths %||% rep(NA_character_, nrow(summary_df))
    }

    core <- list(
      sample_id=sample_id_loaded, elements=elems, mats=mats, matrices=mats,
      norms=norms, map_width=map_width_loaded, map_height=map_height_loaded,
      dims=dim(mats[[1]]), decimal_separator=dec_sep_loaded,
      source_type="project_file", project_name=input$load_project_file$name, summary=summary_df
    )

    updateRadioButtons(session, "decimal_separator", selected=dec_sep_loaded)
    updateTextInput(session, "sample_id", value=sample_id_loaded)
    updateTextInput(session, "global_map_width",  value=format_decimal_value(map_width_loaded,  dec_sep_loaded))
    updateTextInput(session, "global_map_height", value=format_decimal_value(map_height_loaded, dec_sep_loaded))

    input_data_core(core)
    map_clip_state(proj$clip_state %||% list())
    proj_method   <- proj$processing_method %||% default_processing_method(core)
    proj_selected <- proj$selected_elements %||% elems
    if (!is.null(proj$processing_state)) {
      processing_state(proj$processing_state)
    } else {
      processing_state(list(enabled=!identical(proj_method,"raw"), method=proj_method, selected_elements=proj_selected))
    }
    sync_loaded_selectors(core, selected_elements=proj_selected, method_selected=proj_method)
    try(map_saved_maps(proj$saved_element_rgb_maps %||% list()), silent = TRUE)
    try(rgb_saved_maps(proj$saved_rgb_maps %||% list()), silent = TRUE)
    try(last_rgb_plot_data(NULL), silent = TRUE)
    try(cluster_post_state(proj$cluster_state %||% NULL), silent = TRUE)
    try(cluster_color_values(proj$cluster_colors %||% character()), silent = TRUE)
    try(cluster_visible_state(proj$cluster_visible %||% character()), silent = TRUE)
    try(cluster_auto_detect_state(proj$auto_detect_state %||% NULL), silent = TRUE)
    try(correlogram_saved_plots(proj$saved_correlograms %||% list()), silent = TRUE)
    try({
      data_analysis_visible$correlogram <- length(proj$saved_correlograms %||% list()) > 0
    }, silent = TRUE)
    session$sendCustomMessage('veSetSideCollapsed', list(id = 've_input_layout', collapsed = TRUE))
  }, ignoreInit=TRUE)

  output$loaded_data_status <- renderUI({
    core <- input_data_core()
    if (!loaded_data_ready(core)) return(tags$p("No dataset loaded yet."))
    dims_txt <- paste(core$dims[1], "rows x", core$dims[2], "columns")
    w_txt <- if (is.null(core$map_width)  || is.na(core$map_width)  || core$map_width  <= 0) "not defined" else paste0(format_decimal_value(core$map_width,  decimal_sep()), " mm")
    h_txt <- if (is.null(core$map_height) || is.na(core$map_height) || core$map_height <= 0) "not defined" else paste0(format_decimal_value(core$map_height, decimal_sep()), " mm")
    src_txt <- if (identical(core$source_type,"project_file")) paste0("project loaded from file", if (!is.null(core$project_name)) paste0(" (",core$project_name,")") else "") else "data loaded successfully from uploaded files"
    tagList(
      tags$p(tags$b("Status: "), src_txt, "."),
      tags$p(tags$b("Sample ID: "), core$sample_id %||% "not defined"),
      tags$p(tags$b("Elements: "), paste(core$elements, collapse=", ")),
      tags$p(tags$b("Matrix dimensions: "), dims_txt),
      tags$p(tags$b("Map width: "), w_txt),
      tags$p(tags$b("Map height: "), h_txt)
    )
  })

  output$loaded_data_table <- renderTable({
    core <- active_input_core(); req(loaded_data_ready(core))
    out <- core$summary
    out$Normalization <- vapply(out$Normalization, format_decimal_value, character(1), decimal_sep=decimal_sep())
    out
  })

  output$save_project_ui <- renderUI({
    if (!loaded_data_ready(input_data_core())) return(NULL)
    tagList(tags$hr(), downloadButton("save_project", "Save Project"))
  })

  output$save_project <- downloadHandler(
    filename = function() {
      core <- input_data_core()
      base_name <- if (!is.null(core$sample_id) && nzchar(core$sample_id)) core$sample_id
                   else if (!is.null(core$project_name) && nzchar(core$project_name)) tools::file_path_sans_ext(core$project_name)
                   else paste0("visualeasier_project_", Sys.Date())
      paste0(base_name, ".veproj.rds")
    },
    content = function(file) {
      saveRDS(build_project_payload(), file=file, compress=FALSE)
    },
    contentType = "application/octet-stream"
  )



  output$global_save_project_download <- downloadHandler(
    filename = function() {
      core <- input_data_core()
      base_name <- if (!is.null(core) && !is.null(core$sample_id) && nzchar(core$sample_id)) core$sample_id
                   else if (!is.null(core) && !is.null(core$project_name) && nzchar(core$project_name)) tools::file_path_sans_ext(core$project_name)
                   else paste0("visualeasier_project_", Sys.Date())
      paste0(base_name, ".veproj.rds")
    },
    content = function(file) {
      saveRDS(build_project_payload(), file=file, compress=FALSE)
    },
    contentType = "application/octet-stream"
  )

  observeEvent(input$global_new_project_btn, {
    showModal(modalDialog(
      title = "Start new project",
      "Do you want to save the current project before starting a new one?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_and_start_new_project", "Save and start new", class = "btn-primary"),
        actionButton("discard_current_project", "Start without saving", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  output$save_before_new_project <- downloadHandler(
    filename = function() {
      core <- input_data_core()
      base_name <- if (!is.null(core) && !is.null(core$sample_id) && nzchar(core$sample_id)) core$sample_id else paste0("visualeasier_project_", Sys.Date())
      paste0(base_name, ".veproj.rds")
    },
    content = function(file) {
      saveRDS(build_project_payload(), file=file, compress=FALSE)
    },
    contentType = "application/octet-stream"
  )

  observeEvent(input$save_and_start_new_project, {
    session$sendCustomMessage("veSaveThenReload", list(id = "save_before_new_project", delay = 1500))
  })

  observeEvent(input$discard_current_project, {
    removeModal()
    reset_current_session()
    gc(full = TRUE, reset = TRUE)
    session$reload()
  })

  # ---------------------------------------------------------------------------
  # Data Processing
  # ---------------------------------------------------------------------------
  output$processing_variable_selector_ui <- renderUI({
    core <- input_data_core(); req(loaded_data_ready(core))
    selectInput("processing_elements_selected", "Select variables to work with:",
      choices=core$elements, selected=core$elements, multiple=TRUE, selectize=TRUE)
  })

  output$processing_method_ui <- renderUI({
    core <- input_data_core(); req(loaded_data_ready(core))
    st <- processing_state()
    has_norm <- any(vapply(core$norms %||% list(), function(x) isTRUE(is.finite(x) && !is.na(x) && x > 0), logical(1)))
    default_sel <- st$method %||% default_processing_method(core)
    current_sel <- input$processing_method %||% default_sel
    if (!has_norm && identical(current_sel, "cps_wt")) current_sel <- "raw"
    choices <- if (has_norm)
      c("Pixel-wise CPS to Wt%"="cps_wt","Raw value"="raw","Centered Log-Ratio"="clr",
        "Natural logarithm"="ln","MinMax"="minmax","Data Scaling (SD)"="sdscale")
    else
      c("Raw value"="raw","Centered Log-Ratio"="clr","Natural logarithm"="ln",
        "MinMax"="minmax","Data Scaling (SD)"="sdscale")
    tagList(
      selectInput("processing_method", "Active data transform:", choices=choices,
                  selected=current_sel, selectize=FALSE),
      if (!has_norm) tags$p("Pixel-wise CPS to Wt% is unavailable because no concentration values were provided in Input Data.", class="ve-note")
    )
  })

  observeEvent(input$apply_processing, {
    withProgress(message="Applying data processing", value=0, {
      core <- input_data_core()
      validate(need(loaded_data_ready(core), "Please load data first in Input Data."))
      incProgress(0.25, detail="Reading selected variables")
      method <- input$processing_method %||% default_processing_method(core)
      selected_elements <- input$processing_elements_selected
      if (is.null(selected_elements) || length(selected_elements) == 0) selected_elements <- core$elements
      incProgress(0.50, detail="Updating active processed dataset")
      processing_state(list(applied=TRUE, enabled=!identical(method,"raw"),
                            method=method, selected_elements=selected_elements))
      sync_loaded_selectors(core, selected_elements=selected_elements, method_selected=method)
      session$sendCustomMessage('veToggleProcessingPanels', list(settings_open=FALSE, clip_open=TRUE))
      incProgress(0.90, detail="Refreshing downstream controls")
    })
  }, ignoreInit=TRUE)

  observeEvent(input$reset_processing, {
    core <- input_data_core(); req(loaded_data_ready(core))
    default_method <- default_processing_method(core)
    processing_state(list(applied=FALSE, enabled=!identical(default_method,"raw"),
                          method=default_method, selected_elements=core$elements))
    sync_loaded_selectors(core, selected_elements=core$elements, method_selected=default_method)
  }, ignoreInit=TRUE)

  output$processing_status_ui <- renderUI({
    core <- applied_processing_core()
    if (!loaded_data_ready(core)) return(tags$p("Load a dataset first to inspect processing options."))
    st <- processing_state()
    method_label <- processing_method_label(core$processing_method %||% "raw")
    tagList(
      tags$p(tags$b(if (isTRUE(st$applied)) "Applied transform: " else "Default transform: "), method_label),
      tags$p(tags$b("Active variables: "), paste(core$elements, collapse=", ")),
      tags$p(tags$b("Number of active variables: "), length(core$elements)),
      tags$p(tags$b("Matrix dimensions: "), paste(core$dims, collapse=" x "))
    )
  })

  output$missing_summary_table <- renderTable({
    core <- applied_processing_core(); req(loaded_data_ready(core))
    mats <- core$matrices %||% core$mats
    dplyr::bind_rows(lapply(names(mats), function(nm)
      cbind(data.frame(Element=nm, stringsAsFactors=FALSE), round(summary_stats_for_matrix(mats[[nm]]), 6))))
  })

  output$processing_pairs_plot <- renderPlot({
    withProgress(message="Building relationships preview", value=0, {
      st <- processing_state()
      validate(need(isTRUE(input$enable_processing_pairs_preview), "Variables Relationship Preview is disabled. Enable it to generate the ggpairs preview."))
      validate(need(isTRUE(st$applied), "Click Apply processing to generate the relationships preview."))
      core <- active_input_core(); req(loaded_data_ready(core))
      incProgress(0.40, detail="Sampling processed pixels")
      p <- build_processing_pairs_plot(core, max_n=1500, report_mode=FALSE)
      incProgress(0.95, detail="Rendering plot matrix"); p
    })
  })

  # ---------------------------------------------------------------------------
  # Map Processing
  # ---------------------------------------------------------------------------
  output$map_processing_controls_ui <- renderUI({
    common_filters <- c("None","Median","Gaussian blur","Gradient")
    tagList(
      tags$div(class="ve-map-type-grid",
        tags$details(class="ve-collapse-box", open="open",
          tags$summary("Element Map"),
          tags$div(class="ve-collapse-content ve-map-element-filter-box",
            tags$p("Generate one or more individual element maps from the active dataset.", class="ve-note"),
            uiOutput("elemental_selector_ui"),
            selectInput("palette", "Choose color palette:", choices=c("turbo","viridis","plasma","magma","inferno"), selected="turbo"),
            selectInput("elemental_filter", "Image filter:", choices=common_filters, selected="None"),
            checkboxInput("invert_elemental_coords", "Invert map coordinates?", value=FALSE),
            radioButtons("legend_orientation", "Legend orientation:", choices=c("Horizontal"="horizontal","Vertical"="vertical"), selected="vertical", inline=TRUE),
            actionButton("process_data", "Generate element maps")
          )
        ),
        tags$details(class="ve-collapse-box", open="open",
          tags$summary("Ternary Map (RGB)"),
          tags$div(class="ve-collapse-content ve-map-rgb-box",
            tags$p("Generate ternary RGB maps from three selected elements.", class="ve-note"),
            uiOutput("filtered_selector_ui"),
            selectInput("rgb_filter", "Image filter:", choices=common_filters, selected="None"),
            checkboxInput("invert_filtered_coords", "Invert map coordinates?", value=FALSE),
            actionButton("raster", "Generate RGB map")
          )
        )
      )
    )
  })

  output$multicolor_color_inputs_ui <- renderUI({ NULL })
  output$pca_map_outputs <- renderUI({ NULL })

  output$map_cluster_filter_ui <- renderUI({
    dat <- cluster_post_state()
    if (is.null(dat) || is.null(dat$label_map) || nrow(dat$label_map) == 0)
      return(tags$p("Cluster-based map filtering becomes available after Cluster Analysis is generated.", class="ve-note"))
    selectInput("map_cluster_filter_labels", "Filter map by cluster/alias:",
      choices=sort(dat$label_map$cluster_label), selected=sort(dat$label_map$cluster_label), multiple=TRUE)
  })

  output$map_processing_download_ui <- renderUI({
    tags$div(
      style = "display:flex;align-items:flex-end;gap:10px;flex-wrap:wrap;",
      selectInput("download_map_format", "Download format:",
                  choices=c("PDF"="pdf", "TIFF"="tif"), selected="pdf", width="150px"),
      downloadButton("download_saved_maps", "Download maps")
    )
  })

  output$map_processing_preview_ui <- renderUI({
    withSpinner(uiOutput("map_saved_tabs_ui"), type=1, color="black")
  })

  map_saved_maps <- reactiveVal(list())

  output$map_saved_tabs_ui <- renderUI({
    maps <- map_saved_maps()
    if (length(maps) == 0) {
      return(tags$p("Generate Element Maps or RGB maps to add them to the shared preview tabs.", class="ve-note"))
    }
    tabs <- lapply(seq_along(maps), function(i) {
      id <- names(maps)[[i]]
      label <- maps[[i]]$label %||% paste0("Map(", i, ")")
      tabPanel(
        title = tags$span(class="ve-tab-title-with-remove",
          tags$span(label),
          actionButton(paste0("close_map_", id), "×", title="Remove", class="ve-remove-map-btn")
        ),
        value = id,
        plotOutput(outputId=paste0("saved_map_plot_", id), width="100%", height="72vh")
      )
    })
    selected <- names(maps)[[length(maps)]]
    do.call(tabsetPanel, c(list(id="map_saved_tabs", type="pills", selected=selected), tabs))
  })

  observe({
    maps <- map_saved_maps()
    if (length(maps) == 0) return(NULL)
    for (id in names(maps)) local({
      map_id <- id
      output[[paste0("saved_map_plot_", map_id)]] <- renderPlot({
        map_saved_maps()[[map_id]]$plot
      })
    })
  })

  observe({
    maps <- map_saved_maps()
    if (length(maps) == 0) return(NULL)
    for (id in names(maps)) local({
      map_id <- id
      observeEvent(input[[paste0("close_map_", map_id)]], {
        current <- map_saved_maps()
        current[[map_id]] <- NULL
        map_saved_maps(current)
        if (length(current) > 0) {
          next_id <- names(current)[[length(current)]]
          session$onFlushed(function() updateTabsetPanel(session, "map_saved_tabs", selected = next_id), once = TRUE)
        }
        current_rgb <- rgb_saved_maps()
        if (map_id %in% names(current_rgb)) {
          current_rgb[[map_id]] <- NULL
          rgb_saved_maps(current_rgb)
        }
      }, ignoreInit=TRUE, once=TRUE)
    })
  })

  rgb_saved_maps <- reactiveVal(list())
  last_rgb_plot_data <- reactiveVal(NULL)

  output$rgb_saved_tabs_ui <- renderUI({
    maps <- rgb_saved_maps()
    if (length(maps) == 0) {
      return(tags$p("Generate an RGB map to add it to the preview tabs.", class="ve-note"))
    }
    tabs <- lapply(seq_along(maps), function(i) {
      id <- names(maps)[[i]]
      label <- maps[[i]]$label %||% paste0("RGB(", i, ")")
      tabPanel(
        title = tags$span(class="ve-tab-title-with-remove",
          tags$span(label),
          actionButton(paste0("close_", id), "×", title="Remove", class="ve-remove-map-btn")
        ),
        value = id,
        plotOutput(outputId=paste0("rgb_saved_plot_", id), width="100%", height="72vh")
      )
    })
    selected <- names(maps)[[length(maps)]]
    do.call(tabsetPanel, c(list(id="rgb_map_tabs", type="pills", selected=selected), tabs))
  })

  observe({
    maps <- rgb_saved_maps()
    if (length(maps) == 0) return(NULL)
    for (id in names(maps)) local({
      map_id <- id
      output[[paste0("rgb_saved_plot_", map_id)]] <- renderPlot({
        rgb_saved_maps()[[map_id]]$plot
      })
    })
  })

  observe({
    maps <- rgb_saved_maps()
    if (length(maps) == 0) return(NULL)
    for (id in names(maps)) local({
      map_id <- id
      observeEvent(input[[paste0("close_", map_id)]], {
        current <- rgb_saved_maps()
        current[[map_id]] <- NULL
        rgb_saved_maps(current)
        current_maps <- map_saved_maps()
        if (map_id %in% names(current_maps)) {
          current_maps[[map_id]] <- NULL
          map_saved_maps(current_maps)
        }
      }, ignoreInit=TRUE, once=TRUE)
    })
  })

  map_filter_labels <- reactive({
    dat <- cluster_post_state()
    if (is.null(dat) || is.null(dat$label_map) || nrow(dat$label_map) == 0) return(NULL)
    labs <- input$map_cluster_filter_labels
    if (is.null(labs) || length(labs) == 0) return(character(0))
    labs
  })

  output$element_clip_editor_ui <- renderUI({
    core <- active_input_core()
    if (!loaded_data_ready(core)) return(tags$p("Element clipping becomes available after loading data.", class="ve-note"))
    elems <- core$elements
    validate(need(length(elems) >= 1, "No processed element is available for clipping."))
    current_el <- input$clip_edit_element %||% elems[1]
    current_el <- if (current_el %in% elems) current_el else elems[1]
    mat <- (core$matrices %||% core$mats)[[current_el]]
    rng <- range(mat, na.rm=TRUE)
    if (!all(is.finite(rng)) || diff(rng) == 0) rng <- c(0, 1)
    rng <- round(rng, 1); if (diff(rng) <= 0) rng[2] <- rng[1] + 0.1
    saved <- map_clip_state()[[current_el]]
    val <- if (!is.null(saved) && length(saved) == 2 && all(is.finite(saved))) round(saved, 1) else rng
    val <- c(max(rng[1], val[1]), min(rng[2], val[2]))
    tagList(
      tags$h4("Element clipping editor"),
      actionButton("auto_clip_10pct_all", "Auto-adjust"),
      tags$p("Automatically sets the lower clipping limit to approximately 10% of the maximum value for all selected elements. You can still manually adjust each element afterward.", class="ve-note"),
      selectInput("clip_edit_element", "Element to edit", choices=elems, selected=current_el),
      sliderInput("clip_range", "Lower and upper detection limits",
        min=rng[1], max=rng[2], value=val, step=0.1, round=TRUE),
      actionButton("generate_clip_map", "Generate map"),
      withSpinner(plotOutput("clip_preview_plot", width="100%", height="320px"), type=1, color="black"),
      tags$p("Values below the lower detection limit are set to 0. Values above the upper detection limit are set to the upper limit. Save clipping now overwrites the selected processed variable in the active workflow; no *_clip variable is created.", class="ve-note"),
      fluidRow(
        column(6, actionButton("save_element_clip", "Save clipping")),
        column(6, actionButton("reset_element_clip", "Reset clipping"))
      )
    )
  })

  output$clip_preview_plot <- renderPlot({
    validate(need(input$generate_clip_map > 0, "Choose an element and click Generate map to preview clipping."))
    isolate({
      core <- active_input_core(); req(loaded_data_ready(core))
      elems <- core$elements; elem <- input$clip_edit_element %||% elems[1]
      validate(need(elem %in% elems, "Choose a valid element."))
      mat <- (core$matrices %||% core$mats)[[elem]]
      lim <- input$clip_range
      if (!is.null(lim) && length(lim) == 2 && all(is.finite(lim)))
        mat <- clip_matrix_limits(mat, lim)
      df <- matrix_to_df(mat, "value")
      ggplot(df, aes(x=x, y=y, fill=value)) +
        geom_raster() + coord_equal() +
        scale_fill_gradientn(colours=resolve_palette(input$palette %||% "turbo"), na.value="grey90") +
        theme_minimal() + labs(title=paste0("Clipping preview: ", elem), fill=elem) +
        theme(
          plot.title      = element_text(hjust=0.5, face="bold"),
          axis.title      = element_blank(),
          axis.text       = element_blank(),
          axis.ticks      = element_blank(),
          panel.grid      = element_blank(),
          legend.position = "right"
        )
    })
  })

  output$pca_selector_ui <- renderUI({ NULL })

  output$elemental_selector_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    selectInput("elemental_elements_selected", "Select loaded elements to display:",
      choices=core$elements, selected=core$elements, multiple=TRUE, selectize=TRUE)
  })

  output$elemental_filter_targets_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    selected <- input$elemental_elements_selected %||% core$elements
    selected <- intersect(selected, core$elements)
    if (length(selected) == 0) selected <- core$elements
    checkboxGroupInput(
      "elemental_filter_targets",
      "Apply selected image filter to:",
      choices = selected,
      selected = selected
    )
  })

  selected_elements_for_clipping <- reactive({
    core <- active_input_core()
    if (!loaded_data_ready(core)) return(character(0))
    elems <- core$elements
    selected <- unique(c(input$elemental_elements_selected %||% character(0),
                         input$filtered_rgb_elements %||% character(0),
                         input$processing_elements_selected %||% character(0)))
    if (is.null(selected) || length(selected) == 0) selected <- elems
    selected <- intersect(selected, elems)
    if (length(selected) == 0) selected <- elems
    selected
  })

  observeEvent(input$auto_clip_10pct_all, {
    core <- active_input_core(); req(loaded_data_ready(core))
    mats <- core$matrices %||% core$mats
    selected <- selected_elements_for_clipping()
    validate(need(length(selected) > 0, "No selected element is available for automatic clipping."))

    st <- map_clip_state()
    for (el in selected) {
      mat <- mats[[el]]
      rng <- range(mat, na.rm=TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) next
      upper <- round(rng[2], 1)
      lower <- round(0.10 * rng[2], 1)
      lower <- max(round(rng[1], 1), min(lower, upper))
      if (upper <= lower) upper <- lower + 0.1
      st[[el]] <- c(lower, upper)
    }
    map_clip_state(st)
  }, ignoreInit=TRUE)

  observeEvent(input$save_element_clip, {
    core <- active_input_core(); req(loaded_data_ready(core))
    el <- input$clip_edit_element; rng <- input$clip_range
    req(!is.null(el), length(rng) == 2)
    st <- map_clip_state(); st[[el]] <- as.numeric(rng); map_clip_state(st)
  }, ignoreInit=TRUE)

  observeEvent(input$reset_element_clip, {
    el <- input$clip_edit_element; st <- map_clip_state()
    if (!is.null(el) && el %in% names(st)) st[[el]] <- NULL
    map_clip_state(st)
  }, ignoreInit=TRUE)

  elemental_data <- eventReactive(input$process_data, {
    withProgress(message="Generating element maps", value=0, {
      core <- active_input_core()
      validate(need(loaded_data_ready(core), "Please load data first in the Input Data tab."))
      elems <- input$elemental_elements_selected
      if (is.null(elems) || length(elems) == 0) elems <- core$elements
      validate(need(length(elems) >= 1, "Please select at least one element to display."))
      validate(need(all(elems %in% core$elements), "Some selected elements are not available in the loaded dataset."))
      incProgress(0.15, detail="Preparing selected variables")
      mats <- unname(core$matrices[elems])
      legend_dir <- input$legend_orientation %||% "vertical"
      guide_obj <- if (identical(legend_dir, "vertical"))
        guide_colorbar(direction="vertical", barwidth=1, barheight=15)
      else guide_colorbar(direction="horizontal", barwidth=15, barheight=1)
      incProgress(0.20, detail="Preparing selected variables")
      nr_mat <- nrow(mats[[1]]); nc_mat <- ncol(mats[[1]])
      # coord_equal(): y-units per x-unit keeps square pixels undistorted.
      # Axes are intentionally hidden because the graphical scale bar carries the map scale.
      plots <- lapply(seq_along(mats), function(i) {
        incProgress(0.70 / max(1, length(mats)), detail=paste("Rendering", elems[i]))
        # Element Map must display the active data matrix exactly as stored in the active workflow.
        # Do not re-normalize selected elements here: with a single normalized element, pixel-wise
        # closure would force the map to 100% and distort the original spatial signal.
        filter_to_apply <- input$elemental_filter %||% "None"
        display_mat <- apply_filter_to_matrix(mats[[i]], filter_to_apply)
        df <- matrix_to_df(display_mat, "value")
        df <- apply_cluster_filter_df(df, cluster_post_state(), map_filter_labels())
        validate(need(nrow(df) > 0, "No pixels remain after applying the selected cluster filter."))
        df <- maybe_invert_df(df, isTRUE(input$invert_elemental_coords))
        p <- ggplot(df, aes(x, y, fill=value)) +
          geom_raster() +
          coord_equal() +
          scale_fill_gradientn(colours=resolve_palette(input$palette), na.value="grey90", guide=guide_obj) +
          ggpubr::theme_pubr() +
          labs(title=NULL, fill=paste0(elems[i], " (wt.%)")) +
          theme(plot.title=element_text(hjust=0.5),
                legend.position=if (identical(legend_dir,"vertical")) "right" else "bottom")
        p <- p + theme(axis.title=element_blank(), axis.text=element_blank(),
                       axis.ticks=element_blank(), axis.line=element_blank(),
                       panel.grid=element_blank())
        scale_width_mm  <- if (isTRUE(input$invert_elemental_coords)) core$map_height else core$map_width
        scale_height_mm <- if (isTRUE(input$invert_elemental_coords)) core$map_width  else core$map_height
        add_scale_bar(p, build_scale_annotation(df, scale_width_mm, scale_height_mm, y_reversed = FALSE))
      })
      incProgress(0.95, detail="Finalizing map layout")
      current_saved <- map_saved_maps()
      for (i in seq_along(plots)) {
        new_idx <- length(current_saved) + 1L
        new_id <- paste0("element_", as.integer(Sys.time()), "_", new_idx, "_", i)
        current_saved[[new_id]] <- list(
          plot = plots[[i]],
          elements = elems[i],
          mode = "elemental",
          aspect_ratio = compute_aspect_ratio(nrow(mats[[i]]), ncol(mats[[i]]), isTRUE(input$invert_elemental_coords)),
          label = elems[i]
        )
      }
      map_saved_maps(current_saved)
      if (length(current_saved) > 0) {
        last_id <- names(current_saved)[[length(current_saved)]]
        session$onFlushed(function() updateTabsetPanel(session, "map_saved_tabs", selected = last_id), once = TRUE)
      }
      list(mats=mats, plots=plots,
           aspect_ratio=compute_aspect_ratio(nrow(mats[[1]]), ncol(mats[[1]]), isTRUE(input$invert_elemental_coords)),
           elements=elems, mode="elemental")
    })
  })

  pca_map_data <- reactive({ NULL })
  current_grid_plot_data <- reactive({ elemental_data() })

  output$graph_outputs <- renderUI({
    dat <- current_grid_plot_data(); req(dat); plots <- dat$plots
    tabs <- lapply(seq_along(plots), function(i) {
      tabPanel(
        title = dat$elements[[i]] %||% paste0("Map ", i),
        plotOutput(outputId=paste0("element_plot_",i), width="100%", height="72vh")
      )
    })
    do.call(tabsetPanel, c(list(id="element_map_tabs", type="pills"), tabs))
  })

  observe({
    dat <- current_grid_plot_data(); req(dat); plots <- dat$plots
    for (i in seq_along(plots)) local({
      idx <- i
      output[[paste0("element_plot_",idx)]] <- renderPlot({ plots[[idx]] })
    })
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      dat <- current_grid_plot_data(); req(dat)
      fmt <- input$download_map_format %||% "pdf"
      ext <- if (identical(fmt, "tif")) "tif" else "pdf"
      paste0("elemental_", paste(dat$elements, collapse="_"),
             filter_label_for_filename(input$elemental_filter %||% "None"), "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      dat <- current_grid_plot_data(); req(dat)
      n <- length(dat$plots); ncol_plot <- ceiling(sqrt(n)); nrow_plot <- ceiling(n / ncol_plot)
      base_width <- 8; base_height <- base_width * dat$aspect_ratio
      combined_plot <- if (n == 1) dat$plots[[1]] else wrap_plots(dat$plots, ncol=ncol_plot)
      fmt <- input$download_map_format %||% "pdf"
      if (identical(fmt, "tif")) {
        save_plot_as_tiff(combined_plot, file=file,
          width=base_width*ncol_plot, height=base_height*nrow_plot, dpi=300)
      } else {
        save_plot_as_raster_pdf(combined_plot, file=file,
          width=base_width*ncol_plot, height=base_height*nrow_plot, dpi=220)
      }
    }
  )

  output$filtered_selector_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    selectInput("filtered_rgb_elements", "Select 3 loaded elements (R, G, B):",
      choices=core$elements, selected=core$elements[seq_len(min(3,length(core$elements)))], multiple=TRUE)
  })

  generate_rgb_map_data <- function() {
    withProgress(message="Generating RGB map", value=0, {
      core <- active_input_core()
      validate(need(loaded_data_ready(core), "Please load data first in the Input Data tab."))
      incProgress(0.15, detail="Reading selected RGB layers")
      selected <- input$filtered_rgb_elements
      validate(need(length(selected) == 3, "Please select exactly three loaded elements for the RGB map."))
      validate(need(all(selected %in% core$elements), "Selected RGB elements are not available in the loaded dataset."))
      incProgress(0.35, detail="Normalizing RGB channels")
      make_channel <- function(s) {
        mat <- normalize_minmax_matrix(core$matrices[[s]])
        apply_filter_to_matrix(mat, input$rgb_filter %||% "None")
      }
      channels <- lapply(selected, make_channel)
      aspect_ratio <- compute_aspect_ratio(nrow(channels[[1]]), ncol(channels[[1]]), isTRUE(input$invert_filtered_coords))
      incProgress(0.65, detail="Composing RGB image")
      rgb_img <- imappend(lapply(channels, as.cimg), "c")
      df <- cimg_to_df(rgb_img)
      df <- apply_cluster_filter_df(df, cluster_post_state(), map_filter_labels())
      validate(need(nrow(df) > 0, "No pixels remain after applying the selected cluster filter."))
      filter_label <- input$rgb_filter %||% "None"
      title <- paste0("Ternary Map", filter_label_for_title(filter_label))
      incProgress(0.85, detail="Rendering RGB map")
      p <- make_rgb_plot(df=df, title=title, labels=selected,
        map_width_mm=core$map_width, map_height_mm=core$map_height,
        invert_coords=isTRUE(input$invert_filtered_coords))
      list(plot=p, elements=selected, mode="rgb", aspect_ratio=aspect_ratio)
    })
  }

  observeEvent(input$raster, {
    dat <- generate_rgb_map_data()
    last_rgb_plot_data(dat)

    current_saved <- rgb_saved_maps()
    new_idx <- length(current_saved) + 1L
    new_id <- paste0("rgb_", as.integer(Sys.time()), "_", sample.int(1e6, 1), "_", new_idx)
    rgb_label <- if (new_idx == 1L) "RGB" else paste0("RGB(", new_idx - 1L, ")")

    dat_saved <- dat
    dat_saved$label <- rgb_label
    current_saved[[new_id]] <- dat_saved
    rgb_saved_maps(current_saved)

    current_map_saved <- map_saved_maps()
    current_map_saved[[new_id]] <- dat_saved
    map_saved_maps(current_map_saved)

    session$onFlushed(function() updateTabsetPanel(session, "map_saved_tabs", selected = new_id), once = TRUE)
  }, ignoreInit=TRUE)

  filtered_plot_data <- reactive({
    last_rgb_plot_data()
  })

  output$plot_rgb <- renderPlot({ req(filtered_plot_data()); filtered_plot_data()$plot })

  output$download_rgb <- downloadHandler(
    filename = function() {
      dat <- filtered_plot_data(); req(dat)
      fmt <- input$download_map_format %||% "pdf"
      ext <- if (identical(fmt, "tif")) "tif" else "pdf"
      paste0("RGB_", paste(dat$elements, collapse="_"),
             filter_label_for_filename(input$rgb_filter %||% "None"), "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      dat <- filtered_plot_data(); req(dat)
      ar <- dat$aspect_ratio %||% 1
      if (!is.finite(ar) || ar <= 0) ar <- 1
      fmt <- input$download_map_format %||% "pdf"
      if (identical(fmt, "tif")) {
        save_plot_as_tiff(dat$plot, file=file, width=10, height=10*ar, dpi=300)
      } else {
        save_plot_as_raster_pdf(dat$plot, file=file, width=10, height=10*ar, dpi=220)
      }
    }
  )

  output$download_saved_maps <- downloadHandler(
    filename = function() {
      fmt <- input$download_map_format %||% "pdf"
      ext <- if (identical(fmt, "tif")) "tif" else "pdf"
      paste0("QMap_saved_maps_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      maps <- map_saved_maps()
      validate(need(length(maps) > 0, "Generate at least one map before downloading."))
      fmt <- input$download_map_format %||% "pdf"
      plots <- lapply(maps, function(x) x$plot)
      combined <- if (length(plots) == 1) plots[[1]] else patchwork::wrap_plots(plots, ncol=1)
      heights <- vapply(maps, function(x) {
        ar <- x$aspect_ratio %||% 1
        if (!is.finite(ar) || ar <= 0) 1 else ar
      }, numeric(1))
      width <- 10
      height <- min(60, max(8, sum(10 * heights)))
      if (identical(fmt, "tif")) {
        save_plot_as_tiff(combined, file=file, width=width, height=height, dpi=300)
      } else {
        save_plot_as_raster_pdf(combined, file=file, width=width, height=height, dpi=220)
      }
    }
  )


  # ---------------------------------------------------------------------------
  # Cluster Analysis
  # ---------------------------------------------------------------------------
  output$cluster_selector_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    selected_elems <- intersect(input$cluster_elements_selected %||% core$elements, core$elements)
    if (length(selected_elems) == 0) selected_elems <- core$elements
    selectInput("cluster_elements_selected", "Select loaded elements for clustering:",
      choices=core$elements, selected=selected_elems, multiple=TRUE)
  })

  observeEvent(input$ncluster, {
    ncluster_value <- suppressWarnings(as.integer(input$ncluster %||% 2L))
    if (length(ncluster_value) != 1 || !is.finite(ncluster_value) || ncluster_value < 1) ncluster_value <- 2L
    cluster_color_values(toupper(cluster_palette(max(1L, ncluster_value), "Okabe-Ito")))
  }, ignoreInit=FALSE)

  output$cluster_color_controls_ui <- renderUI({
    cols <- cluster_color_values(); req(length(cols) > 0)
    rows <- lapply(seq_along(cols), function(i) {
      current_hex <- toupper(cols[i]); current_rgb <- hex_to_rgb(current_hex)
      tags$div(class = "ve-cluster-color-row",
        tags$div(class = "ve-cluster-color-label", paste0("Cluster ", i)),
        tags$div(class = "ve-cluster-color-picker",
          colourpicker::colourInput(
            paste0("cluster_col_", i),
            NULL,
            value = unname(current_hex),
            showColour = "background",
            allowTransparent = FALSE,
            width = "40px"
          )
        ),
        tags$div(class = "ve-rgb-field",
          tags$label("R", style = "font-weight:600;display:block;margin-bottom:4px;"),
          numericInput(
            paste0("cluster_r_", i),
            NULL,
            value = unname(current_rgb["r"]),
            min = 0,
            max = 255,
            width = "60px"
          )
        ),
        tags$div(class = "ve-rgb-field",
          tags$label("G", style = "font-weight:600;display:block;margin-bottom:4px;"),
          numericInput(
            paste0("cluster_g_", i),
            NULL,
            value = unname(current_rgb["g"]),
            min = 0,
            max = 255,
            width = "60px"
          )
        ),
        tags$div(class = "ve-rgb-field",
          tags$label("B", style = "font-weight:600;display:block;margin-bottom:4px;"),
          numericInput(
            paste0("cluster_b_", i),
            NULL,
            value = unname(current_rgb["b"]),
            min = 0,
            max = 255,
            width = "60px"
          )
        ),
        tags$div(class = "ve-hex-field",
          tags$label("HEX", style = "font-weight:600;display:block;margin-bottom:4px;"),
          textInput(
            paste0("cluster_hex_", i),
            NULL,
            value = unname(current_hex),
            width = "80px"
          )
        )
      )
    })
    tagList(tags$label("Cluster colors:"), do.call(tagList, rows))
  })

  output$cluster_visible_ui <- renderUI({
    dat <- cluster_post_state(); if (is.null(dat)) return(NULL)
    choices <- as.list(stats::setNames(sort(dat$label_map$cluster_label), sort(dat$label_map$cluster_label)))
    valid_selected <- intersect(cluster_visible_state(), unname(dat$label_map$cluster_label))
    if (length(valid_selected) == 0) valid_selected <- sort(unname(dat$label_map$cluster_label))
    checkboxGroupInput("cluster_visible_labels", "Visible clusters in map:",
      choices=choices, selected=valid_selected, inline=TRUE)
  })

  output$cluster_pca_controls_ui <- renderUI({
    if (!isTRUE(input$use_pca_cluster)) return(NULL)
    sliderInput("cluster_pca_variance", "Minimum cumulative variance for component selection",
      min=0.5, max=0.99, value=0.99, step=0.01)
  })

  output$cluster_ambiguity_controls_ui <- renderUI({
    if (!isTRUE(input$enable_ambiguity_filter)) return(NULL)
    sliderInput("entropy_rank_percent", "Maximum Entropy rank retained in statistics (%)",
      min=30, max=100, value=30, step=1, post="%")
  })

  observeEvent(input$auto_detect_clusters, {
    withProgress(message="Auto detecting optimum number of phases", value=0, {
      core <- active_input_core()
      validate(need(loaded_data_ready(core), "Please load data first in the Input Data tab."))
      elems <- input$cluster_elements_selected
      if (is.null(elems) || length(elems) == 0) elems <- core$elements
      elems <- intersect(elems, core$elements)
      validate(need(length(elems) >= 2, "Please select at least two loaded elements for clustering."))

      norm_vec <- suppressWarnings(as.numeric(unlist(core$norms[elems], use.names = FALSE)))
      names(norm_vec) <- elems
      validate(need(length(norm_vec) == length(elems) && all(is.finite(norm_vec)) && all(norm_vec > 0),
                    "Auto Detect requires positive concentration/normalization values for all selected clustering elements."))

      incProgress(0.08, detail="Preparing active variables")
      mats <- unname(core$matrices[elems])
      names(mats) <- elems
      norms <- unname(core$norms[elems])
      merged <- matrix_list_to_merged_df(mats, elems)
      cluster_data <- merged %>% dplyr::select(dplyr::all_of(elems))
      valid_rows <- stats::complete.cases(cluster_data)
      cluster_data <- cluster_data[valid_rows, , drop = FALSE]
      coords <- merged[valid_rows, c("x", "y"), drop = FALSE]
      validate(need(nrow(cluster_data) >= 20, "Auto Detect requires at least 20 valid pixels."))

      cluster_data_for_pct <- apply_global_normalization(mats, norms)
      merged_pct <- matrix_list_to_merged_df(cluster_data_for_pct, elems)
      cluster_pct_only <- merged_pct %>% dplyr::select(dplyr::all_of(elems))
      cluster_pct_only <- cluster_pct_only[valid_rows, , drop = FALSE]
      px_sum <- rowSums(cluster_pct_only, na.rm = TRUE)
      px_sum[!is.finite(px_sum) | px_sum == 0] <- NA_real_
      cluster_data_norm <- as.data.frame(sweep(cluster_pct_only, 1, px_sum, FUN = "/") * 100)
      cluster_data_norm_mat <- as.matrix(cluster_data_norm)
      cluster_data_norm_mat[!is.finite(cluster_data_norm_mat)] <- NA_real_
      cluster_data_norm <- as.data.frame(cluster_data_norm_mat)
      names(cluster_data_norm) <- elems

      n_total <- nrow(cluster_data)
      sample_percent <- suppressWarnings(as.numeric(input$auto_detect_sample_percent %||% 100))
      if (!is.finite(sample_percent) || is.na(sample_percent)) sample_percent <- 100
      sample_percent <- max(1, min(100, sample_percent))
      n_sample <- min(n_total, 500000L, max(20L, ceiling(n_total * sample_percent / 100)))
      set.seed(123)
      sample_idx <- sample.int(n_total, size = n_sample, replace = FALSE)
      cluster_sample <- cluster_data[sample_idx, , drop = FALSE]
      coords_sample <- coords[sample_idx, , drop = FALSE]
      box_sample <- as.data.frame(cluster_data_norm[sample_idx, , drop = FALSE])

      scaled_vars <- scale(cluster_sample, center = TRUE, scale = TRUE)
      scaled_vars[!is.finite(scaled_vars)] <- 0

      incProgress(0.10, detail = "Computing PCA for Auto Detect")
      pca_auto <- tryCatch(
        stats::prcomp(scaled_vars, center = FALSE, scale. = FALSE),
        error = function(e) NULL
      )
      validate(need(!is.null(pca_auto) && !is.null(pca_auto$x) && ncol(pca_auto$x) >= 1,
                    "Auto Detect could not compute PCA from the sampled pixels."))

      pca_var <- pca_auto$sdev^2
      pca_cum <- cumsum(pca_var) / sum(pca_var)
      n_pc <- which(pca_cum >= 0.90)[1]
      if (is.na(n_pc) || !is.finite(n_pc)) n_pc <- min(ncol(pca_auto$x), length(elems))
      n_pc <- max(1L, min(as.integer(n_pc), ncol(pca_auto$x)))
      pca_scores <- as.data.frame(pca_auto$x[, seq_len(n_pc), drop = FALSE])
      pc_names <- paste0("PC", seq_len(n_pc))
      names(pca_scores) <- pc_names
      pca_mat <- as.matrix(pca_scores)
      pca_mat[!is.finite(pca_mat)] <- 0

      distinct_n <- nrow(unique(as.data.frame(pca_mat)))
      k_max <- min(15L, n_sample, distinct_n)
      validate(need(k_max >= 2, "Auto Detect requires at least two distinct sampled PCA scores."))
      k_values <- seq.int(2L, k_max)

      box_sample_bci <- as.data.frame(cluster_pct_only[sample_idx, , drop = FALSE])
      names(box_sample_bci) <- elems
      pure_fraction <- 0.30
      n_runs <- 5L
      res_list <- vector("list", length(k_values))
      for (ii in seq_along(k_values)) {
        k <- k_values[ii]
        incProgress(0.12 + 0.80 * ii / length(k_values),
                    detail = paste0("Testing ", k, " phases using PCA and stability runs"))

        km_runs <- vector("list", n_runs)
        for (rr in seq_len(n_runs)) {
          set.seed(100000 + 1000 * k + rr)
          km_runs[[rr]] <- tryCatch(
            stats::kmeans(pca_mat, centers = k, nstart = 1, iter.max = 75, algorithm = "Lloyd"),
            error = function(e) NULL
          )
        }
        valid_runs <- km_runs[!vapply(km_runs, is.null, logical(1))]
        if (length(valid_runs) == 0) {
          res_list[[ii]] <- data.frame(k = k, BCI = NA_real_, WeightedEntropy = NA_real_,
                                       Stability = NA_real_, CompositeScore = NA_real_,
                                       SampledPixels = n_sample, PurePixels = NA_integer_,
                                       PureFraction = pure_fraction, PCAComponents = n_pc,
                                       Runs = n_runs, SuccessfulRuns = 0L,
                                       stringsAsFactors = FALSE)
          next
        }

        run_clusters <- lapply(valid_runs, function(x) x$cluster)
        stability <- mean_pairwise_ari(run_clusters)
        best_idx <- which.min(vapply(valid_runs, function(x) x$tot.withinss %||% Inf, numeric(1)))
        km <- valid_runs[[best_idx]]

        result_df <- dplyr::bind_cols(coords_sample, cluster_id = as.character(km$cluster))
        result_df$cluster_label <- paste0("Cluster ", result_df$cluster_id)

        # Auto Detect uses PCA to find pseudo-mineral domains. The 30% purest
        # pixels estimate phase compositions; all sampled pixels estimate phase
        # abundance. This avoids both mixed-pixel bias in composition and modal
        # bias from using only pure pixels for abundance.
        amb <- compute_cluster_ambiguity(result_df, pca_scores, pc_names)
        weighted_entropy <- if (nrow(amb) > 0) mean(amb$ambiguity, na.rm = TRUE) else NA_real_
        if (nrow(amb) > 0 && any(is.finite(amb$ambiguity))) {
          ord_pure <- order(amb$ambiguity, decreasing = FALSE, na.last = NA)
          n_pure <- max(2L, ceiling(length(ord_pure) * pure_fraction))
          pure_idx <- ord_pure[seq_len(min(n_pure, length(ord_pure)))]
        } else {
          pure_idx <- seq_len(nrow(result_df))
        }

        recon <- compute_reconstructed_bulk_from_clusters_pure(result_df, box_sample_bci, pure_idx, elems)
        bci <- compute_bulk_composition_index(norm_vec[elems], recon[elems])
        res_list[[ii]] <- data.frame(k = k, BCI = bci, WeightedEntropy = weighted_entropy,
                                     Stability = stability,
                                     SampledPixels = n_sample, PurePixels = length(pure_idx),
                                     PureFraction = pure_fraction, PCAComponents = n_pc,
                                     Runs = n_runs, SuccessfulRuns = length(valid_runs),
                                     stringsAsFactors = FALSE)
      }
      diag_df <- dplyr::bind_rows(res_list)
      validate(need(any(is.finite(diag_df$BCI)),
                    "Auto Detect could not calculate valid diagnostic scores."))
      diag_df <- compute_auto_detect_composite_scores(diag_df)
      diag_df$BCI_relative <- rescale_01_safe(diag_df$BCI)
      diag_df$Entropy_relative <- rescale_01_safe(diag_df$WeightedEntropy)
      opt_idx <- which.max(ifelse(is.finite(diag_df$CompositeScore), diag_df$CompositeScore, -Inf))
      opt_k <- as.integer(diag_df$k[opt_idx])
      cluster_auto_detect_state(list(table = diag_df, optimum_k = opt_k, sample_size = n_sample,
                                     sample_percent = sample_percent, sample_cap = 500000L, pure_fraction = 0.30, pca_components = n_pc,
                                     runs = n_runs,
                                     score_weights = c(BCI = 0.30, Purity = 0.35, Stability = 0.25, Parsimony = 0.10),
                                     total_valid_pixels = n_total, elements = elems,
                                     bulk = norm_vec[elems], detected_at = Sys.time()))
      updateNumericInput(session, "ncluster", value = opt_k)
      session$onFlushed(function() updateTabsetPanel(session, "cluster_preview_tabs", selected = "Auto Detect"), once = TRUE)
      incProgress(0.98, detail = paste0("Optimum selected: ", opt_k, " phases"))
    })
  }, ignoreInit = TRUE)

  resolve_cluster_hex <- function(idx, fallback_hex) {
    col_in <- input[[paste0("cluster_col_",idx)]]
    if (!is.null(col_in)) {
      col_try <- toupper(trimws(as.character(col_in)))
      if (grepl("^#[0-9A-F]{6}$", col_try)) return(col_try)
    }
    hex_in <- input[[paste0("cluster_hex_",idx)]]
    if (!is.null(hex_in)) {
      hex_try <- toupper(trimws(as.character(hex_in)))
      if (grepl("^#[0-9A-F]{6}$", hex_try)) return(hex_try)
    }
    r <- suppressWarnings(as.integer(input[[paste0("cluster_r_",idx)]]))
    g <- suppressWarnings(as.integer(input[[paste0("cluster_g_",idx)]]))
    b <- suppressWarnings(as.integer(input[[paste0("cluster_b_",idx)]]))
    if (all(is.finite(c(r,g,b))) && all(c(r,g,b) >= 0) && all(c(r,g,b) <= 255))
      return(rgb_to_hex_safe(r, g, b))
    toupper(fallback_hex)
  }

  for (color_idx in seq_len(60)) {
    local({
      i <- color_idx
      observeEvent(input[[paste0("cluster_col_", i)]], {
        col <- toupper(trimws(as.character(input[[paste0("cluster_col_", i)]] %||% "")))
        if (!grepl("^#[0-9A-F]{6}$", col)) return(NULL)
        rgb <- hex_to_rgb(col)
        updateTextInput(session, paste0("cluster_hex_", i), value=unname(col))
        updateNumericInput(session, paste0("cluster_r_", i), value=unname(rgb["r"]))
        updateNumericInput(session, paste0("cluster_g_", i), value=unname(rgb["g"]))
        updateNumericInput(session, paste0("cluster_b_", i), value=unname(rgb["b"]))
      }, ignoreInit=TRUE)
      observeEvent(input[[paste0("cluster_hex_", i)]], {
        hx <- toupper(trimws(as.character(input[[paste0("cluster_hex_", i)]] %||% "")))
        if (!grepl("^#[0-9A-F]{6}$", hx)) return(NULL)
        rgb <- hex_to_rgb(hx)
        colourpicker::updateColourInput(session, paste0("cluster_col_", i), value=unname(hx))
        updateNumericInput(session, paste0("cluster_r_", i), value=unname(rgb["r"]))
        updateNumericInput(session, paste0("cluster_g_", i), value=unname(rgb["g"]))
        updateNumericInput(session, paste0("cluster_b_", i), value=unname(rgb["b"]))
      }, ignoreInit=TRUE)
      observeEvent({
        list(input[[paste0("cluster_r_", i)]], input[[paste0("cluster_g_", i)]], input[[paste0("cluster_b_", i)]])
      }, {
        r <- suppressWarnings(as.numeric(input[[paste0("cluster_r_", i)]]))
        g <- suppressWarnings(as.numeric(input[[paste0("cluster_g_", i)]]))
        b <- suppressWarnings(as.numeric(input[[paste0("cluster_b_", i)]]))
        if (!all(is.finite(c(r, g, b)))) return(NULL)
        hx <- toupper(rgb_to_hex_safe(r, g, b))
        colourpicker::updateColourInput(session, paste0("cluster_col_", i), value=unname(hx))
        updateTextInput(session, paste0("cluster_hex_", i), value=unname(hx))
      }, ignoreInit=TRUE)
    })
  }

  cluster_results <- eventReactive(input$cluster, {
    withProgress(message="Calculating clusters", value=0, {
      core <- active_input_core()
      validate(need(loaded_data_ready(core), "Please load data first in the Input Data tab."))
      elems <- input$cluster_elements_selected
      validate(need(length(elems) >= 2, "Please select at least two loaded elements for clustering."))
      validate(need(all(elems %in% core$elements), "Some selected cluster elements are not available in the loaded dataset."))
      ncluster_value <- suppressWarnings(as.integer(input$ncluster %||% 2L))
      validate(need(length(ncluster_value) == 1 && is.finite(ncluster_value) && ncluster_value >= 2,
                    "Please choose at least 2 clusters."))

      base_cols <- toupper(cluster_palette(ncluster_value, "Okabe-Ito"))
      current_cols <- cluster_color_values()
      if (length(current_cols) != ncluster_value) current_cols <- base_cols
      selected_cols <- toupper(vapply(seq_len(ncluster_value), function(i) resolve_cluster_hex(i, current_cols[i]), character(1)))
      names(selected_cols) <- as.character(seq_len(ncluster_value))
      cluster_color_values(selected_cols)

      incProgress(0.15, detail="Preparing selected variables")
      mats <- unname(core$matrices[elems]); norms <- unname(core$norms[elems])
      merged <- matrix_list_to_merged_df(mats, elems)
      cluster_data <- merged %>% select(all_of(elems))
      valid_rows <- complete.cases(cluster_data)
      cluster_data <- cluster_data[valid_rows, , drop=FALSE]
      coords <- merged[valid_rows, c("x","y")]
      validate(need(nrow(cluster_data) >= ncluster_value,
                    "The number of valid pixels must be greater than or equal to the number of clusters."))

      cluster_data_for_pct <- apply_global_normalization(mats, norms)
      merged_pct <- matrix_list_to_merged_df(cluster_data_for_pct, elems)
      cluster_pct_only <- merged_pct %>% select(all_of(elems))
      cluster_pct_only <- cluster_pct_only[valid_rows, , drop=FALSE]
      px_sum <- rowSums(cluster_pct_only, na.rm=TRUE); px_sum[px_sum == 0] <- NA_real_
      cluster_data_norm <- sweep(cluster_pct_only, 1, px_sum, FUN="/") * 100

      incProgress(0.35, detail="Scaling data")
      scaled <- scale(cluster_data, center=TRUE, scale=TRUE); scaled[!is.finite(scaled)] <- 0

      cluster_input <- scaled; pca_info <- NULL; pc_scores_full <- data.frame()
      if (isTRUE(input$use_pca_cluster)) {
        incProgress(0.50, detail="Computing PCA for clustering")
        pca_model <- prcomp(scaled, center=FALSE, scale.=FALSE)
        pc_scores_full <- as.data.frame(pca_model$x)
        names(pc_scores_full) <- paste0("PC", seq_len(ncol(pc_scores_full)))
        var_explained <- (pca_model$sdev^2) / sum(pca_model$sdev^2)
        cum_var <- cumsum(var_explained)
        target_var <- suppressWarnings(as.numeric(input$cluster_pca_variance))
        if (!is.finite(target_var)) target_var <- 0.90
        npc <- which(cum_var >= target_var)[1]
        if (!is.finite(npc) || is.na(npc)) npc <- ncol(pca_model$x)
        npc <- max(1, min(npc, ncol(pca_model$x)))
        cluster_input <- pca_model$x[, seq_len(npc), drop=FALSE]
        pca_info <- list(n_components=npc, cumulative_variance=cum_var[npc])
      } else {
        incProgress(0.50, detail="Using active variables directly")
      }

      incProgress(0.65, detail="Running k-means")
      set.seed(123)
      km <- kmeans(cluster_input, centers=ncluster_value, nstart=10, iter.max=100, algorithm="Lloyd")

      result_df <- bind_cols(coords, cluster_id=as.character(km$cluster))
      result_df$cluster_label <- paste0("Cluster ", result_df$cluster_id)
      result_df <- maybe_invert_df(result_df, isTRUE(input$invert_cluster_coords))

      box_wide <- as.data.frame(cluster_data_norm)
      box_long <- bind_cols(result_df, box_wide) %>%
        pivot_longer(cols=all_of(elems), names_to="Element", values_to="Value")

      aspect_ratio <- compute_aspect_ratio(nrow(mats[[1]]), ncol(mats[[1]]), isTRUE(input$invert_cluster_coords))
      ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
      entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
      incProgress(0.80, detail="Calculating entropy and summary statistics")
      stats_parts <- build_stats_from_filtered(result_df, box_wide, elems, selected_cols,
                       entropy_rank_percent=if (ambiguity_enabled) entropy_rank_percent else NULL)
      stats_parts$summary_table <- apply_original_percentage_to_summary(stats_parts$summary_table, result_df)

      incProgress(0.95, detail="Finalizing cluster outputs")
      n_act <- length(stats_parts$active_ids)
      list(
        result_df=result_df, box_wide=box_wide, box_long=box_long,
        summary_table=stats_parts$summary_table, ambiguity_df=stats_parts$ambiguity_df,
        entropy_rank_percent=if (ambiguity_enabled) entropy_rank_percent else NA_real_,
        ambiguity_enabled=ambiguity_enabled, aspect_ratio=aspect_ratio, pca_info=pca_info,
        k=input$ncluster, facet_ncol=ceiling(sqrt(max(1,n_act))),
        facet_nrow=ceiling(max(1,n_act)/max(1,ceiling(sqrt(max(1,n_act))))),
        cluster_colors=selected_cols, cluster_colors_named=stats_parts$cluster_colors_named,
        active_ids=stats_parts$active_ids, label_map=stats_parts$label_map, elems=elems,
        filtered_n=stats_parts$filtered_n, total_n=stats_parts$total_n,
        stats_box_long=stats_parts$stats_box_long,
        map_width=active_input_core()$map_width, map_height=active_input_core()$map_height,
        invert_coords=isTRUE(input$invert_cluster_coords),
        pc_scores={
          pc_base <- result_df %>% dplyr::select(x,y,cluster_id,cluster_label)
          if (ncol(pc_scores_full) > 0) dplyr::bind_cols(pc_base, pc_scores_full, box_wide)
          else dplyr::bind_cols(pc_base, box_wide)
        }
      )
    })
  }, ignoreInit=TRUE)

  observeEvent(cluster_results(), {
    cluster_post_state(cluster_results())
    dat <- cluster_results()
    if (!is.null(dat) && !is.null(dat$label_map))
      cluster_visible_state(sort(unname(dat$label_map$cluster_label)))
    session$sendCustomMessage("veToggleClusterPanels", list(settings_open=FALSE, edit_open=TRUE, post_open=TRUE))
    updateTabsetPanel(session, "cluster_preview_tabs", selected="Map")
  })

  finalize_cluster_post_state <- function(res, box_wide, colors, dat, ambiguity_enabled, entropy_rank_percent,
                                          erosion_enabled=FALSE, erosion_pixels=1) {
    active_ids <- unique(as.character(res$cluster_id))
    active_ids <- active_ids[order(suppressWarnings(as.numeric(active_ids)), active_ids)]
    full_ambiguity_df <- compute_cluster_ambiguity(res, box_wide, dat$elems)
    stats_res <- res; stats_box <- box_wide; erosion_applied <- FALSE
    if (isTRUE(erosion_enabled)) {
      eroded <- apply_cluster_border_erosion(res, box_wide, erosion_pixels %||% 1)
      validate(need(nrow(eroded$result_df) > 0, "Border erosion removed all pixels. Reduce the erosion distance."))
      stats_res <- eroded$result_df; stats_box <- eroded$box_wide; erosion_applied <- TRUE
    }
    stats_parts <- build_stats_from_filtered(stats_res, stats_box, dat$elems, colors,
                     entropy_rank_percent=if (ambiguity_enabled) entropy_rank_percent else NULL)
    stats_parts$summary_table <- apply_original_percentage_to_summary(stats_parts$summary_table, res)
    n_act <- length(stats_parts$active_ids)
    out <- list(
      result_df=res, box_wide=box_wide,
      box_long=dplyr::bind_cols(res, box_wide) %>% tidyr::pivot_longer(cols=dplyr::all_of(dat$elems), names_to="Element", values_to="Value"),
      summary_table=stats_parts$summary_table, ambiguity_df=full_ambiguity_df,
      entropy_rank_percent=if (ambiguity_enabled) entropy_rank_percent else NA_real_,
      ambiguity_enabled=isTRUE(ambiguity_enabled),
      erosion_enabled=isTRUE(erosion_enabled),
      erosion_pixels=if (isTRUE(erosion_enabled)) suppressWarnings(as.integer(erosion_pixels %||% 1)) else NA_integer_,
      erosion_applied=erosion_applied, aspect_ratio=dat$aspect_ratio %||% 1, pca_info=dat$pca_info,
      k=length(active_ids), facet_ncol=ceiling(sqrt(max(1,n_act))),
      facet_nrow=ceiling(max(1,n_act)/max(1,ceiling(sqrt(max(1,n_act))))),
      cluster_colors=colors, cluster_colors_named=stats_parts$cluster_colors_named,
      active_ids=stats_parts$active_ids, label_map=stats_parts$label_map, elems=dat$elems,
      filtered_n=stats_parts$filtered_n, total_n=stats_parts$total_n,
      stats_box_long=stats_parts$stats_box_long,
      map_width=dat$map_width, map_height=dat$map_height,
      invert_coords=isTRUE(dat$invert_coords),
      pc_scores={
        pcs <- dat$pc_scores
        if (!is.null(pcs) && nrow(pcs) > 0) {
          pcs <- pcs %>% dplyr::select(-cluster_label) %>%
            dplyr::semi_join(res %>% dplyr::select(x,y,cluster_id), by=c("x","y","cluster_id")) %>%
            dplyr::left_join(res %>% dplyr::select(x,y,cluster_id,cluster_label), by=c("x","y","cluster_id"))
        }
        pcs
      }
    )
    cluster_post_state(out)
    if (!is.null(out$label_map)) cluster_visible_state(sort(unname(out$label_map$cluster_label)))
    merge_sources_state(character(0)); merge_target_state("")
    out
  }

  observeEvent(c(input$enable_ambiguity_filter, input$entropy_rank_percent), {
    dat <- cluster_post_state(); req(dat)
    ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
    entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
    finalize_cluster_post_state(dat$result_df, dat$box_wide, dat$cluster_colors, dat,
      ambiguity_enabled, entropy_rank_percent,
      erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 1)
  }, ignoreInit=TRUE)

  observeEvent(input$cluster_visible_labels, {
    cluster_visible_state(input$cluster_visible_labels %||% character(0))
  }, ignoreInit=TRUE)

  observeEvent(input$reset_cluster_post, {
    req(cluster_results()); dat <- cluster_results()
    cluster_post_state(dat)
    if (!is.null(dat$label_map)) cluster_visible_state(sort(unname(dat$label_map$cluster_label)))
    merge_sources_state(character(0)); merge_target_state("")
    updateSelectInput(session, "merge_source_clusters", selected=character(0))
    if (!is.null(input$merge_target_cluster)) updateSelectInput(session, "merge_target_cluster", selected=character(0))
    updateSelectInput(session, "delete_clusters", selected=character(0))
  })

  observeEvent(input$reset_cluster_rename, {
    dat <- cluster_post_state(); req(dat)
    for (cid in dat$active_ids) {
      current_label <- dat$label_map$cluster_label[match(cid, dat$label_map$cluster_id)]
      current_col <- dat$cluster_colors[as.character(cid)] %||% "#999999"
      updateTextInput(session, paste0("cluster_alias_",cid), value=current_label)
      colourpicker::updateColourInput(session, paste0("post_cluster_col_",cid), value=unname(toupper(current_col)))
    }
  })

  .apply_alias <- function(res, dat) {
    for (cid in unique(as.character(res$cluster_id))) {
      alias_val <- trimws(input[[paste0("cluster_alias_",cid)]] %||% "")
      if (!nzchar(alias_val)) alias_val <- paste0("Cluster ",cid)
      res$cluster_label[res$cluster_id == cid] <- alias_val
    }
    res
  }

  observeEvent(input$apply_cluster_rename, {
    dat <- cluster_post_state(); req(dat)
    withProgress(message="Applying cluster labels and colors", value=0, {
      incProgress(0.35, detail="Updating cluster names and map colors")
      res <- .apply_alias(dat$result_df, dat)
      colors <- dat$cluster_colors
      for (cid in dat$active_ids) {
        col_val <- input[[paste0("post_cluster_col_", cid)]] %||% colors[as.character(cid)] %||% "#999999"
        colors[as.character(cid)] <- toupper(col_val)
      }
      label_map <- dat$label_map
      for (cid in dat$active_ids) {
        new_lab <- unique(res$cluster_label[res$cluster_id == cid])[1]
        label_map$cluster_label[label_map$cluster_id == cid] <- new_lab
      }
      summary_table <- dat$summary_table
      if (!is.null(summary_table) && nrow(summary_table) > 0 && "Cluster" %in% names(summary_table)) {
        for (cid in dat$active_ids) {
          old_lab <- dat$label_map$cluster_label[match(cid, dat$label_map$cluster_id)]
          new_lab <- label_map$cluster_label[match(cid, label_map$cluster_id)]
          summary_table$Cluster[summary_table$Cluster == old_lab] <- new_lab
        }
      }
      amb_df <- dat$ambiguity_df
      if (!is.null(amb_df) && nrow(amb_df) > 0 && "cluster_label" %in% names(amb_df)) {
        amb_df <- amb_df %>% dplyr::select(-cluster_label) %>%
          dplyr::left_join(label_map, by = "cluster_id")
      }
      stats_box_long <- dat$stats_box_long
      if (!is.null(stats_box_long) && nrow(stats_box_long) > 0 && "cluster_label" %in% names(stats_box_long)) {
        stats_box_long <- stats_box_long %>% dplyr::select(-cluster_label) %>%
          dplyr::left_join(label_map, by = "cluster_id")
      }
      pc_scores <- dat$pc_scores
      if (!is.null(pc_scores) && nrow(pc_scores) > 0 && "cluster_label" %in% names(pc_scores)) {
        pc_scores <- pc_scores %>% dplyr::select(-cluster_label) %>%
          dplyr::left_join(label_map, by = "cluster_id")
      }
      cluster_colors_named <- colors[as.character(label_map$cluster_id)]
      names(cluster_colors_named) <- label_map$cluster_label
      out <- dat
      out$result_df <- res
      out$label_map <- label_map
      out$summary_table <- summary_table
      out$ambiguity_df <- amb_df
      out$stats_box_long <- stats_box_long
      out$pc_scores <- pc_scores
      out$cluster_colors <- colors
      out$cluster_colors_named <- cluster_colors_named
      cluster_post_state(out)
      cluster_visible_state(sort(unname(label_map$cluster_label)))
      incProgress(1.0, detail="Done")
    })
  })

  observeEvent(input$merge_source_clusters, {
    vals <- input$merge_source_clusters %||% character(0)
    merge_sources_state(as.character(vals))
    current_target <- merge_target_state()
    if (nzchar(current_target) && !(current_target %in% vals)) merge_target_state("")
  }, ignoreInit=TRUE)

  observeEvent(input$merge_target_cluster, {
    merge_target_state(as.character(input$merge_target_cluster %||% ""))
  }, ignoreInit=TRUE)

  output$cluster_edit_clusters_ui <- renderUI({
    dat <- cluster_post_state()
    if (is.null(dat)) return(tags$p("Generate a cluster map first to enable cluster editing controls."))
    tagList(
      lapply(seq_along(dat$active_ids), function(i) {
        cid <- dat$active_ids[i]
        current_label <- dat$label_map$cluster_label[match(cid, dat$label_map$cluster_id)]
        current_col <- dat$cluster_colors[as.character(cid)] %||% cluster_palette(length(dat$active_ids), "Okabe-Ito")[i]
        tags$div(class = "ve-post-cluster-row",
          tags$div(class = "ve-post-cluster-name",
            textInput(paste0("cluster_alias_", cid), label = paste0("Cluster ", cid), value = current_label, width = "500px")
          ),
          tags$div(class = "ve-post-cluster-picker",
            colourpicker::colourInput(
              paste0("post_cluster_col_", cid),
              NULL,
              value = unname(toupper(current_col)),
              showColour = "background",
              allowTransparent = FALSE,
              width = "40px"
            )
          )
        )
      }),
      tags$div(class="ve-post-button-row",
        actionButton("apply_cluster_rename","Apply"),
        actionButton("reset_cluster_rename","Reset")
      )
    )
  })

  output$cluster_postprocess_ui <- renderUI({
    dat <- cluster_post_state()
    if (is.null(dat)) return(tags$p("Generate a cluster map first to enable post-processing controls."))
    cluster_choices_vec <- stats::setNames(dat$active_ids,
      dat$label_map$cluster_label[match(dat$active_ids, dat$label_map$cluster_id)])
    cluster_choices_vec <- cluster_choices_vec[order(names(cluster_choices_vec))]
    cluster_choices <- as.list(cluster_choices_vec)
    selected_sources <- intersect(as.character(merge_sources_state()), unname(cluster_choices_vec))
    target_choices_vec <- cluster_choices_vec[cluster_choices_vec %in% selected_sources]
    target_choices <- as.list(target_choices_vec)
    current_target <- merge_target_state() %||% ""
    selected_target <- if (nzchar(current_target) && current_target %in% unname(target_choices_vec)) current_target else ""
    tagList(
      tags$div(class="ve-post-control-500",
        selectInput("merge_source_clusters","Cluster Merger: merge these clusters",
          choices=cluster_choices, selected=selected_sources, multiple=TRUE)
      ),
      if (length(target_choices) > 0)
        tags$div(class="ve-post-control-500",
          selectInput("merge_target_cluster","Merge target cluster",choices=target_choices,selected=selected_target)
        ),
      tags$div(class="ve-post-button-row",
        actionButton("apply_cluster_merge","Apply"),
        actionButton("reset_cluster_merge","Reset")
      ),
      tags$hr(),
      tags$div(class="ve-post-control-500",
        selectInput("delete_clusters","Delete Cluster",choices=cluster_choices,selected=character(0),multiple=TRUE)
      ),
      tags$div(class="ve-post-button-row",
        actionButton("apply_cluster_delete","Apply"),
        actionButton("reset_cluster_delete","Reset")
      ),
      tags$hr(),
      checkboxInput("enable_cluster_erosion","Erode cluster borders only for summary statistics",value=isTRUE(dat$erosion_enabled)),
      tags$div(class="ve-post-control-500",
        numericInput("cluster_erosion_pixels","Border erosion distance (pixels), statistics only",value=dat$erosion_pixels %||% 1,min=1,step=1,width="500px")
      ),
      tags$p("Erosion masks border pixels only when recalculating means and summary statistics; it does not change the displayed cluster map.",class="ve-note"),
      tags$div(class="ve-post-button-row",
        actionButton("apply_cluster_erosion","Apply"),
        actionButton("reset_cluster_erosion","Reset")
      ),
      tags$hr(),
      tags$div(class="ve-post-control-500",
        selectInput("cluster_mode_window","Moving mode filter window",choices=c("3 x 3"=3,"5 x 5"=5,"8 x 8"=8),selected=3)
      ),
      tags$p("The moving mode filter replaces isolated pixels by the local modal cluster class, reducing salt-and-pepper texture.",class="ve-note"),
      tags$div(class="ve-post-button-row",
        actionButton("apply_cluster_mode_filter","Apply"),
        actionButton("reset_cluster_mode_filter","Reset")
      )
    )
  })

  observeEvent(input$reset_cluster_merge, {
    merge_sources_state(character(0)); merge_target_state("")
    updateSelectInput(session,"merge_source_clusters",selected=character(0))
    if (!is.null(input$merge_target_cluster)) updateSelectInput(session,"merge_target_cluster",selected=character(0))
  })

  observeEvent(input$apply_cluster_merge, {
    dat <- cluster_post_state(); req(dat)
    res <- dat$result_df; box_wide <- dat$box_wide; colors <- dat$cluster_colors
    merge_sources <- merge_sources_state() %||% character(0)
    merge_target <- merge_target_state() %||% ""
    if (length(merge_sources) > 0 && nzchar(merge_target)) {
      merge_sources <- setdiff(as.character(merge_sources), as.character(merge_target))
      if (length(merge_sources) > 0) {
        res$cluster_id[res$cluster_id %in% merge_sources] <- as.character(merge_target)
        colors <- colors[setdiff(names(colors), merge_sources)]
      }
    }
    res <- .apply_alias(res, dat)
    ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
    entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
    finalize_cluster_post_state(res, box_wide, colors, dat, ambiguity_enabled, entropy_rank_percent,
      erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 1)
    merge_sources_state(character(0)); merge_target_state("")
    updateSelectInput(session,"merge_source_clusters",selected=character(0))
    if (!is.null(input$merge_target_cluster)) updateSelectInput(session,"merge_target_cluster",selected=character(0))
  })

  observeEvent(input$reset_cluster_delete, {
    updateSelectInput(session,"delete_clusters",selected=character(0))
  })

  observeEvent(input$apply_cluster_delete, {
    dat <- cluster_post_state(); req(dat)
    res <- dat$result_df; box_wide <- dat$box_wide; colors <- dat$cluster_colors
    delete_ids <- input$delete_clusters %||% character(0)
    if (length(delete_ids) > 0) {
      keep_idx <- !(res$cluster_id %in% delete_ids)
      res <- res[keep_idx,,drop=FALSE]; box_wide <- box_wide[keep_idx,,drop=FALSE]
      colors <- colors[setdiff(names(colors), delete_ids)]
    }
    validate(need(length(unique(as.character(res$cluster_id))) > 0,
                  "At least one cluster must remain after post-processing."))
    res <- .apply_alias(res, dat)
    ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
    entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
    finalize_cluster_post_state(res, box_wide, colors, dat, ambiguity_enabled, entropy_rank_percent,
      erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 1)
    updateSelectInput(session,"delete_clusters",selected=character(0))
  })

  observeEvent(input$reset_cluster_erosion, {
    dat <- cluster_post_state()
    updateCheckboxInput(session,"enable_cluster_erosion",value=FALSE)
    updateNumericInput(session,"cluster_erosion_pixels",value=1)
    if (!is.null(dat)) {
      withProgress(message="Recalculating cluster statistics", value=0, {
        incProgress(0.35, detail="Removing the statistics-only erosion mask")
        ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
        entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
        incProgress(0.70, detail="Updating summary table")
        finalize_cluster_post_state(dat$result_df, dat$box_wide, dat$cluster_colors, dat,
          ambiguity_enabled, entropy_rank_percent, erosion_enabled=FALSE, erosion_pixels=1)
      })
    }
  })

  observeEvent(input$apply_cluster_erosion, {
    dat <- cluster_post_state(); req(dat)
    withProgress(message="Recalculating cluster statistics", value=0, {
      incProgress(0.25, detail="Preparing statistics-only border erosion")
      ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
      entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
      incProgress(0.60, detail="Applying erosion mask to means and summary table")
      finalize_cluster_post_state(dat$result_df, dat$box_wide, dat$cluster_colors, dat,
        ambiguity_enabled, entropy_rank_percent,
        erosion_enabled=isTRUE(input$enable_cluster_erosion),
        erosion_pixels=input$cluster_erosion_pixels %||% 1)
    })
  })

  observeEvent(input$reset_cluster_mode_filter, {
    updateSelectInput(session,"cluster_mode_window",selected=3)
  })

  observeEvent(input$apply_cluster_mode_filter, {
    dat <- cluster_post_state(); req(dat)
    withProgress(message="Applying moving mode filter", value=0, {
      incProgress(0.25, detail="Scanning neighborhood windows")
      res <- apply_cluster_mode_filter(dat$result_df, input$cluster_mode_window %||% 3)
      incProgress(0.70, detail="Recalculating entropy and summary statistics")
      ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
      entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
      finalize_cluster_post_state(res, dat$box_wide, dat$cluster_colors, dat, ambiguity_enabled, entropy_rank_percent,
        erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 1)
    })
  })

  output$plot_cluster <- renderPlot({
    dat <- cluster_post_state(); req(dat)
    visible_labels <- cluster_visible_state()
    if (length(visible_labels) == 0) visible_labels <- unname(dat$label_map$cluster_label)
    plot_df <- dat$result_df %>% filter(cluster_label %in% visible_labels)
    validate(need(nrow(plot_df) > 0, "Select at least one cluster to display in the map."))
    make_cluster_entropy_plot(df=plot_df, fill_var="cluster_label",
      title=paste0("Pseudo-Minerals Map (", dat$k, " phases)"),
      aspect_ratio=dat$aspect_ratio %||% 1,
      fill_scale=scale_fill_manual(values=dat$cluster_colors_named[names(dat$cluster_colors_named) %in% unique(plot_df$cluster_label)],
                                   drop=FALSE, name="Cluster", guide=guide_legend(nrow=ceiling(length(unique(plot_df$cluster_label)) / 4), byrow=TRUE)),
      map_width_mm=dat$map_width, map_height_mm=dat$map_height,
      invert_coords=isTRUE(dat$invert_coords))
  })

  output$plot_cluster_ambiguity <- renderPlot({
    dat <- cluster_post_state(); req(dat)
    amb_df <- dat$ambiguity_df
    visible_labels <- cluster_visible_state()
    if (length(visible_labels) == 0) visible_labels <- unname(dat$label_map$cluster_label)
    amb_df <- amb_df %>% filter(cluster_label %in% visible_labels); req(nrow(amb_df) > 0)
    make_cluster_entropy_plot(df=amb_df, fill_var="ambiguity",
      title="Entropy Map",
      aspect_ratio=dat$aspect_ratio %||% 1,
      fill_scale=scale_fill_viridis_c(option="magma",
                   limits=c(0,max(0.01,max(amb_df$ambiguity,na.rm=TRUE))), name="Entropy",
                   guide=guide_colorbar(direction="horizontal", title.position="top", barwidth=12, barheight=0.6)),
      map_width_mm=dat$map_width, map_height_mm=dat$map_height,
      invert_coords=isTRUE(dat$invert_coords))
  })

  output$cluster_auto_detect_plot_ui <- renderUI({
    auto <- cluster_auto_detect_state()
    if (is.null(auto) || is.null(auto$table) || nrow(auto$table) == 0) return(NULL)
    div(class="ve-box",
      h4(tags$b("Auto Detect diagnostics"), align="center"),
      tags$p(sprintf("Optimum selected: %s phases using the composite score. Diagnostic calculated from %s randomly sampled pixels (%s%% of valid pixels; cap: %s; stability runs per k: %s).",
                     auto$optimum_k,
                     format(auto$sample_size, big.mark=","),
                     auto$sample_percent %||% "",
                     format(auto$sample_cap %||% 500000L, big.mark=","),
                     auto$runs %||% 5), class="ve-note"),
      withSpinner(plotOutput("cluster_auto_detect_plot", width="100%", height="420px"), type=1, color="black"),
      tableOutput("cluster_auto_detect_table")
    )
  })

  output$cluster_auto_detect_plot <- renderPlot({
    auto <- cluster_auto_detect_state(); req(auto, auto$table)
    diag_df <- auto$table
    plot_df <- dplyr::bind_rows(
      data.frame(k = diag_df$k, Metric = "BCI score", RelativeValue = diag_df$BCI_score),
      data.frame(k = diag_df$k, Metric = "Purity score", RelativeValue = diag_df$Purity_score),
      data.frame(k = diag_df$k, Metric = "Stability score", RelativeValue = diag_df$Stability_score),
      data.frame(k = diag_df$k, Metric = "Composite score", RelativeValue = diag_df$Composite_relative)
    )
    ggplot(plot_df, aes(x = k, y = RelativeValue, group = Metric, linetype = Metric, shape = Metric)) +
      geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
      geom_vline(xintercept = auto$optimum_k, linetype = "dashed") +
      scale_x_continuous(breaks = diag_df$k) +
      scale_y_continuous(limits = c(0, 1), name = "Score value") +
      labs(x = "Number of phases", title = "Auto Detect composite score across tested phase numbers") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "top", panel.grid.minor = element_blank())
  })

  output$cluster_auto_detect_table <- renderTable({
    auto <- cluster_auto_detect_state(); req(auto, auto$table)
    out <- auto$table %>%
      dplyr::select(-dplyr::any_of(c("SampledPixels", "PureFraction", "PCAComponents", "Runs",
                                     "SuccessfulRuns", "Stability_score", "Parsimony_score",
                                     "CompositeScore"))) %>%
      dplyr::mutate(
        BCI = round(BCI, 2),
        WeightedEntropy = round(WeightedEntropy, 4),
        Stability = round(Stability, 3),
        BCI_score = round(BCI_score, 3),
        Purity_score = round(Purity_score, 3),
        Composite_relative = round(Composite_relative, 3)
      )
    out
  })

  # ---------------------------------------------------------------------------
  # Cluster Summary Table
  # ---------------------------------------------------------------------------
  format_cluster_summary_for_display <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    out <- data.frame(Cluster=df$Cluster, Percentage=round(df$Percentage,2), stringsAsFactors=FALSE)
    if ("Entropy" %in% names(df)) {
      ent_sd <- if ("Entropy_SD" %in% names(df)) df$Entropy_SD else NA_real_
      out$Entropy <- ifelse(is.na(df$Entropy), "",
                            paste0(sprintf("%.2f",df$Entropy)," \u00b1 ",sprintf("%.2f",ent_sd)))
    }
    elem_cols <- setdiff(names(df), c("Cluster","Percentage","Entropy","Entropy_SD",grep("_SD$",names(df),value=TRUE)))
    for (el in elem_cols) {
      sdcol <- paste0(el,"_SD"); sdx <- if (sdcol %in% names(df)) df[[sdcol]] else NA_real_
      out[[el]] <- ifelse(is.na(df[[el]]), "",
                          paste0(sprintf("%.2f",df[[el]])," \u00b1 ",sprintf("%.2f",sdx)))
    }
    out
  }

  output$table_cluster_ui <- renderUI({
    req(cluster_post_state()); dat <- cluster_post_state()
    tagList(
      tags$h5("Cluster Summary Table", style="font-weight:bold;text-align:left;"),
      tags$p(if (isTRUE(dat$ambiguity_enabled))
               paste0("Statistics currently use ",dat$filtered_n," of ",dat$total_n,
                      " pixels after retaining the selected lowest-entropy rank within each cluster.")
             else paste0("Statistics currently use all ",dat$total_n," pixels."), class="ve-note"),
      tableOutput("table_cluster")
    )
  })

  output$table_cluster <- renderTable({
    req(cluster_post_state())
    format_cluster_summary_for_display(cluster_post_state()$summary_table)
  }, sanitize.text.function=function(x) x)

  output$download_cluster_table <- downloadHandler(
    filename=function() "cluster_table.csv",
    content=function(file) write.csv2(cluster_post_state()$summary_table, file, row.names=FALSE)
  )

  output$download_cluster_map <- downloadHandler(
    filename=function() {
      ext <- if (identical(input$cluster_download_format %||% "pdf", "tiff")) "tif" else "pdf"
      paste0("Pseudo_Minerals_Map.", ext)
    },
    content=function(file) {
      dat <- cluster_post_state(); req(dat)
      visible_labels <- cluster_visible_state()
      if (length(visible_labels) == 0) visible_labels <- unname(dat$label_map$cluster_label)
      plot_df <- dat$result_df %>% filter(cluster_label %in% visible_labels)
      amb_df  <- dat$ambiguity_df %>% filter(cluster_label %in% visible_labels)
      p1 <- make_cluster_entropy_plot(df=plot_df, fill_var="cluster_label",
        title=paste0("Pseudo-Minerals Map (", dat$k, " phases)"),
        aspect_ratio=dat$aspect_ratio %||% 1,
        fill_scale=scale_fill_manual(values=dat$cluster_colors_named[names(dat$cluster_colors_named) %in% unique(plot_df$cluster_label)], drop=FALSE,
          name="Cluster", guide=guide_legend(nrow=ceiling(length(unique(plot_df$cluster_label)) / 4), byrow=TRUE)),
        map_width_mm=dat$map_width, map_height_mm=dat$map_height,
        invert_coords=isTRUE(dat$invert_coords))
      p2 <- make_cluster_entropy_plot(df=amb_df, fill_var="ambiguity",
        title="Entropy Map", aspect_ratio=dat$aspect_ratio %||% 1,
        fill_scale=scale_fill_viridis_c(option="magma", limits=c(0,max(0.01,max(amb_df$ambiguity,na.rm=TRUE))),
          name="Entropy", guide=guide_colorbar(direction="horizontal", title.position="top", barwidth=12, barheight=0.6)),
        map_width_mm=dat$map_width, map_height_mm=dat$map_height,
        invert_coords=isTRUE(dat$invert_coords))
      if (identical(input$cluster_download_format %||% "pdf", "tiff")) {
        grDevices::tiff(filename=file, width=10, height=14, units="in", res=300, compression="lzw", bg="white")
        on.exit(grDevices::dev.off(), add=TRUE)
        print(p1 / p2)
      } else {
        summary_for_pdf <- format_cluster_summary_for_display(dat$summary_table)
        grDevices::cairo_pdf(file=file,width=10,height=8)
        on.exit(grDevices::dev.off(), add=TRUE)
        print(p1)
        print(p2)
        draw_report_table_pages("Cluster Summary Table", summary_for_pdf,
                                subtitle="Mean ± standard deviation, as displayed in the Cluster Analysis tab.",
                                rows_per_page=24, digits=2)
      }
      invisible(NULL)
    }
  )

  # ---------------------------------------------------------------------------
  # Data Analysis
  # ---------------------------------------------------------------------------

  data_analysis_sidebar_collapsed <- reactiveVal(FALSE)
  data_analysis_selected_tab <- reactiveVal("start")
  data_analysis_visible <- reactiveValues(scatter = FALSE, ternary = FALSE, cluster = FALSE, correlogram = FALSE)

  observeEvent(input$toggle_data_analysis_sidebar, {
    new_state <- !isTRUE(data_analysis_sidebar_collapsed())
    data_analysis_sidebar_collapsed(new_state)
    updateActionButton(session, "toggle_data_analysis_sidebar", label = if (isTRUE(new_state)) "▶" else "◀")
    session$sendCustomMessage("veToggleAnalysisSidebar", list(collapsed = new_state))
  }, ignoreInit = TRUE)

  correlogram_saved_plots <- reactiveVal(list())

  output$data_analysis_preview_ui <- renderUI({
    tabs <- list()
    values <- character(0)
    if (isTRUE(data_analysis_visible$scatter)) {
      tabs[[length(tabs) + 1L]] <- tabPanel(
        title = tags$span(class="ve-tab-title-with-remove",
          tags$span("Scatter"),
          actionButton("close_scatter_plot", "×", title="Remove", class="ve-remove-analysis-btn")
        ),
        value = "scatter",
        withSpinner(plotlyOutput("plot_cluster_pc_scatter", width="100%", height="72vh"), type=1, color="black")
      )
      values <- c(values, "scatter")
    }
    if (isTRUE(data_analysis_visible$ternary)) {
      tabs[[length(tabs) + 1L]] <- tabPanel(
        title = tags$span(class="ve-tab-title-with-remove",
          tags$span("Ternary"),
          actionButton("close_ternary_plot", "×", title="Remove", class="ve-remove-analysis-btn")
        ),
        value = "ternary",
        withSpinner(plotlyOutput("plot_cluster_pc_ternary", width="100%", height="72vh"), type=1, color="black")
      )
      values <- c(values, "ternary")
    }
    if (isTRUE(data_analysis_visible$cluster)) {
      tabs[[length(tabs) + 1L]] <- tabPanel(
        title = tags$span(class="ve-tab-title-with-remove",
          tags$span("Cluster View"),
          actionButton("close_cluster_view_plot", "×", title="Remove", class="ve-remove-analysis-btn")
        ),
        value = "cluster",
        withSpinner(plotlyOutput("plot_cluster_view", width="100%", height="78vh"), type=1, color="black")
      )
      values <- c(values, "cluster")
    }
    corr_saved <- tryCatch(correlogram_saved_plots(), error = function(e) list())
    if (length(corr_saved) > 0) {
      for (id in names(corr_saved)) {
        item <- corr_saved[[id]]
        tabs[[length(tabs) + 1L]] <- tabPanel(
          title = tags$span(class="ve-tab-title-with-remove",
            tags$span(item$label %||% id),
            actionButton(paste0("close_correlogram_", id), "×", title="Remove", class="ve-remove-analysis-btn")
          ),
          value = id,
          withSpinner(plotOutput(paste0("plot_correlogram_", id), width="100%", height="80vh"), type=1, color="black")
        )
        values <- c(values, id)
      }
    }
    if (length(tabs) == 0) {
      tabs[[1]] <- tabPanel("Start", value="start", tags$p("Choose analysis settings on the left and generate a plot to display it here.", class="ve-note"))
      values <- "start"
    }
    selected <- data_analysis_selected_tab()
    if (!selected %in% values) selected <- values[1]
    do.call(tabsetPanel, c(list(id="data_analysis_preview_tabs", type="pills", selected=selected), tabs))
  })

  observe({
    saved <- tryCatch(correlogram_saved_plots(), error = function(e) list())
    if (length(saved) == 0) return(NULL)
    for (id in names(saved)) local({
      corr_id <- id
      output[[paste0("plot_correlogram_", corr_id)]] <- renderPlot({
        item <- correlogram_saved_plots()[[corr_id]]
        req(item)
        render_correlogram_with_title(item$matrix, item$title, orientation = item$orientation %||% "vertical")
      })
    })
  })

  observe({
    saved <- tryCatch(correlogram_saved_plots(), error = function(e) list())
    if (length(saved) == 0) return(NULL)
    for (id in names(saved)) local({
      corr_id <- id
      observeEvent(input[[paste0("close_correlogram_", corr_id)]], {
        current <- correlogram_saved_plots()
        current[[corr_id]] <- NULL
        correlogram_saved_plots(current)
        if (length(current) > 0) {
          next_id <- names(current)[[length(current)]]
          data_analysis_selected_tab(next_id)
          session$onFlushed(function() updateTabsetPanel(session, "data_analysis_preview_tabs", selected = next_id), once = TRUE)
        } else {
          data_analysis_visible$correlogram <- FALSE
          data_analysis_selected_tab("start")
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  observeEvent(input$generate_scatter_plot, {
    data_analysis_visible$scatter <- TRUE
    data_analysis_selected_tab("scatter")
  }, ignoreInit = TRUE, priority = -10)
  observeEvent(input$generate_ternary_plot, {
    data_analysis_visible$ternary <- TRUE
    data_analysis_selected_tab("ternary")
  }, ignoreInit = TRUE, priority = -10)
  observeEvent(input$generate_cluster_view_plot, {
    data_analysis_visible$cluster <- TRUE
    data_analysis_selected_tab("cluster")
  }, ignoreInit = TRUE, priority = -10)
  observeEvent(input$close_scatter_plot, { data_analysis_visible$scatter <- FALSE; data_analysis_selected_tab("start") }, ignoreInit = TRUE)
  observeEvent(input$close_ternary_plot, { data_analysis_visible$ternary <- FALSE; data_analysis_selected_tab("start") }, ignoreInit = TRUE)
  observeEvent(input$close_cluster_view_plot, { data_analysis_visible$cluster <- FALSE; data_analysis_selected_tab("start") }, ignoreInit = TRUE)
  output$cluster_pc_scatter_controls_ui <- renderUI({
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    axis_choices <- analysis_axis_choices(dat, core)
    validate(need(length(axis_choices) >= 2, "At least two variables/PCs are required for the scatter plot."))

    pc_names <- analysis_pc_names(dat)
    x_default <- if ("PC1" %in% axis_choices) "PC1" else axis_choices[1]
    y_default <- if ("PC2" %in% axis_choices) "PC2" else axis_choices[min(2, length(axis_choices))]
    xsel <- input$cluster_pc_x %||% x_default
    xsel <- if (xsel %in% axis_choices) xsel else x_default
    ysel <- input$cluster_pc_y %||% y_default
    ysel <- if (ysel %in% axis_choices) ysel else y_default

    color_choices <- core$elements
    color_sel <- input$cluster_pc_color_variable %||% color_choices[1]
    color_sel <- if (color_sel %in% color_choices) color_sel else color_choices[1]

    tagList(
      fluidRow(
        column(4, selectInput("cluster_pc_color_mode", "Color data by",
          choices = c("Input variables" = "variable", "Clusters" = "cluster", "Entropy" = "entropy"),
          selected = input$cluster_pc_color_mode %||% "cluster")),
        column(4, selectInput("cluster_pc_x", "X axis", choices = axis_choices, selected = xsel)),
        column(4, selectInput("cluster_pc_y", "Y axis", choices = axis_choices, selected = ysel))
      ),
      if (identical(input$cluster_pc_color_mode %||% "cluster", "variable"))
        selectInput("cluster_pc_color_variable", "Variable", choices = color_choices, selected = color_sel),
      sliderInput("scatter_sample_pct", "Sampled points used in scatter plot (%; maximum 100,000 points):",
        min = 1, max = 100, value = suppressWarnings(as.numeric(input$scatter_sample_pct %||% 1)), step = 1, post = "%"),
      tags$p("Scatter plots use random sampling without replacement. When clusters are available, sampling preserves the original cluster proportions.", class="ve-note"),
      actionButton("generate_scatter_plot", "Generate map")
    )
  })

  cluster_pc_scatter_ggplot <- eventReactive(input$generate_scatter_plot, {
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    plot_df <- build_cluster_analysis_scores(dat, core)
    req(nrow(plot_df) > 0)
    axis_choices <- analysis_axis_choices(dat, core)
    xvar <- input$cluster_pc_x %||% if ("PC1" %in% axis_choices) "PC1" else axis_choices[1]
    yvar <- input$cluster_pc_y %||% if ("PC2" %in% axis_choices) "PC2" else axis_choices[min(2, length(axis_choices))]
    validate(need(xvar %in% names(plot_df) && yvar %in% names(plot_df), "Choose valid variables or principal components."))

    samp_pct <- max(1, min(100, suppressWarnings(as.numeric(input$scatter_sample_pct %||% 1))))
    plot_df <- sample_rows_analysis(plot_df, frac = samp_pct / 100, max_n = 100000, cluster_col = "cluster_label")
    color_mode <- input$cluster_pc_color_mode %||% "cluster"

    p <- switch(color_mode,
      cluster = ggplot(plot_df, aes(x = .data[[xvar]], y = .data[[yvar]], color = cluster_label, text = cluster_label)) +
                geom_point(alpha = 0.65, size = 1.5) +
                scale_color_manual(values = dat$cluster_colors_named, drop = FALSE) + labs(color = "Cluster"),
      entropy = {
        plot_df <- plot_df %>% dplyr::left_join(dat$ambiguity_df %>% dplyr::select(x, y, ambiguity), by = c("x", "y"))
        ggplot(plot_df, aes(x = .data[[xvar]], y = .data[[yvar]], color = ambiguity, text = cluster_label)) +
          geom_point(alpha = 0.65, size = 1.5) + scale_color_viridis_c(option = "magma") + labs(color = "Entropy")
      },
      {
        var_name <- input$cluster_pc_color_variable %||% core$elements[1]
        validate(need(var_name %in% names(plot_df), "Choose a valid input variable for coloring."))
        ggplot(plot_df, aes(x = .data[[xvar]], y = .data[[yvar]], color = .data[[var_name]], text = cluster_label)) +
          geom_point(alpha = 0.65, size = 1.5) + scale_color_viridis_c(option = "viridis") + labs(color = var_name)
      }
    )
    p + theme_minimal() + labs(title = "Scatter Plot", x = xvar, y = yvar) +
      theme(legend.position = "right", plot.title = element_text(hjust = 0.5))
  })

  output$plot_cluster_pc_scatter <- renderPlotly({
    validate(need(input$generate_scatter_plot > 0, "Choose variables and click Generate map to draw the scatter plot."))
    plotly::ggplotly(cluster_pc_scatter_ggplot(), tooltip = c("x", "y", "colour", "text"))
  })

  output$download_cluster_pca_plot <- downloadHandler(
    filename = function() "Cluster_PCA_Plot.pdf",
    content = function(file) save_plot_as_raster_pdf(cluster_pc_scatter_ggplot(), file = file, width = 10, height = 7, dpi = 220)
  )

  output$cluster_pc_ternary_controls_ui <- renderUI({
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    axis_choices <- analysis_axis_choices(dat, core)
    validate(need(length(axis_choices) >= 3, "At least three variables/PCs are required for the ternary plot."))

    a_default <- if ("PC1" %in% axis_choices) "PC1" else axis_choices[1]
    b_default <- if ("PC2" %in% axis_choices) "PC2" else axis_choices[min(2, length(axis_choices))]
    c_default <- if ("PC3" %in% axis_choices) "PC3" else axis_choices[min(3, length(axis_choices))]

    a_sel <- input$cluster_ternary_a %||% a_default
    a_sel <- if (a_sel %in% axis_choices) a_sel else a_default
    b_choices <- setdiff(axis_choices, a_sel)
    b_sel <- input$cluster_ternary_b %||% b_default
    b_sel <- if (b_sel %in% b_choices) b_sel else b_choices[1]
    c_choices <- setdiff(axis_choices, c(a_sel, b_sel))
    c_sel <- input$cluster_ternary_c %||% c_default
    c_sel <- if (c_sel %in% c_choices) c_sel else c_choices[1]

    color_choices <- core$elements
    color_sel <- input$cluster_ternary_color_variable %||% color_choices[1]
    color_sel <- if (color_sel %in% color_choices) color_sel else color_choices[1]

    tagList(
      fluidRow(
        column(3, selectInput("cluster_ternary_color_mode", "Color data by",
          choices = c("Input variables" = "variable", "Clusters" = "cluster", "Entropy" = "entropy"),
          selected = input$cluster_ternary_color_mode %||% "cluster")),
        column(3, selectInput("cluster_ternary_a", "Axis A", choices = axis_choices, selected = a_sel)),
        column(3, selectInput("cluster_ternary_b", "Axis B", choices = b_choices, selected = b_sel)),
        column(3, selectInput("cluster_ternary_c", "Axis C", choices = c_choices, selected = c_sel))
      ),
      if (identical(input$cluster_ternary_color_mode %||% "cluster", "variable"))
        selectInput("cluster_ternary_color_variable", "Variable for color", choices = color_choices, selected = color_sel),
      sliderInput("ternary_sample_pct", "Sampled points used in ternary plot (%; maximum 100,000 points):",
        min = 1, max = 100, value = suppressWarnings(as.numeric(input$ternary_sample_pct %||% 1)), step = 1, post = "%"),
      tags$p("Ternary plots use independent random sampling without replacement. When clusters are available, sampling preserves the original cluster proportions.", class="ve-note"),
      actionButton("generate_ternary_plot", "Generate map")
    )
  })

  cluster_pc_ternary_plotly <- eventReactive(input$generate_ternary_plot, {
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    plot_df <- build_cluster_analysis_scores(dat, core)
    req(nrow(plot_df) > 0)
    avar <- input$cluster_ternary_a; bvar <- input$cluster_ternary_b; cvar <- input$cluster_ternary_c
    validate(need(length(unique(c(avar, bvar, cvar))) == 3, "Choose three different variables or principal components."))
    validate(need(all(c(avar, bvar, cvar) %in% names(plot_df)), "Choose valid variables or principal components."))

    vals <- cbind(plot_df[[avar]], plot_df[[bvar]], plot_df[[cvar]])
    vals <- abs(vals); rs <- rowSums(vals, na.rm = TRUE); rs[!is.finite(rs) | rs <= 0] <- 1
    tern <- sweep(vals, 1, rs, "/")
    plot_df$a <- tern[, 1]; plot_df$b <- tern[, 2]; plot_df$c <- tern[, 3]

    samp_pct <- max(1, min(100, suppressWarnings(as.numeric(input$ternary_sample_pct %||% 1))))
    plot_df <- sample_rows_analysis(plot_df, frac = samp_pct / 100, max_n = 100000, cluster_col = "cluster_label")
    color_mode <- input$cluster_ternary_color_mode %||% "cluster"

    plt <- if (identical(color_mode, "cluster")) {
      plotly::plot_ly(plot_df, type = "scatterternary", mode = "markers", a = ~a, b = ~b, c = ~c,
        color = ~cluster_label, colors = unname(dat$cluster_colors_named), text = ~cluster_label,
        hovertemplate = paste0("Cluster: %{text}<br>", avar, ": %{a:.3f}<br>", bvar, ": %{b:.3f}<br>", cvar, ": %{c:.3f}<extra></extra>"))
    } else if (identical(color_mode, "entropy")) {
      plot_df <- plot_df %>% dplyr::left_join(dat$ambiguity_df %>% dplyr::select(x, y, ambiguity), by = c("x", "y"))
      plotly::plot_ly(plot_df, type = "scatterternary", mode = "markers", a = ~a, b = ~b, c = ~c,
        color = ~ambiguity, colors = viridisLite::magma(100), text = ~cluster_label,
        hovertemplate = paste0("Cluster: %{text}<br>Entropy: %{marker.color:.3f}<extra></extra>"))
    } else {
      var_name <- input$cluster_ternary_color_variable %||% core$elements[1]
      validate(need(var_name %in% names(plot_df), "Choose a valid input variable for coloring."))
      plotly::plot_ly(plot_df, type = "scatterternary", mode = "markers", a = ~a, b = ~b, c = ~c,
        color = plot_df[[var_name]], colors = viridisLite::viridis(100), text = ~cluster_label,
        hovertemplate = paste0("Cluster: %{text}<br>", var_name, ": %{marker.color:.3f}<extra></extra>"))
    }
    plt %>% plotly::layout(ternary = list(aaxis = list(title = avar), baxis = list(title = bvar), caxis = list(title = cvar)))
  })

  output$plot_cluster_pc_ternary <- renderPlotly({
    validate(need(input$generate_ternary_plot > 0, "Choose variables and click Generate map to draw the ternary plot."))
    cluster_pc_ternary_plotly()
  })

  output$download_cluster_ternary_plot <- downloadHandler(
    filename = function() "Cluster_Ternary_Plot.html",
    content = function(file) htmlwidgets::saveWidget(as_widget(cluster_pc_ternary_plotly()), file = file, selfcontained = TRUE)
  )

  output$cluster_view_controls_ui <- renderUI({
    dat <- cluster_post_state()
    if (is.null(dat)) return(tags$p("Generate a Cluster Analysis map first to enable the interactive cluster view.", class="ve-note"))
    tagList(
      tags$div(class="ve-cluster-view-controls",
        sliderInput("cluster_view_sample_pct", "Cluster View sample size (% of cluster pixels; maximum 250,000 points):",
          min=1, max=100, value=suppressWarnings(as.numeric(input$cluster_view_sample_pct %||% 50)), step=1, post="%"),
        tags$p("The interactive view uses random sampling without replacement and caps the Plotly object at 250,000 pixels to keep browser rendering responsive.", class="ve-note"),
        actionButton("generate_cluster_view_plot", "Generate interactive map")
      )
    )
  })

  make_plotly_discrete_colorscale <- function(cols) {
    cols <- unname(cols)
    n <- length(cols)
    if (n <= 0) return(list(list(0, "#FFFFFF"), list(1, "#FFFFFF")))
    if (n == 1) return(list(list(0, cols[1]), list(1, cols[1])))
    breaks <- seq(0, 1, length.out = n + 1)
    cs <- vector("list", n * 2L)
    k <- 1L
    for (i in seq_len(n)) {
      cs[[k]] <- list(breaks[i], cols[i]); k <- k + 1L
      cs[[k]] <- list(breaks[i + 1L], cols[i]); k <- k + 1L
    }
    cs
  }

  prepare_cluster_view_tiles <- function(dat, pct = 50, max_cells = 250000L) {
    df <- dat$result_df
    visible_labels <- cluster_visible_state()
    if (length(visible_labels) == 0) visible_labels <- unname(dat$label_map$cluster_label)
    df <- df %>% dplyr::filter(cluster_label %in% visible_labels)
    validate(need(nrow(df) > 0, "Select at least one cluster to display."))

    if (!is.null(dat$ambiguity_df) && nrow(dat$ambiguity_df) > 0) {
      df <- df %>% dplyr::left_join(dat$ambiguity_df %>% dplyr::select(x, y, ambiguity), by = c("x", "y"))
    }

    pct <- max(1, min(100, suppressWarnings(as.numeric(pct %||% 50))))
    target_n <- min(nrow(df), as.integer(max_cells), max(1L, ceiling(nrow(df) * pct / 100)))
    stride <- max(1L, ceiling(sqrt(nrow(df) / max(1L, target_n))))

    dt <- data.table::as.data.table(df)
    x0 <- min(dt$x, na.rm = TRUE)
    y0 <- min(dt$y, na.rm = TRUE)
    dt[, xb := as.integer(floor((x - x0) / stride) + 1L)]
    dt[, yb := as.integer(floor((y - y0) / stride) + 1L)]

    if ("ambiguity" %in% names(dt)) {
      counts <- dt[, .(n_pixels = .N, entropy = mean(ambiguity, na.rm = TRUE)),
                   by = .(xb, yb, cluster_label)]
    } else {
      counts <- dt[, .(n_pixels = .N), by = .(xb, yb, cluster_label)]
      counts[, entropy := NA_real_]
    }
    data.table::setorder(counts, xb, yb, -n_pixels)
    agg <- counts[, .SD[1], by = .(xb, yb)]

    cluster_levels <- unname(dat$label_map$cluster_label)
    cluster_levels <- cluster_levels[cluster_levels %in% unique(as.character(agg$cluster_label))]
    missing_levels <- setdiff(unique(as.character(agg$cluster_label)), cluster_levels)
    cluster_levels <- c(cluster_levels, sort(missing_levels))
    validate(need(length(cluster_levels) > 0, "No clusters are available for the interactive view."))

    agg[, z := match(as.character(cluster_label), cluster_levels)]
    x_bins <- sort(unique(agg$xb))
    y_bins <- sort(unique(agg$yb))
    z <- matrix(NA_real_, nrow = length(y_bins), ncol = length(x_bins), dimnames = list(y_bins, x_bins))
    hover <- matrix("", nrow = length(y_bins), ncol = length(x_bins), dimnames = list(y_bins, x_bins))
    ii <- match(agg$yb, y_bins)
    jj <- match(agg$xb, x_bins)
    z[cbind(ii, jj)] <- agg$z
    entropy_txt <- ifelse(is.finite(agg$entropy), paste0("<br>Mean entropy: ", round(agg$entropy, 3)), "")
    hover[cbind(ii, jj)] <- paste0("Cluster: ", agg$cluster_label,
                                   "<br>Pixels represented: ", agg$n_pixels,
                                   entropy_txt)

    cols_named <- dat$cluster_colors_named
    cols <- unname(cols_named[cluster_levels])
    cols[is.na(cols) | !nzchar(cols)] <- "#999999"

    list(
      x = x_bins, y = y_bins, z = z, hover = hover,
      cluster_levels = cluster_levels, colors = cols,
      pct = pct, stride = stride, represented = sum(agg$n_pixels, na.rm = TRUE),
      cells = sum(!is.na(z))
    )
  }

  cluster_view_plotly <- eventReactive(input$generate_cluster_view_plot, {
    dat <- cluster_post_state(); req(dat)
    pct <- max(1, min(100, suppressWarnings(as.numeric(input$cluster_view_sample_pct %||% 50))))
    tile_dat <- prepare_cluster_view_tiles(dat, pct = pct, max_cells = 250000L)
    n_cols <- length(tile_dat$colors)
    cs <- make_plotly_discrete_colorscale(tile_dat$colors)

    plt <- plotly::plot_ly(
      x = tile_dat$x, y = tile_dat$y, z = tile_dat$z,
      type = "heatmap",
      colorscale = cs, zmin = 1, zmax = max(1, n_cols),
      text = tile_dat$hover, hoverinfo = "text", showscale = FALSE
    )

    for (i in seq_along(tile_dat$cluster_levels)) {
      plt <- plotly::add_trace(
        plt, x = NA, y = NA, type = "scatter", mode = "markers",
        marker = list(color = tile_dat$colors[i], size = 10),
        name = tile_dat$cluster_levels[i], showlegend = TRUE,
        hoverinfo = "skip", inherit = FALSE
      )
    }

    plt %>% plotly::layout(
      title = list(
        text = paste0("Interactive Pseudo-Minerals Map (", tile_dat$cells, " tiles; sampled ", tile_dat$pct, "%)"),
        x = 0.5, xanchor = "center", y = 0.98
      ),
      xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, autorange = TRUE, fixedrange = FALSE),
      yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, autorange = TRUE, scaleanchor = "x", scaleratio = 1, fixedrange = FALSE),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08, yanchor = "top"),
      margin = list(t = 75, b = 115, l = 10, r = 10),
      plot_bgcolor = "white", paper_bgcolor = "white"
    ) %>%
      plotly::config(displaylogo = FALSE)
  })

  output$plot_cluster_view <- renderPlotly({
    validate(need(input$generate_cluster_view_plot > 0, "Choose a sampling percentage and click Generate interactive map."))
    cluster_view_plotly()
  })

  output$correlogram_controls_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    dat <- cluster_post_state()
    cluster_choices <- if (!is.null(dat) && !is.null(dat$label_map) && nrow(dat$label_map) > 0)
      c("All samples", sort(unname(dat$label_map$cluster_label))) else "All samples"
    corr_group_sel <- input$corr_group %||% "All samples"
    if (!corr_group_sel %in% cluster_choices) corr_group_sel <- "All samples"

    selected_vars <- intersect(input$corr_variables %||% core$elements, core$elements)
    if (length(selected_vars) < 2) selected_vars <- head(core$elements, min(2, length(core$elements)))
    tagList(
      selectInput("corr_method", "Correlation type",
        choices = c("Spearman" = "spearman", "Pearson" = "pearson", "Moran spatial" = "moran"),
        selected = input$corr_method %||% "spearman"),
      selectInput("corr_data_basis", "Data basis",
        choices = c("Active transformed data" = "processed", "Raw input data" = "raw"),
        selected = input$corr_data_basis %||% "processed"),
      selectInput("corr_group", "Samples used", choices = cluster_choices, selected = corr_group_sel),
      radioButtons("corr_legend_orientation", "Legend",
        choices = c("Vertical" = "vertical", "Horizontal" = "horizontal"),
        selected = input$corr_legend_orientation %||% "vertical", inline = TRUE),
      selectizeInput("corr_variables", "Variables used in correlogram", choices = core$elements, selected = selected_vars,
        multiple = TRUE, options = list(plugins = list("remove_button"))),
      tags$p("Moran spatial uses a grid-neighbor approximation and automatically thins very large maps to keep the interface responsive.", class="ve-note"),
      actionButton("generate_correlogram_plot", "Generate correlogram")
    )
  })

  build_correlogram_title <- function(method, basis, group, vars) {
    method_label <- switch(method,
      moran = "Moran spatial",
      pearson = "Pearson",
      spearman = "Spearman",
      method
    )
    basis_label <- if (identical(basis, "raw")) "raw input data" else "active transformed data"
    group_label <- group %||% "All samples"
    vars_label <- paste(vars, collapse = ", ")
    paste0(method_label, " correlogram | ", basis_label, " | ", group_label,
           " | Variables: ", vars_label)
  }

  build_correlogram_data <- function() {
    core <- active_input_core(); req(loaded_data_ready(core))
    dat <- cluster_post_state()
    use_raw <- identical(input$corr_data_basis %||% "processed", "raw")
    df <- build_analysis_dataframe(core, dat, use_raw = use_raw)
    req(nrow(df) > 0)

    grp <- input$corr_group %||% "All samples"
    if (!identical(grp, "All samples") && "cluster_label" %in% names(df)) {
      df <- df %>% dplyr::filter(as.character(cluster_label) == as.character(grp))
    }
    validate(need(nrow(df) > 1, "The selected sample group does not contain enough pixels for a correlogram."))

    vars <- intersect(input$corr_variables %||% core$elements, core$elements)
    validate(need(length(vars) >= 2, "Choose at least two variables for the correlogram."))
    keep_cols <- c("x", "y", vars)
    df <- df %>% dplyr::select(dplyr::any_of(keep_cols))

    X <- as.matrix(df[, vars, drop=FALSE])
    finite_count <- colSums(is.finite(X))
    finite_sd <- vapply(seq_len(ncol(X)), function(j) {
      x <- X[, j]
      stats::sd(x[is.finite(x)], na.rm = TRUE)
    }, numeric(1))
    keep <- finite_count > 1 & is.finite(finite_sd) & finite_sd > 0
    vars_keep <- vars[keep]
    validate(need(length(vars_keep) >= 2, "At least two selected variables with finite, non-constant values are required for the correlogram."))
    df <- df[, c(intersect(c("x","y"), names(df)), vars_keep), drop=FALSE]
    attr(df, "corr_vars") <- vars_keep
    as.data.frame(df)
  }

  thin_grid_for_moran <- function(coords, X, max_n = 150000L) {
    n <- nrow(coords)
    if (n <= max_n) return(list(coords = coords, X = X, step_x = 1L, step_y = 1L, thinned = FALSE))
    x <- as.integer(coords$x); y <- as.integer(coords$y)
    stride <- max(1L, ceiling(sqrt(n / max_n)))
    x0 <- min(x, na.rm = TRUE); y0 <- min(y, na.rm = TRUE)
    keep <- ((x - x0) %% stride == 0L) & ((y - y0) %% stride == 0L)
    if (sum(keep, na.rm = TRUE) < 10L) keep <- seq_len(n) %% stride == 0L
    coords2 <- coords[keep, , drop = FALSE]
    X2 <- X[keep, , drop = FALSE]
    if (nrow(coords2) > max_n) {
      set.seed(2468)
      idx <- sample.int(nrow(coords2), max_n, replace = FALSE)
      coords2 <- coords2[idx, , drop = FALSE]
      X2 <- X2[idx, , drop = FALSE]
    }
    list(coords = coords2, X = X2, step_x = stride, step_y = stride, thinned = TRUE)
  }

  build_moran_matrix <- function(df, vars) {
    validate(need(all(c("x", "y") %in% names(df)), "Moran spatial correlogram requires x and y coordinates."))
    coords <- df[, c("x", "y"), drop=FALSE]
    X <- as.matrix(df[, vars, drop=FALSE])
    ok <- stats::complete.cases(coords) & stats::complete.cases(X)
    coords <- coords[ok, , drop=FALSE]
    X <- X[ok, , drop=FALSE]
    validate(need(nrow(X) > 3, "Not enough complete pixels are available for Moran spatial correlation."))

    thin <- thin_grid_for_moran(coords, X, max_n = 150000L)
    coords <- thin$coords
    X <- thin$X
    step_x <- as.integer(thin$step_x %||% 1L)
    step_y <- as.integer(thin$step_y %||% 1L)

    validate(need(nrow(X) > 3, "Not enough pixels remain after Moran spatial thinning."))
    key <- paste(as.integer(coords$x), as.integer(coords$y), sep="_")
    idx <- seq_len(nrow(coords)); names(idx) <- key
    right_key <- paste(as.integer(coords$x) + step_x, as.integer(coords$y), sep="_")
    down_key  <- paste(as.integer(coords$x), as.integer(coords$y) + step_y, sep="_")
    j_right <- unname(idx[right_key]); j_down <- unname(idx[down_key])
    pairs <- rbind(cbind(i=seq_along(j_right)[!is.na(j_right)], j=j_right[!is.na(j_right)]),
                   cbind(i=seq_along(j_down)[!is.na(j_down)],  j=j_down[!is.na(j_down)]))
    validate(need(nrow(pairs) > 2, "Moran spatial correlogram could not find adjacent pixels after thinning. Try using all samples or a less restrictive cluster subset."))

    Z <- scale(X, center=TRUE, scale=TRUE)
    Z[!is.finite(Z)] <- 0
    out <- matrix(0, nrow=length(vars), ncol=length(vars), dimnames=list(vars, vars))
    for (a in seq_along(vars)) {
      za_i <- Z[pairs[,"i"], a]
      za_j <- Z[pairs[,"j"], a]
      for (b in seq_along(vars)) {
        v1 <- mean(za_i * Z[pairs[,"j"], b], na.rm=TRUE)
        v2 <- mean(za_j * Z[pairs[,"i"], b], na.rm=TRUE)
        out[a, b] <- mean(c(v1, v2), na.rm=TRUE)
      }
    }
    out[!is.finite(out)] <- 0
    diag(out) <- pmax(pmin(diag(out), 1), -1)
    pmax(pmin(out, 1), -1)
  }

  build_correlogram_matrix <- function(df, method = "spearman") {
    vars <- attr(df, "corr_vars") %||% setdiff(names(df), c("x", "y"))
    vars <- intersect(vars, names(df))
    if (identical(method, "moran")) {
      return(build_moran_matrix(df, vars))
    }
    cor_df <- df[, vars, drop=FALSE]
    cor_mat <- suppressWarnings(stats::cor(cor_df, use = "pairwise.complete.obs", method = method))
    cor_mat <- as.matrix(cor_mat)
    if (any(!is.finite(cor_mat))) cor_mat[!is.finite(cor_mat)] <- 0
    diag(cor_mat) <- 1
    cor_mat
  }

  render_correlogram_with_title <- function(cor_mat, title, orientation = "vertical") {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    title_wrapped <- paste(strwrap(title, width = 88), collapse = "\n")
    render_corrplot(cor_mat, orientation = orientation, title = title_wrapped)
  }

  observeEvent(input$generate_correlogram_plot, {
    withProgress(message = "Generating correlogram", value = 0, {
      incProgress(0.20, detail = "Preparing data")
      df <- build_correlogram_data()
      method <- input$corr_method %||% "spearman"
      basis <- input$corr_data_basis %||% "processed"
      group <- input$corr_group %||% "All samples"
      vars <- attr(df, "corr_vars") %||% setdiff(names(df), c("x", "y"))
      incProgress(0.55, detail = if (identical(method, "moran")) "Computing Moran spatial matrix" else "Computing correlation matrix")
      cor_mat <- build_correlogram_matrix(df, method = method)
      current <- correlogram_saved_plots()
      new_idx <- length(current) + 1L
      corr_label <- if (new_idx == 1L) "Correlogram" else paste0("Correlogram (", new_idx - 1L, ")")
      corr_id <- paste0("corr_", as.integer(Sys.time()), "_", new_idx)
      current[[corr_id]] <- list(
        matrix = cor_mat,
        label = corr_label,
        title = build_correlogram_title(method, basis, group, vars),
        orientation = input$corr_legend_orientation %||% "vertical",
        method = method,
        basis = basis,
        group = group,
        vars = vars
      )
      correlogram_saved_plots(current)
      data_analysis_visible$correlogram <- TRUE
      data_analysis_selected_tab(corr_id)
      session$onFlushed(function() updateTabsetPanel(session, "data_analysis_preview_tabs", selected = corr_id), once = TRUE)
      incProgress(0.95, detail = "Correlogram ready")
    })
  }, ignoreInit = TRUE)

  output$download_correlogram_plot <- downloadHandler(
    filename = function() "Correlogram.pdf",
    content = function(file) {
      saved <- correlogram_saved_plots()
      validate(need(length(saved) > 0, "Generate at least one correlogram before downloading."))
      item <- saved[[length(saved)]]
      grDevices::cairo_pdf(file = file, width = 10, height = 8)
      on.exit(grDevices::dev.off(), add = TRUE)
      render_correlogram_with_title(item$matrix, item$title, orientation = item$orientation %||% "vertical")
    }
  )

  # ---------------------------------------------------------------------------
  # Report
  # ---------------------------------------------------------------------------
  report_section_choices <- c(
    "Processing report" = "processing",
    "Input values table" = "input_values",
    "Summary statistics table" = "summary_table",
    "Data relationships preview (ggpairs)" = "pairs_plot",
    "Element maps (1 per page)" = "element_maps",
    "Active RGB Map (if any)" = "active_rgb_map",
    "Cluster map + entropy map" = "cluster_map",
    "Cluster mean-composition table" = "cluster_summary",
    "Cluster standard-deviation table" = "cluster_sd",
    "Scatter plot" = "scatter_plot",
    "Ternary plot" = "ternary_plot",
    "Active Correlograms (if any)" = "active_correlogram",
    "Auto Detect plot and table (if generated)" = "auto_detect"
  )

  report_preview_dir <- file.path(tempdir(), "qmap_report_preview")
  dir.create(report_preview_dir, showWarnings = FALSE, recursive = TRUE)
  shiny::addResourcePath("qmap_report_preview", report_preview_dir)
  report_preview_file <- reactiveVal(NULL)


  open_report_pdf_device <- function(file, width = 8.27, height = 11.69) {
    if (isTRUE(capabilities("cairo"))) {
      grDevices::cairo_pdf(file = file, width = width, height = height, onefile = TRUE)
    } else {
      grDevices::pdf(file = file, width = width, height = height, onefile = TRUE)
    }
  }

  report_section_safe <- function(title, expr) {
    tryCatch(
      force(expr),
      error = function(e) {
        draw_report_text_page(
          title = paste0(title, " - not generated"),
          paragraphs = paste0(
            "This report section could not be generated in the current session. ",
            "The remaining report sections were preserved. Error message: ",
            conditionMessage(e)
          )
        )
        invisible(NULL)
      }
    )
  }


  output$report_sections_ui <- renderUI({
    selected_sections <- input$report_sections %||% unname(report_section_choices)
    if (isTRUE(input$report_select_all)) selected_sections <- unname(report_section_choices)
    div(class = if (isTRUE(input$report_select_all)) "ve-report-section-selected" else NULL,
      checkboxGroupInput(
        "report_sections",
        "Include in PDF report:",
        choices = report_section_choices,
        selected = selected_sections
      )
    )
  })

  observeEvent(input$report_select_all, {
    if (isTRUE(input$report_select_all)) {
      updateCheckboxGroupInput(session, "report_sections", selected = unname(report_section_choices))
    }
  }, ignoreInit = FALSE)

  observeEvent(input$report_sections, {
    if (!isTRUE(input$report_select_all)) return()
    if (!setequal(input$report_sections %||% character(0), unname(report_section_choices))) {
      updateCheckboxInput(session, "report_select_all", value = FALSE)
    }
  }, ignoreInit = TRUE)

  output$report_preview_ui <- renderUI({
    preview_file <- report_preview_file()
    if (is.null(preview_file) || !file.exists(preview_file)) {
      return(tagList(
        tags$p("Generate a PDF report to preview it here before saving.", class = "ve-note"),
        tags$ul(
          tags$li("The processing report explains the active transform and lists the other transform options available in the current version."),
          tags$li("Cluster analysis pages describe PCA usage, k-means segmentation, post-processing actions, and entropy-based pixel filtering."),
          tags$li("Maps, RGB composites, scatter plots, ternary plots, active correlograms, and Auto Detect diagnostics are exported based on the current application state.")
        )
      ))
    }
    tags$iframe(
      src = paste0("qmap_report_preview/", basename(preview_file), "?t=", as.integer(Sys.time())),
      style = "width:100%;height:82vh;border:1px solid #d9e2ec;border-radius:10px;background:white;"
    )
  })

  generate_pdf_report_file <- function(file, sections_override = NULL) {

      core <- active_input_core(); req(loaded_data_ready(core))
      sections <- sections_override %||% input$report_sections %||% character(0)
      dat <- cluster_post_state()
      has_norm <- any(vapply(core$norms %||% list(), function(x) isTRUE(is.finite(x) && !is.na(x) && x > 0), logical(1)))
      active_method <- core$processing_method %||% processing_state()$method %||% default_processing_method(core)
      method_tbl <- processing_method_catalog(has_norm)

      auto_detect_report_paragraphs <- function() {
        auto <- tryCatch(cluster_auto_detect_state(), error = function(e) NULL)
        if (is.null(auto) || is.null(auto$table)) return(character(0))
        opt_row <- auto$table[which.max(auto$table$Composite_relative), , drop = FALSE]
        base_txt <- paste0(
          "Auto Detect was executed in this session. The routine tested candidate phase numbers from k = 2 to 15 using PCA scores calculated from a random subset of valid pixels. It then ranked each solution with a composite score combining bulk-composition agreement, entropy-based purity, clustering stability across independent k-means runs, and a weak parsimony penalty. In this run, the selected optimum was k = ",
          auto$optimum_k, " phases, based on ", format(auto$sample_size, big.mark = ","),
          " sampled pixels (", auto$sample_percent %||% "", "% of valid pixels; maximum cap ",
          format(auto$sample_cap %||% 500000L, big.mark = ","), ")."
        )
        method_txt <- paste0(
          "The Bulk Composition Index (BCI) compares the informed whole-sample concentration vector with the composition reconstructed from the pseudo-mineral phases. Conceptually, BCI = 100 × [1 - sum(|Bulk_i - Recon_i|) / sum(Bulk_i)], where Bulk_i is the normalized whole-sample composition and Recon_i is reconstructed from phase compositions and modal abundance. Phase compositions are estimated from the 30% lowest-entropy pixels, whereas modal abundance is estimated from all sampled pixels. The purity score is derived from normalized entropy, with lower entropy indicating more confident pixel assignment. Stability is calculated from the similarity between repeated k-means solutions, using an Adjusted Rand Index criterion. Parsimony applies a weak penalty against unnecessary increases in k, reducing overfitting by high phase numbers."
        )
        result_txt <- if (nrow(opt_row) == 1) paste0(
          "For the selected solution, BCI = ", round(opt_row$BCI, 2),
          ", weighted entropy = ", round(opt_row$WeightedEntropy, 4),
          ", stability = ", round(opt_row$Stability, 3),
          ", and relative composite score = ", round(opt_row$Composite_relative, 3), "."
        ) else character(0)
        c(base_txt, method_txt, result_txt)
      }

      open_report_pdf_device(file = file, width = 8.27, height = 11.69)
      on.exit(if (grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)

      withProgress(message="Generating PDF report...", value=0, {
        tryCatch({
        incProgress(0.03, detail="Opening PDF device")

        incProgress(0.04, detail="Writing cover page")
        grid::grid.newpage()
        grid::grid.text(paste0("QMap - VisualEasier Report"), x=0.5, y=0.92, gp=grid::gpar(fontsize=20,fontface="bold"))
        grid::grid.text(paste0("Version: v",APP_VERSION), x=0.5, y=0.88, gp=grid::gpar(fontsize=11))
        grid::grid.text(paste("Sample ID:",core$sample_id %||% "not defined"), x=0.5, y=0.84, gp=grid::gpar(fontsize=12))
        grid::grid.text(paste("Generated:",as.character(Sys.time())), x=0.5, y=0.80, gp=grid::gpar(fontsize=10))
        grid::grid.text(paste("Active variables:",paste(core$elements,collapse=", ")), x=0.5, y=0.75, gp=grid::gpar(fontsize=10))

        if ("processing" %in% sections) {
          incProgress(0.08, detail="Writing processing summary")
          cluster_txt <- if (is.null(dat)) "Cluster analysis was not generated in the current session." else {
            pca_txt <- if (isTRUE(input$use_pca_cluster) && !is.null(dat$pca_info))
              paste0("PCA was enabled before clustering. The number of retained principal components was selected to explain approximately ",
                     round(100*dat$pca_info$cumulative_variance,1),"% of the total variance (",dat$pca_info$n_components," PCs retained). ")
            else "PCA was not used before clustering, so k-means was applied directly to the active transformed variables. "
            current_labels <- if (!is.null(dat$label_map) && nrow(dat$label_map) > 0) paste(dat$label_map$cluster_label,collapse=", ") else "not available"
            change_txt <- if (length(unique(dat$result_df$cluster_id)) < dat$k)
              "The number of active clusters after post-processing is smaller than the initial k value, indicating that one or more clusters were merged or deleted. "
            else "No cluster reduction by merge/delete was detected after the initial segmentation. "
            paste0(pca_txt,"Segmentation was performed with k-means using k = ",dat$k," clusters. ",
                   change_txt,"Current retained/renamed cluster labels are: ",current_labels,".")
          }
          entropy_txt <- if (is.null(dat)) "" else paste0(
            "Entropy was computed as a normalized Shannon entropy using inverse-distance weighting (IDW). For each pixel, weights proportional to 1/distance to each cluster centroid are normalized to sum to 1, forming a probability distribution. Shannon entropy of that distribution is normalized by log(K), producing values between 0 and 1. Lower entropy indicates confident assignment to the nearest cluster; higher entropy indicates that the pixel lies between multiple centroids and may represent a mixed or transitional composition. ",
            if (isTRUE(dat$ambiguity_enabled) && is.finite(dat$entropy_rank_percent))
              paste0("For cluster-summary statistics, the application retained the lowest-entropy pixels within each cluster up to the user-defined percentile threshold of ",round(dat$entropy_rank_percent,0),"%.")
            else "Entropy-based filtering was not applied to the current cluster-summary statistics.")
          processing_txt <- paste0("The active data transformation is '",processing_method_label(active_method,has_norm),"'. This method ",
            tolower(processing_method_description(active_method,has_norm))," The other transformation options currently available in the application are: ",
            paste(method_tbl$label[method_tbl$code != active_method],collapse="; "),".")
          processing_details_txt <- paste0(
            "The active processing state stores the selected variables, the active transformation method, and any clipping limits applied in the Element clipping editor. These settings are reused by Map Processing, Cluster Analysis, and Data Analysis. If clipping was applied, values outside the saved range were clipped before map display and downstream analysis."
          )
          cluster_settings_txt <- if (is.null(dat)) character(0) else paste0(
            "Final cluster-analysis settings: number of phases = ", dat$k,
            "; PCA option = ", if (isTRUE(dat$use_pca_cluster %||% FALSE)) "enabled" else "disabled",
            "; entropy filtering for statistics = ", if (isTRUE(dat$ambiguity_enabled %||% FALSE)) paste0("enabled at ", round(dat$entropy_rank_percent %||% NA_real_, 0), "%") else "disabled",
            "; retained pixels for statistics = ", dat$filtered_n %||% NA_integer_, " of ", dat$total_n %||% NA_integer_, "."
          )
          draw_report_text_page(title="Processing Report", subtitle="Current settings and methodological summary",
            paragraphs=c(
              paste0("The current project contains ",length(core$elements)," active variables and matrices with dimensions ",
                     paste(core$dims,collapse=" x "),". The sample identifier is '",core$sample_id %||% "not defined","'."),
              processing_txt,
              processing_details_txt,
              cluster_settings_txt,
              cluster_txt,
              entropy_txt))
          auto_paragraphs <- auto_detect_report_paragraphs()
          if (length(auto_paragraphs) > 0) {
            draw_report_text_page(
              title = "Auto Detect Summary",
              subtitle = "Optimum phase-number estimation and diagnostic criteria",
              paragraphs = auto_paragraphs
            )
          }
        }

        if ("input_values" %in% sections) {
          incProgress(0.07, detail="Writing input values table")
          input_tbl <- core$summary[, intersect(c("File","Element","Normalization","Rows","Columns"),names(core$summary)), drop=FALSE]
          draw_report_table_pages("Input Values Table", input_tbl, subtitle="Files and metadata loaded in Input Data", rows_per_page=24)
        }

        if ("summary_table" %in% sections) {
          incProgress(0.08, detail="Calculating summary statistics")
          mats <- core$matrices %||% core$mats
          summary_df <- dplyr::bind_rows(lapply(names(mats), function(nm)
            cbind(data.frame(Element=nm,stringsAsFactors=FALSE), round(summary_stats_for_matrix(mats[[nm]]),6))))
          summary_df <- summary_df[,colSums(!(is.na(summary_df)|summary_df==""))>0,drop=FALSE]
          draw_report_table_pages("Summary Statistics Table",summary_df,subtitle="Statistics for the active transformed variables",rows_per_page=22)
        }

        if ("pairs_plot" %in% sections && isTRUE(processing_state()$applied) && isTRUE(input$enable_processing_pairs_preview)) {
          incProgress(0.08, detail="Rendering relationships preview")
          draw_ggplot_raster_page(build_processing_pairs_plot(core,max_n=1500,report_mode=TRUE)+ggtitle("Data Relationships Preview (ggpairs)"),width=8,height=10,dpi=110)
        }

        if ("element_maps" %in% sections) {
          incProgress(0.10, detail="Rendering element maps")
          elem_data <- tryCatch(elemental_data(), error=function(e) NULL)
          if (!is.null(elem_data) && length(elem_data$plots) > 0)
            for (p in elem_data$plots) draw_ggplot_raster_page(p,width=8,height=10,dpi=130)
        }

        if ("active_rgb_map" %in% sections) {
          incProgress(0.07, detail="Rendering RGB maps")
          saved_rgb <- tryCatch(rgb_saved_maps(), error=function(e) list())
          if (length(saved_rgb) > 0) {
            for (rgb_item in saved_rgb) draw_ggplot_raster_page(rgb_item$plot, width=8, height=10, dpi=130)
          } else {
            rgb_dat <- tryCatch(filtered_plot_data(), error=function(e) NULL)
            if (!is.null(rgb_dat)) print(rgb_dat$plot)
          }
        }

        if ("cluster_map" %in% sections && !is.null(dat)) {
          incProgress(0.10, detail="Rendering cluster and entropy maps")
          p1 <- make_cluster_entropy_plot(df=dat$result_df,fill_var="cluster_label",title=paste0("Pseudo-Minerals Map (", dat$k, " phases)"),aspect_ratio=dat$aspect_ratio %||% 1,
            fill_scale=scale_fill_manual(values=dat$cluster_colors_named[names(dat$cluster_colors_named) %in% unique(dat$result_df$cluster_label)],drop=FALSE,name="Cluster", guide=guide_legend(nrow=ceiling(length(unique(dat$result_df$cluster_label)) / 4), byrow=TRUE)),
            map_width_mm=dat$map_width,map_height_mm=dat$map_height,
            invert_coords=isTRUE(dat$invert_coords))
          p2 <- make_cluster_entropy_plot(df=dat$ambiguity_df,fill_var="ambiguity",title="Entropy Map",aspect_ratio=dat$aspect_ratio %||% 1,
            fill_scale=scale_fill_viridis_c(option="magma",name="Entropy", guide=guide_colorbar(direction="horizontal", title.position="top", barwidth=12, barheight=0.6)),
            map_width_mm=dat$map_width,map_height_mm=dat$map_height,
            invert_coords=isTRUE(dat$invert_coords))
          draw_ggplot_raster_page(p1,width=8,height=10,dpi=130)
          draw_ggplot_raster_page(p2,width=8,height=10,dpi=130)
        }

        if ("cluster_summary" %in% sections && !is.null(dat)) {
          incProgress(0.07, detail="Writing cluster mean table")
          mean_tbl <- extract_cluster_mean_table(dat$summary_table)
          mean_tbl <- mean_tbl[,colSums(!(is.na(mean_tbl)|mean_tbl==""))>0,drop=FALSE]
          draw_report_table_pages("Cluster Mean-Composition Table",mean_tbl,subtitle="Average composition by cluster",rows_per_page=20)
        }

        if ("cluster_sd" %in% sections && !is.null(dat)) {
          incProgress(0.07, detail="Writing cluster standard-deviation table")
          sd_tbl <- extract_cluster_sd_table(dat$summary_table)
          sd_tbl <- sd_tbl[,colSums(!(is.na(sd_tbl)|sd_tbl==""))>0,drop=FALSE]
          draw_report_table_pages("Cluster Standard-Deviation Table",sd_tbl,subtitle="Standard deviation by cluster and variable",rows_per_page=20)
        }

        if ("scatter_plot" %in% sections && !is.null(dat)) {
          incProgress(0.06, detail="Rendering scatter plot")
          draw_ggplot_raster_page(cluster_pc_scatter_ggplot(),width=8,height=10,dpi=110)
        }

        if ("ternary_plot" %in% sections && !is.null(dat)) {
          incProgress(0.08, detail="Rendering ternary plot")
          plot_df <- dat$pc_scores
          avar <- input$cluster_ternary_a %||% names(plot_df)[1]
          bvar <- input$cluster_ternary_b %||% names(plot_df)[2]
          cvar <- input$cluster_ternary_c %||% names(plot_df)[3]
          vals <- cbind(resolve_ternary_axis(avar,plot_df,core,dat),
                        resolve_ternary_axis(bvar,plot_df,core,dat),
                        resolve_ternary_axis(cvar,plot_df,core,dat))
          vals <- abs(vals); rs <- rowSums(vals,na.rm=TRUE); rs[!is.finite(rs)|rs<=0] <- 1
          tern <- sweep(vals,1,rs,"/")
          plot_df$a <- tern[,1]; plot_df$b <- tern[,2]; plot_df$c <- tern[,3]
          color_mode <- input$cluster_ternary_color_mode %||% "cluster"
          color_var <- NULL
          if (identical(color_mode,"entropy"))
            plot_df <- plot_df %>% dplyr::left_join(dat$ambiguity_df %>% dplyr::select(x,y,ambiguity),by=c("x","y"))
          else if (identical(color_mode,"variable")) {
            color_var <- input$cluster_ternary_color_variable %||% core$elements[1]
            if (!color_var %in% names(plot_df)) {
              base_df <- build_analysis_dataframe(core,dat,use_raw=FALSE)
              plot_df <- dplyr::left_join(plot_df,base_df %>% dplyr::select(x,y,dplyr::all_of(color_var)),by=c("x","y"))
            }
          }
          plot_df <- sample_rows_analysis(plot_df,frac=max(0.01,as.numeric(input$ternary_sample_pct %||% 1)/100),
                                          max_n=100000,cluster_col="cluster_label")
          p_tern <- make_static_ternary_plot(plot_df,avar,bvar,cvar,color_mode=color_mode,
                                             color_var=color_var,cluster_colors_named=dat$cluster_colors_named)
          draw_ggplot_raster_page(p_tern,width=8,height=10,dpi=110)
        }

        if ("auto_detect" %in% sections) {
          auto <- tryCatch(cluster_auto_detect_state(), error = function(e) NULL)
          if (!is.null(auto) && !is.null(auto$table)) {
            incProgress(0.07, detail = "Writing Auto Detect diagnostics")
            plot_df <- dplyr::bind_rows(
              data.frame(k = auto$table$k, Metric = "BCI score", RelativeValue = auto$table$BCI_score),
              data.frame(k = auto$table$k, Metric = "Purity score", RelativeValue = auto$table$Purity_score),
              data.frame(k = auto$table$k, Metric = "Stability score", RelativeValue = auto$table$Stability_score),
              data.frame(k = auto$table$k, Metric = "Composite score", RelativeValue = auto$table$Composite_relative)
            )
            p_auto <- ggplot(plot_df, aes(x = k, y = RelativeValue, group = Metric, linetype = Metric, shape = Metric)) +
              geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
              geom_vline(xintercept = auto$optimum_k, linetype = "dashed") +
              scale_x_continuous(breaks = auto$table$k) +
              scale_y_continuous(limits = c(0, 1), name = "Score value") +
              labs(x = "Number of phases", title = "Auto Detect composite score across tested phase numbers") +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                    legend.position = "top", panel.grid.minor = element_blank())
            draw_ggplot_raster_page(p_auto, width = 8, height = 10, dpi = 120)
            auto_tbl <- auto$table %>%
              dplyr::select(-dplyr::any_of(c("SampledPixels", "PureFraction", "PCAComponents", "Runs",
                                             "SuccessfulRuns", "Stability_score", "Parsimony_score",
                                             "CompositeScore"))) %>%
              dplyr::mutate(
                BCI = round(BCI, 2),
                WeightedEntropy = round(WeightedEntropy, 4),
                Stability = round(Stability, 3),
                BCI_score = round(BCI_score, 3),
                Purity_score = round(Purity_score, 3),
                Composite_relative = round(Composite_relative, 3)
              )
            draw_report_table_pages("Auto Detect Diagnostic Table", auto_tbl,
                                    subtitle = "Relative scores by tested number of phases", rows_per_page = 22)
          }
        }

        if ("active_correlogram" %in% sections) {
          incProgress(0.08, detail="Rendering correlograms")
          saved_corr <- tryCatch(correlogram_saved_plots(), error = function(e) list())
          if (length(saved_corr) > 0) {
            for (corr_item in saved_corr) {
              render_correlogram_with_title(corr_item$matrix, corr_item$title, orientation = corr_item$orientation %||% "vertical")
            }
          }
        }

        incProgress(1, detail="Finalizing PDF report")
        }, error = function(e) {
          draw_report_text_page(
            title = "PDF report generation warning",
            paragraphs = paste0(
              "The report generator encountered an error while writing one or more sections. ",
              "A valid PDF was still produced so the file can be previewed and saved. Error message: ",
              conditionMessage(e)
            )
          )
        })
      })
    
  }

  observeEvent(input$generate_pdf_report_preview, {
    preview_path <- file.path(report_preview_dir, paste0("QMap_report_preview_", as.integer(Sys.time()), ".pdf"))
    tryCatch({
      generate_pdf_report_file(preview_path)
      if (file.exists(preview_path) && is.finite(file.info(preview_path)$size) && file.info(preview_path)$size > 0) {
        report_preview_file(preview_path)
      } else {
        report_preview_file(NULL)
        showNotification("The PDF report was not created. Please check the selected sections and try again.", type = "error")
      }
    }, error = function(e) {
      report_preview_file(NULL)
      showNotification(paste("PDF report generation failed:", conditionMessage(e)), type = "error", duration = 10)
    })
  })

  output$save_pdf_report <- downloadHandler(
    filename = function() {
      core <- active_input_core()
      paste0(core$sample_id %||% "sample", "_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      preview_file <- report_preview_file()
      copied <- FALSE
      if (!is.null(preview_file) && file.exists(preview_file) && file.info(preview_file)$size > 0) {
        copied <- isTRUE(file.copy(preview_file, file, overwrite = TRUE))
      }
      if (!copied) {
        tryCatch(
          generate_pdf_report_file(file),
          error = function(e) {
            open_report_pdf_device(file = file, width = 8.27, height = 11.69)
            on.exit(if (grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)
            draw_report_text_page(
              title = "PDF report generation failed",
              paragraphs = paste0("The requested report could not be generated. Error message: ", conditionMessage(e))
            )
          }
        )
      }
    },
    contentType = "application/pdf"
  )

}

shinyApp(ui = ui, server = server)
