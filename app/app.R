# QMap - VisualEasier 1.0.0

APP_VERSION <- "1.0.0"

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

options(shiny.maxRequestSize = 1024 * 1024 * 1024)
options(shiny.launch.browser = TRUE)

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

apply_filter_to_matrix <- function(mat, filter_name = "None") {
  mat <- as.matrix(mat)
  filter_name <- filter_name %||% "None"
  if (identical(filter_name, "None")) return(mat)
  rng <- range(mat, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(mat)
  scaled <- (mat - rng[1]) / diff(rng)
  scaled[!is.finite(scaled)] <- 0
  filtered <- switch(filter_name,
    "Median"       = median_filter_3x3(scaled),
    "Gaussian blur" = convolve2d(scaled, matrix(c(1,2,1,2,4,2,1,2,1), nrow=3, byrow=TRUE) / 16),
    "Gradient"     = {
      dx <- cbind(diff(scaled, differences=1, lag=1), 0)
      dy <- rbind(diff(scaled, differences=1, lag=1), 0)
      sqrt(dx^2 + dy^2)
    },
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
  add_scale_bar(p, build_scale_annotation(df, map_width_mm, map_height_mm))
}

build_scale_annotation <- function(df, map_width_mm, map_height_mm) {
  if (is.null(map_width_mm) || !is.finite(map_width_mm) || map_width_mm <= 0 ||
      is.null(map_height_mm) || !is.finite(map_height_mm) || map_height_mm <= 0)
    return(NULL)
  x_span <- diff(range(df$x, na.rm = TRUE))
  if (!is.finite(x_span) || x_span <= 0) return(NULL)
  # Choose a round bar length that is ~20% of the map width
  bar_mm <- 10^floor(log10(map_width_mm * 0.2))
  bar_mm <- round(map_width_mm * 0.2 / bar_mm) * bar_mm
  bar_mm <- max(bar_mm, 0.1)
  list(label = paste0(bar_mm, " mm"))
}

# Scale bar as plot caption — visible under all themes including theme_void.
add_scale_bar <- function(plot_obj, scale_info) {
  if (is.null(scale_info)) return(plot_obj)
  plot_obj +
    labs(caption = paste0("▬  ", scale_info$label)) +
    theme(
      plot.caption       = element_text(hjust = 0.5, size = 10,
                                        margin = margin(t = 6, b = 2),
                                        color  = "black"),
      plot.caption.position = "plot"
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
  if (!is.null(labels) && length(labels) == 3) {
    x_mid <- max(df$x, na.rm = TRUE) / 2
    y_lab  <- max(df$y, na.rm = TRUE) + 20
    p <- p +
      annotate("label", x=x_mid-50, y=y_lab, label=labels[1], fill="red",   color="white", fontface="bold", size=5) +
      annotate("label", x=x_mid,    y=y_lab, label=labels[2], fill="green", color="white", fontface="bold", size=5) +
      annotate("label", x=x_mid+50, y=y_lab, label=labels[3], fill="blue",  color="white", fontface="bold", size=5)
  }
  add_scale_bar(p, build_scale_annotation(df, map_width_mm, map_height_mm))
}

cluster_palette <- function(k, palette_name = "Okabe-Ito") {
  k <- max(1, as.integer(k))
  palette_name <- palette_name %||% "Okabe-Ito"
  switch(palette_name,
    "Okabe-Ito" = rep_len(c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999"), k),
    "Set 2"     = rep_len(RColorBrewer::brewer.pal(8,  "Set2"),   k),
    "Set 3"     = rep_len(RColorBrewer::brewer.pal(12, "Set3"),   k),
    "Paired"    = rep_len(RColorBrewer::brewer.pal(12, "Paired"), k),
    "Dark 2"    = rep_len(RColorBrewer::brewer.pal(8,  "Dark2"),  k),
    "Accent"    = rep_len(RColorBrewer::brewer.pal(8,  "Accent"), k),
    rep_len(grDevices::hcl.colors(max(k, 3), palette="Dark 3", rev=FALSE), k)
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
  tmp_png <- tempfile(fileext = ".png")
  on.exit({ grDevices::dev.off(); unlink(tmp_png) }, add = FALSE)
  ggsave(filename=tmp_png, plot=plot_obj, width=width, height=height,
         dpi=dpi, bg=bg, device="png", limitsize=FALSE)
  img <- png::readPNG(tmp_png)
  grDevices::cairo_pdf(file=file, width=width, height=height, bg=bg)
  grid::grid.newpage()
  grid::grid.raster(img, width=grid::unit(1,"npc"), height=grid::unit(1,"npc"), interpolate=FALSE)
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

render_corrplot <- function(cor_mat, orientation = "vertical") {
  col_fun <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot::corrplot(cor_mat, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
                     col=col_fun(200), addCoef.col="black",
                     cl.pos=if (identical(orientation, "vertical")) "r" else "b", order="AOE")
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

  cor_size  <- if (report_mode) 2  else 10
  strip_size <- if (report_mode) 10 else 12
  axis_size  <- if (report_mode) 7  else 8
  point_size <- if (report_mode) 0.45 else 0.6

  p <- GGally::ggpairs(df, columns=seq_along(core$elements),
    lower = list(continuous = GGally::wrap("points", alpha=0.7, size=point_size)),
    diag  = list(continuous = GGally::wrap("densityDiag", alpha=0.7)),
    upper = list(continuous = GGally::wrap("cor", size=cor_size, alignPercent=1,
                                           justify_labels="center", stars=FALSE))) +
    theme_bw(base_size=if (report_mode) 8 else 11) +
    theme(strip.placement="outside", strip.background=element_blank(),
          strip.text=element_text(size=strip_size, face="bold"),
          strip.text.x.top=element_text(size=strip_size, face="bold"),
          strip.text.y.right=element_text(size=strip_size, face="bold", angle=270),
          axis.text.x=element_text(size=axis_size, angle=45, hjust=1),
          axis.text.y=element_text(size=axis_size),
          axis.title=element_blank(), panel.grid=element_blank(),
          panel.spacing=grid::unit(0.2,"lines"), aspect.ratio=1)

  n_vars <- length(core$elements)
  for (i in seq_len(n_vars)) for (j in seq_len(n_vars)) {
    if (i >= j)
      p[i,j] <- p[i,j] + theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
                                 axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                                 axis.title.x=element_blank(), axis.title.y=element_blank())
  }
  p
}

make_cluster_entropy_plot <- function(df, fill_var, title, aspect_ratio, fill_scale,
                                      map_width_mm = NULL, map_height_mm = NULL) {
  p <- ggplot(df, aes(x=x, y=y, fill=.data[[fill_var]])) +
    geom_raster() + coord_equal() + fill_scale +
    theme_minimal() + labs(title=title) +
    theme(
      plot.title      = element_text(hjust=0.5, face="bold"),
      axis.title      = element_blank(),
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      panel.grid      = element_blank(),
      legend.position = "right",
      plot.background = element_rect(fill = "white", colour = NA)
    )
  add_scale_bar(p, build_scale_annotation(df, map_width_mm, map_height_mm))
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
    "))
  ),
  navbarPage(
    theme = shinytheme("yeti"),
    title = div(class="ve-navbar-title",
      span(paste0("QMap - VisualEasier (v", APP_VERSION, ")"))
    ),

    # ---- Input Data ----
    tabPanel("Input Data",
      fluidRow(
        column(6,
          div(class="ve-box",
            h2("Central data input", align="center"),
            fileInput("load_project_file", "Load Project", accept=c(".rds",".RDS",".veproj.rds")),
            uiOutput("input_data_project_creation_ui")
          )
        ),
        column(6,
          h3(tags$b("Loaded data summary"), align="center"),
          div(class="ve-box", uiOutput("loaded_data_status")),
          div(class="ve-box", tableOutput("loaded_data_table"), uiOutput("save_project_ui"))
        )
      )
    ),

    # ---- Data Processing ----
    tabPanel("Data Processing",
      fluidPage(
        fluidRow(
          column(4,
            div(class="ve-box",
              h2("Processing settings", align="center"),
              uiOutput("processing_variable_selector_ui"),
              uiOutput("processing_method_ui"),
              actionButton("apply_processing", "Apply"),
              actionButton("reset_processing", "Reset"),
              hr(),
              tags$p("The selected variables and transformation will be used by Map Processing, Cluster Analysis, and Data Analysis after applying.", class="ve-note")
            )
          ),
          column(8,
            div(class="ve-box",
              h3(tags$b("Summary table"), align="center"),
              uiOutput("processing_status_ui"),
              tableOutput("missing_summary_table")
            )
          )
        ),
        fluidRow(
          column(12,
            div(class="ve-box",
              h4("Sampled relationships preview", align="center"),
              tags$p("The preview uses a random sample of 15% of the pixels, capped at 1,500 rows, and is shown only after Apply.", class="ve-note"),
              withSpinner(plotOutput("processing_pairs_plot", width="100%", height="82vw"), type=1, color="black")
            )
          )
        )
      )
    ),

    # ---- Map Processing ----
    tabPanel("Map Processing",
      sidebarLayout(
        sidebarPanel(
          h2("Map settings", align="center"),
          radioButtons("type", label=h4("Select map type:"),
            choices=c("Element Map"="elemental","Ternary Map (RGB)"="rgb"), selected="elemental"),
          uiOutput("map_processing_controls_ui"),
          div(class="ve-box", uiOutput("element_clip_editor_ui")),
          uiOutput("map_cluster_filter_ui"),
          uiOutput("map_processing_download_ui")
        ),
        mainPanel(class="ve-main-panel",
          h3(tags$b("Preview"), align="center"),
          tags$p("This tab uses the active dataset defined in Input Data and, optionally, transformed in Data Processing.", class="ve-note"),
          uiOutput("map_processing_preview_ui")
        )
      )
    ),

    # ---- Cluster Analysis ----
    tabPanel("Cluster Analysis",
      fluidPage(
        fluidRow(
          column(6,
            wellPanel(
              h2("Cluster settings", align="center"),
              numericInput("ncluster", "Number of clusters (mineral phases):", value=2, min=2, step=1),
              uiOutput("cluster_color_controls_ui"),
              uiOutput("cluster_selector_ui"),
              checkboxInput("invert_cluster_coords", "Invert map coordinates?", value=FALSE),
              checkboxInput("use_pca_cluster", "Use PCA instead of variables", value=FALSE),
              uiOutput("cluster_pca_controls_ui"),
              checkboxInput("enable_ambiguity_filter", "Exclude highly ambiguous pixels from statistics", value=FALSE),
              uiOutput("cluster_ambiguity_controls_ui"),
              actionButton("cluster", "Generate map")
            )
          ),
          column(6,
            wellPanel(
              h2("Post-processing", align="center"),
              tags$p("Apply merge, delete, and renaming operations over the current clustering result without rerunning k-means.", class="ve-note"),
              uiOutput("cluster_postprocess_ui")
            )
          )
        ),
        fluidRow(
          column(12,
            div(class="ve-main-panel",
              h3(tags$b("Preview"), align="center"),
              tags$p("This tab also reuses the elemental matrices loaded in Input Data.", class="ve-note"),
              fluidRow(column(12, uiOutput("table_cluster_ui"))),
              fluidRow(column(12, div(style="margin-top:8px;margin-bottom:12px;",
                downloadButton("download_cluster_table", "Download Cluster Summary Table")))),
              br(),
              fluidRow(column(12, uiOutput("cluster_visible_ui"))),
              fluidRow(column(12, withSpinner(plotOutput("plot_cluster", width="100%", height="72vh"), type=1, color="black"))),
              br(),
              fluidRow(column(12, tags$h4("Pixel Entropy / Mixture Likelihood", align="center",
                style="font-weight:bold;margin-top:8px;margin-bottom:14px;"))),
              fluidRow(column(12, withSpinner(plotOutput("plot_cluster_ambiguity", width="100%", height="72vh"), type=1, color="black"))),
              fluidRow(column(12, div(style="margin-top:8px;margin-bottom:12px;",
                downloadButton("download_cluster_map", "Download Cluster Map"))))
            )
          )
        )
      )
    ),

    # ---- Data Analysis ----
    tabPanel("Data Analysis",
      fluidPage(
        fluidRow(
          column(12,
            div(class="ve-main-panel",
              uiOutput("data_analysis_sampling_controls_ui"),
              h3(tags$b("Scatter Plot"), align="center"),
              uiOutput("cluster_pc_scatter_controls_ui"),
              withSpinner(plotlyOutput("plot_cluster_pc_scatter", width="100%", height="72vh"), type=1, color="black"),
              div(style="margin-top:8px;margin-bottom:18px;", downloadButton("download_cluster_pca_plot", "Download PCA Plot")),
              tags$hr(),
              h3(tags$b("Ternary Plot"), align="center"),
              uiOutput("cluster_pc_ternary_controls_ui"),
              withSpinner(plotlyOutput("plot_cluster_pc_ternary", width="100%", height="72vh"), type=1, color="black"),
              div(style="margin-top:8px;margin-bottom:18px;", downloadButton("download_cluster_ternary_plot", "Download Ternary Plot")),
              tags$hr(),
              h3(tags$b("Correlogram"), align="center"),
              uiOutput("correlogram_controls_ui"),
              withSpinner(plotOutput("plot_correlogram", width="100%", height="80vh"), type=1, color="black"),
              div(style="margin-top:8px;margin-bottom:18px;", downloadButton("download_correlogram_plot", "Download Correlogram"))
            )
          )
        )
      )
    ),

    # ---- Create Report ----
    tabPanel("Create Report",
      fluidPage(
        fluidRow(
          column(4,
            div(class="ve-box",
              h2("Report settings", align="center"),
              checkboxGroupInput("report_sections", "Include in PDF report:",
                choices=c(
                  "Processing report"="processing",
                  "Input values table"="input_values",
                  "Summary statistics table"="summary_table",
                  "Data relationships preview (ggpairs)"="pairs_plot",
                  "Element maps (1 per page)"="element_maps",
                  "RGB map"="rgb_map",
                  "Cluster map + entropy map"="cluster_map",
                  "Cluster mean-composition table"="cluster_summary",
                  "Cluster standard-deviation table"="cluster_sd",
                  "Scatter plot"="scatter_plot",
                  "Ternary plot"="ternary_plot",
                  "Correlogram"="correlogram"
                ),
                selected=c("processing","input_values","summary_table","pairs_plot","element_maps",
                           "rgb_map","cluster_map","cluster_summary","cluster_sd","scatter_plot",
                           "ternary_plot","correlogram")
              ),
              selectInput("report_rgb_elements", "RGB elements for report", choices=NULL, multiple=TRUE),
              uiOutput("report_corr_group_ui"),
              tags$p("All sections are selected by default. The report is exported in A4 PDF format and uses the current application settings whenever possible.", class="ve-note"),
              downloadButton("download_pdf_report", "Generate PDF Report")
            )
          ),
          column(8,
            div(class="ve-box",
              h3(tags$b("Report preview")),
              tags$p("The report summarizes the loaded input tables, active processing choices, cluster analysis, map outputs, and analytical graphics currently available in the application.", class="ve-note"),
              tags$ul(
                tags$li("The processing report explains the active transform and lists the other transform options available in the current version."),
                tags$li("Cluster analysis pages describe PCA usage, k-means segmentation, post-processing actions, and the entropy-based pixel filtering strategy."),
                tags$li("Maps, scatter plots, ternary plots, and correlograms are exported based on the current state of the application.")
              )
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
            tags$li(tags$b("Number of clusters (k):"), " the number of mineral phases or compositional groups to identify. There is no automatic selection of k in this version — use geological knowledge, the entropy map, and the summary table to guide the choice. Typical geochemical maps require between 3 and 10 clusters."),
            tags$li(tags$b("Cluster colours:"), " customisable per cluster via hex code, RGB sliders, or colour picker. Colours are used consistently in the cluster map, scatter plot, ternary plot, and summary table."),
            tags$li(tags$b("Elements for clustering:"), " subset of active elements to include as clustering features. Excluding an element removes it from distance calculations but it remains available in analytical plots."),
            tags$li(tags$b("Invert map coordinates:"), " same convention as in Map Processing. Must match if you want cluster results to overlay correctly on element maps."),
            tags$li(tags$b("Use PCA instead of variables:"), " replaces the raw element features with principal components before running k-means. The number of PCs is chosen automatically to explain at least the specified cumulative variance (default 90%). PCA can improve clustering when elements are highly collinear or when dimensionality reduction is desirable to reduce noise.")
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
            tags$li(tags$b("Map downloads:"), " PDF (raster-embedded for maximum compatibility). The cluster ternary plot is exported as a self-contained HTML file to preserve interactivity."),
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
          h3("Reference"),
          p("If you use QMap \u2014 VisualEasier in published work, please cite:"),
          div(style="background:#f7f9fb; border:1px solid #d9e2ec; border-radius:6px; padding:12px 16px; font-style:italic;",
            "VisualEasier: An interactive tool for geochemical and mineral chemistry data visualization and analysis. Journal of the Geological Survey of Brazil."
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
    invisible(gc())
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
    processing_state(list(enabled=!identical(proj_method,"raw"), method=proj_method, selected_elements=proj_selected))
    sync_loaded_selectors(core, selected_elements=proj_selected, method_selected=proj_method)
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
      core <- input_data_core()
      validate(need(loaded_data_ready(core), "Please load data before saving a project."))
      saveRDS(list(
        version=APP_VERSION, saved_at=as.character(Sys.time()),
        sample_id=core$sample_id %||% "", decimal_separator=core$decimal_separator %||% decimal_sep(),
        map_width=core$map_width, map_height=core$map_height, elements=core$elements,
        file_names=core$summary$File, input_paths=core$input_paths %||% core$summary$DataPath,
        norms=core$norms, matrices=core$mats %||% core$matrices,
        processing_method=processing_state()$method %||% "raw",
        selected_elements=processing_state()$selected_elements %||% core$elements,
        clip_state=map_clip_state(),
        note="This project file stores only input matrices, metadata, selected variables, clipping settings, and processing settings. Rendered maps, figures, previews, and report outputs are not stored."
      ), file=file, compress=FALSE)
    }
  )

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
    if (identical(input$type, "elemental"))
      tagList(
        uiOutput("elemental_selector_ui"),
        selectInput("palette", "Choose color palette:", choices=c("turbo","viridis","plasma","magma","inferno"), selected="turbo"),
        selectInput("filter", "Image filter:", choices=common_filters, selected="None"),
        checkboxInput("invert_elemental_coords", "Invert map coordinates?", value=FALSE),
        radioButtons("legend_orientation", "Legend orientation:", choices=c("Horizontal"="horizontal","Vertical"="vertical"), selected="horizontal", inline=TRUE),
        actionButton("process_data", "Generate maps")
      )
    else if (identical(input$type, "rgb"))
      tagList(
        uiOutput("filtered_selector_ui"),
        selectInput("filter", "Image filter:", choices=common_filters, selected="None"),
        checkboxInput("invert_filtered_coords", "Invert map coordinates?", value=FALSE),
        radioButtons("legend_orientation", "Legend orientation:", choices=c("Horizontal"="horizontal","Vertical"="vertical"), selected="horizontal", inline=TRUE),
        actionButton("raster", "Generate map")
      )
    else
      tags$p("Choose Element Map or Ternary Map (RGB).", class="ve-note")
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
    tagList(hr(),
      if (identical(input$type, "rgb")) downloadButton("download_rgb", "Download map")
      else downloadButton("download_plot", "Download combined plot")
    )
  })

  output$map_processing_preview_ui <- renderUI({
    if (identical(input$type, "elemental"))
      withSpinner(uiOutput("graph_outputs"), type=1, color="black")
    else if (identical(input$type, "rgb"))
      fluidRow(column(12, withSpinner(plotOutput("plot_rgb", width="100%", height="72vh"), type=1, color="black")))
    else
      tags$p("Choose Element Map or Ternary Map (RGB).", class="ve-note")
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
      req(identical(input$type, "elemental"))
      core <- active_input_core()
      validate(need(loaded_data_ready(core), "Please load data first in the Input Data tab."))
      elems <- input$elemental_elements_selected
      if (is.null(elems) || length(elems) == 0) elems <- core$elements
      validate(need(length(elems) >= 1, "Please select at least one element to display."))
      validate(need(all(elems %in% core$elements), "Some selected elements are not available in the loaded dataset."))
      incProgress(0.15, detail="Preparing selected variables")
      mats <- unname(core$matrices[elems]); norms <- unname(core$norms[elems])
      legend_dir <- input$legend_orientation %||% "horizontal"
      guide_obj <- if (identical(legend_dir, "vertical"))
        guide_colorbar(direction="vertical", barwidth=1, barheight=15)
      else guide_colorbar(direction="horizontal", barwidth=15, barheight=1)
      incProgress(0.20, detail="Preparing selected variables")
      nr_mat <- nrow(mats[[1]]); nc_mat <- ncol(mats[[1]])
      show_axes <- !is.na(core$map_width) && !is.na(core$map_height) &&
                   core$map_width > 0 && core$map_height > 0
      # coord_equal(): y-units per x-unit keeps square pixels undistorted.
      plots <- lapply(seq_along(mats), function(i) {
        incProgress(0.70 / max(1, length(mats)), detail=paste("Rendering", elems[i]))
        display_mat <- apply_filter_to_matrix(compute_display_matrix(i, mats, norms), input$filter %||% "None")
        df <- matrix_to_df(display_mat, "value")
        df <- apply_cluster_filter_df(df, cluster_post_state(), map_filter_labels())
        validate(need(nrow(df) > 0, "No pixels remain after applying the selected cluster filter."))
        df <- maybe_invert_df(df, isTRUE(input$invert_elemental_coords))
        p <- ggplot(df, aes(x, y, fill=value)) +
          geom_raster() +
          coord_equal() +
          scale_fill_gradientn(colours=resolve_palette(input$palette), na.value="grey90", guide=guide_obj) +
          ggpubr::theme_pubr() +
          labs(title=paste0("Map ",i," (",elems[i],")"), fill=elems[i]) +
          theme(plot.title=element_text(hjust=0.5),
                legend.position=if (identical(legend_dir,"vertical")) "right" else "bottom")
        if (show_axes) {
          x_range <- range(df$x, na.rm=TRUE); y_range <- range(df$y, na.rm=TRUE)
          # When coordinates are inverted, x and y axes swap physical dimensions
          if (isTRUE(input$invert_elemental_coords)) {
            x_mm <- core$map_height; y_mm <- core$map_width
          } else {
            x_mm <- core$map_width;  y_mm <- core$map_height
          }
          p <- p +
            scale_x_continuous(name="mm", breaks=pretty(x_range),
              labels=scales::label_number(accuracy=0.1)(scales::rescale(pretty(x_range), to=c(0, x_mm)))) +
            scale_y_continuous(name="mm", breaks=pretty(y_range),
              labels=scales::label_number(accuracy=0.1)(scales::rescale(pretty(y_range), to=c(0, y_mm))))
        } else {
          p <- p + theme(axis.title=element_blank(), axis.text=element_blank(),
                         axis.ticks=element_blank(), axis.line=element_blank(),
                         panel.grid=element_blank())
        }
        add_scale_bar(p, build_scale_annotation(df, core$map_width, core$map_height))
      })
      incProgress(0.95, detail="Finalizing map layout")
      list(mats=mats, plots=plots,
           aspect_ratio=compute_aspect_ratio(nrow(mats[[1]]), ncol(mats[[1]]), isTRUE(input$invert_elemental_coords)),
           elements=elems, mode="elemental")
    })
  })

  pca_map_data <- reactive({ NULL })
  current_grid_plot_data <- reactive({ if (identical(input$type,"elemental")) elemental_data() else NULL })

  output$graph_outputs <- renderUI({
    dat <- current_grid_plot_data(); req(dat); plots <- dat$plots
    # Compute a CSS height that preserves the data aspect ratio within each card.
    # Each card fills half the viewport width (minus gap); height = width * (nr/nc).
    ar <- dat$aspect_ratio  # nr/nc (or nc/nr if inverted)
    card_css_height <- paste0("calc(min(46vh, 46vw * ", round(ar, 4), "))")
    div(class="ve-element-grid",
      lapply(seq_along(plots), function(i)
        div(class="ve-plot-card",
            plotOutput(outputId=paste0("element_plot_",i), width="100%", height=card_css_height))
      )
    )
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
      paste0(input$type,"_",paste(dat$elements,collapse="_"),"_",Sys.Date(),".pdf")
    },
    content = function(file) {
      dat <- current_grid_plot_data(); req(dat)
      n <- length(dat$plots); ncol_plot <- ceiling(sqrt(n)); nrow_plot <- ceiling(n / ncol_plot)
      base_width <- 8; base_height <- base_width * dat$aspect_ratio
      combined_plot <- if (n == 1) dat$plots[[1]] else wrap_plots(dat$plots, ncol=ncol_plot)
      save_plot_as_raster_pdf(combined_plot, file=file,
        width=base_width*ncol_plot, height=base_height*nrow_plot, dpi=220)
    }
  )

  output$filtered_selector_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    selectInput("filtered_rgb_elements", "Select 3 loaded elements (R, G, B):",
      choices=core$elements, selected=core$elements[seq_len(min(3,length(core$elements)))], multiple=TRUE)
  })

  filtered_plot_data <- eventReactive(input$raster, {
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
        apply_filter_to_matrix(mat, input$filter %||% "None")
      }
      incProgress(0.65, detail="Composing RGB image")
      rgb_img <- imappend(lapply(selected, function(s) as.cimg(make_channel(s))), "c")
      df <- cimg_to_df(rgb_img)
      df <- apply_cluster_filter_df(df, cluster_post_state(), map_filter_labels())
      validate(need(nrow(df) > 0, "No pixels remain after applying the selected cluster filter."))
      filter_label <- input$filter %||% "None"
      title <- paste("Ternary Map\n", filter_label, "filter applied")
      incProgress(0.85, detail="Rendering RGB map")
      p <- make_rgb_plot(df=df, title=title, labels=selected,
        map_width_mm=core$map_width, map_height_mm=core$map_height,
        invert_coords=isTRUE(input$invert_filtered_coords))
      list(plot=p, elements=selected, mode="rgb")
    })
  }, ignoreInit=TRUE)

  output$plot_rgb <- renderPlot({ req(filtered_plot_data()); filtered_plot_data()$plot })

  output$download_rgb <- downloadHandler(
    filename = function() {
      req(filtered_plot_data())
      paste0("RGB_",paste(filtered_plot_data()$elements,collapse="_"),"_",input$filter,"_",Sys.Date(),".pdf")
    },
    content = function(file) {
      dat <- filtered_plot_data(); req(dat)
      save_plot_as_raster_pdf(dat$plot, file=file, width=10, height=10*dat$aspect_ratio, dpi=220)
    }
  )

  # ---------------------------------------------------------------------------
  # Cluster Analysis
  # ---------------------------------------------------------------------------
  output$cluster_selector_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    selectInput("cluster_elements_selected", "Select loaded elements for clustering:",
      choices=core$elements, selected=core$elements[seq_len(min(3,length(core$elements)))], multiple=TRUE)
  })

  observeEvent(input$ncluster, {
    req(input$ncluster)
    cluster_color_values(toupper(cluster_palette(max(1, as.integer(input$ncluster)), "Okabe-Ito")))
  }, ignoreInit=FALSE)

  output$cluster_color_controls_ui <- renderUI({
    cols <- cluster_color_values(); req(length(cols) > 0)
    rows <- lapply(seq_along(cols), function(i) {
      current_hex <- toupper(cols[i]); current_rgb <- hex_to_rgb(current_hex)
      fluidRow(style="margin-bottom:8px;display:flex;align-items:end;",
        column(2, tags$label(paste0("Cluster ",i), style="margin-top:8px;font-weight:600;")),
        column(2, colourpicker::colourInput(paste0("cluster_col_",i), NULL, value=current_hex,
                                            showColour="background", allowTransparent=FALSE, width="54px")),
        column(4, div(style="display:flex;gap:8px;align-items:flex-end;",
          div(tags$label("R",style="font-weight:600;display:block;margin-bottom:4px;"),
              numericInput(paste0("cluster_r_",i), NULL, value=current_rgb["r"], min=0, max=255, width="54px")),
          div(tags$label("G",style="font-weight:600;display:block;margin-bottom:4px;"),
              numericInput(paste0("cluster_g_",i), NULL, value=current_rgb["g"], min=0, max=255, width="54px")),
          div(tags$label("B",style="font-weight:600;display:block;margin-bottom:4px;"),
              numericInput(paste0("cluster_b_",i), NULL, value=current_rgb["b"], min=0, max=255, width="72px"))
        )),
        column(4, div(tags$label("HEX",style="font-weight:600;display:block;margin-bottom:4px;"),
                      textInput(paste0("cluster_hex_",i), NULL, value=current_hex, width="54px")))
      )
    })
    tagList(tags$label("Cluster colors:"), do.call(tagList, rows))
  })

  output$cluster_visible_ui <- renderUI({
    dat <- cluster_post_state(); if (is.null(dat)) return(NULL)
    choices <- setNames(sort(dat$label_map$cluster_label), sort(dat$label_map$cluster_label))
    valid_selected <- intersect(cluster_visible_state(), unname(dat$label_map$cluster_label))
    if (length(valid_selected) == 0) valid_selected <- sort(unname(dat$label_map$cluster_label))
    checkboxGroupInput("cluster_visible_labels", "Visible clusters in map:",
      choices=choices, selected=valid_selected, inline=TRUE)
  })

  output$cluster_pca_controls_ui <- renderUI({
    if (!isTRUE(input$use_pca_cluster)) return(NULL)
    sliderInput("cluster_pca_variance", "Minimum cumulative variance for component selection",
      min=0.5, max=0.99, value=0.9, step=0.01)
  })

  output$cluster_ambiguity_controls_ui <- renderUI({
    if (!isTRUE(input$enable_ambiguity_filter)) return(NULL)
    sliderInput("entropy_rank_percent", "Maximum Entropy rank retained in statistics (%)",
      min=30, max=100, value=80, step=1, post="%")
  })

  resolve_cluster_hex <- function(idx, fallback_hex) {
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
    col_in <- input[[paste0("cluster_col_",idx)]]
    if (!is.null(col_in)) {
      col_try <- toupper(trimws(as.character(col_in)))
      if (grepl("^#[0-9A-F]{6}$", col_try)) return(col_try)
    }
    toupper(fallback_hex)
  }

  cluster_results <- eventReactive(input$cluster, {
    withProgress(message="Calculating clusters", value=0, {
      core <- active_input_core()
      validate(need(loaded_data_ready(core), "Please load data first in the Input Data tab."))
      elems <- input$cluster_elements_selected
      validate(need(length(elems) >= 2, "Please select at least two loaded elements for clustering."))
      validate(need(all(elems %in% core$elements), "Some selected cluster elements are not available in the loaded dataset."))
      validate(need(is.numeric(input$ncluster) && input$ncluster >= 2, "Please choose at least 2 clusters."))

      base_cols <- toupper(cluster_palette(input$ncluster, "Okabe-Ito"))
      current_cols <- cluster_color_values()
      if (length(current_cols) != input$ncluster) current_cols <- base_cols
      selected_cols <- toupper(vapply(seq_len(input$ncluster), function(i) resolve_cluster_hex(i, current_cols[i]), character(1)))
      names(selected_cols) <- as.character(seq_len(input$ncluster))

      incProgress(0.15, detail="Preparing selected variables")
      mats <- unname(core$matrices[elems]); norms <- unname(core$norms[elems])
      merged <- matrix_list_to_merged_df(mats, elems)
      cluster_data <- merged %>% select(all_of(elems))
      valid_rows <- complete.cases(cluster_data)
      cluster_data <- cluster_data[valid_rows, , drop=FALSE]
      coords <- merged[valid_rows, c("x","y")]
      validate(need(nrow(cluster_data) >= input$ncluster,
                    "The number of valid pixels must be greater than or equal to the number of clusters."))

      cluster_data_for_pct <- apply_global_normalization(mats, norms)
      merged_pct <- matrix_list_to_merged_df(cluster_data_for_pct, elems)
      cluster_pct_only <- merged_pct %>% select(all_of(elems))
      cluster_pct_only <- cluster_pct_only[valid_rows, , drop=FALSE]
      px_sum <- rowSums(cluster_pct_only, na.rm=TRUE); px_sum[px_sum == 0] <- NA_real_
      cluster_data_norm <- sweep(cluster_pct_only, 1, px_sum, FUN="/") * 100

      incProgress(0.35, detail="Scaling data")
      scaled <- scale(cluster_data, center=TRUE, scale=TRUE); scaled[!is.finite(scaled)] <- 0

      cluster_input <- scaled; pca_info <- NULL
      incProgress(0.50, detail="Computing PCA support scores")
      pca_model <- prcomp(scaled, center=FALSE, scale.=FALSE)
      pc_scores_full <- as.data.frame(pca_model$x)
      names(pc_scores_full) <- paste0("PC", seq_len(ncol(pc_scores_full)))
      var_explained <- (pca_model$sdev^2) / sum(pca_model$sdev^2)
      cum_var <- cumsum(var_explained)

      if (isTRUE(input$use_pca_cluster)) {
        target_var <- suppressWarnings(as.numeric(input$cluster_pca_variance))
        if (!is.finite(target_var)) target_var <- 0.90
        npc <- which(cum_var >= target_var)[1]
        if (!is.finite(npc) || is.na(npc)) npc <- ncol(pca_model$x)
        npc <- max(1, min(npc, ncol(pca_model$x)))
        cluster_input <- pca_model$x[, seq_len(npc), drop=FALSE]
        pca_info <- list(n_components=npc, cumulative_variance=cum_var[npc])
      }

      incProgress(0.65, detail="Running k-means")
      set.seed(123)
      km <- kmeans(cluster_input, centers=input$ncluster, nstart=10, iter.max=100, algorithm="Lloyd")

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
        pc_scores=dplyr::bind_cols(result_df %>% dplyr::select(x,y,cluster_id,cluster_label),
                                   pc_scores_full, box_wide)
      )
    })
  }, ignoreInit=TRUE)

  observeEvent(cluster_results(), {
    cluster_post_state(cluster_results())
    dat <- cluster_results()
    if (!is.null(dat) && !is.null(dat$label_map))
      cluster_visible_state(sort(unname(dat$label_map$cluster_label)))
  })

  finalize_cluster_post_state <- function(res, box_wide, colors, dat, ambiguity_enabled, entropy_rank_percent,
                                          erosion_enabled=FALSE, erosion_pixels=5) {
    active_ids <- unique(as.character(res$cluster_id))
    active_ids <- active_ids[order(suppressWarnings(as.numeric(active_ids)), active_ids)]
    full_ambiguity_df <- compute_cluster_ambiguity(res, box_wide, dat$elems)
    stats_res <- res; stats_box <- box_wide; erosion_applied <- FALSE
    if (isTRUE(erosion_enabled)) {
      eroded <- apply_cluster_border_erosion(res, box_wide, erosion_pixels %||% 5)
      validate(need(nrow(eroded$result_df) > 0, "Border erosion removed all pixels. Reduce the erosion distance."))
      stats_res <- eroded$result_df; stats_box <- eroded$box_wide; erosion_applied <- TRUE
    }
    stats_parts <- build_stats_from_filtered(stats_res, stats_box, dat$elems, colors,
                     entropy_rank_percent=if (ambiguity_enabled) entropy_rank_percent else NULL)
    n_act <- length(stats_parts$active_ids)
    out <- list(
      result_df=res, box_wide=box_wide,
      box_long=dplyr::bind_cols(res, box_wide) %>% tidyr::pivot_longer(cols=dplyr::all_of(dat$elems), names_to="Element", values_to="Value"),
      summary_table=stats_parts$summary_table, ambiguity_df=full_ambiguity_df,
      entropy_rank_percent=if (ambiguity_enabled) entropy_rank_percent else NA_real_,
      ambiguity_enabled=isTRUE(ambiguity_enabled),
      erosion_enabled=isTRUE(erosion_enabled),
      erosion_pixels=if (isTRUE(erosion_enabled)) suppressWarnings(as.integer(erosion_pixels %||% 5)) else NA_integer_,
      erosion_applied=erosion_applied, aspect_ratio=dat$aspect_ratio %||% 1, pca_info=dat$pca_info,
      k=length(active_ids), facet_ncol=ceiling(sqrt(max(1,n_act))),
      facet_nrow=ceiling(max(1,n_act)/max(1,ceiling(sqrt(max(1,n_act))))),
      cluster_colors=colors, cluster_colors_named=stats_parts$cluster_colors_named,
      active_ids=stats_parts$active_ids, label_map=stats_parts$label_map, elems=dat$elems,
      filtered_n=stats_parts$filtered_n, total_n=stats_parts$total_n,
      stats_box_long=stats_parts$stats_box_long,
      map_width=dat$map_width, map_height=dat$map_height,
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
      erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 5)
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
      updateTextInput(session, paste0("cluster_alias_",cid), value=current_label)
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
    withProgress(message="Applying cluster labels", value=0, {
      incProgress(0.20, detail="Updating cluster names")
      res <- .apply_alias(dat$result_df, dat)
      ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
      entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
      incProgress(0.50, detail="Recalculating entropy and statistics")
      finalize_cluster_post_state(res, dat$box_wide, dat$cluster_colors, dat, ambiguity_enabled, entropy_rank_percent,
        erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 5)
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

  output$cluster_postprocess_ui <- renderUI({
    dat <- cluster_post_state()
    if (is.null(dat)) return(tags$p("Generate a cluster map first to enable post-processing controls."))
    cluster_choices <- setNames(dat$active_ids,
      dat$label_map$cluster_label[match(dat$active_ids, dat$label_map$cluster_id)])
    cluster_choices <- cluster_choices[order(names(cluster_choices))]
    selected_sources <- intersect(as.character(merge_sources_state()), unname(cluster_choices))
    target_choices <- cluster_choices[cluster_choices %in% selected_sources]
    current_target <- merge_target_state() %||% ""
    selected_target <- if (nzchar(current_target) && current_target %in% unname(target_choices)) current_target else ""
    tagList(
      tags$label("Rename Cluster"),
      tags$p("You can guess a mineral name or group of minerals.", class="ve-note"),
      lapply(seq_along(dat$active_ids), function(i) {
        cid <- dat$active_ids[i]
        current_label <- dat$label_map$cluster_label[match(cid, dat$label_map$cluster_id)]
        textInput(paste0("cluster_alias_",cid), label=paste0("Cluster ",cid), value=current_label, width="100%")
      }),
      fluidRow(column(6,actionButton("apply_cluster_rename","Apply",width="100%")),
               column(6,actionButton("reset_cluster_rename","Reset",width="100%"))),
      tags$hr(),
      selectInput("merge_source_clusters","Cluster Merger: merge these clusters",
        choices=cluster_choices, selected=selected_sources, multiple=TRUE),
      if (length(target_choices) > 0)
        selectInput("merge_target_cluster","Merge target cluster",choices=target_choices,selected=selected_target),
      fluidRow(column(6,actionButton("apply_cluster_merge","Apply",width="100%")),
               column(6,actionButton("reset_cluster_merge","Reset",width="100%"))),
      tags$hr(),
      selectInput("delete_clusters","Delete Cluster",choices=cluster_choices,selected=character(0),multiple=TRUE),
      fluidRow(column(6,actionButton("apply_cluster_delete","Apply",width="100%")),
               column(6,actionButton("reset_cluster_delete","Reset",width="100%"))),
      tags$hr(),
      checkboxInput("enable_cluster_erosion","Erode cluster borders only for summary statistics",value=isTRUE(dat$erosion_enabled)),
      numericInput("cluster_erosion_pixels","Statistics-only border erosion distance (pixels)",value=dat$erosion_pixels %||% 5,min=1,step=1),
      tags$p("Erosion masks border pixels only when recalculating means and summary statistics; it does not change the displayed cluster map.",class="ve-note"),
      fluidRow(column(6,actionButton("apply_cluster_erosion","Apply",width="100%")),
               column(6,actionButton("reset_cluster_erosion","Reset",width="100%"))),
      tags$hr(),
      selectInput("cluster_mode_window","Moving mode filter window",choices=c("3 x 3"=3,"5 x 5"=5,"8 x 8"=8),selected=3),
      tags$p("The moving mode filter replaces isolated pixels by the local modal cluster class, reducing salt-and-pepper texture.",class="ve-note"),
      fluidRow(column(6,actionButton("apply_cluster_mode_filter","Apply",width="100%")),
               column(6,actionButton("reset_cluster_mode_filter","Reset",width="100%")))
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
      erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 5)
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
      erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 5)
    updateSelectInput(session,"delete_clusters",selected=character(0))
  })

  observeEvent(input$reset_cluster_erosion, {
    dat <- cluster_post_state()
    updateCheckboxInput(session,"enable_cluster_erosion",value=FALSE)
    updateNumericInput(session,"cluster_erosion_pixels",value=5)
    if (!is.null(dat)) {
      withProgress(message="Recalculating cluster statistics", value=0, {
        incProgress(0.35, detail="Removing the statistics-only erosion mask")
        ambiguity_enabled <- isTRUE(input$enable_ambiguity_filter)
        entropy_rank_percent <- resolve_entropy_rank_percent(ambiguity_enabled, input$entropy_rank_percent, default=100)
        incProgress(0.70, detail="Updating summary table")
        finalize_cluster_post_state(dat$result_df, dat$box_wide, dat$cluster_colors, dat,
          ambiguity_enabled, entropy_rank_percent, erosion_enabled=FALSE, erosion_pixels=5)
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
        erosion_pixels=input$cluster_erosion_pixels %||% 5)
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
        erosion_enabled=isTRUE(dat$erosion_enabled), erosion_pixels=dat$erosion_pixels %||% input$cluster_erosion_pixels %||% 5)
    })
  })

  output$plot_cluster <- renderPlot({
    dat <- cluster_post_state(); req(dat)
    visible_labels <- cluster_visible_state()
    if (length(visible_labels) == 0) visible_labels <- unname(dat$label_map$cluster_label)
    plot_df <- dat$result_df %>% filter(cluster_label %in% visible_labels)
    validate(need(nrow(plot_df) > 0, "Select at least one cluster to display in the map."))
    make_cluster_entropy_plot(df=plot_df, fill_var="cluster_label",
      title=paste0("K-means clustering (k = ",dat$k,
                   if (!is.null(dat$pca_info)) paste0(", PCA ",dat$pca_info$n_components," PCs / ",
                                                      round(100*dat$pca_info$cumulative_variance,1),"% var") else "",")" ),
      aspect_ratio=dat$aspect_ratio %||% 1,
      fill_scale=scale_fill_manual(values=dat$cluster_colors_named[names(dat$cluster_colors_named) %in% unique(plot_df$cluster_label)],
                                   drop=FALSE, name="Cluster"),
      map_width_mm=dat$map_width, map_height_mm=dat$map_height)
  })

  output$plot_cluster_ambiguity <- renderPlot({
    dat <- cluster_post_state(); req(dat)
    amb_df <- dat$ambiguity_df
    visible_labels <- cluster_visible_state()
    if (length(visible_labels) == 0) visible_labels <- unname(dat$label_map$cluster_label)
    amb_df <- amb_df %>% filter(cluster_label %in% visible_labels); req(nrow(amb_df) > 0)
    make_cluster_entropy_plot(df=amb_df, fill_var="ambiguity",
      title=if (isTRUE(dat$ambiguity_enabled) && is.finite(dat$entropy_rank_percent))
              paste0("Normalized Shannon entropy map, IDW kernel (retained lowest-entropy pixels per cluster = ",
                     round(dat$entropy_rank_percent,0),"%)")
            else "Normalized Shannon entropy map (IDW)",
      aspect_ratio=dat$aspect_ratio %||% 1,
      fill_scale=scale_fill_viridis_c(option="magma",
                   limits=c(0,max(0.01,max(amb_df$ambiguity,na.rm=TRUE))), name="Entropy"),
      map_width_mm=dat$map_width, map_height_mm=dat$map_height)
  })

  # ---------------------------------------------------------------------------
  # Cluster Summary Table
  # ---------------------------------------------------------------------------
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
    req(cluster_post_state()); df <- cluster_post_state()$summary_table
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
  }, sanitize.text.function=function(x) x)

  output$download_cluster_table <- downloadHandler(
    filename=function() "cluster_table.csv",
    content=function(file) write.csv2(cluster_post_state()$summary_table, file, row.names=FALSE)
  )

  output$download_cluster_map <- downloadHandler(
    filename=function() "Cluster_Map.pdf",
    content=function(file) {
      dat <- cluster_post_state(); req(dat)
      visible_labels <- cluster_visible_state()
      if (length(visible_labels) == 0) visible_labels <- unname(dat$label_map$cluster_label)
      plot_df <- dat$result_df %>% filter(cluster_label %in% visible_labels)
      amb_df  <- dat$ambiguity_df %>% filter(cluster_label %in% visible_labels)
      p1 <- ggplot(plot_df, aes(x=x,y=y,fill=cluster_label)) +
        geom_raster() + coord_equal() +
        scale_fill_manual(values=dat$cluster_colors_named[names(dat$cluster_colors_named) %in% unique(plot_df$cluster_label)], drop=FALSE) +
        ggpubr::theme_pubr() +
        labs(title=paste0("K-means clustering (k = ",dat$k,
                          if (!is.null(dat$pca_info)) paste0(", PCA ",dat$pca_info$n_components," PCs / ",
                                                             round(100*dat$pca_info$cumulative_variance,1),"% var") else "",")"),
             fill="Cluster") +
        theme(plot.title=element_text(hjust=0.5),axis.title=element_blank(),axis.text=element_blank(),
              axis.ticks=element_blank(),legend.position="right")
      p2 <- ggplot(amb_df, aes(x=x,y=y,fill=ambiguity)) +
        geom_raster() + coord_equal() +
        scale_fill_viridis_c(option="magma",limits=c(0,max(0.01,max(amb_df$ambiguity,na.rm=TRUE)))) +
        ggpubr::theme_pubr() +
        labs(title=if (isTRUE(dat$ambiguity_enabled) && is.finite(dat$entropy_rank_percent))
                     paste0("Normalized Shannon entropy map (retained lowest-entropy pixels per cluster = ",
                            round(dat$entropy_rank_percent,0),"%)") else "Normalized Shannon entropy map",
             fill="Entropy") +
        theme(plot.title=element_text(hjust=0.5),axis.title=element_blank(),axis.text=element_blank(),
              axis.ticks=element_blank(),legend.position="right")
      grDevices::cairo_pdf(file=file,width=10,height=8)
      print(p1); print(p2); grDevices::dev.off()
      invisible(NULL)
    }
  )

  # ---------------------------------------------------------------------------
  # Data Analysis
  # ---------------------------------------------------------------------------
  output$data_analysis_sampling_controls_ui <- renderUI({
    tagList(div(class="ve-box",
      h4("Sampling settings",align="center"),
      sliderInput("data_analysis_sample_pct","Sampled points used in plots (%):",
        min=1,max=100,value=suppressWarnings(as.numeric(input$data_analysis_sample_pct %||% 1)),step=1),
      tags$p("Scatter and ternary plots use random sampling without replacement. When clusters are available, sampling preserves the original cluster proportions. The application caps the plotted points at 100,000 for stability.",class="ve-note")
    ))
  })

  output$cluster_pc_scatter_controls_ui <- renderUI({
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    pc_df <- dat$pc_scores; req(!is.null(pc_df), ncol(pc_df) >= 3)
    pc_names <- names(pc_df)[grepl("^PC",names(pc_df))]
    axis_choices <- c(core$elements, pc_names)
    xsel <- input$cluster_pc_x %||% axis_choices[1]
    xsel <- if (xsel %in% axis_choices) xsel else axis_choices[1]
    y_default <- if (length(axis_choices)>1) axis_choices[2] else axis_choices[1]
    ysel <- input$cluster_pc_y %||% y_default
    ysel <- if (ysel %in% axis_choices) ysel else y_default
    tagList(
      fluidRow(
        column(4, selectInput("cluster_pc_color_mode","Color data by",
          choices=c("Input variables"="variable","Clusters"="cluster","Entropy"="entropy"),
          selected=input$cluster_pc_color_mode %||% "cluster")),
        column(4, selectInput("cluster_pc_x","X axis",choices=axis_choices,selected=xsel)),
        column(4, selectInput("cluster_pc_y","Y axis",choices=axis_choices,selected=ysel))
      ),
      if (identical(input$cluster_pc_color_mode %||% "cluster","variable"))
        selectInput("cluster_pc_color_variable","Variable",choices=dat$elems,
                    selected=input$cluster_pc_color_variable %||% dat$elems[1]),
      actionButton("generate_scatter_plot","Generate map")
    )
  })

  cluster_pc_scatter_ggplot <- eventReactive(input$generate_scatter_plot, {
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    pcs <- dat$pc_scores; req(!is.null(pcs), nrow(pcs) > 0)
    pc_names <- names(pcs)[grepl("^PC",names(pcs))]
    axis_choices <- c(core$elements, pc_names)
    xvar <- input$cluster_pc_x %||% axis_choices[1]
    yvar <- input$cluster_pc_y %||% axis_choices[min(2,length(axis_choices))]
    validate(need(xvar %in% axis_choices && yvar %in% axis_choices, "Choose valid variables or principal components."))

    plot_df <- pcs
    if (xvar %in% core$elements && !(xvar %in% names(plot_df))) {
      base_x <- build_analysis_dataframe(core, dat, use_raw=FALSE) %>% dplyr::select(x,y,dplyr::all_of(xvar))
      plot_df <- dplyr::left_join(plot_df, base_x, by=c("x","y"))
    }
    if (yvar %in% core$elements && !(yvar %in% names(plot_df))) {
      base_y <- build_analysis_dataframe(core, dat, use_raw=FALSE) %>% dplyr::select(x,y,dplyr::all_of(yvar))
      plot_df <- dplyr::left_join(plot_df, base_y, by=c("x","y"))
    }

    samp_pct <- max(5, min(100, suppressWarnings(as.numeric(input$data_analysis_sample_pct %||% 1))))
    plot_df <- sample_rows_analysis(plot_df, frac=samp_pct/100, max_n=100000, cluster_col="cluster_label")
    color_mode <- input$cluster_pc_color_mode %||% "cluster"

    p <- switch(color_mode,
      cluster = ggplot(plot_df, aes(x=.data[[xvar]],y=.data[[yvar]],color=cluster_label,text=cluster_label)) +
                geom_point(alpha=0.65,size=1.5) +
                scale_color_manual(values=dat$cluster_colors_named,drop=FALSE) + labs(color="Cluster"),
      entropy = {
        plot_df <- plot_df %>% dplyr::left_join(dat$ambiguity_df %>% dplyr::select(x,y,ambiguity),by=c("x","y"))
        ggplot(plot_df,aes(x=.data[[xvar]],y=.data[[yvar]],color=ambiguity,text=cluster_label)) +
          geom_point(alpha=0.65,size=1.5) + scale_color_viridis_c(option="magma") + labs(color="Entropy")
      },
      {
        var_name <- input$cluster_pc_color_variable %||% dat$elems[1]
        validate(need(var_name %in% names(plot_df),"Choose a valid input variable for coloring."))
        ggplot(plot_df,aes(x=.data[[xvar]],y=.data[[yvar]],color=.data[[var_name]],text=cluster_label)) +
          geom_point(alpha=0.65,size=1.5) + scale_color_viridis_c(option="viridis") + labs(color=var_name)
      }
    )
    p + theme_minimal() + labs(title="Scatter Plot",x=xvar,y=yvar) +
      theme(legend.position="right",plot.title=element_text(hjust=0.5))
  })

  output$plot_cluster_pc_scatter <- renderPlotly({
    validate(need(input$generate_scatter_plot > 0, "Choose variables and click Generate map to draw the scatter plot."))
    plotly::ggplotly(cluster_pc_scatter_ggplot(), tooltip=c("x","y","colour","text"))
  })

  output$download_cluster_pca_plot <- downloadHandler(
    filename=function() "Cluster_PCA_Plot.pdf",
    content=function(file) save_plot_as_raster_pdf(cluster_pc_scatter_ggplot(),file=file,width=10,height=7,dpi=220)
  )

  output$cluster_pc_ternary_controls_ui <- renderUI({
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    pc_df <- dat$pc_scores; pc_names <- names(pc_df)[grepl("^PC",names(pc_df))]
    axis_choices <- c(core$elements, pc_names)
    validate(need(length(axis_choices) >= 3,"At least three variables/PCs are required for the ternary plot."))
    a_sel <- input$cluster_ternary_a %||% axis_choices[1]
    b_choices <- setdiff(axis_choices, a_sel); b_sel <- input$cluster_ternary_b %||% b_choices[1]
    c_choices <- setdiff(axis_choices, c(a_sel,b_sel)); c_sel <- input$cluster_ternary_c %||% c_choices[1]
    tagList(
      fluidRow(
        column(3,selectInput("cluster_ternary_color_mode","Color data by",
          choices=c("Input variables"="variable","Clusters"="cluster","Entropy"="entropy"),
          selected=input$cluster_ternary_color_mode %||% "cluster")),
        column(3,selectInput("cluster_ternary_a","Axis A",choices=axis_choices,selected=a_sel)),
        column(3,selectInput("cluster_ternary_b","Axis B",choices=b_choices,selected=b_sel)),
        column(3,selectInput("cluster_ternary_c","Axis C",choices=c_choices,selected=c_sel))
      ),
      if (identical(input$cluster_ternary_color_mode %||% "cluster","variable"))
        selectInput("cluster_ternary_color_variable","Variable for color",choices=core$elements,
                    selected=input$cluster_ternary_color_variable %||% core$elements[1]),
      actionButton("generate_ternary_plot","Generate map")
    )
  })

  cluster_pc_ternary_plotly <- eventReactive(input$generate_ternary_plot, {
    dat <- cluster_post_state(); core <- active_input_core()
    req(dat, loaded_data_ready(core))
    plot_df <- dat$pc_scores; req(nrow(plot_df) > 0)
    avar <- input$cluster_ternary_a; bvar <- input$cluster_ternary_b; cvar <- input$cluster_ternary_c
    validate(need(length(unique(c(avar,bvar,cvar))) == 3,"Choose three different variables or principal components."))

    vals <- cbind(resolve_ternary_axis(avar,plot_df,core,dat),
                  resolve_ternary_axis(bvar,plot_df,core,dat),
                  resolve_ternary_axis(cvar,plot_df,core,dat))
    vals <- abs(vals); rs <- rowSums(vals,na.rm=TRUE); rs[!is.finite(rs)|rs<=0] <- 1
    tern <- sweep(vals,1,rs,"/")
    plot_df$a <- tern[,1]; plot_df$b <- tern[,2]; plot_df$c <- tern[,3]

    samp_pct <- max(5,min(100,suppressWarnings(as.numeric(input$data_analysis_sample_pct %||% 1))))
    plot_df <- sample_rows_analysis(plot_df,frac=samp_pct/100,max_n=100000,cluster_col="cluster_label")
    color_mode <- input$cluster_ternary_color_mode %||% "cluster"

    plt <- if (identical(color_mode,"cluster")) {
      plotly::plot_ly(plot_df,type="scatterternary",mode="markers",a=~a,b=~b,c=~c,
        color=~cluster_label,colors=dat$cluster_colors_named,text=~cluster_label,
        hovertemplate=paste0("Cluster: %{text}<br>",avar,": %{a:.3f}<br>",bvar,": %{b:.3f}<br>",cvar,": %{c:.3f}<extra></extra>"))
    } else if (identical(color_mode,"entropy")) {
      plot_df <- plot_df %>% dplyr::left_join(dat$ambiguity_df %>% dplyr::select(x,y,ambiguity),by=c("x","y"))
      plotly::plot_ly(plot_df,type="scatterternary",mode="markers",a=~a,b=~b,c=~c,
        color=~ambiguity,colors=viridisLite::magma(100),text=~cluster_label,
        hovertemplate=paste0("Cluster: %{text}<br>Entropy: %{marker.color:.3f}<extra></extra>"))
    } else {
      var_name <- input$cluster_ternary_color_variable %||% core$elements[1]
      if (!var_name %in% names(plot_df)) {
        base_df <- build_analysis_dataframe(core,dat,use_raw=FALSE)
        plot_df <- dplyr::left_join(plot_df,base_df %>% dplyr::select(x,y,dplyr::all_of(var_name)),by=c("x","y"))
      }
      plotly::plot_ly(plot_df,type="scatterternary",mode="markers",a=~a,b=~b,c=~c,
        color=plot_df[[var_name]],colors=viridisLite::viridis(100),text=~cluster_label,
        hovertemplate=paste0("Cluster: %{text}<br>",var_name,": %{marker.color:.3f}<extra></extra>"))
    }
    plt %>% plotly::layout(ternary=list(aaxis=list(title=avar),baxis=list(title=bvar),caxis=list(title=cvar)))
  })

  output$plot_cluster_pc_ternary <- renderPlotly({
    validate(need(input$generate_ternary_plot > 0,"Choose variables and click Generate map to draw the ternary plot."))
    cluster_pc_ternary_plotly()
  })

  output$download_cluster_ternary_plot <- downloadHandler(
    filename=function() "Cluster_Ternary_Plot.html",
    content=function(file) htmlwidgets::saveWidget(as_widget(cluster_pc_ternary_plotly()),file=file,selfcontained=TRUE)
  )

  output$correlogram_controls_ui <- renderUI({
    core <- active_input_core(); req(loaded_data_ready(core))
    dat <- cluster_post_state()
    cluster_choices <- if (!is.null(dat) && !is.null(dat$label_map) && nrow(dat$label_map) > 0)
      c("All samples",sort(dat$label_map$cluster_label)) else "All samples"
    selected_vars <- intersect(input$corr_variables %||% core$elements, core$elements)
    if (length(selected_vars) < 2) selected_vars <- head(core$elements, min(2,length(core$elements)))
    tagList(
      fluidRow(
        column(3,selectInput("corr_method","Correlation type",choices=c("Spearman"="spearman","Pearson"="pearson"),selected=input$corr_method %||% "spearman")),
        column(3,selectInput("corr_data_basis","Data basis",choices=c("Active transformed data"="processed","Raw input data"="raw"),selected=input$corr_data_basis %||% "processed")),
        column(3,selectInput("corr_group","Samples used",choices=cluster_choices,selected=input$corr_group %||% "All samples")),
        column(3,radioButtons("corr_legend_orientation","Legend",choices=c("Vertical"="vertical","Horizontal"="horizontal"),selected=input$corr_legend_orientation %||% "vertical",inline=TRUE))
      ),
      selectizeInput("corr_variables","Variables used in correlogram",choices=core$elements,selected=selected_vars,
        multiple=TRUE,options=list(plugins=list("remove_button"))),
      actionButton("generate_correlogram_plot","Generate correlogram")
    )
  })

  build_correlogram_data <- function() {
    core <- active_input_core(); req(loaded_data_ready(core))
    dat <- cluster_post_state()
    use_raw <- identical(input$corr_data_basis %||% "processed","raw")
    df <- build_analysis_dataframe(core, dat, use_raw=use_raw); req(nrow(df) > 0)
    grp <- input$corr_group %||% "All samples"
    if (!identical(grp,"All samples") && "cluster_label" %in% names(df))
      df <- df %>% dplyr::filter(cluster_label == grp)
    vars <- intersect(input$corr_variables %||% core$elements, core$elements)
    validate(need(length(vars) >= 2,"Choose at least two variables for the correlogram."))
    df <- df %>% dplyr::select(dplyr::all_of(vars))
    df <- df[, colSums(is.finite(as.matrix(df))) > 1, drop=FALSE]
    validate(need(ncol(df) >= 2,"At least two selected variables with finite values are required for the correlogram."))
    as.data.frame(df)
  }

  correlogram_data <- eventReactive(input$generate_correlogram_plot, {
    build_correlogram_data()
  }, ignoreInit=TRUE)

  output$plot_correlogram <- renderPlot({
    validate(need(input$generate_correlogram_plot > 0,"Choose variables and click Generate correlogram to draw the correlogram."))
    df <- correlogram_data()
    cor_mat <- suppressWarnings(cor(df,use="pairwise.complete.obs",method=input$corr_method %||% "spearman"))
    render_corrplot(cor_mat, orientation=input$corr_legend_orientation %||% "vertical")
  })

  output$download_correlogram_plot <- downloadHandler(
    filename=function() "Correlogram.pdf",
    content=function(file) {
      df <- if ((input$generate_correlogram_plot %||% 0) > 0) correlogram_data() else build_correlogram_data()
      cor_mat <- suppressWarnings(cor(df,use="pairwise.complete.obs",method=input$corr_method %||% "spearman"))
      grDevices::cairo_pdf(file=file,width=10,height=8)
      on.exit(grDevices::dev.off(),add=TRUE)
      render_corrplot(cor_mat,orientation=input$corr_legend_orientation %||% "vertical")
    }
  )

  # ---------------------------------------------------------------------------
  # Report
  # ---------------------------------------------------------------------------
  observe({
    core <- active_input_core(); req(loaded_data_ready(core))
    updateSelectInput(session,"report_rgb_elements",choices=core$elements,
                      selected=head(core$elements,min(3,length(core$elements))))
  })

  output$report_corr_group_ui <- renderUI({
    dat <- cluster_post_state()
    choices <- if (!is.null(dat) && !is.null(dat$label_map) && nrow(dat$label_map) > 0)
      c("All samples", dat$label_map$cluster_label) else "All samples"
    selectInput("report_corr_group","Correlogram samples for report",choices=choices,
                selected=input$report_corr_group %||% "All samples")
  })

  output$download_pdf_report <- downloadHandler(
    filename=function() {
      core <- active_input_core()
      paste0(core$sample_id %||% "sample","_report_",Sys.Date(),".pdf")
    },
    content=function(file) {
      core <- active_input_core(); req(loaded_data_ready(core))
      sections <- input$report_sections %||% character(0)
      dat <- cluster_post_state()
      has_norm <- any(vapply(core$norms %||% list(), function(x) isTRUE(is.finite(x) && !is.na(x) && x > 0), logical(1)))
      active_method <- core$processing_method %||% processing_state()$method %||% default_processing_method(core)
      method_tbl <- processing_method_catalog(has_norm)

      grDevices::cairo_pdf(file=file, width=8.27, height=11.69)
      on.exit(grDevices::dev.off(), add=TRUE)

      withProgress(message="Generating PDF report...", value=0, {
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
          draw_report_text_page(title="Processing Report", subtitle="Current settings and methodological summary",
            paragraphs=c(
              paste0("The current project contains ",length(core$elements)," active variables and matrices with dimensions ",
                     paste(core$dims,collapse=" x "),". The sample identifier is '",core$sample_id %||% "not defined","'."),
              processing_txt, cluster_txt, entropy_txt))
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

        if ("pairs_plot" %in% sections && isTRUE(processing_state()$applied)) {
          incProgress(0.08, detail="Rendering relationships preview")
          draw_ggplot_raster_page(build_processing_pairs_plot(core,max_n=1500,report_mode=TRUE)+ggtitle("Data Relationships Preview (ggpairs)"),width=8,height=10,dpi=110)
        }

        if ("element_maps" %in% sections) {
          incProgress(0.10, detail="Rendering element maps")
          elem_data <- tryCatch(elemental_data(), error=function(e) NULL)
          if (!is.null(elem_data) && length(elem_data$plots) > 0)
            for (p in elem_data$plots) draw_ggplot_raster_page(p,width=8,height=10,dpi=130)
        }

        if ("rgb_map" %in% sections) {
          incProgress(0.07, detail="Rendering RGB map")
          rgb_dat <- tryCatch(filtered_plot_data(), error=function(e) NULL)
          if (!is.null(rgb_dat)) print(rgb_dat$plot)
        }

        if ("cluster_map" %in% sections && !is.null(dat)) {
          incProgress(0.10, detail="Rendering cluster and entropy maps")
          p1 <- make_cluster_entropy_plot(df=dat$result_df,fill_var="cluster_label",title="Cluster Map",aspect_ratio=dat$aspect_ratio %||% 1,
            fill_scale=scale_fill_manual(values=dat$cluster_colors_named[names(dat$cluster_colors_named) %in% unique(dat$result_df$cluster_label)],drop=FALSE,name="Cluster"),
            map_width_mm=dat$map_width,map_height_mm=dat$map_height)
          p2 <- make_cluster_entropy_plot(df=dat$ambiguity_df,fill_var="ambiguity",title="Entropy Map",aspect_ratio=dat$aspect_ratio %||% 1,
            fill_scale=scale_fill_viridis_c(option="magma",name="Entropy"),
            map_width_mm=dat$map_width,map_height_mm=dat$map_height)
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
          plot_df <- sample_rows_analysis(plot_df,frac=max(0.01,as.numeric(input$data_analysis_sample_pct %||% 1)/100),
                                          max_n=100000,cluster_col="cluster_label")
          p_tern <- make_static_ternary_plot(plot_df,avar,bvar,cvar,color_mode=color_mode,
                                             color_var=color_var,cluster_colors_named=dat$cluster_colors_named)
          draw_ggplot_raster_page(p_tern,width=8,height=10,dpi=110)
        }

        if ("correlogram" %in% sections) {
          incProgress(0.08, detail="Rendering correlogram")
          use_raw <- identical(input$corr_data_basis %||% "processed","raw")
          df <- build_analysis_dataframe(core,dat,use_raw=use_raw)
          grp <- input$report_corr_group %||% "All samples"
          if (!identical(grp,"All samples") && "cluster_label" %in% names(df))
            df <- df %>% dplyr::filter(cluster_label == grp)
          corr_df <- df %>% dplyr::select(dplyr::all_of(core$elements))
          cor_mat <- suppressWarnings(cor(corr_df,use="pairwise.complete.obs",method=input$corr_method %||% "spearman"))
          par(mar=c(1,1,3,1))
          render_corrplot(cor_mat,orientation=input$corr_legend_orientation %||% "vertical")
          mtext("Correlogram",side=3,line=1.2,cex=1.1,font=2)
          mtext(paste("Samples used:",grp),side=3,line=0.1,cex=0.75)
        }

        incProgress(1, detail="Finalizing PDF report")
      })
    }
  )
}

shinyApp(ui = ui, server = server)
