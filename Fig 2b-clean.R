library(readxl)
library(dplyr)
library(tidyr)

INNER_RADIUS <- 1.0
OUTER_RADIUS <- 3.0
GAP_BETWEEN_INDICATORS <- 0.1
GAP_BETWEEN_BARS <- 0.05
AXIS_ANGLE_OFFSET <- 0
AXIS_START_OFFSET <- 0.1
AXIS_TEXT_OFFSET <- 0.2
AXIS_FONT_SIZE <- 10
TEXT_RING_WIDTH <- 0.6
RING_GAP <- 0.6
FONT_SIZE <- 11
LABEL_MAX_NUM <- 2

set_default_colors <- function(n_samples, n_indicators) {
  sample_colors <- c("#1B7C7C", "#B24A6A", "#4C72B0", "#8E8E8E", "#a6b754", "#bda543")
  if(n_samples > 6) sample_colors <- colorRampPalette(sample_colors)(n_samples)
  indicator_colors <- c(paste0("#F4EDE6", "80"), paste0("#EAF3EE", "80"),
                        paste0("#F1F4E8", "80"), paste0("#F6F2E4", "80"),
                        paste0("#F2E9EC", "80"), paste0("#EDF0F2", "80"))
  if(n_indicators > 6) {
    base_colors <- colorRampPalette(c("#F4EDE6", "#EAF3EE", "#F1F4E8", "#F6F2E4", "#F2E9EC", "#EDF0F2"))(n_indicators)
    indicator_colors <- paste0(base_colors, "80")
  }
  return(list(sample_colors = sample_colors[1:n_samples],
              indicator_colors = indicator_colors[1:n_indicators]))
}

polar_to_cartesian <- function(r, theta) {
  x <- r * cos(theta)
  y <- r * sin(theta)
  return(data.frame(x = x, y = y))
}

create_wedge <- function(r_inner, r_outer, theta_start, theta_end, n_segments = 100) {
  angles <- seq(theta_start, theta_end, length.out = n_segments)
  outer_arc <- polar_to_cartesian(r_outer, angles)
  inner_arc <- polar_to_cartesian(r_inner, rev(angles))
  wedge_points <- rbind(outer_arc, inner_arc, outer_arc[1, ])
  return(wedge_points)
}

create_bar_polygon <- function(r_base, r_top, theta_left, theta_right) {
  points <- data.frame(
    x = c(r_base * cos(theta_left), r_top * cos(theta_left),
          r_top * cos(theta_right), r_base * cos(theta_right)),
    y = c(r_base * sin(theta_left), r_top * sin(theta_left),
          r_top * sin(theta_right), r_base * sin(theta_right))
  )
  return(points)
}

reorder_legend <- function(labels, ncol) {
  n <- length(labels)
  nrow <- ceiling(n / ncol)
  new_labels <- character(n)
  idx <- 1
  for(col in 1:ncol) {
    for(row in 1:nrow) {
      original_idx <- (col - 1) * nrow + row
      if(original_idx <= n) {
        new_labels[idx] <- labels[original_idx]
        idx <- idx + 1
      }
    }
  }
  return(new_labels)
}

generate_integer_ticks <- function(max_val, n_ticks = 5) {
  max_val_rounded <- ceiling(max_val)
  ticks <- seq(0, max_val_rounded, length.out = n_ticks + 1)[-1]
  ticks <- round(ticks)
  if(max_val_rounded < n_ticks) ticks <- 1:max_val_rounded
  return(ticks)
}

create_polar_barplot <- function(data_file, output_name) {
  if(grepl("\\.xlsx$", data_file, ignore.case = TRUE)) {
    df <- read_excel(data_file)
  } else if(grepl("\\.csv$", data_file)) {
    df <- read.csv(data_file)
  } else {
    stop("Only .xlsx or .csv formats are supported")
  }
  required_cols <- c("SampleID", "Indicators", "Value")
  if(!all(required_cols %in% colnames(df))) {
    stop("Data must contain columns: SampleID, Indicators, Value")
  }
  samples <- unique(df$SampleID)
  indicators <- unique(df$Indicators)
  
  colors <- set_default_colors(length(samples), length(indicators))
  sample_colors <- colors$sample_colors
  names(sample_colors) <- samples
  indicator_colors <- colors$indicator_colors
  names(indicator_colors) <- indicators
  
  n_indicators <- length(indicators)
  theta_edges <- seq(0, 2 * pi, length.out = n_indicators + 1)
  max_all_vals <- max(df$Value, na.rm = TRUE)
  estimated_max_radius <- OUTER_RADIUS + max_all_vals * ((OUTER_RADIUS - INNER_RADIUS) / max_all_vals) + RING_GAP + TEXT_RING_WIDTH + 1
  plot_limit <- estimated_max_radius
  
  draw_plot <- function(device = "png") {
    if(device == "png") {
      png(paste0(output_name, ".png"), width = 9, height = 9, units = "in", res = 300)
    } else {
      pdf(paste0(output_name, ".pdf"), width = 9, height = 9)
    }
    par(mar = c(0, 0, 0, 0), xpd = TRUE)
    plot(0, 0, xlim = c(-plot_limit, plot_limit), ylim = c(-plot_limit, plot_limit),
         type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
    
    for(i in seq_along(indicators)) {
      indicator <- indicators[i]
      sub_df <- df %>% filter(Indicators == indicator)
      theta_left <- theta_edges[i] + GAP_BETWEEN_INDICATORS / 2
      theta_right <- theta_edges[i + 1] - GAP_BETWEEN_INDICATORS / 2
      theta_mid <- (theta_left + theta_right) / 2
      theta_axis <- theta_left + AXIS_ANGLE_OFFSET
      
      raw_max_val <- max(sub_df$Value, na.rm = TRUE) * 1.2
      max_val <- ceiling(raw_max_val)
      ticks <- generate_integer_ticks(max_val, 5)
      n_ticks <- length(ticks)
      scale <- (OUTER_RADIUS - INNER_RADIUS) / max_val
      
      bg_color <- indicator_colors[i]
      wedge <- create_wedge(INNER_RADIUS, OUTER_RADIUS, theta_left, theta_right)
      polygon(wedge$x, wedge$y, col = bg_color, border = NA)
      
      angles <- seq(theta_left, theta_right, length.out = 100)
      lines(OUTER_RADIUS * cos(angles), OUTER_RADIUS * sin(angles), col = "black", lwd = 1)
      lines(INNER_RADIUS * cos(angles), INNER_RADIUS * sin(angles), col = "black", lwd = 1)
      segments(INNER_RADIUS * cos(theta_left), INNER_RADIUS * sin(theta_left),
               OUTER_RADIUS * cos(theta_left), OUTER_RADIUS * sin(theta_left), col = "black", lwd = 1)
      segments(INNER_RADIUS * cos(theta_right), INNER_RADIUS * sin(theta_right),
               OUTER_RADIUS * cos(theta_right), OUTER_RADIUS * sin(theta_right), col = "black", lwd = 1)
      
      r_axis_start <- INNER_RADIUS + AXIS_START_OFFSET
      r_axis_end <- INNER_RADIUS + max_val * scale
      segments(r_axis_start * cos(theta_axis), r_axis_start * sin(theta_axis),
               r_axis_end * cos(theta_axis), r_axis_end * sin(theta_axis), col = "white", lwd = 1.5)
      
      for(k in 1:n_ticks) {
        val <- ticks[k]
        r <- INNER_RADIUS + val * scale
        xt <- (r + AXIS_TEXT_OFFSET) * cos(theta_axis)
        yt <- (r + AXIS_TEXT_OFFSET) * sin(theta_axis)
        text(xt, yt, sprintf("%d", val), cex = AXIS_FONT_SIZE / 12, srt = theta_axis * 180 / pi, adj = c(0.5, 0.5))
      }
      for(k in 1:n_ticks) {
        val <- ticks[k]
        r <- INNER_RADIUS + val * scale
        main_tick_angles <- seq(theta_left, theta_right, length.out = 50)
        lines(r * cos(main_tick_angles), r * sin(main_tick_angles), lty = "dashed", col = "gray", lwd = 0.5)
      }
      
      n_bars <- length(samples)
      dtheta <- (theta_right - theta_left) / n_bars
      for(j in seq_along(samples)) {
        sample_id <- samples[j]
        row_value <- sub_df %>% filter(SampleID == sample_id) %>% pull(Value)
        if(length(row_value) == 0) next
        val <- row_value[1]
        r0 <- INNER_RADIUS
        r1 <- INNER_RADIUS + val * scale
        t0 <- theta_left + (j - 1) * dtheta + GAP_BETWEEN_BARS
        t1 <- theta_left + j * dtheta - GAP_BETWEEN_BARS
        tc <- (t0 + t1) / 2
        bar_polygon <- create_bar_polygon(r0, r1, t0, t1)
        polygon(bar_polygon$x, bar_polygon$y, col = sample_colors[sample_id], border = "black", lwd = 0.6)
        if(val > 0) {
          label_r <- r1 + 0.15
          rotation_angle <- tc * 180 / pi
          if(rotation_angle > 90 && rotation_angle < 270) rotation_angle <- rotation_angle + 180
          text(label_r * cos(tc), label_r * sin(tc), sprintf("%d", round(val)),
               cex = 0.8, adj = c(0.5, 0.5), srt = rotation_angle, font = 2)
        }
      }
      
      outer_wedge <- create_wedge(OUTER_RADIUS + RING_GAP, OUTER_RADIUS + RING_GAP + TEXT_RING_WIDTH, theta_left, theta_right)
      polygon(outer_wedge$x, outer_wedge$y, col = bg_color, border = "black", lwd = 1)
      
      r_text <- OUTER_RADIUS + RING_GAP + TEXT_RING_WIDTH / 2
      label <- indicator
      text_rotation <- (theta_mid * 180 / pi) + 90
      if(theta_mid > pi/2 && theta_mid < 3*pi/2) text_rotation <- text_rotation + 180
      text(r_text * cos(theta_mid), r_text * sin(theta_mid), label,
           cex = FONT_SIZE / 12, font = 2, srt = text_rotation, adj = c(0.5, 0.5))
    }
    
    legend_labels <- samples
    legend_colors <- sample_colors
    if(length(legend_labels) > LABEL_MAX_NUM) {
      legend_labels <- reorder_legend(legend_labels, LABEL_MAX_NUM)
      legend_colors <- legend_colors[match(legend_labels, samples)]
    }
    legend("bottom", legend = legend_labels, fill = legend_colors,
           ncol = min(LABEL_MAX_NUM, length(legend_labels)), bty = "o", xpd = TRUE,
           inset = c(0, -0.05), cex = 0.9, title = "Sample category")
    dev.off()
  }
  
  draw_plot("png")
  draw_plot("pdf")
}