library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggVennDiagram)
library(ggplot2)
library(patchwork)
library(scales)

data <- read_excel("host_virus-venn.xlsx")

if (!"virus_new" %in% colnames(data)) stop("lack virus_new")

virus_data <- data %>%
  mutate(row_id = row_number()) %>%
  separate_rows(virus_new, sep = ",") %>%
  mutate(virus = str_trim(virus_new)) %>%
  filter(virus != "" & !is.na(virus)) %>%
  select(row_id, virus, Host_order, Biogeographic_region)

host_groups <- c("Rodentia", "Eulipotyphla", "Lagomorpha")
host_colors <- c(Rodentia = alpha("#A53B27", 0.7),
                 Eulipotyphla = alpha("#237848", 0.7),
                 Lagomorpha = alpha("#166197", 0.7))

region_groups <- c("SC", "NNE", "NW", "QTP")
region_colors <- c(SC = "#789A9C", NNE = "#B3B3B3", NW = "#9E726C", QTP = "#BEA568")

get_sets <- function(df, col, groups) {
  sets <- list()
  for (g in groups) {
    viruses <- df %>% filter(!!sym(col) == g) %>% distinct(virus) %>% pull(virus)
    if (length(viruses) > 0) sets[[g]] <- viruses
  }
  sets
}

draw_venn <- function(sets, colors, title) {
  ggVennDiagram(sets, label_alpha = 0, edge_size = 1, set_size = 4, label_size = 3.5) +
    scale_fill_gradient(low = "transparent", high = "transparent", guide = "none") +
    scale_color_manual(values = unname(colors[names(sets)]), breaks = names(sets)) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), legend.position = "none")
}

host_sets <- get_sets(virus_data, "Host_order", host_groups)
region_sets <- get_sets(virus_data, "Biogeographic_region", region_groups)

stopifnot(length(host_sets) == 3, length(region_sets) == 4)

venn_host <- draw_venn(host_sets, host_colors, "Host_order")
venn_region <- draw_venn(region_sets, region_colors, "Biogeographic_region")

combined <- (venn_host + ggtitle("Host_order") + theme(plot.title = element_text(hjust = 0.5))) +
  (venn_region + ggtitle("Biogeographic_region") + theme(plot.title = element_text(hjust = 0.5))) +
  plot_annotation(title = "venn_plot",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))

ggsave("Fig_4d-e_venn.pdf", combined, width = 12, height = 6, device = cairo_pdf)