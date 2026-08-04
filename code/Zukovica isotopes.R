library(ggplot2)

# Data per unit max and min
data <- data.frame(
  excavation_unit = c(
    124, 122, 121, 119, 116, 115, 113.5, 114, 118, 113,
    108, 112, 25, 111, 24, 23, 110, 22, 109, 107,
    20, 19, 106, 105, 16
  ),
  d18O_max = c(
    1.92, 2.52, 2.46, 2.6, 2.11, 1.48, 2.21, NA, 2.5, 2.49,
    1.76, 2.62, 2.65, 2.2, 2.13, 2.59, 2.46, 1.99, 2.76, 2.48,
    1.9, 2.37, 2.35, 2.36, 2.18
  ),
  d18O_min = c(
    -1.22, -0.53, -0.4, -0.29, -0.57, -0.59, -0.39, -0.16, -0.89, -0.51,
    -0.59, -1.18, -0.61, -0.73, -0.66, NA, -0.56, -0.91, -0.51, -0.59,
    NA, -0.54, -1.14, -0.5, -0.49
  )
)

# Stratigraphic order
order <- c(
  124, 122, 121, 119, 116, 115, 114, 113.5, 118, 113,
  112, 111, 110, 109, 108, 107, 106, 105,
  25, 24, 23, 22, 20, 19, 16
)

data$excavation_unit <- factor(data$excavation_unit, levels = order)

# Convert to long format
data_long <- tidyr::pivot_longer(
  data,
  cols = c(d18O_min, d18O_max),
  names_to = "type",
  values_to = "d18O"
)

# KEY STEP: order factor so max comes after min
data_long$type <- factor(
  data_long$type,
  levels = c("d18O_min", "d18O_max"),
  labels = c("Minimum", "Maximum")
)

# Plot
ggplot(data_long, aes(x = excavation_unit, y = d18O, color = type, group = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_reverse() +
  theme_classic() +
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~("\u2030")),
    color = ""
  ) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Data
data <- data.frame(
  excavation_unit = c(
    124, 122, 121, 119, 116, 115, 113.5, 114, 118, 113,
    108, 112, 25, 111, 24, 23, 110, 22, 109, 107,
    20, 19, 106, 105, 16
  ),
  d18O_max = c(
    1.92, 2.52, 2.46, 2.6, 2.11, 1.48, 2.21, NA, 2.5, 2.49,
    1.76, 2.62, 2.65, 2.2, 2.13, 2.59, 2.46, 1.99, 2.76, 2.48,
    1.9, 2.37, 2.35, 2.36, 2.18
  ),
  d18O_min = c(
    -1.22, -0.53, -0.4, -0.29, -0.57, -0.59, -0.39, -0.16, -0.89, -0.51,
    -0.59, -1.18, -0.61, -0.73, -0.66, NA, -0.56, -0.91, -0.51, -0.59,
    NA, -0.54, -1.14, -0.5, -0.49
  )
)

# Stratigraphic order
order <- c(
  124, 122, 121, 119, 116, 115, 114, 113.5, 118, 113,
  112, 111, 110, 109, 108, 107, 106, 105,
  25, 24, 23, 22, 20, 19, 16
)

data$excavation_unit <- factor(data$excavation_unit, levels = order)

# Long format
data_long <- pivot_longer(
  data,
  cols = c(d18O_min, d18O_max),
  names_to = "type",
  values_to = "d18O"
)

data_long$type <- factor(
  data_long$type,
  levels = c("d18O_min", "d18O_max"),
  labels = c("Minimum", "Maximum")
)

# Numeric x for regression
data_long$x_num <- as.numeric(data_long$excavation_unit)

# Plot (NO lines connecting points)
ggplot(data_long, aes(x = x_num, y = d18O, color = type)) +
  
  # Points only
  geom_point(size = 2) +
  
  # Regression lines per group
  geom_smooth(
    aes(color = type),
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = 1:length(order),
    labels = order
  ) +
  
  scale_y_reverse() +
  
  theme_classic() +
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~("\u2030")),
    color = ""
  ) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#division por trenches

library(ggplot2)
library(tidyr)
library(dplyr)

data <- data.frame(
  excavation_unit = c(
    124, 122, 121, 119, 116, 115, 113.5, 114, 118, 113,
    108, 112, 25, 111, 24, 23, 110, 22, 109, 107,
    20, 19, 106, 105, 16
  ),
  Trench = c(
    2, 2, 2, 2, 2, 1, 1, 1, 2, 2,
    2, 1, 1, 2, 1, 1, 2, 1, 2, 2,
    1, 1, 2, 2, 1
  ),
  d18O_max = c(
    1.92, 2.52, 2.46, 2.6, 2.11, 1.48, 2.21, -0.02, 2.5, 2.49,
    1.76, 2.62, 2.65, 2.2, 2.13, 2.59, 2.46, 1.99, 2.76, 2.48,
    1.9, 2.37, 2.35, 2.36, 2.18
  ),
  d18O_min = c(
    -1.22, -0.53, -0.4, -0.29, -0.57, -0.59, -0.39, -0.16, -0.89, -0.51,
    -0.59, -1.18, -0.61, -0.73, -0.66, 0.07, -0.56, -0.91, -0.51, -0.59,
    0.72, -0.54, -1.14, -0.5, -0.49
  )
)

plot_trench <- function(df, trench_id) {
  
  df_filt <- df %>%
    filter(Trench == trench_id)
  
  order <- sort(unique(df_filt$excavation_unit), decreasing = TRUE)
  
  df_filt$excavation_unit <- factor(df_filt$excavation_unit, levels = order)
  
  df_long <- pivot_longer(
    df_filt,
    cols = c(d18O_min, d18O_max),
    names_to = "type",
    values_to = "d18O"
  )
  
  df_long$type <- factor(
    df_long$type,
    levels = c("d18O_min", "d18O_max"),
    labels = c("Minimum", "Maximum")
  )
  
  df_long$x_num <- as.numeric(df_long$excavation_unit)
  
  ggplot(df_long, aes(x = x_num, y = d18O, color = type)) +
    
    geom_point(size = 2) +
    
    geom_smooth(
      aes(color = type),
      method = "loess",
      se = FALSE,
      linewidth = 1
    ) +
    
    scale_x_continuous(
      breaks = 1:length(order),
      labels = order
    ) +
    
    scale_y_reverse() +
    
    theme_classic() +
    labs(
      title = paste("Trench", trench_id),
      x = "Excavation unit",
      y = expression(delta^{18}*O~("\u2030")),
      color = ""
    ) +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_trench(data, 1)
plot_trench(data, 2)



#todos los valores
library(readxl)
library(ggplot2)
library(ggplot2)
library(dplyr)


# Data
data <- data.frame(
  Unit = c(124,124,124,124),
  Shell_ID = c("ŽU_2_124_B1","ŽU_2_124_C1","ŽU_2_124_D1","ŽU_2_124_E1"),
  d18O = c(-1.22, -0.76, 1.92, -0.67),
  season = c("Summer","Summer","Winter","Summer")
)

# Plot
ggplot(data, aes(x = factor(Unit), y = d18O, color = season)) +
  
  geom_jitter(width = 0.15, size = 3) +
  
  theme_classic() +
  
  labs(
    x = "Unit",
    y = expression(delta^{18}*O~("\u2030")),
    color = "Season"
  )

library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# 1. IMPORTA TU EXCEL
# -----------------------------
# valores <- read_excel("ruta/valores.xlsx")

# Si ya lo tienes cargado, salta este paso

# -----------------------------
# 2. ASEGURAR TIPOS CORRECTOS
# -----------------------------
valores <- valores %>%
  mutate(
    Unit = as.factor(Unit),
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season)
  )

# -----------------------------
# 3. ORDEN ESTRATIGRÁFICO (IMPORTANTE)
# -----------------------------
orden <- c(
  124, 122, 121, 119, 116, 115, 113.5, 114, 118, 113,
  108, 112, 25, 111, 24, 23, 110, 22, 109, 107,
  20, 19, 106, 105, 16
)

valores$Unit <- factor(valores$Unit, levels = orden)

# -----------------------------
# 4. PLOT FINAL
# -----------------------------
ggplot(valores, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(
    width = 0.2,
    size = 2,
    alpha = 0.8
  ) +
  
  theme_classic() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~("\u2030")),
    color = "Season"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#con curvas
library(dplyr)
library(ggplot2)

valores <- valores %>%
  mutate(
    Unit = as.factor(Unit),
    Unit_num = as.numeric(factor(Unit, levels = orden)),
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season)
  )

ggplot(valores, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(
    width = 0.15,
    size = 2,
    alpha = 0.7
  ) +
  
  geom_smooth(
    aes(x = Unit_num),
    method = "loess",
    se = FALSE,
    span = 1   # más estable para tendencia general
  ) +
  
  theme_classic() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~("\u2030")),
    color = "Season"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#summer y winter cambiados de posicion

library(dplyr)
library(ggplot2)

valores <- valores %>%
  mutate(
    Unit = as.factor(Unit),
    Unit_num = as.numeric(factor(Unit, levels = orden)),
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season),
    
    # desplazamiento visual
    d18O_plot = ifelse(season == "Summer",
                       `18O value` + 1,   # arriba
                       `18O value` - 1)    # abajo
  )

ggplot(valores, aes(x = Unit, y = d18O_plot, color = season)) +
  
  geom_jitter(
    aes(y = d18O_plot),
    width = 0.15,
    size = 2,
    alpha = 0.7
  ) +
  
  geom_smooth(
    aes(x = Unit_num, y = d18O_plot),
    method = "loess",
    se = FALSE,
    span = 1
  ) +
  
  theme_classic() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~("\u2030")~"(offset for visualization)"),
    color = "Season"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


library(dplyr)
library(ggplot2)

# Preparación de datos
valores <- valores %>%
  mutate(
    Unit = factor(Unit, levels = orden),  # fija el orden correcto
    Unit_num = as.numeric(Unit),          # respeta ese orden
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season)
  )

# Plot principal con curvas
ggplot(valores, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(
    width = 0.15,
    size = 2,
    alpha = 0.7
  ) +
  
  geom_smooth(
    aes(x = Unit_num),
    method = "loess",
    se = FALSE,
    span = 1
  ) +
  
  theme_classic() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~("\u2030")),
    color = "Season"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

library(dplyr)
library(ggplot2)

# Preparación de datos
valores <- valores %>%
  mutate(
    Unit = factor(Unit, levels = orden),
    Unit_num = as.numeric(Unit),
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season)
  )

# Plot principal con curvas
ggplot(valores, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(
    width = 0.15,
    size = 2,
    alpha = 0.7
  ) +
  
  geom_smooth(
    aes(x = Unit_num),
    method = "loess",
    se = FALSE,
    span = 1
  ) +
  
  scale_y_reverse() +
  
  theme_classic() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~("\u2030")),
    color = "Season"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
orden <- unique(valores$Unit)

valores <- valores %>%
  mutate(
    Unit = factor(Unit, levels = orden),
    Unit_num = as.numeric(Unit),
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season)
  )

ggplot(valores, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(
    width = 0.15,
    size = 2,
    alpha = 0.7
  ) +
  
  geom_smooth(
    aes(x = Unit_num),
    method = "loess",
    se = FALSE,
    span = 1
  ) +
  
  scale_y_reverse(
    limits = c(3, min(valores$`18O value`, na.rm = TRUE))
  ) +
  
  theme_minimal() +
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  )
   +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(
  filename = "plot_18O.png",
  path = "C:/Users/arnizmateos/Downloads",
  width = 12,
  height = 6,
  dpi = 300
)


library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)

# -----------------------------
# 1. Preparación de datos
# -----------------------------

valores <- valores %>%
  mutate(
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season),
    Trench = as.factor(Trench)
  )

# -----------------------------
# 2. Separar trenches
# -----------------------------

valores_t1 <- valores %>%
  filter(Trench == 1 | Trench == 3) %>%
  mutate(
    Unit = factor(Unit, levels = unique(Unit)),
    Unit_num = as.numeric(Unit)
  )

valores_t2 <- valores %>%
  filter(Trench == 2 | Trench == 3) %>%
  mutate(
    Unit = factor(Unit, levels = unique(Unit)),
    Unit_num = as.numeric(Unit)
  )

# -----------------------------
# 3. Plot Trench 1
# -----------------------------

p1 <- ggplot(valores_t1, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  
  geom_smooth(
    aes(x = Unit_num),
    method = "loess",
    se = FALSE,
    span = 1
  ) +
  
  scale_y_reverse(
    limits = c(3, min(valores$`18O value`, na.rm = TRUE))
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Trench 1",
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

ggsave(
  filename = "Trench1_18O.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p1,
  width = 12,
  height = 6,
  dpi = 300
)

# -----------------------------
# 4. Plot Trench 2
# -----------------------------

p2 <- ggplot(valores_t2, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  
  geom_smooth(
    aes(x = Unit_num),
    method = "loess",
    se = FALSE,
    span = 1
  ) +
  
  scale_y_reverse(
    limits = c(3, min(valores$`18O value`, na.rm = TRUE))
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Trench 2",
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2
ggsave(
  filename = "Trench2_18O.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p2,
  width = 12,
  height = 6,
  dpi = 300
)

#after cleaning
library(dplyr)
library(ggplot2)

# -----------------------------
# 1. Preparación de datos
# -----------------------------

valores <- valores %>%
  mutate(
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season),
    Trench = as.factor(Trench)
  )

# -----------------------------
# 2. Separar trenches
#    (solo muestras certain = yes)
# -----------------------------

valores_t1 <- valores %>%
  filter(
    Trench %in% c(1, 3),
    Certain == "Yes"
  ) %>%
  mutate(
    Unit = factor(Unit, levels = unique(Unit)),
    Unit_num = as.numeric(Unit)
  )

valores_t2 <- valores %>%
  filter(
    Trench %in% c(2, 3),
    Certain == "Yes"
  ) %>%
  mutate(
    Unit = factor(Unit, levels = unique(Unit)),
    Unit_num = as.numeric(Unit)
  )

# -----------------------------
# 3. Plot Trench 1
# -----------------------------

p1 <- ggplot(valores_t1, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  
  geom_smooth(
    aes(x = Unit_num),
    se = FALSE,
    span = 0.5
  ) +
  
  scale_y_reverse(
    limits = c(3, min(valores_t1$`18O value`, na.rm = TRUE))
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Trench 1",
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
nrow(valores_t1)
p1

ggsave(
  filename = "Trench1_18Ocleaning.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p1,
  width = 12,
  height = 6,
  dpi = 300
)

# -----------------------------
# 4. Plot Trench 2
# -----------------------------

p2 <- ggplot(valores_t2, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  
  geom_smooth(
    aes(x = Unit_num),
    se = FALSE,
    span = 0.5
  ) +
  
  scale_y_reverse(
    limits = c(3, min(valores_t2$`18O value`, na.rm = TRUE))
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Trench 2",
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
nrow(valores_t2)

p2

ggsave(
  filename = "Trench2_18Ocleaning.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p2,
  width = 12,
  height = 6,
  dpi = 300
)


#todo junto
library(dplyr)
library(ggplot2)

# -----------------------------
# 1. Preparación de datos
# -----------------------------

valores <- valores %>%
  mutate(
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season),
    Trench = as.factor(Trench)
  )

# -----------------------------
# 2. Filtrado único (sin separar trenches)
# -----------------------------

valores_plot <- valores %>%
  filter(
    Trench %in% c(1, 2, 3),
    Certain == "Yes"
  ) %>%
  mutate(
    Unit = factor(Unit, levels = unique(Unit)),
    Unit_num = as.numeric(Unit)
  )

# -----------------------------
# 3. Plot único (TODO JUNTO)
# -----------------------------

p <- ggplot(valores_plot, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  
  geom_smooth(
    aes(x = Unit_num),
    se = FALSE,
    span = 0.5
  ) +
  
  scale_y_reverse(
    limits = c(3, min(valores_plot$`18O value`, na.rm = TRUE))
  ) +
  
  theme_minimal() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# mostrar número de muestras (opcional)
nrow(valores_plot)

# mostrar gráfico
p

# -----------------------------
# 4. Guardar
# -----------------------------

ggsave(
  filename = "All_Trenches_18Ocleaning.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300
)

# -----------------------------
# 4. Guardar figura
# -----------------------------

ggsave(
  filename = "All_Trenches_18Ocleaning.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300
)



# -----------------------------
# 1. Preparación de datos
# -----------------------------

valoresconmerge <- valoresconmerge %>%
  mutate(
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season),
    Unit = as.character(Unit)
  )

# -----------------------------
# 2. Filtrado
# -----------------------------

valores_plot <- valoresconmerge %>%
  filter(Certain == "Yes")

# -----------------------------
# 3. Orden estratigráfico (según Excel)
# -----------------------------

orden_units <- valores_plot %>%
  distinct(Unit) %>%
  pull(Unit)

valores_plot <- valores_plot %>%
  mutate(
    Unit = factor(Unit, levels = orden_units)
  )

# -----------------------------
# 4. Plot SOLO PUNTOS (sin líneas)
# -----------------------------

p <- ggplot(valores_plot, aes(x = Unit, y = `18O value`, color = season)) +
  
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  
  scale_y_reverse() +
  
  theme_minimal() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# mostrar
p

ggsave(
  filename = "18O_valoresconmerge_sin_lineas.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300
)

# -----------------------------
# 1. Preparación
# -----------------------------

valoresconmerge <- valoresconmerge %>%
  mutate(
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season),
    Unit = as.character(Unit)
  )

# -----------------------------
# 2. Filtrado
# -----------------------------

valores_plot <- valoresconmerge %>%
  filter(Certain == "Yes")

# -----------------------------
# 3. Orden estratigráfico
# -----------------------------

orden_units <- valores_plot %>%
  distinct(Unit) %>%
  pull(Unit)

valores_plot <- valores_plot %>%
  mutate(
    Unit = factor(Unit, levels = orden_units)
  )

# -----------------------------
# 4. Definir extremos
# -----------------------------

# verano -> valores más BAJOS
summer_low <- valores_plot %>%
  filter(season == "Summer") %>%
  group_by(Unit) %>%
  slice_min(`18O value`, n = 1, with_ties = FALSE)

# invierno -> valores más ALTOS
winter_high <- valores_plot %>%
  filter(season == "Winter") %>%
  group_by(Unit) %>%
  slice_max(`18O value`, n = 1, with_ties = FALSE)

# resto de puntos (sin extremos)
normal_points <- valores_plot %>%
  anti_join(
    bind_rows(summer_low, winter_high),
    by = c("Unit", "18O value", "season")
  )

# -----------------------------
# 5. Plot
# -----------------------------

p <- ggplot() +
  
  # puntos normales (pastel por season)
  geom_jitter(
    data = normal_points,
    aes(x = Unit, y = `18O value`, color = season),
    width = 0.15,
    size = 2,
    alpha = 0.4
  ) +
  
  # verano extremo (pastel rojo/rosa suave)
  geom_point(
    data = summer_low,
    aes(x = Unit, y = `18O value`),
    color = "#F4A6B8",
    size = 3
  ) +
  
  # invierno extremo (pastel azul)
  geom_point(
    data = winter_high,
    aes(x = Unit, y = `18O value`),
    color = "#8FBCE6",
    size = 3
  ) +
  
  scale_y_reverse() +
  
  theme_minimal() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p



# -----------------------------
# 1. Preparación de datos
# -----------------------------

valoresconmerge <- valoresconmerge %>%
  mutate(
    `18O value` = as.numeric(`18O value`),
    season = as.factor(season),
    Unit = as.character(Unit)
  )

# -----------------------------
# 2. Filtrado
# -----------------------------

valores_plot <- valoresconmerge %>%
  filter(Certain == "Yes")

# -----------------------------
# 3. Orden estratigráfico
# -----------------------------

orden_units <- valores_plot %>%
  distinct(Unit) %>%
  pull(Unit)

valores_plot <- valores_plot %>%
  mutate(
    Unit = factor(Unit, levels = orden_units)
  )

# -----------------------------
# 4. Separar extremos
# -----------------------------

summer_low <- valores_plot %>%
  filter(season == "Summer") %>%
  group_by(Unit) %>%
  slice_min(`18O value`, n = 1, with_ties = FALSE)

winter_high <- valores_plot %>%
  filter(season == "Winter") %>%
  group_by(Unit) %>%
  slice_max(`18O value`, n = 1, with_ties = FALSE)

normal_points <- valores_plot %>%
  anti_join(
    bind_rows(summer_low, winter_high),
    by = c("Unit", "18O value", "season")
  )

# -----------------------------
# 5. Paletas de color
# -----------------------------

season_colors <- c(
  "Summer" = "#F4A6B8",
  "Winter" = "#8FBCE6"
)

season_colors_dark <- c(
  "Summer" = "#D94B6A",
  "Winter" = "#2F6FA3"
)

# -----------------------------
# 6. Plot
# -----------------------------

p <- ggplot() +
  
  # puntos normales (pastel)
  geom_jitter(
    data = normal_points,
    aes(x = Unit, y = `18O value`, color = season),
    width = 0.15,
    size = 2,
    alpha = 0.4
  ) +
  
  # verano extremo (intenso)
  geom_point(
    data = summer_low,
    aes(x = Unit, y = `18O value`, color = "Summer"),
    size = 3
  ) +
  
  # invierno extremo (intenso)
  geom_point(
    data = winter_high,
    aes(x = Unit, y = `18O value`, color = "Winter"),
    size = 3
  ) +
  
  scale_color_manual(
    values = c(
      season_colors,
      "Summer" = season_colors_dark["Summer"],
      "Winter" = season_colors_dark["Winter"]
    )
  ) +
  
  scale_y_reverse() +
  
  theme_minimal() +
  
  labs(
    x = "Excavation unit",
    y = expression(delta^{18}*O~(VPDB~"\u2030")),
    color = "Season"
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# 7. Mostrar
# -----------------------------

nrow(valores_plot)
p

# -----------------------------
# 8. Guardar
# -----------------------------

ggsave(
  filename = "18O_valoresconmerge_pastel_extremos.png",
  path = "C:/Users/arnizmateos/Downloads",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300
)