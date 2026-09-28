
library(readxl)
library(tidyverse)
library(here)
library(cowplot)

data <- read_csv(here("data","raw","zukovica_seasons.csv"))

data_long <- data %>%
  pivot_longer(
    cols = Winter:Autumn,
    names_to = "Season",
    values_to = "Value"
  )

data_long$Season <- factor(data_long$Season,
                           levels = c("Winter", "Spring", "Summer", "Autumn"))


season_colors <- c(
  Winter = "#9DC3C1",
  Spring = "#60D394",  
  Summer = "#FFD97D", 
  Autumn = "#FF8552"
)


season_bubbles <- ggplot(data_long,
                         aes(x = Season,
                             y = factor(excavation_unit,
                                        levels = rev(unique(excavation_unit))),
                             size = Value,
                             fill = Season)) +
  
  geom_point(shape = 21,
             colour = "white",
             stroke = 0.3,
             alpha = 0.65,
             na.rm = TRUE) +
  
  geom_text(aes(label = Value),
            size = 3,
            colour = "black",
            na.rm = TRUE) +
  
  scale_size(range = c(4,16)) +
  
  scale_fill_manual(values = season_colors) +
  
  labs(
    x = NULL,
    y = "Excavation unit"
  ) +
  
  theme_light() +
  
  theme(
    legend.position = "",
    panel.grid = element_blank(),
    axis.title.y = element_text(size = 12, face = "plain"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 11)
  )

print(season_bubbles)

ggsave(
  "allunits.png",
  season_bubbles,
  width = 15,
  height = 7,
  dpi = 300
)




data_long %>%
  ggplot()+ aes(x = factor(excavation_unit, levels = rev(unique(excavation_unit))), y = Value, fill = Season) +
  geom_bar( stat = "identity")+
  scale_fill_manual(values = season_colors)+
  theme_cowplot()+
  theme(legend.position = "")+
  facet_wrap(~Season)+
  labs(x="")