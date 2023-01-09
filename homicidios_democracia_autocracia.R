
# Homicídios em países democratas e autocratas ---------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 08/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/homicides -----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### 

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

hom <- read.csv("share-of-deaths-homicides.csv")
view(hom)
names(hom)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

hom <- hom %>%
  select(-Code) %>%
  rename(por_hom = Deaths...Interpersonal.violence...Sex..Both...Age..All.Ages..Percent.) %>%
  view()

hom1 <- hom %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_hom),
            sd = sd(por_hom), n = n(),
            se = sd/sqrt(n)) %>%
  view()

hom2 <- hom %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()

hom3 <- hom %>%
  filter(Entity %in% c("United States", "Brazil", "China")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(hom1, aes(x = fct_reorder(Entity, media), y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Japão", "Alemanha", "China",
                              "Coreia do Norte", "Estados Unidos", "Cuba")) +
  labs(x = "Países", y = "Mortes por homicídio (%)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none", axis.text = element_text(color = "black"))

ggplot(hom2, aes(x = Year, y = por_hom, 
                 group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "Mortes por homicídio (%)",
       color = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black"))

ggplot(hom3, aes(x = Year, y = por_hom, 
                 group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "Mortes por homicídio (%)",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
  
  
  
  
  
  