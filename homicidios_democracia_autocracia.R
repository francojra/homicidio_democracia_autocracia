
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


