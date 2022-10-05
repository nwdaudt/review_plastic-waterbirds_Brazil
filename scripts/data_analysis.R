## 
## A synthesis of interactions between waterbirds and plastics in Brazil 
## reveals substantial knowledge gaps and opportunities for research
## 
## Nicholas W Daudt
## Sept 2022
##
## @ This codes builds all results presented in the above-mentioned reference.
## @ At the very end, you can find `sessionInfo()` details.

## Libraries ####
library(tidyverse)
library(sf)
library(mgcv)
library(rnaturalearth)
library(RColorBrewer)
library(patchwork)
library(ggspatial)

#### Data ####

## .csv files were saved using: 
## character set as Unicode UTF-8
## field delimiters as , (comma) 
## string delimiter as "

index_raw_data <- readr::read_csv("./data_raw/index_data.csv")

## a. Index and general info ####

## For analyses, need to filter studies 
## from 2022, grey but published, and a double-published one.

rm_vec <- 
  index_raw_data %>%
  dplyr::filter(Published_Grey == "published_double") %>% 
  dplyr::select(ID) %>%
  dplyr::pull()

rm_vec <-
  append(rm_vec, 
         values = 
           index_raw_data %>%
           dplyr::filter(if_grey_Published == "yes") %>% 
           dplyr::select(ID) %>%
           dplyr::pull())

rm_vec <-
  append(rm_vec, 
         values = 
           index_raw_data %>%
           dplyr::filter(YearPublication >= 2022) %>% 
           dplyr::select(ID) %>%
           dplyr::pull())

# The lower case 'a' is used only for mapping purposes, so get rid of it
rm_vec <- data.frame(ID = rm_vec[! rm_vec %in% ("95a")])

index_data <- dplyr::anti_join(index_raw_data, rm_vec, by = "ID")

## b. Taxa analysed / with-plastic, by reference ####

## From these data sets, it also needs to filter studies 
## from 2022, grey but published, and a double-published one.
## For this, use: "rm_vec"

### Orders
order_analysed <- 
  readr::read_csv("./data_raw/orders_analysed.csv") %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::anti_join(., rm_vec, by = "ID") %>% 
  tidyr::pivot_longer(cols = ! ID,
                      names_to = "orders",
                      values_to = "n") %>%
  dplyr::group_by(orders) %>% 
  dplyr::summarise(n_analysed = sum(n, na.rm = TRUE)) %>%
  dplyr::ungroup(.)

order_w_plastics <- 
  readr::read_csv("./data_raw/orders_with_plastic.csv") %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::anti_join(., rm_vec, by = "ID") %>% 
  tidyr::pivot_longer(cols = ! ID,
                      names_to = "orders",
                      values_to = "n") %>%
  dplyr::group_by(orders) %>% 
  dplyr::summarise(n_with_plastics = sum(n, na.rm = TRUE)) %>%
  dplyr::ungroup(.)

order_final <- 
  dplyr::left_join(order_analysed, order_w_plastics, by = "orders") %>%
  dplyr::mutate(n_without_plastics = n_analysed - n_with_plastics,
                order_FO = 
                  round(((n_with_plastics / n_analysed)*100), digits = 1)) %>%
  dplyr::mutate(order_FO = ifelse(is.nan(order_FO), 0, order_FO)) %>% # for 0/0
  dplyr::arrange(desc(n_analysed))

rm(list = "order_analysed", "order_w_plastics")

### Families
family_analysed <- 
  readr::read_csv("./data_raw/families_analysed.csv") %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::anti_join(., rm_vec, by = "ID") %>% 
  tidyr::pivot_longer(cols = ! ID,
                      names_to = "families",
                      values_to = "n") %>%
  dplyr::group_by(families) %>% 
  dplyr::summarise(n_analysed = sum(n, na.rm = TRUE)) %>%
  dplyr::ungroup(.)

family_w_plastics <- 
  readr::read_csv("./data_raw/families_with_plastic.csv") %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::anti_join(., rm_vec, by = "ID") %>% 
  tidyr::pivot_longer(cols = ! ID,
                      names_to = "families",
                      values_to = "n") %>%
  dplyr::group_by(families) %>% 
  dplyr::summarise(n_with_plastics = sum(n, na.rm = TRUE)) %>%
  dplyr::ungroup(.)

family_final <- 
  dplyr::left_join(family_analysed, family_w_plastics, by = "families") %>%
  dplyr::mutate(n_without_plastics = n_analysed - n_with_plastics,
                family_FO = 
                  round(((n_with_plastics / n_analysed)*100), digits = 1)) %>%
  dplyr::mutate(family_FO = ifelse(is.nan(family_FO), 0, family_FO)) %>% # for 0/0
  dplyr::arrange(desc(n_analysed))

rm(list = "family_analysed", "family_w_plastics")

### Species
species_analysed <- 
  readr::read_csv("./data_raw/species_analysed.csv") %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::anti_join(., rm_vec, by = "ID") %>% 
  tidyr::pivot_longer(cols = ! ID,
                      names_to = "species",
                      values_to = "n") %>%
  dplyr::group_by(species) %>% 
  dplyr::summarise(n_analysed = sum(n, na.rm = TRUE)) %>%
  dplyr::ungroup(.)

species_w_plastics <- 
  readr::read_csv("./data_raw/species_with_plastic.csv") %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::anti_join(., rm_vec, by = "ID") %>% 
  tidyr::pivot_longer(cols = ! ID,
                      names_to = "species",
                      values_to = "n") %>%
  dplyr::group_by(species) %>% 
  dplyr::summarise(n_with_plastics = sum(n, na.rm = TRUE)) %>%
  dplyr::ungroup(.)

species_final <- 
  dplyr::left_join(species_analysed, species_w_plastics, by = "species") %>%
  dplyr::mutate(n_without_plastics = n_analysed - n_with_plastics,
                species_FO = 
                  round(((n_with_plastics / n_analysed)*100), digits = 1)) %>%
  dplyr::mutate(species_FO = ifelse(is.nan(species_FO), 0, species_FO)) %>% # for 0/0
  dplyr::arrange(desc(n_analysed))

rm(list = "species_analysed", "species_w_plastics")

## c. Order / Family ####

taxa_order <- readr::read_csv("./data_raw/taxa_orders.csv")
taxa_family <- readr::read_csv("./data_raw/taxa_families.csv")

#### Results ####
## General information ####

## From review folders (number of documents):
# Published -- 
## 159 w/o plastics + 62 w plastics + 4 'bycatch' + 2 reviews (no new data) = 227
# Grey -- 
## 43 w/o plastics + 34 w plastics = 77
# >> TOTAL NUMBER OF STUDIES SCREENED: 304

## How many published and grey literature?
index_raw_data %>%    # Use "raw" here first
  dplyr::filter(YearPublication < 2022) %>%
  dplyr::group_by(Published_Grey) %>%
  dplyr::summarise(n = n())
# >> Published 61, Grey 34, 1 'double-published' (total of 96)

index_data %>%
  dplyr::group_by(Published_Grey) %>%
  dplyr::summarise(n = n())
# >> Published 60, Grey 26 (total of 87)

## From Grey literature, how many were published later on?
index_raw_data %>%    # Use "raw" here
  dplyr::filter(YearPublication < 2022) %>%
  dplyr::filter(Published_Grey == "grey") %>%
  dplyr::group_by(if_grey_Published) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(percentage = n/34*100)
# >> 8 published, 1 partial, 25 not published

## In which Language the study was published?
index_data %>%
  dplyr::group_by(Language) %>%
  dplyr::summarise(n = n())
# >> 41 English, 45 Portuguese, Spanish 1

## From those written in Portuguese, how many were published?
index_data %>%
  dplyr::filter(Language == "pt") %>%
  dplyr::group_by(Published_Grey) %>%
  dplyr::summarise(n = n())
# >> 20 published, 25 grey

## How many documents report on seabirds?
index_data %>%
  dplyr::filter(Published_Grey == "published" | Published_Grey == "grey") %>%
  dplyr::group_by(is_seabird) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate('%' = (n/87)*100)
# >> 78% seabirds, 22% non-seabird

## What are the oldest and newest sample year (and study)?
index_data %>%
  dplyr::filter(YearSampleStart == min(YearSampleStart, na.rm = TRUE)) %>%
  dplyr::select(Author, YearPublication, Journal_Unpub, YearSampleStart)
# >> Oldest sample (1981) comes from Bege & Pauli's Book;
# > (followed by (1982) from Zarzur 1995 B.Sc. (Hons.), which focused on plastics)

index_data %>%
  dplyr::filter(YearSampleEnd == max(YearSampleEnd, na.rm = TRUE)) %>%
  dplyr::select(Author, YearPublication, Journal_Unpub, YearSampleEnd)
# >> Newest sample (2021) comes from Vanstreels et al. 2021 Mar. Pol. Bull.

## The study sampled dead or alive specimens?
index_data %>%
  dplyr::group_by(BirdCondition) %>%
  dplyr::summarise(n = n())
# >> alive 33; dead 51; (both) alive_dead 3

## Frequency of the sampled number of Orders/Families/Species by study

# max(index_data[index_data$YearPublication < 2022, ]$NumOrders, na.rm = TRUE)
hist(index_data$NumOrders,
     xlab = "Number of Orders", main = "", breaks = 7)

# max(index_data[index_data$YearPublication < 2022, ]$NumFamilies, na.rm = TRUE)
hist(index_data$NumFamilies,
     xlab = "Number of Families", main = "", breaks = 13)

# max(index_data[index_data$YearPublication < 2022, ]$NumSpecies, na.rm = TRUE)
hist(index_data$NumSpecies,
     xlab = "Number of Species", main = "", breaks = 22)

## For how long the sampling was carried out? Mean (sd) of months sampled
mean(index_data$MonthsSampled, na.rm = TRUE)
sd(index_data$MonthsSampled, na.rm = TRUE)
# >> 29.4 (44.4) months == 2.45 (3.7) years

## 1. Is there some temporal bias? #############################################
### 1.1 GAM model ####

# Base data
histTemporal <- 
  index_data %>%
  dplyr::filter(! Published_Grey == "extra_latlon_pub" &
                ! Published_Grey == "extra_latlon_grey") %>%
  dplyr::mutate(Published_Grey = dplyr::recode(Published_Grey,
                                               grey = "Grey",
                                               published = "Published")) %>%
  dplyr::group_by(Published_Grey, YearPublication) %>%
  dplyr::summarise(n = n())

## Fit GAM using both Poisson and Negative Binomial to check best fit
gam_p <- mgcv::gam(n ~ s(YearPublication), data = histTemporal, family = poisson(),
                   method = "REML")
# gam_nb <- mgcv::gam(n ~ s(YearPublication), data = histTemporal, family = "nb",
#                     method = "REML")

mgcv::gam.check(gam_p)
# mgcv::gam.check(gam_nb)

mgcv::plot.gam(gam_p)
# mgcv::plot.gam(gam_nb)

# AIC(gam_p, gam_nb)

# >> Residuals and plot look the same, but Poisson has slightly lower AIC (-2.04)

summary(gam_p)
# Parametric coefficients:
#              Estimate  Std. Error  z value  Pr(>|z|)    
# (Intercept)   0.7908     0.1114      7.096  1.28e-12 ***
#
# Approximate significance of smooth terms:
#                     edf  Ref.df   Chi.sq   p-value  
# s(YearPublication)   1      1      5.741    0.0166 *
# 
# R-sq.(adj) =  0.104   Deviance explained = 16.2%

# rm("gam_nb")

### 1.2 Histogram Published/Grey, with GAM ####

# Poisson GAM
gg_histTemporal_gam <-
  ggplot(histTemporal) + 
  geom_col(aes(x = YearPublication, y = n, fill = factor(Published_Grey))) +
  geom_smooth(data = 
                (histTemporal %>% 
                dplyr::group_by(YearPublication) %>%
                dplyr::summarise(n = n())),
              aes(x = YearPublication, y = n, group = 1),
              method = "gam", formula = y ~ s(x),
              method.args = list(family = poisson(), method = "REML"),
              size = 1.8, color = "black") + 
  labs(x = "", y = "Number of studies") + ## x = "Year of publication"
  scale_fill_brewer(palette = "Dark2") + 
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  theme_bw() + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13.5, color = "black", face = "bold"),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.85),
        legend.text = element_text(size = 12))

### 1.3 Study aim ~ Year ####

# Base data
histAimTemporal <- 
  index_data %>%
  dplyr::filter(! Published_Grey == "extra_latlon_pub" &
                ! Published_Grey == "extra_latlon_grey") %>%
  dplyr::group_by(YearPublication, StudyAim) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(StudyAim = 
                  dplyr::recode(StudyAim,
                                breeding = "Breeding",
                                breeding_diet = "Breeding & diet",
                                humanInteractions = "Human interactions",
                                naturalHistory = "Natural history",
                                occasionalReport = "Occasional",
                                plasticInteraction = "Plastic interaction",
                                stomachContent_diet = "Diet"))

gg_histAimTemporal <-
  ggplot(histAimTemporal) + 
  geom_col(aes(x = YearPublication, y = n, fill = factor(StudyAim))) +
  labs(x = "Year of publication", y = "Number of studies") + 
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  theme_bw() + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13.5, color = "black", face = "bold"),
        legend.title = element_blank(),
        legend.position = c(0.16, 0.675),
        legend.text = element_text(size = 12))

### 1.4 (Patchwork plots) ####
gg_temporal <- gg_histTemporal_gam / gg_histAimTemporal

ggsave(gg_temporal, 
       filename = "./results/histTemporal_gam-aims.png", 
       width = 18, height = 19, units = "cm", dpi = 300)

rm(list = 
     "histAimTemporal", "histTemporal",
     "gg_histTemporal_gam", "gg_histAimTemporal", "gg_temporal", 
     "gam_p")

## 2. Is there some geographical bias? #########################################
### 2.1 Any bias in Environments and Biome sampled? ####

# >> Environment
index_data %>% 
  dplyr::filter(! Published_Grey == "extra_latlon_pub" &
                ! Published_Grey == "extra_latlon_grey") %>%
  dplyr::select(MainSampledEnvironment) %>%
  dplyr::group_by(MainSampledEnvironment) %>% 
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(percentage = n/87*100) %>%
  dplyr::arrange(desc(n))

#                              n    percentage
# 1 sea                       68      78.20
# 2 estuary                    6       6.90
# 3 freshwater                 6       6.90
# 4 mangrove                   3       3.45
# 5 estuary_sea                2       2.30
# 6 freshwater_estuary_sea     1       1.15
# 7 freshwater_sea             1       1.15

# >> Biome
index_data %>% 
  dplyr::filter(! Published_Grey == "extra_latlon_pub" &
                ! Published_Grey == "extra_latlon_grey") %>%
  dplyr::select(BrazilianBiome) %>%
  dplyr::group_by(BrazilianBiome) %>% 
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

# 1 coastalMarine     79
# 2 AtlanticForest     7
# 3 Pampas             1

### 2.2.1 Data prep and base maps ####

## BASE MAP *************************************************************************
# Brazil
brazil_map <-
  rnaturalearth::ne_countries(country = "brazil",
                              returnclass = "sf")
# mapview::mapview(brazil_map)

SA_map <-
  rnaturalearth::ne_countries(continent = 'south america',
                              type = 'countries',
                              returnclass = "sf")
# mapview::mapview(SA_map)

# French Guiana is not part of "South America", apparently.
# So needed to do a bit of a hack here...
FG_map <- 
  rnaturalearth::ne_countries(country = 'france',
                              returnclass = "sf") %>% 
  sf::st_cast(., "MULTIPOLYGON") %>% 
  sf::st_cast(., "POLYGON", do_split = TRUE) %>% 
  tibble::rownames_to_column() %>%
  dplyr::filter(rowname == "55") # French Guiana
# mapview::mapview(FG_map)

# Join South America and French Guiana into a single feature
SA_map <- sf::st_union(SA_map, FG_map)
# mapview::mapview(SA_map) # Done!

rm(list = "FG_map")

### Base maps (ggplot2) ********************************************************
gg_SouthAmerica <-
  ggplot() + 
  geom_sf(data = SA_map, colour = "grey", fill = "lightgrey") +
  theme_bw()

gg_brazil <- 
  gg_SouthAmerica + 
  geom_sf(data = brazil_map, colour = "black", fill = "darkgrey") + 
  labs(x = "Longitude", y = "Latitude") +
  xlim(c(-75, -26)) + ylim(c(-35, 5)) + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 13.5), 
        axis.text = element_text(colour = "black", size = 12),
        axis.line = element_line(colour = "black"))

rm(list = "gg_SouthAmerica")

### Data to map ****************************************************************

# All data (published and unpublished) -- only used for making the grid (below)
data_map_all <-
  index_data %>%
  dplyr::select(Author, YearPublication, Journal_Unpub,
                Lat, Long) %>%
  dplyr::filter(! is.na(Lat)) %>%
  dplyr::mutate(lon = Long, lat = Lat) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_map_all)

### Only published studies
data_map_pub <- 
  index_data %>% 
  # Filter only published studies
  dplyr::filter(! Published_Grey == "grey" &
                ! Published_Grey == "extra_latlon_grey") %>%
  dplyr::select(ID, Author, YearPublication, Journal_Unpub,
                Lat, Long) %>% 
  dplyr::filter(! is.na(Lat)) %>%  # Sick (1997) is dropped off here
  dplyr::mutate(lon = Long, lat = Lat) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_map_pub)

### Only unpublished studies
data_map_unpub <- 
  index_data %>%
  # Filter only grey literature
  dplyr::filter(! Published_Grey == "published" &
                ! Published_Grey == "extra_latlon_pub") %>%
  # Filter only studies that were not published
  dplyr::filter(! if_grey_Published == "yes") %>%
  dplyr::select(ID, Author, YearPublication, Journal_Unpub,
                Lat, Long) %>%
  dplyr::mutate(lon = Long, lat = Lat) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_map_pub)

# Create a union between Brazil polygon (which is only the mainland)
# and the 'centroid' points spatial features from reviewed studies 
# to create & crop the grid (next)
data_sf_union <- sf::st_union(brazil_map, data_map_all)

# Create grid
brazil_grid <- 
  sf::st_make_grid(data_sf_union, 
                   cellsize = c(1,1),
                   square = FALSE)
# mapview::mapview(brazil_grid)

# Transform it into a simple feature (sf) and create an "grid_id"
brazil_grid <- 
  brazil_grid %>% 
  sf::st_sf(.) %>%
  tibble::rowid_to_column(., var = "grid_id")

rm(list = "data_sf_union", "data_map_all")

### Summarise data, per grid *****************************

### Published
n_studies <- sf::st_join(data_map_pub, brazil_grid)

n_studies <- 
  as.data.frame(n_studies) %>% 
  dplyr::group_by(grid_id) %>% 
  dplyr::summarise(n = n())

n_studies <- 
  merge(n_studies, brazil_grid, by = "grid_id") %>% 
  sf::st_as_sf()
# mapview::mapview(n_studies, zcol = "n")

### Unpublished
n_studies_unpub <- sf::st_join(data_map_unpub, brazil_grid)

n_studies_unpub <- 
  as.data.frame(n_studies_unpub) %>% 
  dplyr::group_by(grid_id) %>% 
  dplyr::summarise(n = n())

n_studies_unpub <- 
  merge(n_studies_unpub, brazil_grid, by = "grid_id") %>% 
  sf::st_as_sf()
# mapview::mapview(n_studies_unpub, zcol = "n")

### 2.2.2 Resulting maps ####

# Palette "magma" is color-blind friendly (check also 'cividis')

## Published
gg_n_studies_magma_pub <- 
  gg_brazil + 
  annotate(geom = "text", x = -73, y = 3, label = "a",
           fontface = "bold", color = "black", size = 10) +
  geom_sf(data = n_studies, aes(fill = as.factor(n)), colour = "black") + 
  scale_fill_viridis_d(option = "magma", name = "Published") +
  theme(legend.position = c(0.17, 0.28), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9.5), 
        legend.background = element_rect(linetype = "solid", colour = "black"))

## Unpublished (grey)
gg_n_studies_magma_unpub <- 
  gg_brazil + 
  annotate(geom = "text", x = -73, y = 3, label = "b",
           fontface = "bold", color = "black", size = 10) +
  geom_sf(data = n_studies_unpub, aes(fill = as.factor(n)), colour = "black") + 
  scale_fill_viridis_d(option = "magma", name = "Grey") +
  ggspatial::annotation_scale(style = "bar", location = "br", text_cex = 1, 
                              height = unit(0.20, "cm"),
                              pad_x = unit(4, "mm"), pad_y = unit(5, "mm")) + 
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering(), 
                                    location = "br", which_north = "true", 
                                    height = unit(1, "cm"), width = unit(1, "cm"),
                                    pad_x = unit(8, "mm"), pad_y = unit(8, "mm")) +
  theme(legend.position = c(0.17, 0.22), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9.5), 
        legend.background = element_rect(linetype = "solid", colour = "black"),
        axis.title.y.left = element_blank())

### 2.2.3 (Patchwork maps) ####
map_pub_unpub <- 
  gg_n_studies_magma_pub + gg_n_studies_magma_unpub

ggsave(map_pub_unpub,
       filename = "./results/n_studies_mag.png",
       height = 12, width = 25, units = "cm", dpi = 300)

rm(list = 
      "SA_map", "brazil_map", "map_pub_unpub", "brazil_grid", "gg_brazil",
      "n_studies", "data_map_pub", "n_studies_unpub", "data_map_unpub",
      "gg_n_studies_magma_pub", "gg_n_studies_magma_unpub")

## 3. Is there some taxonomic bias? ############################################
### 3.1.1 Orders and Families - % species analysed ####

### Orders

taxa_order_plot <- 
  taxa_order %>% 
  dplyr::mutate(order_n = paste0(order," ","(",spp_total_order,")")) %>% 
  dplyr::mutate(percent_not_analysed = 
                  round(((spp_total_order - spp_analysed_order) / spp_total_order) * 100,
                        digits = 0),
                percent_analysed = 
                  round((spp_analysed_order / spp_total_order) * 100,
                        digits = 0)) %>% 
  dplyr::mutate(order_n = forcats::fct_reorder(order_n, spp_total_order)) %>%
  dplyr::select(order_n, percent_not_analysed, percent_analysed) %>% 
  tidyr::pivot_longer(cols = -order_n,
                      names_to = "variable",
                      values_to = "value")

percent_orders <- 
  ggplot(taxa_order_plot, aes(y = value, x = order_n, fill = factor(variable))) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("%") +
  scale_fill_grey() + # labels = c("Analysed", "Not analysed")
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        # axis.title = element_text(size = 7, color = "black", face = "bold"),
        axis.title.x = element_blank())

### Families

taxa_families_plot <- 
  taxa_family %>% 
  dplyr::mutate(families_n = paste0(family," ","(",spp_total_family,")")) %>% 
  dplyr::mutate(percent_not_analysed = 
                  round(((spp_total_family - spp_analysed_family) / spp_total_family) * 100,
                        digits = 0),
                percent_analysed = 
                  round((spp_analysed_family / spp_total_family) * 100,
                        digits = 0)) %>% 
  dplyr::mutate(families_n = forcats::fct_reorder(families_n, spp_total_family)) %>%
  dplyr::select(families_n, percent_not_analysed, percent_analysed) %>% 
  tidyr::pivot_longer(cols = -families_n,
                      names_to = "variable",
                      values_to = "value")

percent_families <- 
  ggplot(taxa_families_plot, aes(y = value, x = families_n, fill = factor(variable))) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("%") +
  scale_fill_grey() +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13.5, color = "black", face = "bold"))

### 3.1.2 Orders and Families - no. studies ####

# "Paired" palette is color-blind friendly,
# besides, it matches blue for 'oceanic' and green for 'continental' =)
# RColorBrewer::display.brewer.pal(n = 6, name = "Paired")

## Histogram Orders
order_plot <- 
  order_final %>% 
  dplyr::left_join(., taxa_order[,c("order", "spp_total_order")], by = c("orders" = "order")) %>%
  # Create a categorical and ordered variable
  dplyr::mutate(name = forcats::fct_reorder(orders, spp_total_order)) %>% 
  dplyr::select(orders, name, n_with_plastics, n_without_plastics) %>% 
  tidyr::pivot_longer(cols = ! c(orders, name),
                      names_to = "n",
                      values_to = "value")

# Re-label to use in 'fill' argument on the plot
order_plot <- 
  order_plot %>% 
  dplyr::mutate(
    n2 = 
      # Marine
      ifelse(name == "Procellariiformes" & n == "n_with_plastics" | 
             name == "Phaethontiformes" & n == "n_with_plastics" | 
             name == "Sphenisciformes" & n == "n_with_plastics", 
             yes = "n_w_Marine", 
      no = ifelse(name == "Procellariiformes" & n == "n_without_plastics" | 
                  name == "Phaethontiformes" & n == "n_without_plastics" | 
                  name == "Sphenisciformes" & n == "n_without_plastics", 
                  yes = "n_wo_Marine", 
      # Coastal
      no = ifelse(name == "Charadriiformes" & n == "n_with_plastics" | 
                  name == "Suliformes" & n == "n_with_plastics", 
                  yes = "n_w_Coastal", 
      no = ifelse(name == "Charadriiformes" & n == "n_without_plastics"| 
                  name == "Suliformes" & n == "n_without_plastics", 
                  yes = "n_wo_Coastal", 
      # Continental
      no = ifelse(name == "Pelecaniformes" & n == "n_with_plastics" | 
                  name == "Ciconiiformes" & n == "n_with_plastics" | 
                  name == "Gruiformes" & n == "n_with_plastics" | 
                  name == "Coraciiformes" & n == "n_with_plastics", 
                  yes = "n_w_Continental", 
                  no = "n_wo_Continental"))))))

# Set levels from 'n2' (new label) to the right order
order_plot$n2 <- factor(order_plot$n2, 
                        levels = c("n_wo_Marine", "n_w_Marine", 
                                   "n_wo_Continental", "n_w_Continental", 
                                   "n_wo_Coastal", "n_w_Coastal"))

## Plot
gg_Order <-
  ggplot(order_plot, aes(fill = n2, y = value, x = name)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_brewer(palette = "Paired") + 
  ylab("Number of studies") + xlab("") + 
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        # axis.title = element_text(size = 7, color = "black", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_blank())

## Histogram Families
family_plot <- 
  family_final %>% 
  dplyr::left_join(., taxa_family[,c("family", "spp_total_family")], by = c("families" = "family")) %>%
  # Create a categorical and ordered variable
  dplyr::mutate(name = forcats::fct_reorder(families, spp_total_family)) %>% 
  dplyr::select(families, name, n_with_plastics, n_without_plastics) %>% 
  tidyr::pivot_longer(cols = ! c(families, name),
                      names_to = "n",
                      values_to = "value")

# Re-label to use in 'fill' argument on the plot
family_plot <- 
  family_plot %>% 
  dplyr::mutate(
    n2 = 
      # Marine
      ifelse(name == "Procellariidae" & n == "n_with_plastics" | 
               name == "Spheniscidae" & n == "n_with_plastics" | 
               name == "Diomedeidae" & n == "n_with_plastics" | 
               name == "Stercorariidae" & n == "n_with_plastics" | 
               name == "Phaethontidae" & n == "n_with_plastics",
               yes = "n_w_Marine", 
      no = ifelse(name == "Procellariidae" & n == "n_without_plastics" | 
                  name == "Spheniscidae" & n == "n_without_plastics" | 
                  name == "Diomedeidae" & n == "n_without_plastics" | 
                  name == "Stercorariidae" & n == "n_without_plastics" | 
                  name == "Phaethontidae" & n == "n_without_plastics",
                  yes = "n_wo_Marine", 
      # Coastal
      no = ifelse(name == "Sternidae" & n == "n_with_plastics" | 
                  name == "Sulidae" & n == "n_with_plastics" | 
                  name == "Laridae" & n == "n_with_plastics" | 
                  name == "Fregatidae" & n == "n_with_plastics" |  
                  name == "Haematopodidae" & n == "n_with_plastics" | 
                  name == "Charadriidae" & n == "n_with_plastics" | 
                  name == "Rynchopidae" & n == "n_with_plastics" | 
                  name == "Scolopacidae" & n == "n_with_plastics",
                  yes = "n_w_Coastal", 
      no = ifelse(name == "Sternidae" & n == "n_without_plastics"| 
                  name == "Sulidae" & n == "n_without_plastics" | 
                  name == "Laridae" & n == "n_without_plastics" | 
                  name == "Fregatidae" & n == "n_without_plastics" |  
                  name == "Haematopodidae" & n == "n_without_plastics" | 
                  name == "Charadriidae" & n == "n_without_plastics" | 
                  name == "Rynchopidae" & n == "n_without_plastics" | 
                  name == "Scolopacidae" & n == "n_without_plastics",
                  yes = "n_wo_Coastal", 
      # Continental
      no = ifelse(name == "Ardeidae" & n == "n_with_plastics" | 
                  name == "Phalacrocoracidae" & n == "n_with_plastics" | 
                  name == "Threskiornithidae" & n == "n_with_plastics" | 
                  name == "Alcedinidae" & n == "n_with_plastics" | 
                  name == "Anhingidae" & n == "n_with_plastics" | 
                  name == "Ciconiidae" & n == "n_with_plastics" | 
                  name == "Rallidae" & n == "n_with_plastics", 
                  yes = "n_w_Continental", 
                  no = "n_wo_Continental"))))))

# Set levels from 'n2' (new label) the right order
family_plot$n2 <- factor(family_plot$n2, 
                        levels = c("n_wo_Marine", "n_w_Marine", 
                                   "n_wo_Continental", "n_w_Continental", 
                                   "n_wo_Coastal", "n_w_Coastal"))

## Plot
gg_Family <-
  ggplot(family_plot, aes(fill = n2, y = value, x = name)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_brewer(palette = "Paired") + 
  ylab("Number of studies") + xlab("") + ylim(c(0, 31)) +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13.5, color = "black", face = "bold"),
        axis.text.y = element_blank())

### 3.1.3 (Patchwork plots) ####
ordersss <- 
  percent_orders + gg_Order

familiesss <- 
  percent_families + gg_Family

gg_Taxa <- 
  ordersss / familiesss +
  patchwork::plot_layout(heights = c(.75, 1.25))

ggsave(gg_Taxa, filename = "./results/histTaxa.png",
       width = 15, height = 20, units = "cm", dpi = 300)

rm(list = 
     "taxa_order_plot", "order_plot", "taxa_families_plot", "family_plot",
     "percent_orders", "gg_Order", "percent_families", "gg_Family", 
     "ordersss", "familiesss","gg_Taxa")

### 3.2 The most studied/reported species ####
head(species_final, n = 10)
# # Species                       n_studies   with_plastics   without_plastics   FO%
# 1 Spheniscus_magellanicus             24              24                  0      100  
# 2 Puffinus_puffinus                   16              13                  3       81.2
# 3 Procellaria_aequinoctialis          15              15                  0      100  
# 4 Ardenna_gravis                      13              13                  0      100  
# 5 Thalassarche_chlororhynchos         13              11                  2       84.6
# 6 Sula_leucogaster                    12              11                  1       91.7
# 7 Thalassarche_melanophris            12              12                  0      100  
# 8 Calonectris_borealis                10               9                  1       90  
# 9 Larus_dominicanus                   10               5                  5       50  
# 10 Ardenna_grisea                      8               6                  2       75

## >> see script "table1.R"

# How many analysed species had zero detection of plastics (FO == 0%)?
plyr::count(species_final$species_FO == 0) # 17 spp., but see below:
View(dplyr::arrange(species_final, species_FO)) # Do not consider 'Oceanites_oceanicus' -- study published in 2022
## >> 16 spp. analysed, and without plastics

## 4. What was the aim of studies that reported interactions with plastics? ####

study_aim <- 
  index_data %>% 
  dplyr::filter(! Published_Grey == "extra_latlon_pub" &
                ! Published_Grey == "extra_latlon_grey") %>%
  dplyr::select(StudyAim) %>% 
  dplyr::group_by(StudyAim) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::filter(! is.na(StudyAim)) %>% 
  dplyr::mutate(StudyAim = 
                  dplyr::recode(StudyAim,
                                breeding = "Breeding",
                                breeding_diet = "Breeding & diet",
                                humanInteractions = "Human interactions",
                                naturalHistory = "Natural history",
                                occasionalReport = "Occasional",
                                plasticInteraction = "Plastic interaction",
                                stomachContent_diet = "Diet")) %>%
  # Create a categorical and ordered variable
  dplyr::mutate(StudyAim_f = forcats::fct_reorder(StudyAim, n))

## How many studies did not target plastic interactions?
# n
sum(study_aim[! study_aim$StudyAim == "Plastic interaction", ]$n)
# %
(sum(study_aim[! study_aim$StudyAim == "Plastic interaction", ]$n) / sum(study_aim$n)) * 100
# >> n = 69 (79 %)

## Plot - bar circular
gg_study_aim <- 
  ggplot(data = study_aim, 
         aes(x = StudyAim_f, y = n, fill = StudyAim_f)) +
  geom_col() + 
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 13.5, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.22),
        legend.text = element_text(size = 11.5)) + 
  coord_polar()

ggsave(gg_study_aim, 
       filename = "./results/study_aim.png", 
       width = 13, height = 13, units = "cm", dpi = 300)

rm(list = "study_aim", "gg_study_aim")

## 5. What is the most commonly reported type of interaction? ##################

interactions <- 
  index_data %>% 
  dplyr::filter(! Published_Grey == "extra_latlon_pub" &
                ! Published_Grey == "extra_latlon_grey") %>%
  dplyr::select(Author, YearPublication, Interaction)

interactions %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(n_per_cent = round(n / nrow(interactions) * 100, digits = 1)) %>% 
  dplyr::arrange(desc(n))

# Interaction                  n    n_per_cent
# 1 ingestion                 58       66.7
# 2 nest                      17       19.5
# 3 entanglement               7        8  
# 4 ingestion_entanglement     3        3.4
# 5 other                      2        2.3

rm("interactions")

## 6. Which quantitative metrics are used most by researchers to describe interactions? ####

cols <- colnames(index_data[, c(38:48)])

index_data %>%
  dplyr::select(ID, all_of(cols)) %>%
  dplyr::filter(! is.na(Model)) %>% # Random column only for dropping NA's off
  dplyr::select(-ID) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "metrics",
                      values_to = "value") %>%
  dplyr::group_by(metrics) %>%
  dplyr::summarise(n = sum(value),
                   "%" = round(n/87*100, digits = 0)) %>% # 87 == total nr studies
  dplyr::arrange(desc(n))

# metrics     n   `%`
# FO         47    55 -- frequency of occurrence
# Men        30    34 -- mention
# N          28    33 -- number
# Col        13    15 -- colour
# O          11    13 -- occurrence
# S           9    10 -- size
# M           8     9 -- mass
# FN          6     7 -- numeric frequency
# Model       4     5 -- model
# Pol         3     3 -- polymer
# V           2     2 -- volume

rm("cols")

## 7. How many studies address microplastic in comparison with macroplastics? ####

# Microplastics falls between 1-5 mm, 
# according to Provencher et al. 2017 (Analytical Methods 9: 1454--1469)

# Studies that measured the plastic fragments
size <- index_data %>% dplyr::filter(S == 1)

### Studies that found microplastics
microplastics <- index_data %>% dplyr::filter(MicroPlastic == 1)

rm("size", "microplastics")

## Bonus - aim of the 'grey' studies in the last decade (2010--2020) ###################

grey_aim <-
  index_raw_data %>%
  dplyr::filter(Published_Grey == "grey") %>% 
  dplyr::filter(YearPublication >= 2010 & YearPublication <= 2020) %>%
  dplyr::group_by(StudyAim) %>%
  dplyr::summarise(n = n())

sum(grey_aim$n) # >> 23 'grey' studies in total in the last decade
grey_aim[grey_aim$StudyAim == "plasticInteraction",]$n # >> 4 plasticInteraction

grey_aim[grey_aim$StudyAim == "plasticInteraction",]$n / sum(grey_aim$n) * 100
# >> 17.4 % of 'grey' studies in the last decade (2010--2020) had focused on 
# plastic interactions

#### sessionInfo() ############################################################
sessionInfo()

# R version 4.2.0 (2022-04-22)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.5 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
# 
# locale:
# [1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C               LC_TIME=en_NZ.UTF-8        LC_COLLATE=en_NZ.UTF-8    
# [5] LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8    LC_PAPER=en_NZ.UTF-8       LC_NAME=C                 
# [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] ggspatial_1.1.6     patchwork_1.1.1     RColorBrewer_1.1-3  rnaturalearth_0.1.0 mgcv_1.8-40        
# [6] nlme_3.1-157        sf_1.0-8            forcats_0.5.1       stringr_1.4.0       dplyr_1.0.9        
# [11] purrr_0.3.4         readr_2.1.2         tidyr_1.2.0         tibble_3.1.7        ggplot2_3.3.6      
# [16] tidyverse_1.3.1    
# 
# loaded via a namespace (and not attached):
# [1] fs_1.5.2                satellite_1.0.4         lubridate_1.8.0         bit64_4.0.5            
# [5] webshot_0.5.3           httr_1.4.3              mapview_2.11.0          tools_4.2.0            
# [9] backports_1.4.1         utf8_1.2.2              R6_2.5.1                KernSmooth_2.23-20     
# [13] DBI_1.1.3               colorspace_2.0-3        raster_3.5-21           withr_2.5.0            
# [17] sp_1.5-0                tidyselect_1.1.2        leaflet_2.1.1           bit_4.0.4              
# [21] compiler_4.2.0          leafem_0.2.0            cli_3.3.0               rvest_1.0.2            
# [25] xml2_1.3.3              labeling_0.4.2          scales_1.2.0            classInt_0.4-7         
# [29] proxy_0.4-27            systemfonts_1.0.4       digest_0.6.29           svglite_2.1.0          
# [33] base64enc_0.1-3         pkgconfig_2.0.3         htmltools_0.5.2         dbplyr_2.2.1           
# [37] fastmap_1.1.0           htmlwidgets_1.5.4       rlang_1.0.4             readxl_1.4.0           
# [41] rstudioapi_0.13         farver_2.1.1            generics_0.1.3          jsonlite_1.8.0         
# [45] crosstalk_1.2.0         vroom_1.5.7             magrittr_2.0.3          s2_1.0.7               
# [49] Matrix_1.4-1            Rcpp_1.0.9              munsell_0.5.0           fansi_1.0.3            
# [53] lifecycle_1.0.1         terra_1.6-7             yaml_2.3.5              stringi_1.7.6          
# [57] plyr_1.8.7              grid_4.2.0              parallel_4.2.0          crayon_1.5.1           
# [61] lattice_0.20-45         haven_2.5.0             splines_4.2.0           hms_1.1.1              
# [65] leafpop_0.1.0           pillar_1.7.0            uuid_1.1-0              codetools_0.2-18       
# [69] stats4_4.2.0            wk_0.6.0                reprex_2.0.1            glue_1.6.2             
# [73] leaflet.providers_1.9.0 modelr_0.1.8            vctrs_0.4.1             png_0.1-7              
# [77] tzdb_0.3.0              cellranger_1.1.0        gtable_0.3.0            assertthat_0.2.1       
# [81] broom_1.0.0             e1071_1.7-11            viridisLite_0.4.0       class_7.3-20           
# [85] units_0.8-0             brew_1.0-7              ellipsis_0.3.2         