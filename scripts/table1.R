## 
## A synthesis of interactions between waterbirds and plastics in Brazil 
## reveals substantial knowledge gaps and opportunities for research
## 
## Nicholas W Daudt
## April 2022
##
## This code builds Table 1 from the main text.
## Note that data was inputed manually for each reference, within each species.
## At the very end, you can find sessionInfo() specifications.

library(readr)
library(dplyr)

### Data ####

## Index

index_raw_data <- readr::read_csv("./data_raw/index_data.csv")

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

## Species

species_analysed <- 
  readr::read_csv("./data_raw/species_analysed.csv") %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::anti_join(., rm_vec, by = "ID")

rm("rm_vec", "index_raw_data")

### Table ######################################################################
### 1 Spheniscus magellanicus ####
s_mag_id <- 
  species_analysed %>% 
  dplyr::filter(Spheniscus_magellanicus == 1) %>%
  dplyr::select(ID)

s_magellanicus <- 
  index_data %>% 
  dplyr::semi_join(., s_mag_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

s_magellanicus <- 
  dplyr::mutate(s_magellanicus,
                sample_size = c(67, 144, 144, 40, 9, 47, 
                                175, 14875, 46, 86, 365, 41, 
                                63, 13, 1, 73, 40, 25, 
                                20, 16, 470, 35, 41, 18),
                freq_occ = c(16.4, 79.9, 79.9, 30, 22.2, 13, 
                             14.9, 0.05, 87, 67.4, 15.3, 29.3, 
                             42.9, 38, 100, 43.8, 37.5, 20,
                             2, 50, 15.1, 22.8, 39, 16.7)) %>%
  # "Split" (by hand) Pelanda's data, to have one row for ingestion / entanglement
  rbind(data.frame(ID = "56a", Author = "Pelanda, AA", YearPublication = 2007, 
                   YearSampleStart = 2006, YearSampleEnd = 2007,
                   Interaction = "entanglement", sample_size = 155, freq_occ = 2.6)) %>%
  dplyr::mutate(Interaction = ifelse(Interaction == "ingestion_entanglement", "ingestion",
                                     Interaction))

s_magellanicus <- 
  s_magellanicus %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0((sum(freq_occ > 0, na.rm = T)/Studies*100), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Spheniscus magellanicus", .before = everything()) %>%
  dplyr::ungroup(.)

rm("s_mag_id")

### 2 Puffinus puffinus * ####
p_puff_id <- 
  species_analysed %>% 
  dplyr::filter(Puffinus_puffinus == 1) %>%
  dplyr::select(ID)

p_puffinus <- 
  index_data %>% 
  dplyr::semi_join(., p_puff_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

p_puffinus <- 
  dplyr::mutate(p_puffinus,
                sample_size = c(NA, 25, 25, 7, 25, 6, 5, 295, 32, 
                                9, 52, 16, 1, 3, 20, 34),
                freq_occ = c(NA, 32, 48, 85.7, 60, 17, 60, 0, 12.5, 
                             33.3, 50, 31.2, 0, 0, 35, 38.2)) %>%
  # No entanglement analysed for this species in Pelanda's study
  dplyr::mutate(Interaction = ifelse(Interaction == "ingestion_entanglement", "ingestion",
                                     Interaction))

p_puffinus <- 
  p_puffinus %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0((sum(freq_occ > 0, na.rm = T)/Studies*100), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Puffinus puffinus", .before = everything()) %>%
  dplyr::ungroup(.)

rm("p_puff_id")

### 3 Procellaria aequinoctialis ####
p_aequi_id <- 
  species_analysed %>% 
  dplyr::filter(Procellaria_aequinoctialis == 1) %>%
  dplyr::select(ID)

p_aequinoctialis <- 
  index_data %>% 
  dplyr::semi_join(., p_aequi_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

p_aequinoctialis <- 
  dplyr::mutate(p_aequinoctialis,
                sample_size = c(36, 32, 4, 41, 34, 1, 256, 114, 
                                10, 1, 6, 32, 4, 4, 11),
                freq_occ = c(33.3, 56.2, 25, 49, 44, 100, 1.2, 51.7,
                             50, 100, 50, 68.7, 25, 75, 54.5)) %>%
  # No entanglement analysed for this species in Pelanda's study
  dplyr::mutate(Interaction = ifelse(Interaction == "ingestion_entanglement", "ingestion",
                                     Interaction))

p_aequinoctialis <- 
  p_aequinoctialis %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0((sum(freq_occ > 0, na.rm = T)/Studies*100), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Procellaria aequinoctialis", .before = everything()) %>%
  dplyr::ungroup(.)

rm("p_aequi_id")

### 4 Ardenna gravis ####
a_grav_id <- 
  species_analysed %>% 
  dplyr::filter(Ardenna_gravis == 1) %>%
  dplyr::select(ID)

a_gravis <- 
  index_data %>% 
  dplyr::semi_join(., a_grav_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

a_gravis <- 
  dplyr::mutate(a_gravis,
                sample_size = c(110, 121, 29, 18, 6, 4, 428, 
                                16, 1, 4, 21, 1, 46),
                freq_occ = c(70, 71.9, 72.4, 89, 100, 75, 0.23, 
                             56.3, 100, 75, 95.2, 100, 78.3)) %>%
  # No entanglement analysed for this species in Pelanda's study
  dplyr::mutate(Interaction = ifelse(Interaction == "ingestion_entanglement", "ingestion",
                                     Interaction))

a_gravis <- 
  a_gravis %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0((sum(freq_occ > 0, na.rm = T)/Studies*100), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Ardenna gravis", .before = everything()) %>%
  dplyr::ungroup(.)

rm("a_grav_id")

### 5 Thalassarche chlororhynchos ####
th_chloro_id <- 
  species_analysed %>% 
  dplyr::filter(Thalassarche_chlororhynchos == 1) %>%
  dplyr::select(ID)

th_chlororhynchos <- 
  index_data %>% 
  dplyr::semi_join(., th_chloro_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

th_chlororhynchos <- 
  dplyr::mutate(th_chlororhynchos,
                sample_size = c(7, 26, 9, 27, 13, 170, 1, 6, 
                                2, 17, 5, 80, 2),
                freq_occ = c(14.3, 11, 44.4, 7, 8, 1.2, 100, 33.3, 
                             0, 11.8, 60, 1.2, 0))

th_chlororhynchos <- 
  th_chlororhynchos %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0(round((sum(freq_occ > 0, na.rm = T)/Studies*100), digits = 1), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Thalassarche chlororhynchos", .before = everything()) %>%
  dplyr::ungroup(.)

rm("th_chloro_id")

### 6 Sula leucogaster * ####
s_leuco_id <- 
  species_analysed %>% 
  dplyr::filter(Sula_leucogaster == 1) %>%
  dplyr::select(ID)

s_leucogaster <- 
  index_data %>% 
  dplyr::semi_join(., s_leuco_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

s_leucogaster <- 
  dplyr::mutate(s_leucogaster,
                sample_size = c(NA, 31, 123, NA, 171, 203, 44, 
                                93, 12, 21, 126, 288),
                freq_occ = c(NA, 3.2, NA, NA, NA, 61, 18, 
                             20.4, 0, 4.8, 30, 16.3)) %>% 
  # No entanglement analysed for this species in Pelanda's study
  dplyr::mutate(Interaction = ifelse(Interaction == "ingestion_entanglement", "ingestion",
                                   Interaction)) %>%
  # In addition, we've split Kohlrausch's data to reflect different colonies;
  # above, data from ASPSP; below, data from Atol das Rocas
  rbind(data.frame(ID = "93a", Author = "Kohlrausch, AB", YearPublication = 2003, 
                   YearSampleStart = 1999, YearSampleEnd = 2002,
                   Interaction = "nest", sample_size = 268, freq_occ = 0))

## Schulz-Neto 1998 and Krul 2004 did not report sample_size neither freq_occ;
## Coelho et al. 2004 and Barbosa-Filho & Vooren 2009 did not report freq_occ;
## All those authors only mention the use of plastics as nest material.

s_leucogaster <- 
  s_leucogaster %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0(round((sum(freq_occ > 0, na.rm = T)/Studies*100), digits = 1), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Sula leucogaster", .before = everything()) %>%
  dplyr::ungroup(.)

## Remember that 'No. studies' is now with 1 study more, as we treated different
## colonies of Kohlrausch's work as different data-points.

rm("s_leuco_id")

### 7 Thalassarche chlororhynchos ####
th_mela_id <- 
  species_analysed %>% 
  dplyr::filter(Thalassarche_melanophris == 1) %>%
  dplyr::select(ID)

th_melanophris <- 
  index_data %>% 
  dplyr::semi_join(., th_mela_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

th_melanophris <- 
  dplyr::mutate(th_melanophris,
                sample_size = c(35, 45, 35, 26, 59, 31, 2, 
                                207, 7, 10, 25, 8),
                freq_occ = c(20, 14, 29, 73, 12, 6, 100,
                             1.9, 57.1, 10, 12, 12.5))

th_melanophris <- 
  th_melanophris %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0(round((sum(freq_occ > 0, na.rm = T)/Studies*100), digits = 1), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Thalassarche melanophris", .before = everything()) %>%
  dplyr::ungroup(.)

rm("th_mela_id")

### 8 Calonectris borealis ####
c_bore_id <- 
  species_analysed %>% 
  dplyr::filter(Calonectris_borealis == 1) %>%
  dplyr::select(ID)

c_borealis <- 
  index_data %>% 
  dplyr::semi_join(., c_bore_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

c_borealis <- 
  dplyr::mutate(c_borealis,
                sample_size = c(112, 5, 185, 349, 38, 7, 34, 4, 5, 1),
                freq_occ = c(100, 100, 81, 0, 23.7, 42.8, 38.2, 50, 80, 0))

c_borealis <- 
  c_borealis %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0(round((sum(freq_occ > 0, na.rm = T)/Studies*100), digits = 1), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Calonectris borealis", .before = everything()) %>%
  dplyr::ungroup(.)

rm("c_bore_id")

### 9 Larus dominicanus * ####
l_dom_id <- 
  species_analysed %>% 
  dplyr::filter(Larus_dominicanus == 1) %>%
  dplyr::select(ID)

l_dominicanus <- 
  index_data %>% 
  dplyr::semi_join(., l_dom_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction) #%>%

l_dominicanus <- 
  dplyr::mutate(l_dominicanus,
                sample_size = c(NA, 4, NA, 212, 19, 13, 120, 30, 2, 107),
                freq_occ = c(0, 0, NA, 1.9, 0, 0, 21.7, 0, 50, 0.9)) %>%
  # No entanglement analysed for this species in Pelanda's study
  dplyr::mutate(Interaction = ifelse(Interaction == "ingestion_entanglement", "ingestion",
                                     Interaction))

## Krul 2004 did not report sample_size, but mention no plastics;
## Scherer et al. 2011 did not report sample_size, therefore we are unable to 
## calculate FO% -- however, authors reported 1 individual entangled in fishing net;
## Assuming the shoelace reported by Soares & Schiefler 1995 happened once,
## we calculated freq_occ accordingly.

l_dominicanus <- 
  l_dominicanus %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0(round((sum(freq_occ > 0, na.rm = T)/Studies*100), digits = 1), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Larus dominicanus", .before = everything()) %>%
  dplyr::ungroup(.)

# Given explanation above, modify cell accordingly
l_dominicanus$`Studies with plastics (%)`[l_dominicanus$Interaction == "entanglement"] <- "100%"

rm("l_dom_id")

### 10 Ardenna grisea ####
a_gris_id <- 
  species_analysed %>% 
  dplyr::filter(Ardenna_grisea == 1) %>%
  dplyr::select(ID)

a_grisea <- 
  index_data %>% 
  dplyr::semi_join(., a_gris_id, by = "ID") %>%
  dplyr::select(ID, Author, YearPublication,
                YearSampleStart, YearSampleEnd, Interaction)

a_grisea <- 
  dplyr::mutate(a_grisea,
                sample_size = c(27, 17, 11, 1, 142, 1, 4, 3),
                freq_occ = c(37, 59, 63.6, 100, 0, 0, 50, 66.7)) %>% 
  # No entanglement analysed for this species in Pelanda's study
  dplyr::mutate(Interaction = ifelse(Interaction == "ingestion_entanglement", "ingestion",
                                     Interaction))

a_grisea <- 
  a_grisea %>%
  dplyr::group_by(Interaction) %>% 
  dplyr::summarise(
    "Studies" = n(),
    "Studies with plastics (%)" = 
      paste0(round((sum(freq_occ > 0, na.rm = T)/Studies*100), digits = 1), "%"),
    "Year (range)" = paste0(min(YearSampleStart, na.rm = T), "–",
                            max(YearSampleEnd, na.rm = T)),
    "Sample size (total)" = sum(sample_size, na.rm = T),
    "Sample size (range)" = 
      paste0(min(sample_size, na.rm = T), "–", max(sample_size, na.rm = T)),
    "Frequency of occurrence (mean ± SD)" = 
      paste0(
        round(mean(freq_occ, na.rm = T), digits = 1), "±", 
        round(sd(freq_occ, na.rm = T), digits = 1)),
    "Frequency of occurrence (range)" = 
      paste0(min(freq_occ, na.rm = T), "–", max(freq_occ, na.rm = T))) %>% 
  dplyr::arrange(desc(Studies)) %>%
  dplyr::mutate(Species = "Ardenna grisea", .before = everything()) %>%
  dplyr::ungroup(.)

rm("a_gris_id")

### Row bind them and save it ####
table_1 <- 
  rbind(s_magellanicus, p_puffinus, p_aequinoctialis,
        a_gravis, th_chlororhynchos, s_leucogaster,
        th_melanophris, c_borealis, l_dominicanus,
        a_grisea)

readr::write_csv(table_1,
                 "./results/table1.csv")

### sessionInfo() ##############################################################
sessionInfo()

# R version 4.1.2 (2021-11-01)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.3 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
# 
# locale:
# [1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C              
# [3] LC_TIME=en_NZ.UTF-8        LC_COLLATE=en_NZ.UTF-8    
# [5] LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8   
# [7] LC_PAPER=en_NZ.UTF-8       LC_NAME=C                 
# [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] dplyr_1.0.6 readr_1.4.0
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.7          pillar_1.6.2        compiler_4.1.2      forcats_0.5.1      
# [5] class_7.3-19        tools_4.1.2         gtable_0.3.0        lifecycle_1.0.0    
# [9] tibble_3.1.3        nlme_3.1-152        lattice_0.20-45     mgcv_1.8-38        
# [13] pkgconfig_2.0.3     rlang_0.4.11        Matrix_1.4-0        DBI_1.1.1          
# [17] cli_3.0.1           rstudioapi_0.13     patchwork_1.1.1     e1071_1.7-7        
# [21] generics_0.1.0      vctrs_0.3.8         hms_1.1.0           classInt_0.4-3     
# [25] grid_4.1.2          tidyselect_1.1.1    glue_1.4.2          sf_0.9-8           
# [29] R6_2.5.0            rnaturalearth_0.1.0 fansi_0.5.0         sp_1.4-5           
# [33] ggplot2_3.3.5       tidyr_1.1.3         purrr_0.3.4         magrittr_2.0.1     
# [37] scales_1.1.1        ellipsis_0.3.2      splines_4.1.2       units_0.7-1        
# [41] assertthat_0.2.1    colorspace_2.0-2    utf8_1.2.2          KernSmooth_2.23-20 
# [45] proxy_0.4-25        munsell_0.5.0       ggspatial_1.1.5     crayon_1.4.1 