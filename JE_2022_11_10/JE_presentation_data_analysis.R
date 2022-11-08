library(tidyverse)
library(readxl)
library(qdapRegex) # for ex_between
library(stringi) # for stri_trans_general
library(stringdist) # for phoenetic
library(data.table) # faster on some processes
library(sf) # spatial data
library(tmap)

tmap_mode("view")

#### importing and addins on data init ####
# networks datasets
reseau_jacoubet_1836 <- st_read(dsn = "spatial_data/1836_jacoubet.shp") %>%
  rowid_to_column()
reseau_andriveau_1849 <- st_read(dsn = "spatial_data/1849_andriveau.shp")%>%
  rowid_to_column()
reseau_1854 <- st_read(dsn = "spatial_data/1854.shp")%>%
  rowid_to_column()
reseau_1871 <- st_read(dsn = "spatial_data/1871.shp")%>%
  rowid_to_column()
reseau_poubelle_1888 <- st_read(dsn = "spatial_data/1888_poubelle.shp")%>%
  rowid_to_column()

reseau_actuel_2018 <- st_read(dsn = "spatial_data/lineaire_voie_paris_2018.geojson") %>%
  st_transform(x = ., crs = 2154) %>%
  rowid_to_column()

### plotting networks
netwokrs_carto <- reseau_jacoubet_1836 %>%
  mutate(tempo = 1836) %>%
  select(tempo) %>%
  bind_rows(
    reseau_andriveau_1849 %>%
      mutate(tempo = 1849) %>%
      select(tempo)
  ) %>%
  bind_rows(
    reseau_1854 %>%
      mutate(tempo = 1854) %>%
      select(tempo)
  ) %>%
  bind_rows(
    reseau_1871 %>%
      mutate(tempo = 1871) %>%
      select(tempo)
  ) %>%
  bind_rows(
    reseau_poubelle_1888 %>%
      mutate(tempo = 1888) %>%
      select(tempo)
  )

ggplot() +
  geom_sf(data = netwokrs_carto, color = "grey70", size = 0.2) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(subtitle = "Street networks",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: GeoHistoricalData") +
  facet_wrap(~tempo)

ggsave(filename = "outputs_visuals/street_network.png", plot = last_plot(), 
       dpi = 300, width = 20, height = 18, units = "cm")

# data based on selected Annuaires
general_data <- read_excel(path = "geocoded_entries.xlsx")
general_data

# population data
pop_before_1860 <- read_excel(path = "spatial_data/pop_avt_1860.xlsx")
before_1860_paris <- pop_before_1860 %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

pop_after_1860 <- read_excel(path = "spatial_data/pop_apres_1860.xlsx")
after_1860_paris <- pop_after_1860 %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

pop_all_long <- before_1860_paris %>%
  select(`1836_S001_p8_pop_droit`:`1856_S001_p8_pop_droit`) %>%
  pivot_longer(cols = everything(), names_to = "date", values_to = "pop") %>%
  mutate(date = str_sub(string = date, start = 1, end = 4)) %>%
  mutate(published = case_when(
    date == 1841 ~ 1839,
    date == 1846 ~ 1845,
    date == 1856 ~ 1855,
    TRUE ~ 0
  )) %>%
  bind_rows(
    after_1860_paris %>%
      select(`1866_S001_pop_droit`:`1896_S005_pop_fait`) %>%
      pivot_longer(cols = everything(), names_to = "date", values_to = "pop") %>%
      mutate(droifait = str_sub(string = date, start = 15, end = 20)) %>%
      mutate(date = str_sub(string = date, start = 1, end = 4)) %>%
      filter(droifait == "droit") %>%
      select(-droifait) %>%
      mutate(published = case_when(
        date == 1866 ~ 1864,
        date == 1876 ~ 1875,
        date == 1886 ~ 1885,
        date == 1891 ~ 1893,
        TRUE ~ 0
      ))
  )

#### adding informations from extracting NER elements patterns ####
general_data_addin <- general_data %>%
  # when new line in string pattern "\n", function pattern does not work
  mutate(ner_xml = str_replace_all(string = ner_xml, pattern = "\n", " ")) %>%
  # extracting elements
  mutate(persons = qdapRegex::ex_between(text.var = ner_xml, left = "<PER>", right = "</PER>")) %>%
  mutate(activity = ex_between(text.var = ner_xml, left = "<ACT>", right = "</ACT>")) %>%
  # removing specific characters for activity by applying the encoding Latin-ASCII
  mutate(act_new = stri_trans_general(str = as.character(activity), id = "Latin-ASCII")) %>%
  # phonetic index
  mutate(act_phoenetic = phonetic(x = act_new)) %>%
  mutate(location = ex_between(text.var = ner_xml, left = "<LOC>", right = "</LOC>")) %>%
  mutate(cardinality = ex_between(text.var = ner_xml, left = "<CARDINAL>", right = "</CARDINAL>")) %>%
  # application of count functions
  mutate(nb_activities = as.numeric(lapply(activity, function(x) length(x)))) %>%
  mutate(nb_activities = if_else(is.na(activity), 0, nb_activities)) %>%
  mutate(nb_persons = as.numeric(lapply(persons, function(x) length(x)))) %>%
  mutate(nb_persons = if_else(is.na(persons), 0, nb_persons)) %>%
  mutate(nb_loc = as.numeric(lapply(location, function(x) length(x)))) %>%
  mutate(nb_loc = if_else(is.na(location), 0, nb_loc)) %>%
  mutate(nb_card = as.numeric(lapply(cardinality, function(x) length(x)))) %>%
  mutate(nb_card = if_else(is.na(cardinality), 0, nb_card)) %>%
  # as.character for final dataframe
  mutate(activity = as.character(activity)) %>%
  mutate(persons = as.character(persons)) %>%
  mutate(location = as.character(location)) %>%
  mutate(cardinality = as.character(cardinality))

#### adding phoenetic index frequency ####
# specific function
max_freq <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# applying function on data as data.table (faster)
general_data_addin <- setDT(general_data_addin)[, freq_max:= max_freq(act_new), by = act_phoenetic][] %>%
  as_tibble()

# specific for pres ~ no 1904
general_data_addin <- general_data_addin %>%
  filter(published != 1904)

#### boulangers ####
# exploration of patterns filtering on data
general_data_addin %>%
  mutate(patternsearch = str_detect(string = act_new, pattern = "boulang|boutang|boulany|boutany")) %>%
  filter(patternsearch == TRUE) %>%
  select(activity, act_new) %>%
  distinct() %>%
  view()
# need supressions too

# creation of dataset specific to bakers
boulanger <- general_data_addin %>%
  mutate(patternsearch = str_detect(string = act_new, pattern = "boulang|boutang|boulany|boutany")) %>%
  filter(patternsearch == TRUE) %>%
  mutate(patternsearch = str_detect(string = act_new, pattern = "bureau|bois|directeur|fabr|plac|ustens|vent")) %>%
  filter(patternsearch == FALSE)

#### carto initiale ####
# create boulanger spatial data as "certain" (need improvment! here just nb_loc = 1)
# critical vision
boulanger %>%
  mutate(geoloca = if_else(condition = is.na(precise.geom), "no", "yes")) %>%
  group_by(nb_loc, geoloca) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = nb_loc, y = freq, fill = geoloca)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggthemes::scale_fill_economist() +
  scale_x_continuous(name = "n locations by entry", breaks = seq(1, 12, 1)) +
  labs(subtitle = "Bakers in selected Annuaires",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: Annuaires, GeoHistoricalData")

ggsave(filename = "outputs_visuals/bakers_sources_criticism.png", plot = last_plot(), 
       dpi = 300, width = 16, height = 11, units = "cm")

# dataset
boulanger_sf <- boulanger %>%
  mutate(geoloca = if_else(condition = is.na(precise.geom), "no", "yes")) %>%
  filter(geoloca == "yes") %>%
  filter(nb_loc == 1 & !is.na(overall_score)) %>%
  st_as_sf(wkt = "precise.geom", crs = 2154)

tm_shape(boulanger_sf) + tm_dots()

### snap points patterns of Annuaires on each of matching network
# list of snap_points by Annuaire
liste_patterns_profession <- boulanger_sf %>%
  arrange(published) %>%
  group_split(.tbl = ., published) # equivalent to date

# list of network equivalent length to matching choices with Annuaires
list_of_networks <- list(reseau_jacoubet_1836, reseau_andriveau_1849, reseau_1854, reseau_1871, reseau_1871, reseau_poubelle_1888,
                         reseau_poubelle_1888)

# snaping points on networks
liste_patterns_snap <- list()

for (i in 1:length(liste_patterns_profession)) {
  snap_point_geometrique <- maptools::snapPointsToLines(points = as(liste_patterns_profession[[i]], "Spatial"), 
                                                        lines = as(list_of_networks[[i]], "Spatial"), 
                                                        maxDist = 100, idField = "rowid")
  
  snap_point_geometrique <- st_as_sf(snap_point_geometrique) %>%
    mutate(source = liste_patterns_profession[[i]]$directory[1]) %>%
    mutate(source_annee = liste_patterns_profession[[i]]$published[1])
  
  liste_patterns_snap[[i]] <- snap_point_geometrique
  
}

## cartography of bakers
liste_snap_as_one_sf <- do.call(rbind, liste_patterns_snap)
netwokrs_carto_2 <- netwokrs_carto %>% 
  mutate(source_annee = case_when(
    tempo == 1836 ~ 1839,
    tempo == 1849 ~ 1845,
    tempo == 1854 ~ 1855,
    tempo == 1871 ~ 1864,
    TRUE ~ 1885
  )) %>%
  bind_rows(
    netwokrs_carto %>% filter(tempo %in% c(1871, 1888)) %>% mutate(source_annee = if_else(tempo == 1871, 1875, 1893))
  )

ggplot() +
  geom_sf(data = netwokrs_carto_2, color = "grey70", size = 0.2) +
  geom_sf(data = liste_snap_as_one_sf, alpha = 0.7, color = "red") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(subtitle = "Bakers",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: GeoHistoricalData") +
  facet_wrap(~source_annee, nrow = 2)

ggsave(filename = "outputs_visuals/bakers_cartography.png", plot = last_plot(), 
       dpi = 300, width = 25, height = 25, units = "cm")

# writting for backgroundjob
saveRDS(object = liste_patterns_snap, file = "data_for_bckgrd/bakers_list.rds")
saveRDS(object = list_of_networks, file = "data_for_bckgrd/network_list.rds")


### observed VS simulated data distances ####
# reading results from background job: distances matrices
tableau_plot <- readRDS("data_for_bckgrd/tableau_plot_bakers.rds")

# plotting data 
# !!! auto warning !!! breaks of axis x and y are here chosen
plots_rss <- list()

for (i in 1:length(tableau_plot)) {
  plots_rss[[i]] <- tableau_plot[[i]] %>%
    mutate(type = if_else(
      condition = is.na(sim), "observed", "random\n(20 sim.)" 
    )) %>%
    ggplot(aes(x = dist_pi_p/1000, group = sim, color = type)) +
    geom_density(size = 0.3) +
    ggthemes::scale_color_colorblind() +
    ggthemes::theme_igray() +
    scale_x_continuous(name = "Distance (in km)", breaks = c(seq(0,15,2.5)), limits = c(0,14)) +
    scale_y_continuous(breaks = c(seq(0, 0.35, 0.1)), limits = c(0,0.35)) +
    ggtitle(tableau_plot[[i]]$source_annee[1])
}

distribution <- ggpubr::ggarrange(plotlist = plots_rss, 
                                  common.legend = TRUE, legend = c("bottom"), nrow = 2, ncol = 4)

ggpubr::annotate_figure(distribution, top = "Bakers", fig.lab.size = 15)

ggsave(filename = "outputs_visuals/bakers_matrices_distribution.png", plot = last_plot(), 
       dpi = 300, width = 25, height = 17, units = "cm")

#### epiciers ####
# exploration of patterns filtering on data
general_data_addin %>%
  mutate(patternsearch = str_detect(string = act_new, pattern = "epici|cpic|e picier")) %>%
  filter(patternsearch == TRUE) %>%
  select(activity, act_new, act_phoenetic) %>%
  distinct() %>%
  view()
# need supressions too

# creation of dataset specific to bakers
epicier <- general_data_addin %>%
  mutate(patternsearch = str_detect(string = act_new, pattern = "epici|cpic|e picier")) %>%
  filter(patternsearch == TRUE) %>%
  mutate(patternsearch = str_detect(string = act_new, pattern = "repic|qepic")) %>%
  filter(patternsearch == FALSE)

#### carto initiale ####
# create boulanger spatial data as "certain" (need improvment! here just nb_loc = 1)
# critical vision
epicier %>%
  mutate(geoloca = if_else(condition = is.na(precise.geom), "no", "yes")) %>%
  group_by(nb_loc, geoloca) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = nb_loc, y = freq, fill = geoloca)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggthemes::scale_fill_economist() +
  scale_x_continuous(name = "n locations by entry", breaks = seq(1, 12, 1)) +
  labs(subtitle = "Grocers in selected Annuaires",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: Annuaires, GeoHistoricalData")

ggsave(filename = "outputs_visuals/epiciers_sources_criticism.png", plot = last_plot(), 
       dpi = 300, width = 16, height = 11, units = "cm")

# dataset
epicier_sf <- epicier %>%
  mutate(geoloca = if_else(condition = is.na(precise.geom), "no", "yes")) %>%
  filter(geoloca == "yes") %>%
  filter(nb_loc == 1 & !is.na(overall_score)) %>%
  st_as_sf(wkt = "precise.geom", crs = 2154)

tm_shape(epicier_sf) + tm_dots()

### snap points patterns of Annuaires on each of matching network
# list of snap_points by Annuaire
liste_patterns_profession <- epicier_sf %>%
  arrange(published) %>%
  group_split(.tbl = ., published) # equivalent to date

# snaping points on networks
liste_patterns_snap <- list()

for (i in 1:length(liste_patterns_profession)) {
  snap_point_geometrique <- maptools::snapPointsToLines(points = as(liste_patterns_profession[[i]], "Spatial"), 
                                                        lines = as(list_of_networks[[i]], "Spatial"), 
                                                        maxDist = 100, idField = "rowid")
  
  snap_point_geometrique <- st_as_sf(snap_point_geometrique) %>%
    mutate(source = liste_patterns_profession[[i]]$directory[1]) %>%
    mutate(source_annee = liste_patterns_profession[[i]]$published[1])
  
  liste_patterns_snap[[i]] <- snap_point_geometrique
  
}

## cartography of epicier_sf
liste_snap_as_one_sf <- do.call(rbind, liste_patterns_snap)

ggplot() +
  geom_sf(data = netwokrs_carto_2, color = "grey70", size = 0.2) +
  geom_sf(data = liste_snap_as_one_sf, alpha = 0.7, color = "red") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(subtitle = "Grocers - 'épiciers'",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: GeoHistoricalData") +
  facet_wrap(~source_annee, nrow = 2)

ggsave(filename = "outputs_visuals/grocers_cartography.png", plot = last_plot(), 
       dpi = 300, width = 25, height = 25, units = "cm")

# writting for backgroundjob
saveRDS(object = liste_patterns_snap, file = "data_for_bckgrd/grocers_list.rds")

### observed VS simulated data distances ####
# reading results from background job: distances matrices
tableau_plot <- readRDS("data_for_bckgrd/tableau_plot_grocers.rds")

# plotting data
# !!! auto warning !!! breaks of axis x and y are here chosen
plots_rss <- list()

for (i in 1:length(tableau_plot)) {
  plots_rss[[i]] <- tableau_plot[[i]] %>%
    mutate(type = if_else(
      condition = is.na(sim), "observed", "random\n(20 sim.)" 
    )) %>%
    ggplot(aes(x = dist_pi_p/1000, group = sim, color = type)) +
    geom_density(size = 0.3) +
    ggthemes::scale_color_colorblind() +
    ggthemes::theme_igray() +
    scale_x_continuous(name = "Distance (in km)", breaks = c(seq(0,15,2.5)), limits = c(0,14)) +
    scale_y_continuous(breaks = c(seq(0, 0.35, 0.1)), limits = c(0,0.35)) +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    ggtitle(tableau_plot[[i]]$source_annee[1])
}

distribution <- ggpubr::ggarrange(plotlist = plots_rss, 
                                  common.legend = TRUE, legend = c("bottom"), nrow = 2, ncol = 4)

ggpubr::annotate_figure(distribution, top = "Grocers - 'épiciers'", fig.lab.size = 15)

ggsave(filename = "outputs_visuals/grocers_matrices_distribution.png", plot = last_plot(), 
       dpi = 300, width = 25, height = 17, units = "cm")


#### bijoutiers ####
# creation of dataset specific to bakers
bijoutier <- general_data_addin %>%
  mutate(pattern = str_detect(string = act_new, pattern = "bijo")) %>% 
  filter(pattern == TRUE) %>%
  mutate(bijoutiers_distinction = case_when(
    str_detect(act_phoenetic, pattern = "B") ~ "Bijoutier",
    str_detect(act_phoenetic, pattern = "J") ~ "Joalliers-bijoutier",
    str_detect(act_new, pattern = "fabr.|fab") ~ "Fabricant de bijoux",
    TRUE ~ "Autres"
  )) %>%
  filter(bijoutiers_distinction != "Autres") %>%
  select(-bijoutiers_distinction, -pattern)

bijoutier %>%
  select(activity, act_new, act_phoenetic) %>%
  distinct() %>%
  view()

#### carto initiale ####
# create boulanger spatial data as "certain" (need improvment! here just nb_loc = 1)
# critical vision
bijoutier %>%
  mutate(geoloca = if_else(condition = is.na(precise.geom), "no", "yes")) %>%
  group_by(nb_loc, geoloca) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = nb_loc, y = freq, fill = geoloca)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggthemes::scale_fill_economist() +
  scale_x_continuous(name = "n locations by entry", breaks = seq(1, 12, 1)) +
  labs(subtitle = "Jewellers in selected Annuaires",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: Annuaires, GeoHistoricalData")

ggsave(filename = "outputs_visuals/jewellers_sources_criticism.png", plot = last_plot(), 
       dpi = 300, width = 16, height = 11, units = "cm")

# dataset
bijoutier_sf <- bijoutier %>%
  mutate(geoloca = if_else(condition = is.na(precise.geom), "no", "yes")) %>%
  filter(geoloca == "yes") %>%
  filter(nb_loc == 1 & !is.na(overall_score)) %>%
  st_as_sf(wkt = "precise.geom", crs = 2154)

tm_shape(bijoutier_sf) + tm_dots()

### snap points patterns of Annuaires on each of matching network
# list of snap_points by Annuaire
liste_patterns_profession <- bijoutier_sf %>%
  arrange(published) %>%
  group_split(.tbl = ., published) # equivalent to date

# snaping points on networks
liste_patterns_snap <- list()

for (i in 1:length(liste_patterns_profession)) {
  snap_point_geometrique <- maptools::snapPointsToLines(points = as(liste_patterns_profession[[i]], "Spatial"), 
                                                        lines = as(list_of_networks[[i]], "Spatial"), 
                                                        maxDist = 100, idField = "rowid")
  
  snap_point_geometrique <- st_as_sf(snap_point_geometrique) %>%
    mutate(source = liste_patterns_profession[[i]]$directory[1]) %>%
    mutate(source_annee = liste_patterns_profession[[i]]$published[1])
  
  liste_patterns_snap[[i]] <- snap_point_geometrique
  
}

## cartography of epicier_sf
liste_snap_as_one_sf <- do.call(rbind, liste_patterns_snap)

ggplot() +
  geom_sf(data = netwokrs_carto_2, color = "grey70", size = 0.2) +
  geom_sf(data = liste_snap_as_one_sf, alpha = 0.7, color = "red") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(subtitle = "Jewellers - 'bijoutiers'",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: GeoHistoricalData") +
  facet_wrap(~source_annee, nrow = 2)

ggsave(filename = "outputs_visuals/jewellers_cartography.png", plot = last_plot(), 
       dpi = 300, width = 25, height = 25, units = "cm")

# writting for backgroundjob
saveRDS(object = liste_patterns_snap, file = "data_for_bckgrd/jewellers_list.rds")

### observed VS simulated data distances ####
# reading results from background job: distances matrices
tableau_plot <- readRDS("data_for_bckgrd/tableau_plot_grocers.rds")

# plotting data
# !!! auto warning !!! breaks of axis x and y are here chosen
plots_rss <- list()

for (i in 1:length(tableau_plot)) {
  plots_rss[[i]] <- tableau_plot[[i]] %>%
    mutate(type = if_else(
      condition = is.na(sim), "observed", "random\n(20 sim.)" 
    )) %>%
    ggplot(aes(x = dist_pi_p/1000, group = sim, color = type)) +
    geom_density(size = 0.3) +
    ggthemes::scale_color_colorblind() +
    ggthemes::theme_igray() +
    scale_x_continuous(name = "Distance (in km)", breaks = c(seq(0,15,2.5)), limits = c(0,14)) +
    scale_y_continuous(breaks = c(seq(0, 0.35, 0.1)), limits = c(0,0.35)) +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    ggtitle(tableau_plot[[i]]$source_annee[1])
}

distribution <- ggpubr::ggarrange(plotlist = plots_rss, 
                                  common.legend = TRUE, legend = c("bottom"), nrow = 2, ncol = 4)

ggpubr::annotate_figure(distribution, top = "Grocers - 'épiciers'", fig.lab.size = 15)

ggsave(filename = "outputs_visuals/grocers_matrices_distribution.png", plot = last_plot(), 
       dpi = 300, width = 25, height = 17, units = "cm")


#### temporal analysis with population ####
epicier %>%
  group_by(published) %>%
  summarise(n = n()) %>%
  left_join(x = ., y = pop_all_long, by = "published") %>%
  mutate(n_pop = n/pop*10000) %>%
  ggplot(aes(x = published, y = n_pop)) +
  geom_line() +
  geom_point() +
  ggthemes::scale_fill_economist() +
  theme(axis.title = element_blank()) +
  labs(subtitle = "N grocers per 10,000 inhabitants - 'épiciers'",
       caption = "J. Gravier 2022 | ANR SoDUCo. Data: Annuaires, GeoHistoricalData")
















