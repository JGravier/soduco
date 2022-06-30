library(tidyverse)
library(data.table)
library(stringdist)
library(stringi)
library(openxlsx)
library(patchwork)

#### data init ####
# 2 data versions of the same annuaires: Lamy 1839 and Didot 1845
# first version, named v1, is dated of 2022-03-02
# second version is dated of 2022-06-20
# json were previously transformed in csv files, selecting only "ENTRIES" (as tag from OCR)
# in the same time: extraction of "activity", "persons" and "location" from NER in new columns and counting N elements for each ENTRY
# extraction too of the annuaire's name and page number from json initial names

## reading versions data
data_no_geoloca_v1 <- read.csv(file = "tableau_1839_1845_medec_data_v1.csv", stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  as_tibble()

data_no_geoloca_v2 <- read.csv(file = "tableau_1839_1845_medec_data_v2.csv", stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  as_tibble()


## reading data references
# directories_adress_lists_index_20220201 refers to the knowledge of type of lists in annuaires,
# i.e. alphabetical list, professional or street list from page x to page y in initial .pdf 
data_reference <- read.csv(file = "Directories-and-the-City-main/directories_adress_lists_index_20220201.csv", 
                           stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  as_tibble()



#### data enrichment ####
## enrichment of versionned data with data references
data_no_geoloca_v1 <- data_no_geoloca_v1 %>%
  rowid_to_column() %>%
  left_join(x = ., y = data_reference %>% select(Code_fichier, liste_type, liste_cat, npage_pdf_d, npage_pdf_f),
            by = c("source" = "Code_fichier")) %>%
  mutate(sup_debut = if_else(
    condition = page >= npage_pdf_d,
    true = "dans liste",
    false = "hors liste"
  )) %>%
  mutate(inf_fin = if_else(
    condition = page <= npage_pdf_f,
    true = "dans liste",
    false = "hors liste"
  )) %>%
  mutate(dans_liste = if_else(
    sup_debut == "dans liste" & inf_fin == "dans liste",
    "dans liste", "hors liste"
  )) %>%
  filter(dans_liste == "dans liste") %>%
  select(-sup_debut:-dans_liste)

data_no_geoloca_v2 <- data_no_geoloca_v2 %>%
  rowid_to_column() %>%
  left_join(x = ., y = data_reference %>% select(Code_fichier, liste_type, liste_cat, npage_pdf_d, npage_pdf_f),
            by = c("source" = "Code_fichier")) %>%
  mutate(sup_debut = if_else(
    condition = page >= npage_pdf_d,
    true = "dans liste",
    false = "hors liste"
  )) %>%
  mutate(inf_fin = if_else(
    condition = page <= npage_pdf_f,
    true = "dans liste",
    false = "hors liste"
  )) %>%
  mutate(dans_liste = if_else(
    sup_debut == "dans liste" & inf_fin == "dans liste",
    "dans liste", "hors liste"
  )) %>%
  filter(dans_liste == "dans liste") %>%
  select(-sup_debut:-dans_liste)



## creating groups of soundex index: see https://www.archives.gov/research/census/soundex for more information
data_no_geoloca_v1 <- data_no_geoloca_v1 %>%
  # removal of special characters by applying the encoding Latin-ASCII
  mutate(act_new = stri_trans_general(str = activity, id = "Latin-ASCII")) %>%
  # application of soundex index
  mutate(act_phoenetic = phonetic(x = act_new))

data_no_geoloca_v2 <- data_no_geoloca_v2 %>%
  mutate(act_new = stri_trans_general(str = activity, id = "Latin-ASCII")) %>%
  mutate(act_phoenetic = phonetic(x = act_new))


## creating new variable based on soundex groups and extracing the most frequent designation of "activity" (in Latin-ASCII)

# function creation: faster
# see http://stackoverflow.com/questions/29255473/most-frequent-value-mode-by-group for more information
max_freq <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# applying function (with data.table, faster)
data_no_geoloca_v1 <- setDT(data_no_geoloca_v1)[, freq_max:= max_freq(act_new), by = act_phoenetic][] %>%
  as_tibble()

data_no_geoloca_v2 <- setDT(data_no_geoloca_v2)[, freq_max:= max_freq(act_new), by = act_phoenetic][] %>%
  as_tibble()


#### selecting medical activities for 1st corpus ####
### for more information, see: http://hackmd.io/lYpC-ImRQvOvGIY_sZMQtA?view

## creating one tibble for the 2 data versions of alphabetical lists
data_two_versions <- data_no_geoloca_v1 %>%
  filter(liste_type == "ListNoms") %>%
  mutate(version = "v1:2022-03-06") %>%
  bind_rows(data_no_geoloca_v2 %>%
              filter(liste_type == "ListNoms") %>%
              mutate(version = "v2:2022-06-20"))

medical_activities_corpus_1 <- data_two_versions %>%
  # selection from discussion of "medec"
  filter(str_detect(string = act_new, pattern = "medec") | act_phoenetic == "M300") %>%
  bind_rows(
    data_two_versions %>%
      # selection from discussion of "decin"
      filter(str_detect(string = act_new, pattern = "decin"))
  ) %>%
  bind_rows(
    data_two_versions %>%
      # selection from discussion of "off"
      filter(str_detect(string = act_new, pattern = "sant"))
  ) %>%
  bind_rows(
    data_two_versions %>%
      # selection from discussion of "chiru"
      filter(str_detect(string = act_new, pattern = "chiru"))
  ) %>%
  bind_rows(
    data_two_versions %>%
      # selection from discussion of "denti"
      filter(str_detect(string = act_new, pattern = "denti"))
  ) %>%
  bind_rows(
    data_two_versions %>%
      # selection from discussion of "occul"
      filter(str_detect(string = act_new, pattern = "occul"))
  ) %>%
  bind_rows(
    data_two_versions %>%
      # selection from discussion of "sage-femme"
      filter(act_phoenetic %in% c("S2?1", "S150") | act_phoenetic == "S215" & str_detect(string = act_new, pattern = "sage") |
               act_phoenetic == "S225" & str_detect(string = act_new, pattern = "sage") |
               act_phoenetic ==  "F500" & str_detect(string = persons, pattern = "sage"))
  ) %>%
  unique() # remove possible duplicates, e.g. a "medecin-chirugien" that would have been selected once in "medec" and once in "chiru"


## adding columns for simplier filtering post-processing
medical_activities_corpus_1 <- medical_activities_corpus_1 %>%
  # for medecin
  mutate(medec = str_detect(string = act_new, pattern = "medec") | act_phoenetic == "M300") %>%
  mutate(decin = str_detect(string = act_new, pattern = "decin")) %>%
  # for elements link to sante
  mutate(sant = str_detect(string = act_new, pattern = "sant")) %>%
  # for officier de sante
  mutate(officier_de_sante = if_else(
    condition = sant == TRUE & act_phoenetic %in% c("O123", "O126"),
    true = TRUE,
    false = FALSE
  )) %>%
  # for elements link to chirugie
  mutate(chiru = str_detect(string = act_new, pattern = "chiru")) %>%
  # for chirugien and not for manufacturer of surgical instruments
  mutate(chirugien = if_else(
    condition = chiru == TRUE & str_detect(string = act_new, pattern = "fab", negate = TRUE),
    true = TRUE,
    false = FALSE
  )) %>%
  # for denti(ste)
  mutate(denti = str_detect(string = act_new, pattern = "denti")) %>%
  # for occuliste
  mutate(occul = str_detect(string = act_new, pattern = "occul")) %>%
  # for sage-femme
  mutate(sage_femme = act_phoenetic %in% c("S2?1", "S150") | act_phoenetic == "S215" & str_detect(string = act_new, pattern = "sage") |
           act_phoenetic == "S225" & str_detect(string = act_new, pattern = "sage") |
           act_phoenetic ==  "F500" & str_detect(string = persons, pattern = "sage"))

## output
write.xlsx(x = medical_activities_corpus_1, file = "medical_listnoms_lamy_1839_didot_1845.xlsx", overwrite = TRUE)
