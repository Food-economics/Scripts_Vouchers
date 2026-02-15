#Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")

#Chargement de l'envrionnement de travail -----------------
researcher<-"adenieul" #"vbellassen" edumont
if (researcher == "adenieul") {
  setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else {
  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))
}

#Nov 22
Carnet_nov_22 <- read.xlsx(
  xlsxFile = paste(
    "Données analysées - Article N°1 chèques/",
    "Fichiers_nettoyés/Fichiers_prétraités/",
    "Carnets_Tableaux_nov_22.xlsx",
    sep = ""
  ),
  sheet    = "Données_brutes_nettoyées"
)

#Mars23
Carnet_mars_23<- read.xlsx(
  xlsxFile = paste(
    "Données analysées - Article N°1 chèques/",
    "Fichiers_nettoyés/Fichiers_prétraités/",
    "Carnets_Tableaux_mars_23.xlsx",
    sep = ""
  ),
  sheet    = "Données_brutes_nettoyées"
)

Metadata <- read.xlsx(
  xlsxFile = paste(
    "Données analysées - Article N°1 chèques/",
    "Fichiers_nettoyés/Fichiers_prétraités/",
    "Carnets_Tableaux_mars_23.xlsx",
    sep = ""
  )
)

#Nov23
Carnet_nov_23<- read.xlsx(
  xlsxFile = paste(
    "Données analysées - Article N°1 chèques/",
    "Fichiers_nettoyés/Fichiers_prétraités/",
    "Carnets_Tableaux_nov_23.xlsx",
    sep = ""
  ),
  sheet    = "Données_brutes_nettoyées"
)

sgsdata<- read.xlsx(
  xlsxFile = paste(
    "Données analysées - Article N°1 chèques/",
    "Fichiers_nettoyés/Fichier_traitement/",
    "sgsdata.xlsx",
    sep = ""

))

#Mars24
Carnet_mars_24<- read.xlsx(
  xlsxFile = paste(
    "Données analysées - Article N°1 chèques/",
    "Fichiers_nettoyés/Fichiers_prétraités/",
    "Carnets_Tableaux_mars_24.xlsx",
    sep = ""
  ),
  sheet    = "Données_brutes_nettoyées"
)


Carnet_nov_22 <- Carnet_nov_22 %>% semi_join(sgsdata, by = "Identifiant")
Carnet_mars_23 <- Carnet_mars_23 %>% semi_join(sgsdata, by = "Identifiant")
Carnet_nov_23 <- Carnet_nov_23 %>% semi_join(sgsdata, by = "Identifiant")
Carnet_mars_24 <- Carnet_mars_24 %>%  semi_join(sgsdata, by = "Identifiant")


to_remove <- c("EAU", "CAFE_THE", "EPICES_CONDIMENTS", "AUTRE", "ALCOOL", "NA", "LAIT",
               "FRUITS_JUS", "SODAS_SUCRES", "SODAS_LIGHT")

# fonction de nettoyage
clean_groupe <- function(df) {
  df %>%
    filter(
      !groupe_TI_TdC %in% to_remove,   # exclut les modalités indésirables
      !is.na(groupe_TI_TdC)            # exclut les NA
    )
}

# application à vos 4 tables
Carnet_nov_22  <- clean_groupe(Carnet_nov_22)
Carnet_mars_23 <- clean_groupe(Carnet_mars_23)
Carnet_nov_23  <- clean_groupe(Carnet_nov_23)
Carnet_mars_24 <- clean_groupe(Carnet_mars_24)

#nov 22
recode_groupe_TI_TdC <- function(df, col = "groupe_TI_TdC") {
  # Liste des nouveaux groupes et de leurs niveaux originaux
  mapping <- list(
    FV = c("FRUITS", "FRUITS_SECS", "NOIX", "LEGUMES"),
    FEC = c("FEC_NON_RAF", "FEC_RAF"),
    PDTS_LAITIERS = c("LAIT", "LAITAGES", "FROMAGES"),
    POULET_OEUFS = c("POULET", "OEUFS"),
    AUTRE_PDTS_ANIMAUX = c("CHARCUTERIE_HORS_JB", "JAMBON_BLANC"),
    PLATS_PREP = c("PLATS_PREP_CARNES", "PLATS_PREP_VEGETARIENS", "QUICHES_PIZZAS_TARTES_SALEES"),
    VIANDE_ROUGE_PORC = c("VIANDE_ROUGE", "PORC"),
    MG = c("MGA", "MGV"),
    PDTS_DISCRETIONNAIRES = c("SNACKS_AUTRES", "CEREALES_PD", "DESSERTS_LACTES", "PDTS_SUCRES", "SAUCES"),
    SSB = c("SODAS_SUCRES", "SODAS_LIGHT", "FRUITS_JUS")
  )
  
  # Boucle sur chaque groupe pour faire le recodage
  for (new_group in names(mapping)) {
    levels_to_replace <- mapping[[new_group]]
    idx <- df[[col]] %in% levels_to_replace
    df[[col]][idx] <- new_group
  }
  
  return(df)
}

# 3. Application aux dataframes pour le lieu "Lieu1"
# Recodage des groupes
Carnet_nov_22   <- recode_groupe_TI_TdC(Carnet_nov_22)
Carnet_nov_23   <- recode_groupe_TI_TdC(Carnet_nov_23)
Carnet_mars_23  <- recode_groupe_TI_TdC(Carnet_mars_23)
Carnet_mars_24  <- recode_groupe_TI_TdC(Carnet_mars_24)

#nov 22
recode_lieu <- function(df, col = "Lieu1") {
  # Liste des nouveaux groupes et de leurs niveaux originaux
  mapping <- list(
    GMS = c("Hypermarchés", "Supermarchés"),
    RHD = c("RHD_COM", "RHD_COL")
  )
  
  # Boucle sur chaque groupe pour faire le recodage
  for (new_group in names(mapping)) {
    levels_to_replace <- mapping[[new_group]]
    idx <- df[[col]] %in% levels_to_replace
    df[[col]][idx] <- new_group
  }
  
  return(df)
}

# 3. Application aux dataframes pour le lieu "Lieu1"
# Recodage des groupes
Carnet_nov_22   <- recode_lieu(Carnet_nov_22)
Carnet_nov_23   <- recode_lieu(Carnet_nov_23)
Carnet_mars_23  <- recode_lieu(Carnet_mars_23)
Carnet_mars_24  <- recode_lieu(Carnet_mars_24)











# 1. On re-crée la liste de data.frames d'origine
dfs <- list(
  nov_22  = Carnet_nov_22,
  mars_23 = Carnet_mars_23,
  nov_23  = Carnet_nov_23,
  mars_24 = Carnet_mars_24
)


#CREATION colonne voie de recrutement ----------------------------------
Carnet_nov_22 <- Carnet_nov_22 %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques
Carnet_nov_22$voie_de_recrutement <- ifelse((Carnet_nov_22$voie_de_recrutement =="Episourire" |Carnet_nov_22$voie_de_recrutement =="Epimut"|Carnet_nov_22$voie_de_recrutement =="A-Epimut"),("Epicerie"), (Carnet_nov_22$voie_de_recrutement) )
Carnet_nov_22$voie_de_recrutement <- ifelse((Carnet_nov_22$voie_de_recrutement =="SP-CCAS"|Carnet_nov_22$voie_de_recrutement =="PE-CCAS"),("CCAS"), (Carnet_nov_22$voie_de_recrutement) )

Carnet_mars_23 <- Carnet_mars_23 %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques
Carnet_mars_23$voie_de_recrutement <- ifelse((Carnet_mars_23$voie_de_recrutement =="Episourire" |Carnet_mars_23$voie_de_recrutement =="Epimut"|Carnet_mars_23$voie_de_recrutement =="A-Epimut"),("Epicerie"), (Carnet_mars_23$voie_de_recrutement) )
Carnet_mars_23$voie_de_recrutement <- ifelse((Carnet_mars_23$voie_de_recrutement =="SP-CCAS"|Carnet_mars_23$voie_de_recrutement =="PE-CCAS"),("CCAS"), (Carnet_mars_23$voie_de_recrutement) )

Carnet_nov_23 <- Carnet_nov_23 %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques

Carnet_mars_24 <- Carnet_mars_24 %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques


Carnet_nov_22_Epic <- Carnet_nov_22 %>% filter(voie_de_recrutement == "Epicerie" )

Carnet_nov_22_CCAS <- Carnet_nov_22 %>% filter(voie_de_recrutement == "CCAS" )
Carnet_mars_23_Epic <- Carnet_mars_23 %>% filter(voie_de_recrutement == "Epicerie" )
Carnet_mars_23_CCAS <- Carnet_mars_23 %>% filter(voie_de_recrutement == "CCAS" )

Carnet_nov_22_Epic <- Carnet_nov_22_Epic %>%
  semi_join(Carnet_mars_23_Epic, by = "Identifiant")

Carnet_nov_22_CCAS <- Carnet_nov_22_CCAS %>%
  semi_join(Carnet_mars_23_CCAS, by = "Identifiant")
Carnet_nov_23_LE <- Carnet_nov_23 %>% filter(voie_de_recrutement == "LE" )
Carnet_nov_23_PS <- Carnet_nov_23 %>% filter(voie_de_recrutement == "PS" )
Carnet_mars_24_LE <- Carnet_mars_24 %>% filter(voie_de_recrutement == "LE" )
Carnet_mars_24_PS <- Carnet_mars_24 %>% filter(voie_de_recrutement == "PS" )
#Poids des epiceries sociales chez les personnes identifiées CCAS
#epicerie_sociale <- Carnet_nov_22_CCAS %>%
#  group_by(Identifiant) %>%
#  filter(any(Lieu1 == "Epicerie")) %>%
#  ungroup()
##SPF
#spf <- Carnet_nov_22_CCAS %>%
#  group_by(Identifiant) %>%
#  filter(any(grepl("SP", Identifiant, ignore.case = TRUE))) %>%
#  ungroup()


Carnet_nov_23_LE <- Carnet_nov_23_LE %>%
  semi_join(Carnet_mars_24_LE, by = "Identifiant")

Carnet_nov_23_PS <- Carnet_nov_23_PS %>%
  semi_join(Carnet_mars_24_PS, by = "Identifiant")


unique(Carnet_nov_22_Epic$Identifiant)
unique(Carnet_nov_22_CCAS$Identifiant)
unique(Carnet_nov_23_LE$Identifiant)
unique(Carnet_nov_23_PS$Identifiant)
print(unique(Carnet_nov_22$Identifiant))

##Répartition des lieus d'appro par catégorie d'aliments -------------------------

# 2. Fonction de résumé qui groupe sur Lieu1 et recalcule totaux + % par Lieu1
summarise_by_Lieu1_cat <- function(df) {
  df %>%
    group_by(Lieu1) %>%
    summarise(
      total_poids = sum(Poids_consomme, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    # maintenant, pour chacun des Lieu1 on calcule le pourcentage interne
    mutate(
      pct_poids = total_poids / sum(total_poids) * 100,
    ) %>%
    ungroup()
}

résumé_nov22_Epic <- summarise_by_Lieu1_cat(Carnet_nov_22_Epic)
résumé_nov22_CCAS <- summarise_by_Lieu1_cat(Carnet_nov_22_CCAS)
résumé_nov23_LE <- summarise_by_Lieu1_cat(Carnet_nov_23_LE)
résumé_nov23_PS <- summarise_by_Lieu1_cat(Carnet_nov_23_PS)

#CAS spécifique
#résumé_nov22_EP <- summarise_by_Lieu1_cat(epicerie_sociale)
#résumé_nov22_spf <- summarise_by_Lieu1_cat(spf)
##Répartition des catégories d'aliments par lieu d'appro ----------------------


summarise_by_cat_lieu <- function(df) {
  df %>%
    group_by(groupe_TI_TdC, Lieu1) %>%
    summarise(
      total_poids = sum(Poids_consomme, na.rm = TRUE),
      total_prix  = sum(Prix,       na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    # pour chaque catégorie, on calcule le % de chaque Lieu1
    group_by(groupe_TI_TdC) %>%
    mutate(
      pct_poids = total_poids / sum(total_poids) * 100,
      pct_prix  = total_prix  / sum(total_prix)  * 100
    ) %>%
    ungroup()
}


résumé_nov22_Epic_bis <- summarise_by_cat_lieu(Carnet_nov_22_Epic)
résumé_nov22_CCAS_bis <- summarise_by_cat_lieu(Carnet_nov_22_CCAS)
résumé_nov23_LE_bis <- summarise_by_cat_lieu(Carnet_nov_23_LE)
résumé_nov23_PS_bis <- summarise_by_cat_lieu(Carnet_nov_23_PS)




poids_pourcentages_nov22_Epic_bis <- résumé_nov22_Epic_bis %>%
  dplyr::select(Lieu1, groupe_TI_TdC, pct_poids) %>%
  tidyr::pivot_wider(
    names_from  = groupe_TI_TdC,
    values_from = pct_poids,
    values_fill = 0
  )


poids_pourcentages_nov22_CCAS_bis <- résumé_nov22_CCAS_bis %>%
  dplyr::select(Lieu1, groupe_TI_TdC, pct_poids) %>%
  tidyr::pivot_wider(
    names_from  = groupe_TI_TdC,
    values_from = pct_poids,
    values_fill = 0
  )

poids_pourcentages_nov23_LE_bis <- résumé_nov23_LE_bis %>%
  dplyr::select(Lieu1, groupe_TI_TdC, pct_poids) %>%
  tidyr::pivot_wider(
    names_from  = groupe_TI_TdC,
    values_from = pct_poids,
    values_fill = 0
  )

poids_pourcentages_nov23_PS_bis <- résumé_nov23_PS_bis %>%
  dplyr::select(Lieu1, groupe_TI_TdC, pct_poids) %>%
  tidyr::pivot_wider(
    names_from  = groupe_TI_TdC,
    values_from = pct_poids,
    values_fill = 0
  )
##Répartition des catégories d'aliments par voie de recrutement ----------------------

summarise_recrutement <- function(df) {
  # Calcul du total global avant tout
  total_global <- sum(df$Poids_consomme, na.rm = TRUE)
  
  df %>%
    group_by(groupe_TI_TdC, voie_de_recrutement) %>%
    summarise(
      total_poids = sum(Poids_consomme, na.rm = TRUE),
      .groups     = "drop"           # on lâche immédiatement tous les groupes
    ) %>%
    # regrouper par catégorie seule pour calculer le dénominateur
    group_by(groupe_TI_TdC) %>%
    mutate(
      total_poids_categorie = sum(total_poids),            # somme par catégorie
      pct_poids              = total_poids / total_poids_categorie * 100
    ) %>%
    ungroup() %>%
    # enrichir chaque ligne avec le total global
    mutate(
      total_poids_global = total_global
    ) %>%
    # regrouper par catégorie seule pour calculer le dénominateur
    group_by(groupe_TI_TdC) %>%
    mutate(
      pct_poids_vf              = total_poids /total_poids_global * 100
    )
} 


résumé_nov22_Epic_bis_bis <- summarise_recrutement(Carnet_nov_22_Epic)
résumé_nov22_CCAS_bis_bis <- summarise_recrutement(Carnet_nov_22_CCAS)
résumé_nov23_LE_bis_bis <- summarise_recrutement(Carnet_nov_23_LE)
résumé_nov23_PS_bis_bis <- summarise_recrutement(Carnet_nov_23_PS)


##Type de produits éligibles  --------------
#Cru/Surgelé/Appertisé
filtre <- resultats_codachats %>% 
  filter(groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES", "LEG_SECS", "FRUITS_SECS", "NOIX"))

filtre <- filtre %>% 
  mutate(
    gamme = case_when(
      str_detect(LibelleCIQUAL, regex("Cru|fraîche|Fraîche|Mesclun|Mâche", ignore_case = TRUE)) ~ "Frais",
      str_detect(LibelleCIQUAL, regex("sauce|sauté|crème|poêlé|appertisé|Olive|cuisinée|Ratatouille|égoutté|conserve|bocal|bouilli|Macédoine|cuit", ignore_case = TRUE)) ~ "Conserve",
      str_detect(LibelleCIQUAL, regex("Soupe", ignore_case = TRUE)) ~ "Soupe",
      str_detect(LibelleCIQUAL, regex("Purée|Compote|Petit pot|Hoummous|Tapenade", ignore_case = TRUE)) ~ "Purée",
      str_detect(LibelleCIQUAL, regex("Surgelé", ignore_case = TRUE)) ~ "Surgelé",
      str_detect(LibelleCIQUAL, regex("Nuggets|Seitan|Escalope|Galette|Haché|Pavé|Tofu|Falafel|Boulette", ignore_case = TRUE)) ~ "Préparation",
      str_detect(LibelleCIQUAL, regex("Noisette|Noix|Sèche|Sec|graine|séchée|grillée|Cacahuète|Amande|graîne", ignore_case = TRUE)) ~ "Sec",
      TRUE ~ NA_character_
    ),
    # Ajout du préfixe TI_TdC (ou colonne groupe_TI_TdC1)
    gamme = ifelse(!is.na(gamme), paste0(groupe_TI_TdC1, "_", gamme), NA)
  )

# Tableau long : un total par Identifiant x gamme
table_gamme_long <- filtre %>% 
  group_by(Identifiant, gamme) %>% 
  summarise(
    Poids_consomme_vf = sum(Poids_consomme_vf, na.rm = TRUE),
    UC_TI = dplyr::first(UC_TI),
    .groups = "drop"
  ) %>% 
  mutate(
    Poids_consomme_vf = Poids_consomme_vf / (UC_TI * Nj)
  ) 



# Tableau large : une colonne par type de gamme
table_gamme_wide <- table_gamme_long %>% 
  pivot_wider(
    names_from = gamme,
    values_from = Poids_consomme_vf,
    values_fill = 0
  )

table_gamme_wide <- table_gamme_wide[, !names(table_gamme_wide) %in% "UC_TI"]


# Répartition par Identifiant x gamme x Lieu1
table_lieu_gamme_long <- filtre %>% 
  group_by(Identifiant, gamme, Lieu1) %>% 
  summarise(
    Poids_consomme_vf = sum(Poids_consomme_vf, na.rm = TRUE),
    UC_TI = dplyr::first(UC_TI),
    .groups = "drop"
  ) %>% 
  mutate(
    Poids_par_UC_Nj = Poids_consomme_vf / (UC_TI * Nj)
  )

table_lieu_gamme_long <- table_lieu_gamme_long %>%
  mutate(gamme_lieu = paste0(gamme, "_", Lieu1))
table_lieu_gamme_long <- table_lieu_gamme_long[, !(names(table_lieu_gamme_long) %in% c("Poids_consomme_vf", "UC_TI","gamme","Lieu1"))]


# Passage en large : une colonne par type de gamme_lieu
table_gamme <- table_lieu_gamme_long %>%
  pivot_wider(
    names_from  = gamme_lieu,
    values_from = Poids_par_UC_Nj,
    values_fill = 0
  )


##TELECHARGEMENT----------------------------------
# Créer un nouvel objet workbook
wb <- createWorkbook()

addWorksheet(wb, "rec_nov22_Epic")
writeData(wb, sheet = "rec_nov22_Epic", résumé_nov22_Epic_bis_bis )

addWorksheet(wb, "rec_nov22_CCAS")
writeData(wb, sheet = "rec_nov22_CCAS", résumé_nov22_CCAS_bis_bis  )

addWorksheet(wb, "rec_nov23_LE")
writeData(wb, sheet = "rec_nov23_LE", résumé_nov23_LE_bis_bis )

addWorksheet(wb, "rec_nov23_PS")
writeData(wb, sheet = "rec_nov23_PS", résumé_nov23_PS_bis_bis )



addWorksheet(wb, "cat_nov22_Epic")
writeData(wb, sheet = "cat_nov22_Epic", poids_pourcentages_nov22_Epic_bis  )

addWorksheet(wb, "cat_nov22_CCAS")
writeData(wb, sheet = "cat_nov22_CCAS", poids_pourcentages_nov22_CCAS_bis  )

addWorksheet(wb, "cat_nov23_LE")
writeData(wb, sheet = "cat_nov23_LE", poids_pourcentages_nov23_LE_bis  )


addWorksheet(wb, "cat_nov23_PS")
writeData(wb, sheet = "cat_nov23_PS", poids_pourcentages_nov23_PS_bis)
          

addWorksheet(wb, "lieu_nov22_Epic")
writeData(wb, sheet = "lieu_nov22_Epic", résumé_nov22_Epic  )

addWorksheet(wb, "lieu_nov22_CCAS")
writeData(wb, sheet = "lieu_nov22_CCAS", résumé_nov22_CCAS  )

addWorksheet(wb, "lieu_nov23_LE")
writeData(wb, sheet = "lieu_nov23_LE", résumé_nov23_LE )

addWorksheet(wb, "lieu_nov23_PS")
writeData(wb, sheet = "lieu_nov23_PS", résumé_nov23_PS )

saveWorkbook(wb,(paste0("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichier_traitement/stat_desc_poidsb.xlsx")))



sgsdata<- read.xlsx(
  xlsxFile = paste(
    "Données analysées - Article N°1 chèques/",
    "Fichiers_nettoyés/Fichier_traitement/",
    "sgsdata.xlsx",
    sep = ""
    
  ))

#F&V
sgsdata <- sgsdata %>%
  filter(Mesure == "Carnet")

#Selection compliant

# 1. Définir les deux ensembles d'identifiants
ids_groupe1_camp1 <- sgsdata %>% 
  filter(groupe == 1, Campagne == 1, utilisation2_FFQ == 1, limitation_FFQ == 1, Periode == 1) %>% 
  pull(Identifiant)

ids_groupe1_camp2 <- sgsdata %>% 
  filter(groupe == 1, Campagne == 2,
         Prop_montant_theorique_saisie >= 0.3, 
         Prop_montant_theorique_saisie <= 1.7,
         compliance >= 0.7, Periode == 1) %>% 
  pull(Identifiant)

# 2. Fusionner les deux ensembles
ids_compliant_all <- union(ids_groupe1_camp1, ids_groupe1_camp2)

# 3. Créer la colonne dans sgsdata
sgsdata <- sgsdata %>% 
  mutate(Compliant_all = if_else(Identifiant %in% ids_compliant_all, 1, 0))

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# --- Préparation des sous-échantillons ---
base_all <- sgsdata %>%
  filter(Mesure == "Carnet")

# Sous-échantillon compliant : uniquement traités compliant, + tous les témoins
base_compliant <- base_all %>%
  filter((groupe == 1 & Compliant_all == 1) | groupe == 0)




###GRAPH DE DIFF n DIFF


# ---- Dépendances ----
library(dplyr)
library(ggplot2)
library(patchwork)
library(rlang)

plot_pair_outcome <- function(
    data,
    var,                                # "FV_POIDS" ou FV_POIDS
    y_label       = "Valeur (unité)",
    title         = "Titre global",
    left_subtitle = "Sous-titre panneau gauche",
    right_subtitle= "Sous-titre panneau droit",
    ref_line      = 400,                # mettre NULL pour désactiver
    ref_text      = "Référence",
    multiply_by   = 1000,               # ex: kg -> g (mettre 1 si déjà en g)
    show_ref      = TRUE,               # <-- NOUVEAU : active/désactive l’affichage de la référence
    ref_color     = "darkgreen",        # optionnel : couleur de la ref
    ref_text_nudge= 5                   # optionnel : décalage vertical du texte de ref
){
  var_sym <- ensym(var)
  var_chr <- as_string(var_sym)
  
  sgs <- data %>% filter(Mesure == "Carnet")
  
  ids_g1_c1 <- sgs %>%
    filter(groupe == 1, Campagne == 1, utilisation2_FFQ == 1, limitation_FFQ == 1, Periode == 1) %>%
    pull(Identifiant)
  
  ids_g1_c2 <- sgs %>%
    filter(groupe == 1, Campagne == 2,
           Prop_montant_theorique_saisie >= 0.3,
           Prop_montant_theorique_saisie <= 1.7,
           compliance >= 0.7, Periode == 1) %>%
    pull(Identifiant)
  
  ids_compliant_all <- union(ids_g1_c1, ids_g1_c2)
  
  sgs <- sgs %>% mutate(Compliant_all = if_else(Identifiant %in% ids_compliant_all, 1L, 0L))
  
  base_all <- sgs
  base_compliant <- base_all %>% filter((groupe == 1 & Compliant_all == 1) | groupe == 0)
  
  n_temoins_all <- base_all %>% filter(groupe==0) %>% distinct(Identifiant) %>% nrow()
  n_trait_all   <- base_all %>% filter(groupe==1) %>% distinct(Identifiant) %>% nrow()
  n_trait_compl <- base_compliant %>% filter(groupe==1) %>% distinct(Identifiant) %>% nrow()
  
  diffs_all <- base_all %>%
    group_by(Periode, groupe) %>%
    summarise(mean_val = mean(.data[[var_chr]], na.rm = TRUE), .groups = "drop") %>%
    mutate(SousGroupe = if_else(
      groupe == 1,
      sprintf("Treatment : intention to treat (n=%d)", n_trait_all),
      sprintf("Control (n=%d)", n_temoins_all)
    ))
  
  diffs_comp <- base_compliant %>%
    group_by(Periode, groupe) %>%
    summarise(mean_val = mean(.data[[var_chr]], na.rm = TRUE), .groups = "drop") %>%
    filter(groupe == 1) %>%
    mutate(SousGroupe = sprintf("Treatment : compliant (n=%d)", n_trait_compl))
  
  diffs_left <- bind_rows(diffs_all, diffs_comp) %>%
    mutate(
      Periode = factor(Periode, levels = c(0,1),
                       labels = c("Before intervention", "After intervention")),
      mean_val = mean_val * multiply_by
    )
  
  base_LE <- sgs %>% filter(voie_de_recrutement == "LE")
  n_le_temoins <- base_LE %>% filter(groupe==0) %>% distinct(Identifiant) %>% nrow()
  n_le_trait   <- base_LE %>% filter(groupe==1) %>% distinct(Identifiant) %>% nrow()
  
  diffs_right <- base_LE %>%
    group_by(Periode, groupe) %>%
    summarise(mean_val = mean(.data[[var_chr]], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      SousGroupe = if_else(groupe == 1,
                           sprintf("Treatment (n=%d)", n_le_trait),
                           sprintf("Control (n=%d)", n_le_temoins)),
      Periode = factor(Periode, levels = c(0,1),
                       labels = c("Before intervention", "After intervention")),
      mean_val = mean_val * multiply_by
    )
  
  build_panel <- function(diffs_df, subtitle_here) {
    p <- ggplot(
      diffs_df,
      aes(x = Periode, y = mean_val, color = SousGroupe, group = SousGroupe, linetype = SousGroupe)
    ) +
      geom_point(size = 2) +
      geom_line(linewidth = 1)
    
    # --- Affichage optionnel de la référence ---
    if (isTRUE(show_ref) && !is.null(ref_line)) {
      p <- p +
        geom_hline(yintercept = ref_line, linetype = "dashed", color = ref_color) +
        annotate("text", x = 1.5, y = ref_line + ref_text_nudge, label = ref_text,
                 color = ref_color, hjust = 0.5, vjust = -0.5,
                 size = 3.8, fontface = "italic")
    }
    
    endpoints <- diffs_df %>%
      mutate(
        label  = as.character(signif(mean_val, 3)),
        hjustv = if_else(Periode == "After intervention", 1.05, -0.05),
        vjustv = -0.3
      )
    
    p +
      labs(
        subtitle = subtitle_here,
        x = "Period",
        y = y_label,
        color = "",
        linetype = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.subtitle = element_text(hjust = 0.5, size = 11, margin = margin(b = 8)),
        plot.margin = margin(t = 8, r = 10, b = 8, l = 10),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width  = unit(1, "cm")
      ) +
      geom_text(
        data = endpoints,
        aes(label = label, hjust = hjustv, vjust = vjustv),
        show.legend = FALSE,
        size = 3.3
      )
  }
  
  left  <- build_panel(diffs_left,  left_subtitle)
  right <- build_panel(diffs_right, right_subtitle)
  
  (left | right) +
    plot_layout(guides = "keep", widths = c(1, 1)) +
    plot_annotation(
      title = title,
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10))
      )
    )
}

# ---- Exemple d'appel (reprend les textes actuels) ----
figure_finale <- plot_pair_outcome(
  data          = sgsdata,
  var           = LEG_SECS_POIDS,  # ou "FV_POIDS"
  y_label       = "Legumes",
  title         = "Effects of the intervention on legumes intake",
  left_subtitle = "Comparison control, intention to treat and compliant",
  right_subtitle= "All households in the automated-control subgroup",
  ref_line      = 57,
  ref_text      = "PNNS Recommendation",
  multiply_by   = 1000      # si FV_POIDS est en kg -> g
)


figure_finale



# ---- Dépendances ----
library(dplyr)
library(ggplot2)
library(patchwork)
library(rlang)

# ---- Fonction : barres de proportion atteignant le seuil ----
plot_pair_bar_target <- function(
    data,
    var,
    title         = "Proportion meeting the recommendation",
    left_subtitle = "Comparison control, intention to treat and compliant",
    right_subtitle= "Automated control subgroup (LE)",
    threshold_g   = 400,
    multiply_by   = 1000
){
  var_sym <- ensym(var)
  var_chr <- as_string(var_sym)
  
  # --- Base carnet ---
  sgs <- data %>% filter(Mesure == "Carnet")
  
  # --- Définition "utilisation conforme" (ta logique existante) ---
  ids_g1_c1 <- sgs %>%
    filter(groupe == 1, Campagne == 1,
           utilisation2_FFQ == 1, limitation_FFQ == 1, Periode == 1) %>%
    pull(Identifiant)
  
  ids_g1_c2 <- sgs %>%
    filter(groupe == 1, Campagne == 2,
           Prop_montant_theorique_saisie >= 0.3,
           Prop_montant_theorique_saisie <= 1.7,
           compliance >= 0.7, Periode == 1) %>%
    pull(Identifiant)
  
  ids_compliant_all <- union(ids_g1_c1, ids_g1_c2)
  sgs <- sgs %>%
    mutate(Compliant_all = if_else(Identifiant %in% ids_compliant_all, 1L, 0L))
  
  # --- Sous-échantillons ---
  base_all <- sgs
  base_compliant <- base_all %>% filter((groupe == 1 & Compliant_all == 1) | groupe == 0)
  
  # --- Fonction interne : part de ménages atteignant le seuil ---
  agg_prop <- function(df, label_trait, label_temoins){
    n_temoins <- df %>% filter(groupe == 0) %>% distinct(Identifiant) %>% nrow()
    n_trait   <- df %>% filter(groupe == 1) %>% distinct(Identifiant) %>% nrow()
    
    df %>%
      group_by(Identifiant, groupe, Periode) %>%
      summarise(mean_val = mean(.data[[var_chr]], na.rm = TRUE), .groups = "drop") %>%
      mutate(hit = (mean_val * multiply_by) >= threshold_g) %>%
      group_by(Periode, groupe) %>%
      summarise(prop = mean(hit, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        SousGroupe = if_else(
          groupe == 1, sprintf("%s (n=%d)", label_trait, n_trait),
          sprintf("%s (n=%d)", label_temoins, n_temoins)
        ),
        prop_pc = 100 * prop,
        Periode = factor(Periode, levels = c(0, 1),
                         labels = c("Before intervention", "After intervention"))
      )
  }
  
  # --- Panneau gauche (toute pop + conformes) ---
  prop_all  <- agg_prop(base_all, "Treatment : intention to treat", "Control")
  prop_comp <- agg_prop(base_compliant, "Treatment : compliant", "Control") %>%
    filter(groupe == 1)
  
  props_left <- bind_rows(prop_all, prop_comp)
  
  # --- Panneau droit (LE uniquement) ---
  base_LE <- sgs %>% filter(voie_de_recrutement == "LE")
  props_right <- agg_prop(base_LE, "Treated", "Control")
  
  # --- Fonction interne : diagramme en barres ---
  build_bar_panel <- function(df, subtitle_here) {
    ggplot(df, aes(x = Periode, y = prop_pc, fill = SousGroupe)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = paste0(round(prop_pc), "%")),
        position = position_dodge(width = 0.8),
        vjust = -0.3,
        size = 3.5
      ) +
      scale_y_continuous(labels = function(x) paste0(x, " %"), limits = c(0, 100)) +
      labs(
        subtitle = subtitle_here,
        x = "Period", y = " ",
        fill = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.subtitle = element_text(hjust = 0.5, size = 11, margin = margin(b = 8)),
        plot.margin = margin(t = 8, r = 10, b = 8, l = 10),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width  = unit(1, "cm")
      )
  }
  
  # --- Création des deux panneaux avec leurs légendes respectives ---
  left  <- build_bar_panel(props_left,  left_subtitle)
  right <- build_bar_panel(props_right, right_subtitle)
  
  # --- Assemblage : deux graphiques côte à côte, légendes sous chacun ---
  final_plot <- (
    (left / plot_spacer()) |
      (right / plot_spacer())
  ) +
    plot_layout(heights = c(1, 0.05)) + # espace pour les légendes individuelles
    plot_annotation(
      title = title,
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10))
      )
    )
  
  return(final_plot)
}

# ---- Exemple d'appel ----
figure_bar_400 <- plot_pair_bar_target(
  data          = sgsdata,
  var           = LEG_SECS_POIDS,
  title         = "Proportion of households meeting the legumes recommendation",
  left_subtitle = "Comparison of the control group, intention-to-treat, and compliant use",
  right_subtitle= "All households in the automated-control subgroup",
  threshold_g   = 57,
  multiply_by   = 1000
)

figure_bar_400
