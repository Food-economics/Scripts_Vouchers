#Preparation des données ------------
rm(list = ls())
library(haven)
library(readxl)
library(tidyverse)
library(openxlsx)
library(car)
library(dplyr)
library(broom)
library(scales)
library(modelsummary)
library(ggplot2)
library(effsize)
library(lfe)
library(ggpubr)
library(vtable)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(Metrics)
library(poLCA)
library(webshot)
library(nlme)
library(fixest)
library(plm)
library(lmtest)
library(htmltools)
library(clubSandwich)
library(Matrix)
library(lme4)
library(cobalt)
library(knitr)
library(tableone)
library(purrr)
library(plotly)
library(htmlwidgets)

# Définir le répertoire de travail en fonction du chercheur
researcher <- "adenieul"
if (researcher == "adenieul") {
  setwd("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else {
  setwd(paste0("C:/Users/", researcher, "/Owncloud/TI Dijon/donnees"))
}

# Chargement des données
sgsdata <- read.xlsx("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichier_traitement/sgsdata.xlsx")


sgsdata <- sgsdata %>%
  mutate(
    across(
      contains("_prix_kg"),
      ~ replace_na(.x, 0)
    )
  )

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)


library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)

# --- 0) S'assurer que 'Compliant' existe (même logique que précédemment) ---
if (!"Compliant" %in% names(sgsdata)) {
  ids_groupe1_camp1 <- sgsdata %>% 
    filter(groupe == 1, Campagne == 1,
           utilisation2_FFQ == 1, limitation_FFQ == 1,
           Periode == 1, Mesure == "Carnet") %>% 
    distinct(Identifiant)
  
  ids_groupe1_camp2 <- sgsdata %>% 
    filter(groupe == 1, Campagne == 2,
           Prop_montant_theorique_saisie >= 0.3,
           Prop_montant_theorique_saisie <= 1.7,
           compliance >= 0.7, Periode == 1, Mesure == "Carnet") %>% 
    distinct(Identifiant)
  
  ids_compliant_all <- union(ids_groupe1_camp1$Identifiant, ids_groupe1_camp2$Identifiant)
  
  sgsdata <- sgsdata %>%
    mutate(Compliant = if_else(Identifiant %in% ids_compliant_all, 1, 0))
}

# --- 1) Filtrage baseline ---
data <- sgsdata %>%
  filter(!is.na(Mesure),
         Mesure == "Carnet",
         Identifiant != "LE300",
         Periode == 0) %>%
  mutate(across(where(is.character), ~na_if(.x, "")))

data_LE <- data %>% filter(voie_de_recrutement == "LE")

# Définition stricte des NonCompliant: traités mais non compliant
is_noncomp <- with(data, (groupe == 1 & Compliant == 0))

# --- 2) Variables continues ---
cont_vars <- c(
  "Age_Central", "Combien.de.personnes.vivent.dans.votre.foyer",
  "Différentiel_aide", "Part_Kcal_BIO", "Part_Kcal_Epiceries",
  "Part_Kcal_Supermarchés", "Income_UC_INSEE",
  "FV_POIDS", "FRUITS_POIDS", "FRUITS_SECS_POIDS", "NOIX_POIDS", "LEGUMES_POIDS",
  "LEG_SECS_POIDS", "FV_prix_kg", "FRUITS_prix_kg", "FRUITS_SECS_prix_kg",
  "NOIX_prix_kg", "LEGUMES_prix_kg", "LEG_SECS_prix_kg", "depense_alim_uc",
  "VIANDES_POIDS", "VIANDE_ROUGE_PORC_POIDS", "AUTRE_PDTS_ANIMAUX_POIDS",
  "POULET_OEUFS_POIDS", "POISSONS_POIDS", "PLATS_PREP_POIDS",
  "FEC_POIDS", "PDTS_LAITIERS_POIDS", "MG_POIDS", "PDTS_DISCRETIONNAIRES_POIDS",
  "SSB_POIDS", "ALCOOL_POIDS", "MAR", "MER", "SOMME_POIDS_KCAL",
  "energy_densite", "SOMME_POIDS_HORS_BOISSON", "climat_env", "HENI_TOT"
)

present_cont <- intersect(cont_vars, names(data))
data    <- data    %>% mutate(across(all_of(present_cont), ~ suppressWarnings(as.numeric(.x))))
data_LE <- data_LE %>% mutate(across(all_of(present_cont), ~ suppressWarnings(as.numeric(.x))))

# --- 3) Fonctions p-values ---
# Welch; mets var.equal=TRUE si tu veux Student strict
p_ci_cont <- function(x, g) {
  x0 <- x[g == 0 & !is.na(x)]
  x1 <- x[g == 1 & !is.na(x)]
  if (length(x0) < 2 || length(x1) < 2) return(NA_real_)
  as.numeric(tryCatch(t.test(x0, x1, var.equal = FALSE)$p.value, error = function(e) NA_real_))
}
p_cc_cont <- function(x, g, comp) {
  x0 <- x[g == 0 & !is.na(x)]
  xc <- x[comp == 1 & !is.na(x)]
  if (length(x0) < 2 || length(xc) < 2) return(NA_real_)
  as.numeric(tryCatch(t.test(x0, xc, var.equal = FALSE)$p.value, error = function(e) NA_real_))
}
p_cn_cont <- function(x, comp, noncomp_flag) {
  xc  <- x[comp == 1 & !is.na(x)]
  xnc <- x[noncomp_flag & !is.na(x)]
  if (length(xc) < 2 || length(xnc) < 2) return(NA_real_)
  as.numeric(tryCatch(t.test(xc, xnc, var.equal = FALSE)$p.value, error = function(e) NA_real_))
}

p_from_2x2 <- function(a, b, c, d) {
  mat <- matrix(c(a,b,c,d), nrow = 2, byrow = TRUE)
  if (any(mat < 5)) as.numeric(tryCatch(fisher.test(mat)$p.value, error = function(e) NA_real_))
  else as.numeric(tryCatch(chisq.test(mat, correct = FALSE)$p.value, error = function(e) NA_real_))
}

val_eq <- function(x, target) str_trim(str_to_lower(as.character(x))) == str_trim(str_to_lower(target))
is_yes <- function(x) str_to_upper(as.character(x)) %in% c("OUI","1","TRUE","YES")
is_one <- function(x) suppressWarnings(as.numeric(x)) == 1

# --- 4) CONTINUES : moyennes + p-values ---
cont_tbl <- tibble(Variable = present_cont) %>%
  mutate(
    Control         = map_dbl(Variable, ~ mean(data[[.x]][data$groupe == 0], na.rm = TRUE)),
    Intervention    = map_dbl(Variable, ~ mean(data[[.x]][data$groupe == 1], na.rm = TRUE)),
    Compliant       = map_dbl(Variable, ~ mean(data[[.x]][data$Compliant == 1], na.rm = TRUE)),
    p_CI            = map_dbl(Variable, ~ p_ci_cont(data[[.x]], data$groupe)),
    p_CC            = map_dbl(Variable, ~ p_cc_cont(data[[.x]], data$groupe, data$Compliant)),
    # Sous-groupe LE
    Control_LE      = map_dbl(Variable, ~ mean(data_LE[[.x]][data_LE$groupe == 0], na.rm = TRUE)),
    Intervention_LE = map_dbl(Variable, ~ mean(data_LE[[.x]][data_LE$groupe == 1], na.rm = TRUE)),
    p_CI_LE         = map_dbl(Variable, ~ p_ci_cont(data_LE[[.x]], data_LE$groupe)),
    # NonCompliant (traités mais Compliant==0) + p_CN
    NonCompliant    = map_dbl(Variable, ~ mean(data[[.x]][is_noncomp], na.rm = TRUE)),
    p_CN            = map_dbl(Variable, ~ p_cn_cont(data[[.x]], data$Compliant, is_noncomp))
  )

# --- 5) CATEGORIELLES : % et p-values ---
g    <- data$groupe
comp <- data$Compliant
g_LE <- data_LE$groupe
nonc <- is_noncomp

cat_list <- list()
if ("Sexe" %in% names(data))                           cat_list[["Gender-Women (%)"]]             <- val_eq(data$Sexe, "Femme")
if ("Quel.est.votre.pays.de.naissance." %in% names(data)) cat_list[["Birth place - France (%)"]]     <- val_eq(data$Quel.est.votre.pays.de.naissance., "France")
if ("Aide_alim" %in% names(data))                      cat_list[["Food aid (%)"]]                 <- is_yes(data$Aide_alim)
if ("Foyer_monoparental" %in% names(data))             cat_list[["Single-Parent Household (%)"]]  <- is_one(data$Foyer_monoparental)
if ("Situation_emploi" %in% names(data)) {
  cat_list[["Employed (%)"]]    <- val_eq(data$Situation_emploi, "Occupe un emploi")
  cat_list[["Unemployed (%)"]]  <- val_eq(data$Situation_emploi, "Inactif(ve)")
  cat_list[["Student (%)"]]     <- val_eq(data$Situation_emploi, "Etudiant(e)")
  cat_list[["Retired (%)"]]     <- val_eq(data$Situation_emploi, "Retraité(e)")
}
if ("Education" %in% names(data)) {
  cat_list[["Long-term studies (%)"]]         <- val_eq(data$Education, "2e ou 3e cycle universitaire, grande école")
  cat_list[["Short term studies (%)"]]        <- val_eq(data$Education, "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale")
  cat_list[["Baccalaureate (%)"]]             <- val_eq(data$Education, "Baccalauréat")
  cat_list[["Pre-baccalaureate diploma (%)"]]<- val_eq(data$Education, "Diplôme en dessous du baccalauréat")
  cat_list[["No qualifications (%)"]]         <- val_eq(data$Education, "Aucun diplôme")
}

cat_tbl <- imap_dfr(cat_list, function(flag, lab){
  # full
  n0 <- sum(g == 0, na.rm = TRUE); y0 <- sum(flag & g == 0, na.rm = TRUE)
  n1 <- sum(g == 1, na.rm = TRUE); y1 <- sum(flag & g == 1, na.rm = TRUE)
  nc <- sum(comp == 1, na.rm = TRUE); yc <- sum(flag & comp == 1, na.rm = TRUE)
  nnc <- sum(nonc, na.rm = TRUE); ync <- sum(flag & nonc, na.rm = TRUE)
  
  # LE
  flag_LE <- switch(lab,
                    "Gender-Women (%)"              = if ("Sexe" %in% names(data_LE)) val_eq(data_LE$Sexe, "Femme") else rep(NA, nrow(data_LE)),
                    "Birth place - France (%)"      = if ("Quel.est.votre.pays.de.naissance." %in% names(data_LE)) val_eq(data_LE$Quel.est.votre.pays.de.naissance., "France") else rep(NA, nrow(data_LE)),
                    "Food aid (%)"                  = if ("Aide_alim" %in% names(data_LE)) is_yes(data_LE$Aide_alim) else rep(NA, nrow(data_LE)),
                    "Single-Parent Household (%)"   = if ("Foyer_monoparental" %in% names(data_LE)) is_one(data_LE$Foyer_monoparental) else rep(NA, nrow(data_LE)),
                    "Employed (%)"                  = if ("Situation_emploi" %in% names(data_LE)) val_eq(data_LE$Situation_emploi, "Occupe un emploi") else rep(NA, nrow(data_LE)),
                    "Unemployed (%)"                = if ("Situation_emploi" %in% names(data_LE)) val_eq(data_LE$Situation_emploi, "Inactif(ve)") else rep(NA, nrow(data_LE)),
                    "Student (%)"                   = if ("Situation_emploi" %in% names(data_LE)) val_eq(data_LE$Situation_emploi, "Etudiant(e)") else rep(NA, nrow(data_LE)),
                    "Retired (%)"                   = if ("Situation_emploi" %in% names(data_LE)) val_eq(data_LE$Situation_emploi, "Retraité(e)") else rep(NA, nrow(data_LE)),
                    "Long-term studies (%)"         = if ("Education" %in% names(data_LE)) val_eq(data_LE$Education, "2e ou 3e cycle universitaire, grande école") else rep(NA, nrow(data_LE)),
                    "Short term studies (%)"        = if ("Education" %in% names(data_LE)) val_eq(data_LE$Education, "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale") else rep(NA, nrow(data_LE)),
                    "Baccalaureate (%)"             = if ("Education" %in% names(data_LE)) val_eq(data_LE$Education, "Baccalauréat") else rep(NA, nrow(data_LE)),
                    "Pre-baccalaureate diploma (%)" = if ("Education" %in% names(data_LE)) val_eq(data_LE$Education, "Diplôme en dessous du baccalauréat") else rep(NA, nrow(data_LE)),
                    "No qualifications (%)"         = if ("Education" %in% names(data_LE)) val_eq(data_LE$Education, "Aucun diplôme") else rep(NA, nrow(data_LE)),
                    rep(NA, nrow(data_LE))
  )
  n0_LE <- sum(g_LE == 0, na.rm = TRUE); y0_LE <- sum(flag_LE & g_LE == 0, na.rm = TRUE)
  n1_LE <- sum(g_LE == 1, na.rm = TRUE); y1_LE <- sum(flag_LE & g_LE == 1, na.rm = TRUE)
  
  tibble(
    Variable        = lab,
    Control         = 100 * y0 / n0,
    Intervention    = 100 * y1 / n1,
    Compliant       = 100 * yc / nc,
    p_CI            = p_from_2x2(y0, n0 - y0, y1, n1 - y1),
    p_CC            = p_from_2x2(y0, n0 - y0, yc, nc - yc),
    Control_LE      = 100 * y0_LE / n0_LE,
    Intervention_LE = 100 * y1_LE / n1_LE,
    p_CI_LE         = p_from_2x2(y0_LE, n0_LE - y0_LE, y1_LE, n1_LE - y1_LE),
    NonCompliant    = 100 * ync / nnc,
    p_CN            = p_from_2x2(yc, nc - yc, ync, nnc - ync) # Compliant vs NonCompliant
  )
})

# --- 6) Totaux en tête ---
totals <- tibble(
  Variable        = "Total participants (N)",
  Control         = sum(data$groupe == 0, na.rm = TRUE),
  Intervention    = sum(data$groupe == 1, na.rm = TRUE),
  Compliant       = sum(data$Compliant == 1, na.rm = TRUE),
  p_CI            = NA_real_,
  p_CC            = NA_real_,
  Control_LE      = sum(data_LE$groupe == 0, na.rm = TRUE),
  Intervention_LE = sum(data_LE$groupe == 1, na.rm = TRUE),
  p_CI_LE         = NA_real_,
  NonCompliant    = sum(is_noncomp, na.rm = TRUE),
  p_CN            = NA_real_
)

# --- 7) Assemblage final (NonCompliant en DERNIÈRE colonne) ---
tab_socio_demo <- bind_rows(
  totals,
  cat_tbl,
  cont_tbl
) %>%
  mutate(
    Control         = round(Control, 3),
    Intervention    = round(Intervention, 3),
    Compliant       = round(Compliant, 3),
    NonCompliant    = round(NonCompliant, 3),
    p_CI            = ifelse(is.na(p_CI), NA, round(p_CI, 4)),
    p_CC            = ifelse(is.na(p_CC), NA, round(p_CC, 4)),
    p_CN            = ifelse(is.na(p_CN), NA, round(p_CN, 4)),
    Control_LE      = round(Control_LE, 3),
    Intervention_LE = round(Intervention_LE, 3),
    p_CI_LE         = ifelse(is.na(p_CI_LE), NA, round(p_CI_LE, 4))
  ) %>%
  # NonCompliant à la fin :
  select(Variable, Control, Intervention, Compliant, p_CI, p_CC,
         Control_LE, Intervention_LE, p_CI_LE, NonCompliant, p_CN)

tab_socio_demo



#tableau attrition 
Carnets_Nov_22_att <- read.xlsx("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/Carnets_Tableaux_nov_22_attrition.xlsx")
Carnets_Nov_23_att <- read.xlsx("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/Carnets_Tableaux_nov_23_attrition.xlsx")
FFQ_Nov_22_att <- read.xlsx("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/FFQ_Tableaux_nov_22.xlsx")
FFQ_Nov_23_att  <- read.xlsx("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/FFQ_Tableaux_nov_23.xlsx")
Liste  <- read.xlsx("Données analysées - Article N°1 chèques/Tableaux_annexes/Liste traités_comp.xlsx")


Carnets_att <- rbind(
  dplyr::select(Carnets_Nov_22_att, intersect(names(Carnets_Nov_22_att), names(Carnets_Nov_23_att))),
  dplyr::select(Carnets_Nov_23_att, intersect(names(Carnets_Nov_22_att), names(Carnets_Nov_23_att))))
Carnets_att <- left_join(Carnets_att, Liste, by="Identifiant")
library(dplyr)
library(stringr)

Carnets_att <- Carnets_att %>%
  rename_with(~ str_replace(.x, "_CARNET$", "_POIDS"))

Carnets_att$FV_POIDS <- Carnets_att$FRUITS_POIDS + Carnets_att$FRUITS_SECS_POIDS  + Carnets_att$NOIX_POIDS + Carnets_att$LEGUMES_POIDS 
Carnets_att$FEC_POIDS <- Carnets_att$FEC_NON_RAF_POIDS + Carnets_att$FEC_RAF_POIDS
Carnets_att$PDTS_LAITIERS_POIDS <- Carnets_att$LAIT_POIDS + Carnets_att$LAITAGES_POIDS + Carnets_att$FROMAGES_POIDS
Carnets_att$POULET_OEUFS_POIDS <- Carnets_att$POULET_POIDS + Carnets_att$OEUFS_POIDS
Carnets_att$AUTRE_PDTS_ANIMAUX_POIDS <- Carnets_att$CHARCUTERIE_HORS_JB_POIDS  + Carnets_att$JAMBON_BLANC_POIDS 
Carnets_att$PLATS_PREP_POIDS <- Carnets_att$PLATS_PREP_CARNES_POIDS + Carnets_att$PLATS_PREP_VEGETARIENS_POIDS + Carnets_att$QUICHES_PIZZAS_TARTES_SALEES_POIDS
Carnets_att$VIANDE_ROUGE_PORC_POIDS <- Carnets_att$VIANDE_ROUGE_POIDS+ Carnets_att$PORC_POIDS
Carnets_att$VIANDES_POIDS <- Carnets_att$VIANDE_ROUGE_POIDS + Carnets_att$POULET_POIDS + Carnets_att$PLATS_PREP_CARNES_POIDS + Carnets_att$JAMBON_BLANC_POIDS + Carnets_att$PORC_POIDS + Carnets_att$CHARCUTERIE_HORS_JB_POIDS
Carnets_att$MG_POIDS <- Carnets_att$MGA_POIDS + Carnets_att$MGV_POIDS
Carnets_att$PDTS_DISCRETIONNAIRES_POIDS <-   Carnets_att$CEREALES_PD_POIDS + Carnets_att$DESSERTS_LACTES_POIDS + Carnets_att$PDTS_SUCRES_POIDS + Carnets_att$SAUCES_POIDS  + Carnets_att$SNACKS_AUTRES_POIDS 
Carnets_att$SSB_POIDS <-  Carnets_att$SODAS_SUCRES_POIDS + Carnets_att$SODAS_LIGHT_POIDS +Carnets_att$FRUITS_JUS_POIDS 



Carnets_att$Situation_emploi <- ifelse(( Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.== "Autre inactif (invalide, handicapé, en congé maladie > 3 mois, titulaire d’une pension de réversion)"|
                                       Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.== "Chômeur inscrit ou non au Pôle-Emploi"|  Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.=="Femme ou homme au foyer (y compris congé parental)" |
                                       Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.==  "Situation administrative ne permettant pas de travailler" ),
                                   ("Inactif(ve)"), (Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.))

Carnets_att$Situation_emploi <- ifelse(( Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.== "Refuse de répondre"|
                                       Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.==  "Ne sait pas" ),
                                   ("Autre"), (Carnets_att$Situation_emploi))

Carnets_att$Situation_emploi <- ifelse(Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.== "Etudiant, élève, en formation, en stage non rémunéré",
                                   ("Etudiant(e)"), (Carnets_att$Situation_emploi))

Carnets_att$Situation_emploi <- ifelse(Carnets_att$Quelle.est.votre.situation.professionnelle.actuelle.== "Retraitée ancien salarié ou préretraitée",
                                   ("Retraité(e)"), (Carnets_att$Situation_emploi))

Carnets_att$Source_revenu <- ifelse(( Carnets_att$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Minimas sociaux (RSA, Allocations familiales...)"|
                                    Carnets_att$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Indemnités chômage" ),
                                ("Aides sociales"), (Carnets_att$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))

Carnets_att$Source_revenu <- ifelse(( Carnets_att$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Ne sait pas"|
                                    Carnets_att$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Ne veut pas répondre" |
                                    Carnets_att$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Autre" |
                                    Carnets_att$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Refuse de répondre" ),
                                ("Autre"), (Carnets_att$Source_revenu))

Carnets_att$Source_revenu <- ifelse(( Carnets_att$Source_revenu== "Travail (salarié, autoentrepreneur...)"),
                                ("Travail"), (Carnets_att$Source_revenu))


Carnets_att$Education <- ifelse(( Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale"|
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "2e ou 3e cycle universitaire, grande école") ,
                            ("Université"), (Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))


Carnets_att$Education <- ifelse(( Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Baccalauréat"|
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Baccalauréat technologique ou professionnel" ) ,
                            ("Baccalauréat"), (Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))

Carnets_att$Education <- ifelse(( Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Diplôme en-dessous du baccalauréat"|
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "CAP, BEP, BEPC, brevet élémentaire, BEPS" |
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Brevet de technicien, Brevet Professionnel (BP), BEI, BEC, BEA" |
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Certificat d'études primaires (CEP), diplôme de fin d'études obligatoire") ,
                            ("Diplôme en dessous du baccalauréat"), (Carnets_att$Education))

Carnets_att$Education <- ifelse(( Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Ne sait pas"|
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Refuse de répondre" |
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Autre" |
                                Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Ne veut pas répondre"|
                                is.na(Carnets_att$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.)) ,
                            ("Autre"), (Carnets_att$Education))



sgsdata <- read.xlsx("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichier_traitement/sgsdata.xlsx")
data <- sgsdata %>%  filter(!is.na(Mesure),Mesure == "Carnet",Identifiant != "LE300",Periode == 0) %>% mutate(across(where(is.character), ~na_if(.x, "")))
library(dplyr)

# 1️⃣ Identifier les identifiants communs
ids_communs <- intersect(data$Identifiant, Carnets_att$Identifiant)

# 2️⃣ Comparer les groupes entre les deux tableaux
verif_groupes <- data %>%
  dplyr::filter(Identifiant %in% ids_communs) %>%
  dplyr::mutate(groupe_data = groupe) %>%
  dplyr::select(Identifiant, groupe_data) %>%
  dplyr::left_join(
    Carnets_att %>%
      dplyr::mutate(groupe_att = groupe) %>%
      dplyr::select(Identifiant, groupe_att),
    by = "Identifiant"
  )

# 3️⃣ Identifier les divergences de groupe
divergents <- verif_groupes %>%
  dplyr::filter(groupe_data != groupe_att)

# 4️⃣ Vérification
if (nrow(divergents) == 0) {
  message("✅ Tous les identifiants communs appartiennent bien au même groupe.")
} else {
  message("⚠️ Des divergences existent. Voici les identifiants concernés :")
  print(divergents)
}


#Créer une colonne attrition 
Carnets_att <- Carnets_att %>%
  mutate(
    attrition = if_else(Identifiant %in% data$Identifiant, 1, 0)
  )



# --- Fonction générique reprise ---
make_attrition_table <- function(df, group_label = "groupe inconnu") {
  cont_vars_here <- intersect(cont_vars, names(df))
  df <- df %>% mutate(across(all_of(cont_vars_here), ~ suppressWarnings(as.numeric(.x))))
  
  p_cont <- function(x, attr) {
    x0 <- x[attr == 0 & !is.na(x)]
    x1 <- x[attr == 1 & !is.na(x)]
    if (length(x0) < 2 || length(x1) < 2) return(NA_real_)
    as.numeric(tryCatch(t.test(x0, x1, var.equal = FALSE)$p.value, error = function(e) NA_real_))
  }
  p_from_2x2 <- function(a, b, c, d) {
    mat <- matrix(c(a,b,c,d), nrow = 2, byrow = TRUE)
    if (any(mat < 5)) as.numeric(tryCatch(fisher.test(mat)$p.value, error = function(e) NA_real_))
    else as.numeric(tryCatch(chisq.test(mat, correct = FALSE)$p.value, error = function(e) NA_real_))
  }
  val_eq <- function(x, target) str_trim(str_to_lower(as.character(x))) == str_trim(str_to_lower(target))
  is_yes <- function(x) str_to_upper(as.character(x)) %in% c("OUI","1","TRUE","YES")
  is_one <- function(x) suppressWarnings(as.numeric(x)) == 1
  
  cont_tbl <- tibble(Variable = cont_vars_here) %>%
    mutate(
      Attr0   = map_dbl(Variable, ~ mean(df[[.x]][df$attrition == 0], na.rm = TRUE)),
      Attr1   = map_dbl(Variable, ~ mean(df[[.x]][df$attrition == 1], na.rm = TRUE)),
      p_value = map_dbl(Variable, ~ p_cont(df[[.x]], df$attrition))
    )
  
  cat_list <- list()
  if ("Sexe" %in% names(df))                              cat_list[["Gender-Women (%)"]]             <- val_eq(df$Sexe, "Femme")
  if ("Quel.est.votre.pays.de.naissance." %in% names(df)) cat_list[["Birth place - France (%)"]]     <- val_eq(df$Quel.est.votre.pays.de.naissance., "France")
  if ("Aide_alim" %in% names(df))                         cat_list[["Food aid (%)"]]                 <- is_yes(df$Aide_alim)
  if ("Foyer_monoparental" %in% names(df))                cat_list[["Single-Parent Household (%)"]]  <- is_one(df$Foyer_monoparental)
  if ("Situation_emploi" %in% names(df)) {
    cat_list[["Employed (%)"]]   <- val_eq(df$Situation_emploi, "Occupe un emploi")
    cat_list[["Unemployed (%)"]] <- val_eq(df$Situation_emploi, "Inactif(ve)")
    cat_list[["Student (%)"]]    <- val_eq(df$Situation_emploi, "Etudiant(e)")
    cat_list[["Retired (%)"]]    <- val_eq(df$Situation_emploi, "Retraité(e)")
  }
  if ("Education" %in% names(df)) {
    cat_list[["Long-term studies (%)"]]         <- val_eq(df$Education, "2e ou 3e cycle universitaire, grande école")
    cat_list[["Short term studies (%)"]]        <- val_eq(df$Education, "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale")
    cat_list[["Baccalaureate (%)"]]             <- val_eq(df$Education, "Baccalauréat")
    cat_list[["Pre-baccalaureate diploma (%)"]] <- val_eq(df$Education, "Diplôme en dessous du baccalauréat")
    cat_list[["No qualifications (%)"]]         <- val_eq(df$Education, "Aucun diplôme")
  }
  
  cat_tbl <- imap_dfr(cat_list, function(flag, lab){
    n0 <- sum(df$attrition == 0, na.rm = TRUE); y0 <- sum(flag & df$attrition == 0, na.rm = TRUE)
    n1 <- sum(df$attrition == 1, na.rm = TRUE); y1 <- sum(flag & df$attrition == 1, na.rm = TRUE)
    tibble(
      Variable = lab,
      Attr0    = if (n0 > 0) 100 * y0 / n0 else NA_real_,
      Attr1    = if (n1 > 0) 100 * y1 / n1 else NA_real_,
      p_value  = if (n0 > 0 && n1 > 0) p_from_2x2(y0, n0 - y0, y1, n1 - y1) else NA_real_
    )
  })
  
  totals <- tibble(
    Variable = paste0("Total participants", group_label),
    Attr0    = sum(df$attrition == 0, na.rm = TRUE),
    Attr1    = sum(df$attrition == 1, na.rm = TRUE),
    p_value  = NA_real_
  )
  
  bind_rows(totals, cat_tbl, cont_tbl) %>%
    mutate(
      Attr0   = round(Attr0, 3),
      Attr1   = round(Attr1, 3),
      p_value = ifelse(is.na(p_value), NA, round(p_value, 4))
    ) %>%
    select(Variable, Attr0, Attr1, p_value)
}

# --- 1) Groupe = 1, toute l'expérience ---
data_g1 <- Carnets_att %>%
  filter(groupe == 1, !is.na(attrition)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))
tab_attrition_g1 <- make_attrition_table(data_g1, group_label = "Groupe 1 (toute expérience)")

# --- 1) Groupe = 0, toute l'expérience ---
data_g0 <- Carnets_att %>%
  filter(groupe == 0, !is.na(attrition)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))
tab_attrition_g0 <- make_attrition_table(data_g0, group_label = "Groupe 0 (toute expérience)") 

# --- 2) Groupe = 1, uniquement identifiants 'LE' ---
data_g1_LE <- Carnets_att %>%
  filter(groupe == 1, !is.na(attrition), str_detect(Identifiant, "^LE")) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))
tab_attrition_g1_LE <- make_attrition_table(data_g1_LE, group_label = "Groupe 1 — Identifiants 'LE'")


# --- 2) Groupe = 1, uniquement identifiants 'LE' ---
data_g0_LE <- Carnets_att %>%
  filter(groupe == 0, !is.na(attrition), str_detect(Identifiant, "^LE")) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))
tab_attrition_g0_LE <- make_attrition_table(data_g0_LE, group_label = "Groupe 0 — Identifiants 'LE'")


library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# --- Helpers communs ---
p_from_2x2 <- function(a, b, c, d) {
  mat <- matrix(c(a,b,c,d), nrow = 2, byrow = TRUE)
  if (any(mat < 5)) as.numeric(tryCatch(fisher.test(mat)$p.value, error = function(e) NA_real_))
  else as.numeric(tryCatch(chisq.test(mat, correct = FALSE)$p.value, error = function(e) NA_real_))
}
val_eq <- function(x, target) str_trim(str_to_lower(as.character(x))) == str_trim(str_to_lower(target))
is_yes <- function(x) str_to_upper(as.character(x)) %in% c("OUI","1","TRUE","YES")
is_one <- function(x) suppressWarnings(as.numeric(x)) == 1

# --- Nouvelle fonction de balance par groupe (0 vs 1), sans attrition ---
make_group_balance_table <- function(df, subset_label = "") {
  stopifnot("groupe" %in% names(df))
  df <- df %>% mutate(across(where(is.character), ~na_if(.x, "")))
  
  # Continues
  cont_vars_here <- intersect(cont_vars, names(df))
  df <- df %>% mutate(across(all_of(cont_vars_here), ~ suppressWarnings(as.numeric(.x))))
  
  p_cont <- function(x, g){
    x0 <- x[g == 0 & !is.na(x)]
    x1 <- x[g == 1 & !is.na(x)]
    if (length(x0) < 2 || length(x1) < 2) return(NA_real_)
    as.numeric(tryCatch(t.test(x0, x1, var.equal = FALSE)$p.value, error = function(e) NA_real_))
  }
  
  cont_tbl <- tibble(Variable = cont_vars_here) %>%
    mutate(
      G0_mean = map_dbl(Variable, ~ mean(df[[.x]][df$groupe == 0], na.rm = TRUE)),
      G1_mean = map_dbl(Variable, ~ mean(df[[.x]][df$groupe == 1], na.rm = TRUE)),
      p_value = map_dbl(Variable, ~ p_cont(df[[.x]], df$groupe))
    )
  
  # Catégorielles (définies comme indicateurs 0/1)
  cat_list <- list()
  if ("Sexe" %in% names(df))                              cat_list[["Gender-Women (%)"]]            <- val_eq(df$Sexe, "Femme")
  if ("Quel.est.votre.pays.de.naissance." %in% names(df)) cat_list[["Birth place - France (%)"]]    <- val_eq(df$Quel.est.votre.pays.de.naissance., "France")
  if ("Aide_alim" %in% names(df))                         cat_list[["Food aid (%)"]]                <- is_yes(df$Aide_alim)
  if ("Foyer_monoparental" %in% names(df))                cat_list[["Single-Parent Household (%)"]] <- is_one(df$Foyer_monoparental)
  if ("Situation_emploi" %in% names(df)) {
    cat_list[["Employed (%)"]]   <- val_eq(df$Situation_emploi, "Occupe un emploi")
    cat_list[["Unemployed (%)"]] <- val_eq(df$Situation_emploi, "Inactif(ve)")
    cat_list[["Student (%)"]]    <- val_eq(df$Situation_emploi, "Etudiant(e)")
    cat_list[["Retired (%)"]]    <- val_eq(df$Situation_emploi, "Retraité(e)")
  }
  if ("Education" %in% names(df)) {
    cat_list[["Long-term studies (%)"]]         <- val_eq(df$Education, "2e ou 3e cycle universitaire, grande école")
    cat_list[["Short term studies (%)"]]        <- val_eq(df$Education, "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale")
    cat_list[["Baccalaureate (%)"]]             <- val_eq(df$Education, "Baccalauréat")
    cat_list[["Pre-baccalaureate diploma (%)"]] <- val_eq(df$Education, "Diplôme en dessous du baccalauréat")
    cat_list[["No qualifications (%)"]]         <- val_eq(df$Education, "Aucun diplôme")
  }
  
  cat_tbl <- imap_dfr(cat_list, function(flag, lab){
    n0 <- sum(df$groupe == 0, na.rm = TRUE); y0 <- sum(flag & df$groupe == 0, na.rm = TRUE)
    n1 <- sum(df$groupe == 1, na.rm = TRUE); y1 <- sum(flag & df$groupe == 1, na.rm = TRUE)
    tibble(
      Variable = lab,
      G0       = if (n0 > 0) 100 * y0 / n0 else NA_real_,
      G1       = if (n1 > 0) 100 * y1 / n1 else NA_real_,
      p_value  = if (n0 > 0 && n1 > 0) p_from_2x2(y0, n0 - y0, y1, n1 - y1) else NA_real_
    )
  })
  
  totals <- tibble(
    Variable = paste0("Total participants ", subset_label),
    G0       = sum(df$groupe == 0, na.rm = TRUE),
    G1       = sum(df$groupe == 1, na.rm = TRUE),
    p_value  = NA_real_
  )
  
  # Harmoniser noms colonnes entre cont_tbl et cat_tbl
  cont_tbl <- cont_tbl %>% rename(G0 = G0_mean, G1 = G1_mean)
  
  bind_rows(totals, cat_tbl, cont_tbl) %>%
    mutate(
      G0      = round(G0, 3),
      G1      = round(G1, 3),
      p_value = ifelse(is.na(p_value), NA, round(p_value, 4))
    ) %>%
    select(Variable, G0, G1, p_value)
}

# --- 1) Comparaison groupes 0 vs 1 : toute l’expérience, sans attrition ---
tab_balance_all <- Carnets_att %>%
  filter(!is.na(groupe)) %>%
  make_group_balance_table(subset_label = "(toute l'expérience)")

# --- 2) Comparaison groupes 0 vs 1 : sous-échantillon LE ---
#    (via identifiants commençant par "LE" ; adapte si tu préfères 'voie_de_recrutement == "LE"')
tab_balance_LE <- Carnets_att %>%
  filter(!is.na(groupe), str_detect(Identifiant, "^LE")) %>%
  make_group_balance_table(subset_label = "(voie_de_recrutement = 'LE')")

