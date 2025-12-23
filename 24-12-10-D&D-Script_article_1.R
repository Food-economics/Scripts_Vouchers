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
CALNUT  <- read.xlsx((paste("Données analysées - Article N°1 chèques/Tableaux_annexes/Alim_CALNUT_CODAPPRO_CARNET.xlsx", sep="")))
sgsdata$branche <- ifelse(sgsdata$voie_de_recrutement == "LE", "1", "0")
sgsdata$Traitement <- sgsdata$groupe * sgsdata$Periode
sgsdata$cheque_UC <- sgsdata$cheque_theo / sgsdata$UC_TI
sgsdata <- sgsdata %>%
  mutate(
    across(
      matches("_prix_kg|Si\\.noncombien\\.de\\.fois\\.la\\.semaine\\.derniere\\.|Gapillage\\.produits\\.non\\.entames|Epicerie|Surgelé"),
      ~ replace_na(.x, 0)
    )
  )

## Analyse "Carnet"------------

##COMPLIANCE ------------------------------
#Prop montant théorique saisie
##result_comp2 <- sgsdata %>%
##  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300" & groupe ==1 &
##            Periode ==1 ) #& Campagne ==1 )
##
#mean(result_comp2$Prop_montant_theorique_saisie)

result_comp2 <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300" & groupe ==1 &
           Campagne ==2 &  Periode ==1   &        Prop_montant_theorique_saisie >= 0.3 &
          Prop_montant_theorique_saisie <= 1.7
           )


result_comp2$comp_carnet <- ifelse(
  result_comp2$compliance >= 0.7 & 
    result_comp2$Prop_montant_theorique_saisie >= 0.3 & 
    result_comp2$Prop_montant_theorique_saisie <= 1.7,
  1, 
  0
)

result_comp2 %>%
  count(comp_carnet) %>%
  mutate(prop = n / sum(n))





#result_comp2$Les.avez.vous.utilises. == "Oui certains"
result_comp2$Les.avez.vous.utilises.2 <- ifelse((result_comp2$Les.avez.vous.utilises. == "Oui tous" ),(1),(0))
mean(result_comp2$Les.avez.vous.utilises.2, na.rm=TRUE)
result_comp2$utilisation_FFQ <- ifelse(( result_comp2$Qu.avez.vous.achete.avec.=="Fruits;Légumes;Légumineuses"|
                                           result_comp2$Qu.avez.vous.achete.avec.=="Fruits;Légumes"|
                                           result_comp2$Qu.avez.vous.achete.avec.=="Légumes;Légumineuses"|
                                           result_comp2$Qu.avez.vous.achete.avec.=="Fruits;Légumineuses"|
                                           result_comp2$Qu.avez.vous.achete.avec.=="Fruits"|
                                           result_comp2$Qu.avez.vous.achete.avec.=="Légumes"|
                                           result_comp2$Qu.avez.vous.achete.avec.=="Légumineuses"),(1),(0))
mean(result_comp2$utilisation_FFQ, na.rm=TRUE)
result_comp2$limitation_FFQ <- ifelse((result_comp2$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.=="Oui"),(1),(0))
mean(result_comp2$limitation_FFQ, na.rm=TRUE)

result_comp2$Comp_FFQ <- ifelse(( result_comp2$utilisation_FFQ ==1 & result_comp2$limitation_FFQ ==1 ),(1),(0))
result_comp2$Comp_FFQ <- ifelse(( is.na(result_comp2$Comp_FFQ) ),(0),(result_comp2$Comp_FFQ))

mean(result_comp2$Comp_FFQ)


cor(result_comp2$comp_carnet, result_comp2$Comp_FFQ, method = "pearson")
#result_comp2$utilisation_FFQ==1 & 
library(vcd)
tbl <- table(result_comp2$comp_carnet, result_comp2$Comp_FFQ)
assocstats(tbl)$phi

result_comp2$Comp_FFQ <- factor(result_comp2$Comp_FFQ)
result_comp2$comp_carnet <- factor(result_comp2$comp_carnet)

confusion_mat <- table(result_comp2$Comp_FFQ,result_comp2$comp_carnet)
count_zero <- sum(result_comp2$comp_carnet == 0)

# Extraire les éléments de la matrice
true_positive <- confusion_mat[2, 2]
true_negative <- confusion_mat[1, 1]
false_positive <- confusion_mat[1, 2]
false_negative <- confusion_mat[2, 1]

# Calculer l'exactitude
accuracy <- (true_positive + true_negative) / sum(confusion_mat)
print(paste("Accuracy: ", accuracy))

# Calculer la sensibilité (recall)
sensitivity <- true_positive / (true_positive + false_negative)
print(paste("Sensitivity: ", sensitivity))

# Calculer la spécificité
specificity <- true_negative / (true_negative + false_positive)
print(paste("Specificity: ", specificity))

#Calculer la précision
precision <- true_positive / (true_positive + false_positive)
print(paste("precision: ", precision))

# Estimer la corrélation
correlation <- cor(as.numeric(result_comp2$Comp_FFQ),as.numeric(result_comp2$comp_carnet))
print(paste("Correlation: ", correlation))

median(result_comp2$Compliant_booklet)
median(result_comp2$compliance)

result_comp2 %>%
  group_by(Comp_FFQ) %>%
  summarise(compliance = mean(compliance, na.rm = TRUE))

sgsdata$compliance <- ifelse((sgsdata$Campagne==1 & sgsdata$Compliant_FFQ ==1),(0.794), (sgsdata$compliance))
sgsdata$compliance <- ifelse((sgsdata$Campagne==1 & sgsdata$Compliant_FFQ !=1),(0.632), (sgsdata$compliance))


## Sous-ensemble des données pour Carnet
sgsdata_IT <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300" )

##VERIF des chiffres de la compliance avant application des filtres  
verif <- sgsdata_IT %>%
  filter(
    Traitement == 1,
    Campagne == 2,
    groupe == 1,
    Periode == 1
  ) %>%
  group_by(Campagne) %>%
  summarise(
    total = n_distinct(Identifiant),
    n_sup_0_7 = n_distinct(Identifiant[
      Compliant_booklet >= 0.7 &
        Prop_montant_theorique_saisie >= 0.3 &
        Prop_montant_theorique_saisie <= 1.7
      #utilisation2_FFQ == 1& limitation_FFQ == 1
    ]),
    ratio = n_sup_0_7 / total
  )


# Sélection des groupes pour le filtrage
ids_groupe0 <- sgsdata_IT %>% filter(groupe == 0, Periode == 1) %>% select(Identifiant) %>% distinct()
ids_groupe1_camp1 <- sgsdata_IT %>% 
  filter(groupe == 1, Campagne == 1, utilisation2_FFQ == 1, limitation_FFQ == 1, Periode == 1) %>% 
  select(Identifiant) %>% distinct()
ids_groupe1_camp2 <- sgsdata_IT %>% 
  filter(groupe == 1, Campagne == 2,
         Prop_montant_theorique_saisie >= 0.3, 
         Prop_montant_theorique_saisie <= 1.7,
         compliance >= 0.7, Periode == 1) %>% 
  select(Identifiant) %>% distinct()

ids_combines <- bind_rows(ids_groupe0, ids_groupe1_camp1, ids_groupe1_camp2)
sgsdata_TR <- sgsdata_IT %>% semi_join(ids_combines, by = "Identifiant")
48+57

sgsdata_LE <- subset(sgsdata_IT, voie_de_recrutement == "LE")



# Liste des filtres à appliquer
filter_conditions <- list(
  "Tous" = sgsdata_IT,
  "TR" = sgsdata_TR,
  "LE" = sgsdata_LE
)

# Initialisation d'un tableau vide pour stocker les résultats
final_results <- data.frame()

# Boucle sur les filtres
for (filter_name in names(filter_conditions)) {
  subset_data <- filter_conditions[[filter_name]]
  if (nrow(subset_data) > 0) {
    fixed_models <- list(
      "Prix tot eligible" = feols(M0 ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Produits éligibles [g/d/cu]" = feols(PVS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fruits / Vegetables [g/d/cu]" = feols(FV_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fruits [g/d/cu]"              = feols(FRUITS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data),
      "Dry fruits [g/d/cu]"          = feols(FRUITS_SECS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Nuts [g/d/cu]"                = feols(NOIX_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Vegetables [g/d/cu]"          = feols(LEGUMES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Legumes [g/d/cu]"             = feols(LEG_SECS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "prix unitaire eligible[€/kg]"       = feols(p0 ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "FV prix unitaire[€/kg] "       = feols(FV_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Unit price fruits [€/kg]"     = feols(FRUITS_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Unit price dry fruits [€/kg]" = feols(FRUITS_SECS_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Unit price nuts [€/kg]"       = feols(NOIX_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Unit price vegetables [€/kg]" = feols(LEGUMES_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Unit price legumes [€/kg]"    = feols(LEG_SECS_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Food budget [€/cu/month]"     = feols(depense_alim_uc ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Meat [g/d/cu]"                = feols(VIANDES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Red meat/Pork [g/d/cu]"       = feols(VIANDE_ROUGE_PORC_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Cold cuts [g/d/cu]"           = feols(AUTRE_PDTS_ANIMAUX_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Chicken/eggs [g/d/cu]"        = feols(POULET_OEUFS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fish [g/d/cu]"                = feols(POISSONS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Prepared meal [g/d/cu]"       = feols(PLATS_PREP_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Starchy food [g/d/cu]"        = feols(FEC_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Dairy products [g/d/cu]"      = feols(PDTS_LAITIERS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Added fats [g/d/cu]"          = feols(MG_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Discretionnary food [g/d/cu]"   = feols(PDTS_DISCRETIONNAIRES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Sugary sweet beverages [ml/d/cu]" = feols(SSB_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Alcohol [ml/d/cu]"            = feols(ALCOOL_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
         
      "MAR [% adequacy/ 2000 Kcal/d/cu]" = feols(MAR ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "MER [% excess/2000 Kcal/d/cu]"   = feols(MER ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Total caloric intake [Kcal/d/cu]" = feols(SOMME_POIDS_KCAL ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Solid energy density [kcal/100g/d/cu]" = feols(energy_densite ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      
      "Total weight without bev [Kg/cu]"= feols(SOMME_POIDS_HORS_BOISSON ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Carbon footprint [gCO2/Kg/d/cu]" = feols(climat_env ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "HENI"= feols(HENI_TOT ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Poids_eligible"= feols( q0 ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Prix_unitaire_eligible"= feols(p0 ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "White ham"= feols(JAMBON_BLANC_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Dairy dessert"= feols(DESSERTS_LACTES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Pork"= feols(PORC_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Water"= feols(EAU_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Cold cuts excluding white ham [€/Kg]"= feols(CHARCUTERIE_HORS_JB_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Sauces[€/Kg]"= feols(SAUCES_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Sugary sodas [€/Kg]"= feols(SODAS_SUCRES_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Salted snacks [€/Kg]"= feols(SNACKS_AUTRES_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fruit juice [€/Kg]"= feols(FRUITS_JUS_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Dairy product [€/Kg]"= feols(LAITAGES_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fish[€/Kg]"= feols(POISSONS_prix_kg ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Gaspillage restes"= feols( Si.noncombien.de.fois.la.semaine.derniere. ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Gapillage.produits.non.entames"= feols( Gapillage.produits.non.entames ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fresh fruits"= feols(FRUITS_Frais_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Canned fruits"= feols(FRUITS_Conserve_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Mashed fruits"= feols(FRUITS_Purée_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Frozen fruits"= feols(FRUITS_Surgelé_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fresh Vegetables"= feols(LEGUMES_Frais_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Canned Vegetables"= feols(LEGUMES_Conserve_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Soup vegetables"= feols(LEGUMES_Soupe_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Mashed vegetables"= feols(LEGUMES_Purée_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Frozen vegetables"= feols(LEGUMES_Surgelé_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Canned legumes"= feols(LEG_SECS_Conserve_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Prepared legumes"= feols(LEG_SECS_Préparation_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Dried legumes"= feols(LEG_SECS_Sec_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Soup legumes"= feols(LEG_SECS_Soupe_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant)
      ) 

    # Récupération des résultats sous forme de data.frame
    model_summary <- modelsummary(fixed_models, output = "data.frame", 
                                  gof_omit = "LogLik|Deviance|Adj.R2|FE|R2 Within|Std.Errors",
                                  statistic = c("std.error", "p.value"), fmt = "%.2g")
    # Ajout du nom du filtre
    model_summary$Selection <- filter_name
    final_results <- bind_rows(final_results, model_summary)
  }
}
# Nettoyage des résultats
terms_to_exclude <- c("Intercept", "Identifiant", "Temps", "Observations", "Std.Errors",
                      "ICC", "R2 Adj.", "R2 Cond.", "R2", "AIC", "BIC", "RMSE")
final_results <- final_results[!grepl(paste(terms_to_exclude, collapse = "|"), final_results$term), ]
final_results <- final_results %>% filter(part != "gof")
final_results <- final_results %>% 
  mutate(statistic = paste0(statistic, "_", Selection)) %>% 
  select(-part, -term, -Selection)

# Transposition et fusion de la première ligne en en-tête
model_summary <- as.data.frame(t(final_results))
new_header <- model_summary[1, ]
model_summary <- model_summary[-1, ]
colnames(model_summary) <- apply(new_header, 2, paste, collapse = " ")

# Création de la colonne Variable_Dépendante
dep_vars <- sapply(fixed_models, function(model) all.vars(formula(model))[1])
dep_vars_df <- data.frame(Variable_Dépendante = dep_vars, row.names = NULL)
if (nrow(model_summary) == length(dep_vars)) {
  model_summary <- cbind(dep_vars_df, model_summary)
} else {
  warning("Problème de correspondance entre model_summary et dep_vars_df")
}

# Nettoyage des valeurs et suppression du suffixe si présent
model_summary[] <- lapply(model_summary, function(x) { if(is.character(x)) gsub("[()]", "", x) else x })
model_summary$Variable_Dépendante <- sub("_diff$", "", model_summary$Variable_Dépendante)

# Calcul de la moyenne (Mean_T0) à partir des données de Periode 0
sgsdata0 <- subset(sgsdata_IT, Periode == 0)
existence_and_means <- sapply(model_summary$Variable_Dépendante, function(var) {
  if (var %in% colnames(sgsdata0)) {
    mean_value <- mean(sgsdata0[[var]], na.rm = TRUE)
    return(c("OK", mean_value))
  } else {
    return(c("", NA))
  }
})
model_summary$Mean_T0 <- as.numeric(existence_and_means[2, ])
model_summary <- model_summary %>% relocate(Mean_T0, .before = 1)

# Ajustement pour certaines variables (ex. POIDS) : multiplication par 1000
columns_to_scale <- c("Mean_T0", "estimate_Tous", "std.error_Tous", "estimate_TR", "std.error_TR", "estimate_LE", "std.error_LE")
rows_to_scale <- grepl("POIDS", model_summary[, 2]) & model_summary[, 2] != "SOMME_POIDS_KCAL"
for (col in columns_to_scale) {
  model_summary[[col]] <- as.numeric(model_summary[[col]])
}
model_summary[rows_to_scale, columns_to_scale] <- model_summary[rows_to_scale, columns_to_scale] * 1000

# Réorganisation et renommage des colonnes
model_summary$Variable_Dépendante <- NULL
colnames(model_summary) <- gsub("estimate", "Change (%)", colnames(model_summary))
colnames(model_summary) <- gsub("std.error", "Std.error (%)", colnames(model_summary))
numeric_cols <- grep("Chang|error", colnames(model_summary), value = TRUE)
for (col in numeric_cols) {
  model_summary[[col]] <- (as.numeric(model_summary[[col]]) / model_summary$Mean_T0) * 100
}
model_summary[] <- lapply(model_summary, function(x) { if(is.numeric(x)) format(x, scientific = FALSE) else x })

# Sauvegarde du tableau final pour l'analyse Carnet
models_carnets <- model_summary



ids_filtrés <- sgsdata_IT %>%
  filter(Periode == 0, Campagne==1, voie_de_recrutement=="Episourire" )
mean(ids_filtrés$FV_POIDS)
#0.3095044 (Epimut)
#0.3118102 (EPisourire )


## Analyse "FFQ"---------------------

# Sous-ensemble des données pour FFQ
ffqdata_IT <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "FFQ" & Identifiant != "LE300")

# Sélection des groupes pour FFQ
ids_groupe0 <- ffqdata_IT %>% filter(groupe == 0, Periode == 1) %>% select(Identifiant) %>% distinct()
ids_groupe1_camp1 <- ffqdata_IT %>% 
  filter(groupe == 1, Campagne == 1, utilisation2_FFQ == 1, limitation_FFQ == 1, Periode == 1) %>% 
  select(Identifiant) %>% distinct()
ids_groupe1_camp2 <- ffqdata_IT %>% 
  filter(groupe == 1, Campagne == 2,
         Prop_montant_theorique_saisie >= 0.3,
         Prop_montant_theorique_saisie <= 1.7,
         compliance >= 0.7, Periode == 1) %>% 
  select(Identifiant) %>% distinct()
ids_combines <- bind_rows(ids_groupe0, ids_groupe1_camp1, ids_groupe1_camp2)
ffqdata_TR <- ffqdata_IT %>% semi_join(ids_combines, by = "Identifiant")
ffqdata_LE <- subset(ffqdata_IT, voie_de_recrutement == "LE")

# Définition des filtres pour FFQ
filter_conditions <- list(
  "Tous" = ffqdata_IT,
  "TR" = ffqdata_TR,
  "LE" = ffqdata_LE
)



# Initialisation du tableau final pour FFQ
final_results <- data.frame()
for (filter_name in names(filter_conditions)) {
  subset_data <- filter_conditions[[filter_name]]
  
  if (nrow(subset_data) > 0) {
    fixed_models <- list(
      "Produits éligibles [g/d/cu]" = feols(PVS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fruits / Vegetables [g/d/cu]" = feols(FV_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fruits [g/d/cu]"              = feols(FRUITS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Dry fruits [g/d/cu]"          = feols(FRUITS_SECS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Nuts [g/d/cu]"                = feols(NOIX_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Vegetables [g/d/cu]"          = feols(LEGUMES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Legumes [g/d/cu]"             = feols(LEG_SECS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Meat [g/d/cu]"                = feols(VIANDES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Red meat/Pork [g/d/cu]"       = feols(VIANDE_ROUGE_PORC_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Cold cuts [g/d/cu]"           = feols(AUTRE_PDTS_ANIMAUX_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Chicken/eggs [g/d/cu]"        = feols(POULET_OEUFS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Fish [g/d/cu]"                = feols(POISSONS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Prepared meal [g/d/cu]"       = feols(PLATS_PREP_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Starchy food [g/d/cu]"        = feols(FEC_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Dairy products [g/d/cu]"      = feols(PDTS_LAITIERS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Added fats [g/d/cu]"          = feols(MG_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Discretionnary food [g/d/cu]"   = feols(PDTS_DISCRETIONNAIRES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Sugary sweet beverages [ml/d/cu]" = feols(SSB_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Alcohol [ml/d/cu]"            = feols(ALCOOL_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "MAR [% adequacy/ 2000 Kcal/d/cu]" = feols(MAR ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "MER [% excess/2000 Kcal/d/cu]"   = feols(MER ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Total caloric intake [Kcal/d/cu]" = feols(SOMME_POIDS_KCAL ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Solid energy density [kcal/100g/d/cu]" = feols(energy_densite ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "HENI" = feols(HENI_TOT ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Carbon footprint [gCO2/Kg/d/cu]" = feols(climat_env ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Gaspillage.restes." = feols(Si.noncombien.de.fois.la.semaine.derniere. ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Gapillage.produits.non.entames" = feols(Gapillage.produits.non.entames ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant)
    )
    model_summary <- modelsummary(fixed_models, output = "data.frame", 
                                  gof_omit = "LogLik|Deviance|Adj.R2|FE|R2 Within|Std.Errors",
                                  statistic = c("std.error", "p.value"), fmt = "%.2g")
    model_summary$Selection <- filter_name
    final_results <- bind_rows(final_results, model_summary)
  }
}

terms_to_exclude <- c("Intercept", "Identifiant", "Temps", "Observations", "Std.Errors", 
                      "ICC", "R2 Adj.", "R2 Cond.", "R2", "AIC", "BIC", "RMSE")
final_results <- final_results[!grepl(paste(terms_to_exclude, collapse = "|"), final_results$term), ]
final_results <- final_results %>% filter(part != "gof")
final_results <- final_results %>% 
  mutate(statistic = paste0(statistic, "_", Selection)) %>% 
  select(-part, -term, -Selection)

model_summary <- as.data.frame(t(final_results))
new_header <- model_summary[1, ]
model_summary <- model_summary[-1, ]
colnames(model_summary) <- apply(new_header, 2, paste, collapse = " ")

dep_vars <- sapply(fixed_models, function(model) all.vars(formula(model))[1])
dep_vars_df <- data.frame(Variable_Dépendante = dep_vars, row.names = NULL)
if (nrow(model_summary) == length(dep_vars)) {
  model_summary <- cbind(dep_vars_df, model_summary)
} else {
  warning("Problème de correspondance entre model_summary et dep_vars_df")
}
model_summary[] <- lapply(model_summary, function(x) { if(is.character(x)) gsub("[()]", "", x) else x })

ffqdata0 <- subset(ffqdata_IT, Periode == 0)
existence_and_means <- sapply(model_summary$Variable_Dépendante, function(var) {
  if (var %in% colnames(ffqdata0)) {
    mean_value <- mean(ffqdata0[[var]], na.rm = TRUE)
    return(c("OK", mean_value))
  } else {
    return(c("", NA))
  }
})
model_summary$Mean_T0 <- as.numeric(existence_and_means[2, ])
model_summary <- model_summary %>% relocate(Mean_T0, .before = 1)

columns_to_scale <- c("Mean_T0", "estimate_Tous", "std.error_Tous", "estimate_TR", "std.error_TR", "estimate_LE", "std.error_LE")
rows_to_scale <- grepl("POIDS", model_summary[, 2]) & model_summary[, 2] != "SOMME_POIDS_KCAL"
for (col in columns_to_scale) {
  model_summary[[col]] <- as.numeric(model_summary[[col]])
}
model_summary[rows_to_scale, columns_to_scale] <- model_summary[rows_to_scale, columns_to_scale] * 1000

model_summary$Variable_Dépendante <- NULL
colnames(model_summary) <- gsub("estimate", "Change (%)", colnames(model_summary))
colnames(model_summary) <- gsub("std.error", "Std.error (%)", colnames(model_summary))
numeric_cols <- grep("Chang|error", colnames(model_summary), value = TRUE)
for (col in numeric_cols) {
  model_summary[[col]] <-((as.numeric(model_summary[[col]]) / model_summary$Mean_T0) * 100)
}
model_summary[] <- lapply(model_summary, function(x) { if(is.numeric(x)) format(x, scientific = FALSE) else x })

# Sauvegarde du tableau final pour l'analyse FFQ
models_ffq <- model_summary

## Analyse "HENI"------------

# Sous-ensemble des données pour Carnet
sgsdata_IT <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300")

# Sélection des groupes pour le filtrage
ids_groupe0 <- sgsdata_IT %>% filter(groupe == 0, Periode == 1) %>% select(Identifiant) %>% distinct()
ids_groupe1_camp1 <- sgsdata_IT %>% 
  filter(groupe == 1, Campagne == 1, utilisation2_FFQ == 1, limitation_FFQ == 1, Periode == 1 ) %>% 
  select(Identifiant) %>% distinct()
ids_groupe1_camp2 <- sgsdata_IT %>% 
  filter(groupe == 1, Campagne == 2,
         Prop_montant_theorique_saisie >= 0.3, 
         Prop_montant_theorique_saisie <= 1.7,
         compliance >= 0.7, Periode == 1) %>% 
  select(Identifiant) %>% distinct()

ids_combines <- bind_rows(ids_groupe0, ids_groupe1_camp1, ids_groupe1_camp2)
sgsdata_TR <- sgsdata_IT %>% semi_join(ids_combines, by = "Identifiant")
sgsdata_LE <- subset(sgsdata_IT, voie_de_recrutement == "LE")

# Liste des filtres à appliquer
filter_conditions <- list(
  "Tous" = sgsdata_IT,
  "TR" = sgsdata_TR,
  "LE" = sgsdata_LE
)

# Initialisation d'un tableau vide pour stocker les résultats
final_results <- data.frame()

# Boucle sur les filtres
for (filter_name in names(filter_conditions)) {
  subset_data <- filter_conditions[[filter_name]]
  
  if (nrow(subset_data) > 0) {
    fixed_models <- list(
      "CAFE_THE"= feols(CAFE_THE_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "CEREALES_PD"= feols(CEREALES_PD_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "CHARCUTERIE_HORS_JB"= feols(CHARCUTERIE_HORS_JB_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "DESSERTS_LACTES"= feols(DESSERTS_LACTES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "FEC_NON_RAF"= feols(FEC_NON_RAF_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "FEC_RAF"= feols(FEC_RAF_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "FROMAGES"= feols(FROMAGES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "FRUITS"              = feols(FRUITS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "FRUITS_JUS"= feols(FRUITS_JUS_POIDS  ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "FRUITS_SECS"          = feols(FRUITS_SECS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "JAMBON_BLANC"= feols(JAMBON_BLANC_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "LAIT"= feols(LAIT_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "LAITAGES"= feols(LAITAGES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "LEG_SECS"             = feols(LEG_SECS_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "LEGUMES"          = feols(LEGUMES_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "MGA" = feols(MGA_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "MGV"= feols(MGV_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "NOIX"                = feols(NOIX_POIDS ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "OEUFS"= feols(OEUFS_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "PDTS_SUCRES"= feols(PDTS_SUCRES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "PLATS_PREP_CARNES"= feols(PLATS_PREP_CARNES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "PLATS_PREP_VEGETARIENS"= feols(PLATS_PREP_VEGETARIENS_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "POISSONS"=feols(POISSONS_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "PORC"= feols(PORC_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "POULET"= feols(POULET_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "QUICHES_PIZZAS_TARTES_SALEES"= feols(QUICHES_PIZZAS_TARTES_SALEES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "SAUCES"= feols(SAUCES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "SNACKS_AUTRES"= feols(SNACKS_AUTRES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "SODAS_LIGHT"= feols(SODAS_LIGHT_POIDS  ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "SODAS_SUCRES"=  feols(SODAS_SUCRES_POIDS ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "VIANDE_ROUGE"= feols(VIANDE_ROUGE_POIDS  ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant),
      "Climat_env "= feols(climat_env  ~  Traitement |  Identifiant + Periode ,data = subset_data, vcov = ~Identifiant)
      )
    
    # Récupération des résultats sous forme de data.frame
    model_summary <- modelsummary(fixed_models, output = "data.frame", 
                                  gof_omit = "LogLik|Deviance|Adj.R2|FE|R2 Within|Std.Errors",
                                  statistic = c("std.error", "p.value"), fmt = "%.2g")
    # Ajout du nom du filtre
    model_summary$Selection <- filter_name
    final_results <- bind_rows(final_results, model_summary)
  }
}

# Nettoyage des résultats
terms_to_exclude <- c("Intercept", "Identifiant", "Temps", "Observations", "Std.Errors",
                      "ICC", "R2 Adj.", "R2 Cond.", "R2", "AIC", "BIC", "RMSE")
final_results <- final_results[!grepl(paste(terms_to_exclude, collapse = "|"), final_results$term), ]
final_results <- final_results %>% filter(part != "gof")
final_results <- final_results %>% 
  mutate(statistic = paste0(statistic, "_", Selection)) %>% 
  select(-part, -term, -Selection)

# Transposition et fusion de la première ligne en en-tête
model_summary <- as.data.frame(t(final_results))
new_header <- model_summary[1, ]
model_summary <- model_summary[-1, ]
colnames(model_summary) <- apply(new_header, 2, paste, collapse = " ")


model_summary$std.error_Tous <- NULL
model_summary$p.value_Tous <- NULL
model_summary$std.error_TR <- NULL
model_summary$p.value_TR <- NULL
model_summary$std.error_LE <- NULL
model_summary$p.value_LE <- NULL
# Multiplier par 1000 toutes les valeurs sauf celles où Categorie vaut "Climat env"

model_summary <- rownames_to_column(model_summary, var = "variable")
num_cols <- sapply(model_summary, is.numeric)

resultat <- CALNUT %>%
  filter(!is.na(Classif_HENI)) %>%      # On garde uniquement les lignes où Classif_HENI est "TI"
  group_by(groupe_TI_TdC) %>%           # On groupe par la colonne groupe_TI_TdC
  summarise(moyenne_HENI = mean(HENI, na.rm = TRUE))

resultat<- resultat %>%
  rename(variable = groupe_TI_TdC)

resultat$moyenne_HENI <- resultat$moyenne_HENI * 1000

model_summary <- model_summary %>%
  left_join(resultat, by = "variable")

model_summary$HENI_TOUS <- as.numeric(model_summary$moyenne_HENI) * as.numeric(model_summary$estimate_Tous, vcov = ~Identifiant)
model_summary$HENI_TR <- as.numeric(model_summary$moyenne_HENI) * as.numeric(model_summary$estimate_TR, vcov = ~Identifiant)
model_summary$HENI_LE <- as.numeric(model_summary$moyenne_HENI) * as.numeric(model_summary$estimate_LE, vcov = ~Identifiant)


# Sauvegarde du tableau final pour l'analyse Carnet
models_carnets_HENI <- model_summary


## Affichage des résultats finaux--------------------
print("Résultats - Carnet")
print(models_carnets)
print("Résultats - FFQ")
print(models_ffq)




##D-COHEN CARNET-------------------------------------------------------

# Liste des modèles et des variables
calculate_d_cohen <- function(models, data) {
  # Chargement du package nécessaire
  library(fixest)
  
  # Initialisation des résultats
  d_cohen_results <- list()
  
  # Boucle sur chaque modèle
  for (model_name in names(models)) {
    variable <- models[[model_name]]
    
    # Vérifier si la variable existe dans le jeu de données
    if (!(variable %in% names(data))) {
      warning(paste("La variable", variable, "n'existe pas dans le jeu de données."))
      d_cohen_results[[model_name]] <- NA
      next
    }
    
    # Ajuster le modèle
    model <- feols(as.formula(paste0(variable, " ~ as.factor(Traitement) | Identifiant + Periode")), data = data)
    
    # Extraire le coefficient associé au traitement
    beta_3 <- coef(model)["as.factor(Traitement)1"]
    
    # Calcul des variances et tailles des sous-groupes
    control_pre <- data[[variable]][data$groupe == 0 & data$Periode == 0]
    control_post <- data[[variable]][data$groupe == 0 & data$Periode == 1]
    treated_pre <- data[[variable]][data$groupe == 1 & data$Periode == 0]
    treated_post <- data[[variable]][data$groupe == 1 & data$Periode == 1]
    
    var_control_pre <- var(control_pre, na.rm = TRUE)
    var_control_post <- var(control_post, na.rm = TRUE)
    var_treated_pre <- var(treated_pre, na.rm = TRUE)
    var_treated_post <- var(treated_post, na.rm = TRUE)
    
    n_control_pre <- sum(!is.na(control_pre))
    n_control_post <- sum(!is.na(control_post))
    n_treated_pre <- sum(!is.na(treated_pre))
    n_treated_post <- sum(!is.na(treated_post))
    
    # Calcul de l'écart-type poolé
    pooled_sd <- sqrt(
      ((n_control_pre - 1) * var_control_pre +
         (n_control_post - 1) * var_control_post +
         (n_treated_pre - 1) * var_treated_pre +
         (n_treated_post - 1) * var_treated_post) /
        (n_control_pre + n_control_post + n_treated_pre + n_treated_post - 4)
    )
    
    # Calcul du d de Cohen
    d_cohen <- beta_3 / pooled_sd
    
    # Stocker le résultat
    d_cohen_results[[model_name]] <- d_cohen
  }
  
  # Retourner les résultats sous forme de data.frame
  return(data.frame(
    Model = names(d_cohen_results),
    D_Cohen = unlist(d_cohen_results)
  ))
}

models <- list(
  
  
  "Prix tot eligible" = "M0",
  "prix unitaire eligible[€/kg]"  = "p0",   
  "Produits éligibles [g/d/cu]" = "PVS",
  
  "Fruits / Vegetables (Kg/d/Uc)" = "FV_POIDS",
  "Fruits (Kg/d/Uc)" = "FRUITS_POIDS",
  "Fruits secs (Kg/d/Uc)" = "FRUITS_SECS_POIDS",
  "Noix (Kg/d/Uc)" = "NOIX_POIDS", 
  "Legumes (Kg/d/Uc)" = "LEG_SECS_POIDS",
  "Vegetables (Kg/d/Uc)" = "LEGUMES_POIDS",
  "Unit prix FV" = "FV_prix_kg", 
  "Unit price fruits (€/kg)" = "FRUITS_prix_kg",
  "Unit price Vegetables (€/kg)" = "LEGUMES_prix_kg",
  "Unit price dry fruit (€/kg)" = "FRUITS_SECS_prix_kg",
  "Unit price nuts (€/kg)" = "NOIX_prix_kg",
  "Unit price legumes (€/kg)" = "LEG_SECS_prix_kg",
  "Meat [Kg/d]" = "VIANDES_POIDS",
  "Red meat/Pork [Kg]" = "VIANDE_ROUGE_PORC_POIDS",
  "Cold cuts [Kg/d]" = "AUTRE_PDTS_ANIMAUX_POIDS",
  "Chicken/eggs [Kg/d]" = "POULET_OEUFS_POIDS",
  "Fish [Kg/d]" = "POISSONS_POIDS",
  "Prepared meal [Kg/d]" = "PLATS_PREP_POIDS",
  "Starchy food [Kg/d]" = "FEC_POIDS",
  "Dairy products [Kg/d]" = "PDTS_LAITIERS_POIDS",
  "Added fats [Kg/d]" = "MG_POIDS",
  "Discretionnary food [Kg/d]" = "PDTS_DISCRETIONNAIRES_POIDS",
  "Sugary sweet beverages [l/d]" = "SSB_POIDS",
  "Alcohol [l/d]" = "ALCOOL_POIDS",
  "MAR (%)" = "MAR",
  "MER (%)" = "MER",
  "HENI" = "HENI_TOT",
  "Apport calorique total" = "SOMME_POIDS_KCAL",
  "Energy_density " = "energy_densite",
  "Food spending" = "depense_alim_uc",
  "Carbon footprint (g/CO2/UC)" = "climat_env"
)

# Appeler la fonction
d_cohen_values <- calculate_d_cohen(models, sgsdata_IT)
d_cohen_values <- calculate_d_cohen(models, sgsdata_TR)
d_cohen_values <- calculate_d_cohen(models, sgsdata_LE)

# Initialisation d'un tableau vide pour stocker les résultats
cohen_results <- data.frame(Model = names(models))

# Boucle sur chaque sous-échantillon et ajout des résultats dans des colonnes distinctes
for (filter_name in c("Tous", "TR", "LE")) {
  subset_data <- switch(filter_name,
                        "Tous" = sgsdata_IT,
                        "TR" = sgsdata_TR,
                        "LE" = sgsdata_LE)
  
  if (nrow(subset_data) > 0) {
    d_cohen_values <- calculate_d_cohen(models, subset_data)
    
    # Renommer la colonne des résultats en fonction de la sélection
    colnames(d_cohen_values)[2] <- filter_name  # "D_Cohen" devient "Tous", "TR" ou "LE"
    
    # Fusionner les résultats dans le tableau final
    cohen_results <- left_join(cohen_results, d_cohen_values, by = "Model")
  }
}



# RESULTATS SECONDAIRES  -----------------------------
# Calcul des différences (différences entre Periode == "1" et Periode == "0")
sgsdata_INT <- sgsdata_IT %>%
  group_by(Identifiant) %>%
  mutate(
    M0_diff =  M0[Periode == "1"] - M0[Periode == "0"],
    p0_diff =  p0[Periode == "1"] - p0[Periode == "0"],
    PVS_POIDS_diff = PVS[Periode == "1"] - PVS[Periode == "0"],
    FV_POIDS_diff = FV_POIDS[Periode == "1"] - FV_POIDS[Periode == "0"],
    FRUITS_POIDS_diff = FRUITS_POIDS[Periode == "1"] - FRUITS_POIDS[Periode == "0"],
    FRUITS_SECS_POIDS_diff = FRUITS_SECS_POIDS[Periode == "1"] - FRUITS_SECS_POIDS[Periode == "0"],
    NOIX_POIDS_diff = NOIX_POIDS[Periode == "1"] - NOIX_POIDS[Periode == "0"],
    LEGUMES_POIDS_diff = LEGUMES_POIDS[Periode == "1"] - LEGUMES_POIDS[Periode == "0"],
    LEG_SECS_POIDS_diff = LEG_SECS_POIDS[Periode == "1"] - LEG_SECS_POIDS[Periode == "0"],
    FV_prix_kg_diff = FV_prix_kg[Periode == "1"] - FV_prix_kg[Periode == "0"],
    FRUITS_prix_kg_diff = FRUITS_prix_kg[Periode == "1"] - FRUITS_prix_kg[Periode == "0"],
    LEGUMES_prix_kg_diff = LEGUMES_prix_kg[Periode == "1"] - LEGUMES_prix_kg[Periode == "0"],
    FRUITS_SECSS_prix_kg_diff = FRUITS_SECS_prix_kg[Periode == "1"] - FRUITS_SECS_prix_kg[Periode == "0"],
    NOIXS_prix_kg_diff = NOIX_prix_kg[Periode == "1"] - NOIX_prix_kg[Periode == "0"],
    LEG_SECS_prix_kg_diff = LEG_SECS_prix_kg[Periode == "1"] - LEG_SECS_prix_kg[Periode == "0"],
    VIANDES_POIDS_diff = VIANDES_POIDS[Periode == "1"] - VIANDES_POIDS[Periode == "0"],
    VIANDE_ROUGE_PORC_POIDS_diff = VIANDE_ROUGE_PORC_POIDS[Periode == "1"] - VIANDE_ROUGE_PORC_POIDS[Periode == "0"],
    AUTRE_PDTS_ANIMAUX_POIDS_diff = AUTRE_PDTS_ANIMAUX_POIDS[Periode == "1"] - AUTRE_PDTS_ANIMAUX_POIDS[Periode == "0"],
    POULET_OEUFS_POIDS_diff = POULET_OEUFS_POIDS[Periode == "1"] - POULET_OEUFS_POIDS[Periode == "0"],
    POISSONS_POIDS_diff = POISSONS_POIDS[Periode == "1"] - POISSONS_POIDS[Periode == "0"],
    POISSONS_prix_kg_diff = POISSONS_prix_kg[Periode == "1"] - POISSONS_prix_kg[Periode == "0"],
    FEC_POIDS_diff = FEC_POIDS[Periode == "1"] - FEC_POIDS[Periode == "0"],
    PDTS_LAITIERS_POIDS_diff = PDTS_LAITIERS_POIDS[Periode == "1"] - PDTS_LAITIERS_POIDS[Periode == "0"],
    PDTS_DISCRETIONNAIRES_POIDS_diff = PDTS_DISCRETIONNAIRES_POIDS[Periode == "1"] - PDTS_DISCRETIONNAIRES_POIDS[Periode == "0"],
    PLATS_PREP_POIDS_diff = PLATS_PREP_POIDS[Periode == "1"] - PLATS_PREP_POIDS[Periode == "0"],
    MG_POIDS_diff = MG_POIDS[Periode == "1"] - MG_POIDS[Periode == "0"],
    SSB_POIDS_diff = SSB_POIDS[Periode == "1"] - SSB_POIDS[Periode == "0"],
    ALCOOL_POIDS_diff = ALCOOL_POIDS[Periode == "1"] - ALCOOL_POIDS[Periode == "0"],
    depense_alim_uc_diff = depense_alim_uc[Periode == "1"] - depense_alim_uc[Periode == "0"],
    MER_diff = MER[Periode == "1"] - MER[Periode == "0"],
    MAR_diff = MAR[Periode == "1"] - MAR[Periode == "0"],
    energy_densite_diff = energy_densite[Periode == "1"] - energy_densite[Periode == "0"],
    SOMME_POIDS_KCAL_diff = SOMME_POIDS_KCAL[Periode == "1"] - SOMME_POIDS_KCAL[Periode == "0"],
    climat_env_diff = climat_env[Periode == "1"] - climat_env[Periode == "0"],
    CAFE_THE_diff = CAFE_THE_POIDS[Periode == "1"] - CAFE_THE_POIDS[Periode == "0"],
    CEREALES_PD_POIDS_diff = CEREALES_PD_POIDS[Periode == "1"] - CEREALES_PD_POIDS[Periode == "0"],
    CHARCUTERIE_HORS_JB_POIDS_diff = CHARCUTERIE_HORS_JB_POIDS[Periode == "1"] - CHARCUTERIE_HORS_JB_POIDS[Periode == "0"],
    DESSERTS_LACTES_POIDS_diff = DESSERTS_LACTES_POIDS[Periode == "1"] - DESSERTS_LACTES_POIDS[Periode == "0"],
    FEC_NON_RAF_POIDS_diff = FEC_NON_RAF_POIDS[Periode == "1"] - FEC_NON_RAF_POIDS[Periode == "0"],
    FEC_RAF_POIDS_diff = FEC_RAF_POIDS[Periode == "1"] - FEC_RAF_POIDS[Periode == "0"],
    FROMAGES_POIDS_diff = FROMAGES_POIDS[Periode == "1"] - FROMAGES_POIDS[Periode == "0"],
    FRUITS_JUS_POIDS_diff = FRUITS_JUS_POIDS[Periode == "1"] - FRUITS_JUS_POIDS[Periode == "0"],
    JAMBON_BLANC_POIDS_diff = JAMBON_BLANC_POIDS[Periode == "1"] - JAMBON_BLANC_POIDS[Periode == "0"],
    LAIT_POIDS_diff = LAIT_POIDS[Periode == "1"] - LAIT_POIDS[Periode == "0"],
    LAITAGES_POIDS_diff = LAITAGES_POIDS[Periode == "1"] - LAITAGES_POIDS[Periode == "0"],
    MGA_POIDS_diff = MGA_POIDS[Periode == "1"] - MGA_POIDS[Periode == "0"],
    MGV_POIDS_diff = MGV_POIDS[Periode == "1"] - MGV_POIDS[Periode == "0"],
    OEUFS_POIDS_diff = OEUFS_POIDS[Periode == "1"] - OEUFS_POIDS[Periode == "0"],
    PDTS_SUCRES_POIDS_diff = PDTS_SUCRES_POIDS[Periode == "1"] - PDTS_SUCRES_POIDS[Periode == "0"],
    PLATS_PREP_CARNES_POIDS_diff = PLATS_PREP_CARNES_POIDS[Periode == "1"] - PLATS_PREP_CARNES_POIDS[Periode == "0"],
    PLATS_PREP_VEGETARIENS_POIDS_diff = PLATS_PREP_VEGETARIENS_POIDS[Periode == "1"] - PLATS_PREP_VEGETARIENS_POIDS[Periode == "0"],
    POISSONS_POIDS_diff = POISSONS_POIDS[Periode == "1"] - POISSONS_POIDS[Periode == "0"],
    PORC_POIDS_diff = PORC_POIDS[Periode == "1"] - PORC_POIDS[Periode == "0"],
    POULET_POIDS_diff = POULET_POIDS[Periode == "1"] - POULET_POIDS[Periode == "0"],
    QUICHES_PIZZAS_TARTES_SALEES_POIDS_diff = QUICHES_PIZZAS_TARTES_SALEES_POIDS[Periode == "1"] - QUICHES_PIZZAS_TARTES_SALEES_POIDS[Periode == "0"],
    SAUCES_POIDS_diff = SAUCES_POIDS[Periode == "1"] - SAUCES_POIDS[Periode == "0"],
    SNACKS_AUTRES_POIDS_diff = SNACKS_AUTRES_POIDS[Periode == "1"] - SNACKS_AUTRES_POIDS[Periode == "0"],
    SODAS_SUCRES_POIDS_diff = SODAS_SUCRES_POIDS[Periode == "1"] - SODAS_SUCRES_POIDS[Periode == "0"],
    SODAS_LIGHT_POIDS_diff = SODAS_LIGHT_POIDS[Periode == "1"] - SODAS_LIGHT_POIDS[Periode == "0"],
    VIANDE_ROUGE_POIDS_diff = VIANDE_ROUGE_POIDS[Periode == "1"] - VIANDE_ROUGE_POIDS[Periode == "0"],
    FRUITS_SECS_prix_kg_diff = FRUITS_SECS_prix_kg[Periode == "1"] - FRUITS_SECS_prix_kg[Periode == "0"],
    NOIX_prix_kg_diff = NOIX_prix_kg[Periode == "1"] - NOIX_prix_kg[Periode == "0"],
    FRUITS_SECS_POIDS_diff = FRUITS_SECS_POIDS[Periode == "1"] - FRUITS_SECS_POIDS[Periode == "0"],
    NOIX_POIDS_diff = NOIX_POIDS[Periode == "1"] - NOIX_POIDS[Periode == "0"],
    SOMME_POIDS_HORS_BOISSON_diff = SOMME_POIDS_HORS_BOISSON[Periode == "1"] - SOMME_POIDS_HORS_BOISSON[Periode == "0"],
    SODAS_LIGHT_POIDS_diff = SODAS_LIGHT_POIDS[Periode == "1"] - SODAS_LIGHT_POIDS[Periode == "0"],
    EAU_POIDS_diff =  EAU_POIDS[Periode == "1"] - EAU_POIDS[Periode == "0"],
    SODAS_SUCRES_prix_kg_diff = SODAS_SUCRES_prix_kg[Periode == "1"] - SODAS_SUCRES_prix_kg[Periode == "0"],
    CHARCUTERIE_HORS_JB_prix_kg_diff = CHARCUTERIE_HORS_JB_prix_kg[Periode == "1"] - CHARCUTERIE_HORS_JB_prix_kg[Periode == "0"],
    SNACKS_AUTRES_prix_kg_diff =  SNACKS_AUTRES_prix_kg[Periode == "1"] -  SNACKS_AUTRES_prix_kg[Periode == "0"],
    POISSONS_prix_kg_diff = POISSONS_prix_kg[Periode == "1"] - POISSONS_prix_kg[Periode == "0"],
    PORC_prix_kg_diff =PORC_prix_kg[Periode == "1"] - PORC_prix_kg[Periode == "0"],
    FROMAGES_prix_kg_diff = FROMAGES_prix_kg[Periode == "1"] - FROMAGES_prix_kg[Periode == "0"],
    FEC_NON_RAF_prix_kg_diff= FEC_NON_RAF_prix_kg[Periode == "1"] - FEC_NON_RAF_prix_kg[Periode == "0"]
  ) %>%
  ungroup()

# Pour les analyses en diff, on conserve uniquement les observations de Periode == 0
sgsdata_IT_filtered <- sgsdata_INT %>% filter(Periode == 0)

## Analyse d'hétérogénéité - Interaction avec Différentiel_aide-------------------------------

# Liste des filtres à appliquer
filter_conditions <- list(
  "Tous" = sgsdata_IT,
  "TR" = sgsdata_TR,
  "LE" = sgsdata_LE
)

filter_conditions <- list("Tous" = sgsdata_IT_filtered)
final_results <- data.frame()
for (filter_name in names(filter_conditions)) {
  subset_data <- filter_conditions[[filter_name]]
  if(nrow(subset_data) > 0) {
    fixed_models <- list(
      "M0" = feols(M0_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Eligibles [g/d/cu]" = feols(PVS_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits / Vegetables [g/d/cu]" = feols(FV_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits [g/d/cu]"              = feols(FRUITS_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits secs[g/d/cu]"              = feols(FRUITS_SECS_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Noix [g/d/cu]"              = feols(NOIX_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Vegetables [g/d/cu]"          = feols(LEGUMES_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Legumes [g/d/cu]"             = feols(LEG_SECS_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "FV [€/Kg]"                 = feols(FV_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits [€/Kg]"                 = feols(FRUITS_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits secs[€/Kg]"              = feols(FRUITS_SECS_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Noix [€/Kg]"              = feols(NOIX_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      
      "Vegetable [€/Kg]"              =  feols(LEGUMES_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Legumes [€/Kg]"                = feols(LEG_SECS_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Meat [g/d/cu]"                = feols(VIANDES_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Red meat/Pork [g/d/cu]"       = feols(VIANDE_ROUGE_PORC_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cold cuts [g/d/cu]"           = feols(AUTRE_PDTS_ANIMAUX_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Chicken/eggs [g/d/cu]"        = feols(POULET_OEUFS_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fish [g/d/cu]"                = feols(POISSONS_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Prepared meal [g/d/cu]"       = feols(PLATS_PREP_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Starchy food [g/d/cu]"        = feols(FEC_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Dairy products [g/d/cu]"      = feols(PDTS_LAITIERS_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Added fats [g/d/cu]"          = feols(MG_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Discretionnary food [g/d/cu]"   = feols(PDTS_DISCRETIONNAIRES_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Sugary sweet beverages [ml/d/cu]" = feols(SSB_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Alcohol [ml/d/cu]"            = feols(ALCOOL_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "MAR [% adequacy/ 2000 Kcal/d/cu]" = feols(MAR_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "MER [% excess/2000 Kcal/d/cu]"   = feols(MER_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Total caloric intake [Kcal/d/cu]" = feols(SOMME_POIDS_KCAL_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Solid energy density [kcal/100g/d/cu]" = feols(energy_densite_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Total weight [Kg/cu]" = feols(SOMME_POIDS_HORS_BOISSON_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Food budget [€/CU]" = feols(depense_alim_uc_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      " climat_env_diff" = feols( climat_env_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      
      "Light sodas [gCO2/Kg/d/cu]" = feols(SODAS_LIGHT_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Water [L/d/cu]" = feols(EAU_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Sugary sodas [K/d/cu]" = feols(SODAS_SUCRES_POIDS_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cold cuts excluding white ham [€/Kg]" = feols(CHARCUTERIE_HORS_JB_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Other snacks [€/kg]" = feols(SNACKS_AUTRES_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fish [€/Kg]" = feols(POISSONS_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Pork [€/Kg]" = feols(PORC_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cheeses [€/kg]" = feols(FROMAGES_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Unrefined starches [€/kg]" = feols(FEC_NON_RAF_prix_kg_diff ~ as.factor(groupe) + Différentiel_aide + as.factor(groupe):Différentiel_aide |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant)
      
      
    ) 

    
    
    
    model_summary <- modelsummary(fixed_models, output = "data.frame", 
                                  gof_omit = "LogLik|Deviance|Adj.R2|FE|R2 Within|Std.Errors",
                                  statistic = c("std.error", "p.value"), fmt = "%.2g")
    model_summary$Selection <- filter_name
    final_results <- bind_rows(final_results, model_summary)
  }
}

# Nettoyage des résultats
terms_to_exclude <- c("Intercept", "Identifiant", "Temps", "Observations", "Std.Errors",
                      "ICC", "R2 Adj.", "R2 Cond.", "R2", "AIC", "BIC", "RMSE")
final_results <- final_results[!grepl(paste(terms_to_exclude, collapse = "|"), final_results$term), ]
final_results <- final_results %>% filter(part != "gof") %>% select(-part, -Selection)

# Transposition et création du nouvel en-tête
model_summary_diff <- as.data.frame(t(final_results))
new_header <- paste(model_summary_diff[1, ], model_summary_diff[2, ], sep = "_")
colnames(model_summary_diff) <- new_header
model_summary_diff <- model_summary_diff[-c(1,2), ]

# Ajout de la colonne Variable_Dépendante
dep_vars <- sapply(fixed_models, function(model) all.vars(formula(model))[1])
dep_vars_df <- data.frame(Variable_Dépendante = dep_vars, row.names = NULL)
if(nrow(model_summary_diff) == length(dep_vars)) {
  model_summary_diff <- cbind(dep_vars_df, model_summary_diff)
} else {
  warning("Problème de correspondance entre model_summary_diff et dep_vars_df")
}
model_summary_diff[] <- lapply(model_summary_diff, function(x) if(is.character(x)) gsub("[()]", "", x) else x)
model_summary_diff$Variable_Dépendante <- sub("_diff$", "", model_summary_diff$Variable_Dépendante)

## Calcul de Mean_T0 à partir des données de Periode 0
#sgsdata0 <- subset(sgsdata_IT, Periode == 1, groupe ==0)
#existence_and_means <- sapply(model_summary_diff$Variable_Dépendante, function(var) {
#  if(var %in% colnames(sgsdata0)) {
#    mean_value <- mean(sgsdata0[[var]], na.rm = TRUE)
#    return(c("OK", mean_value))
#  } else {
#    return(c("", NA))
#  }
#})
#model_summary_diff$Mean_T0 <- as.numeric(existence_and_means[2, ])
#model_summary_diff <- model_summary_diff %>% relocate(Mean_T0, .before = 1)
# Ajustement des coefficients pour certaines variables (multiplication par 1000 pour POIDS)
#columns_to_scale <- c("Mean_T0", "as.factor(groupe)1_estimate", "as.factor(groupe)1_std.error", 
#                      
#                      "Différentiel_aide_estimate" , "Différentiel_aide_std.error", "as.factor(groupe)1 × Différentiel_aide_estimate", "as.factor(groupe)1 × Différentiel_aide_std.error")
#rows_to_scale <- grepl("POIDS", model_summary_diff[, 2]) & model_summary_diff[, 2] != "SOMME_POIDS_KCAL"
#for(col in columns_to_scale) {
#  model_summary_diff[[col]] <- as.numeric(model_summary_diff[[col]])
#}
#model_summary_diff[rows_to_scale, columns_to_scale] <- model_summary_diff[rows_to_scale, columns_to_scale] * 1000
#
## Remplacement dans les noms de colonnes
#model_summary_diff$Variable_Dépendante <- NULL
#colnames(model_summary_diff) <- gsub("estimate", "Change (%)", colnames(model_summary_diff))
#colnames(model_summary_diff) <- gsub("std.error", "Std.error (%)", colnames(model_summary_diff))
#numeric_cols <- grep("Chang|error", colnames(model_summary_diff), value = TRUE)
#for(col in numeric_cols) {
#  model_summary_diff[[col]] <- (as.numeric(model_summary_diff[[col]]) / model_summary_diff$Mean_T0) * 100
#}
#model_summary_diff[] <- lapply(model_summary_diff, function(x) if(is.numeric(x)) format(x, scientific = FALSE) else x)

models_diff <- model_summary_diff
#TEST

## Analyse d'hétérogénéité - Interaction avec Income_UC_INSEE--------------------
filter_conditions <- list("Tous" = sgsdata_IT_filtered)
final_results <- data.frame()
for (filter_name in names(filter_conditions)) {
  subset_data <- filter_conditions[[filter_name]]
  if(nrow(subset_data) > 0) {
    fixed_models <- list(
      "M0" = feols(M0_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Eligibles [g/d/cu]" = feols(PVS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits / Vegetables [g/d/cu]" = feols(FV_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits [g/d/cu]"              = feols(FRUITS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits secs[g/d/cu]"              = feols(FRUITS_SECS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Noix [g/d/cu]"              = feols(NOIX_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Vegetables [g/d/cu]"          = feols(LEGUMES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Legumes [g/d/cu]"             = feols(LEG_SECS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "FV [€/Kg]"                 = feols(FV_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits [€/Kg]"                 = feols(FRUITS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits secs[€/Kg]"              = feols(FRUITS_SECS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Noix [€/Kg]"              = feols(NOIX_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      
      "Vegetable [€/Kg]"              =  feols(LEGUMES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Legumes [€/Kg]"                = feols(LEG_SECS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Meat [g/d/cu]"                = feols(VIANDES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Red meat/Pork [g/d/cu]"       = feols(VIANDE_ROUGE_PORC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cold cuts [g/d/cu]"           = feols(AUTRE_PDTS_ANIMAUX_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Chicken/eggs [g/d/cu]"        = feols(POULET_OEUFS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fish [g/d/cu]"                = feols(POISSONS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Prepared meal [g/d/cu]"       = feols(PLATS_PREP_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Starchy food [g/d/cu]"        = feols(FEC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Dairy products [g/d/cu]"      = feols(PDTS_LAITIERS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Added fats [g/d/cu]"          = feols(MG_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Discretionnary food [g/d/cu]"   = feols(PDTS_DISCRETIONNAIRES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Sugary sweet beverages [ml/d/cu]" = feols(SSB_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Alcohol [ml/d/cu]"            = feols(ALCOOL_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "MAR [% adequacy/ 2000 Kcal/d/cu]" = feols(MAR_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "MER [% excess/2000 Kcal/d/cu]"   = feols(MER_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Total caloric intake [Kcal/d/cu]" = feols(SOMME_POIDS_KCAL_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Solid energy density [kcal/100g/d/cu]" = feols(energy_densite_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Total weight [Kg/cu]" = feols(SOMME_POIDS_HORS_BOISSON_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Food budget [€/CU]" = feols(depense_alim_uc_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "climat_env" = feols(climat_env_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      
      
      "Light sodas [gCO2/Kg/d/cu]" = feols(SODAS_LIGHT_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Water [L/d/cu]" = feols(EAU_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Sugary sodas [K/d/cu]" = feols(SODAS_SUCRES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cold cuts excluding white ham [€/Kg]" = feols(CHARCUTERIE_HORS_JB_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Other snacks [€/kg]" = feols(SNACKS_AUTRES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fish [€/Kg]" = feols(POISSONS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Pork [€/Kg]" = feols(PORC_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cheeses [€/kg]" = feols(FROMAGES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Unrefined starches [€/kg]" = feols(FEC_NON_RAF_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE |  as.factor(branche) , data = sgsdata_IT_filtered, vcov = ~Identifiant)
      
      
    ) 

    model_summary <- modelsummary(fixed_models, output = "data.frame", 
                                  gof_omit = "LogLik|Deviance|Adj.R2|FE|R2 Within|Std.Errors",
                                  statistic = c("std.error", "p.value"), fmt = "%.2g")
    model_summary$Selection <- filter_name
    final_results <- bind_rows(final_results, model_summary)
  }
}

terms_to_exclude <- c("Intercept", "Identifiant", "Temps", "Observations", "Std.Errors",
                      "ICC", "R2 Adj.", "R2 Cond.", "R2", "AIC", "BIC", "RMSE")
final_results <- final_results[!grepl(paste(terms_to_exclude, collapse = "|"), final_results$term), ]
final_results <- final_results %>% filter(part != "gof") %>% select(-part, -Selection)

model_summary_inc <- as.data.frame(t(final_results))
new_header <- paste(model_summary_inc[1, ], model_summary_inc[2, ], sep = "_")
colnames(model_summary_inc) <- new_header
model_summary_inc <- model_summary_inc[-c(1,2), ]

dep_vars <- sapply(fixed_models, function(model) all.vars(formula(model))[1])
dep_vars_df <- data.frame(Variable_Dépendante = dep_vars, row.names = NULL)
if(nrow(model_summary_inc) == length(dep_vars)) {
  model_summary_inc <- cbind(dep_vars_df, model_summary_inc)
} else {
  warning("Problème de correspondance entre model_summary_inc et dep_vars_df")
}
model_summary_inc[] <- lapply(model_summary_inc, function(x) { if(is.character(x)) gsub("[()]", "", x) else x })
model_summary_inc$Variable_Dépendante <- sub("_diff$", "", model_summary_inc$Variable_Dépendante)

#sgsdata0 <- subset(sgsdata_IT, Periode == 1, groupe ==0)
#existence_and_means <- sapply(model_summary_inc$Variable_Dépendante, function(var) {
#  if(var %in% colnames(sgsdata0)) {
#    mean_value <- median(sgsdata0[[var]], na.rm = TRUE)
#    return(c("OK", mean_value))
#  } else {
#    return(c("", NA))
#  }
#})
#model_summary_inc$Mean_T0 <- as.numeric(existence_and_means[2, ])
#model_summary_inc <- model_summary_inc %>% relocate(Mean_T0, .before = 1)
#
#colnames(model_summary_inc)
#columns_to_scale <- c("Mean_T0",  "as.factor(groupe)1_estimate"   , "as.factor(groupe)1_std.error"  ,                         
#                      "Income_UC_INSEE_estimate" ,                             
#                      "Income_UC_INSEE_std.error" ,
#                      "as.factor(groupe)1 × Income_UC_INSEE_estimate" , 
#                      "as.factor(groupe)1 × Income_UC_INSEE_std.error")
#rows_to_scale <- grepl("POIDS", model_summary_inc[, 2]) & model_summary_inc[, 2] != "SOMME_POIDS_KCAL"
#for(col in columns_to_scale) {
#  model_summary_inc[[col]] <- as.numeric(model_summary_inc[[col]])
#}
#model_summary_inc[rows_to_scale, columns_to_scale] <- model_summary_inc[rows_to_scale, columns_to_scale] * 1000
#
#model_summary_inc$Variable_Dépendante <- NULL
#colnames(model_summary_inc) <- gsub("estimate", "Change (%)", colnames(model_summary_inc))
#colnames(model_summary_inc) <- gsub("std.error", "Std.error (%)", colnames(model_summary_inc))
#numeric_cols <- grep("Chang|error", colnames(model_summary_inc), value = TRUE)
#for(col in numeric_cols) {
#  model_summary_inc[[col]] <-(as.numeric(model_summary_inc[[col]]) / model_summary_inc$Mean_T0) * 100
#}
#model_summary_inc[] <- lapply(model_summary_inc, function(x) { if(is.numeric(x)) format(x, scientific = FALSE) else x })


models_inc <- model_summary_inc





## Analyse d'hétérogénéité - Interaction avec Income_UC_INSEE--------------------
filter_conditions <- list("Tous" = sgsdata_IT_filtered)
final_results <- data.frame()
for (filter_name in names(filter_conditions)) {
  subset_data <- filter_conditions[[filter_name]]
  if(nrow(subset_data) > 0) {
    fixed_models <- list(
      "M0" = feols(M0_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Eligibles [g/d/cu]" = feols(PVS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits / Vegetables [g/d/cu]" = feols(FV_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits [g/d/cu]"              = feols(FRUITS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits secs[g/d/cu]"              = feols(FRUITS_SECS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Noix [g/d/cu]"              = feols(NOIX_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Vegetables [g/d/cu]"          = feols(LEGUMES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Legumes [g/d/cu]"             = feols(LEG_SECS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "FV [€/Kg]"                 = feols(FV_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits [€/Kg]"                 = feols(FRUITS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fruits secs[€/Kg]"              = feols(FRUITS_SECS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Noix [€/Kg]"              = feols(NOIX_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      
      "Vegetable [€/Kg]"              =  feols(LEGUMES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Legumes [€/Kg]"                = feols(LEG_SECS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Meat [g/d/cu]"                = feols(VIANDES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Red meat/Pork [g/d/cu]"       = feols(VIANDE_ROUGE_PORC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cold cuts [g/d/cu]"           = feols(AUTRE_PDTS_ANIMAUX_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Chicken/eggs [g/d/cu]"        = feols(POULET_OEUFS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fish [g/d/cu]"                = feols(POISSONS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Prepared meal [g/d/cu]"       = feols(PLATS_PREP_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Starchy food [g/d/cu]"        = feols(FEC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Dairy products [g/d/cu]"      = feols(PDTS_LAITIERS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Added fats [g/d/cu]"          = feols(MG_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Discretionnary food [g/d/cu]"   = feols(PDTS_DISCRETIONNAIRES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Sugary sweet beverages [ml/d/cu]" = feols(SSB_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Alcohol [ml/d/cu]"            = feols(ALCOOL_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "MAR [% adequacy/ 2000 Kcal/d/cu]" = feols(MAR_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "MER [% excess/2000 Kcal/d/cu]"   = feols(MER_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Total caloric intake [Kcal/d/cu]" = feols(SOMME_POIDS_KCAL_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Solid energy density [kcal/100g/d/cu]" = feols(energy_densite_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Total weight [Kg/cu]" = feols(SOMME_POIDS_HORS_BOISSON_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Food budget [€/CU]" = feols(depense_alim_uc_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "climat_env" = feols(climat_env_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
  
       "Light sodas [gCO2/Kg/d/cu]" = feols(SODAS_LIGHT_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Water [L/d/cu]" = feols(EAU_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Sugary sodas [K/d/cu]" = feols(SODAS_SUCRES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cold cuts excluding white ham [€/Kg]" = feols(CHARCUTERIE_HORS_JB_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Other snacks [€/kg]" = feols(SNACKS_AUTRES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Fish [€/Kg]" = feols(POISSONS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Pork [€/Kg]" = feols(PORC_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Cheeses [€/kg]" = feols(FROMAGES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant),
      "Unrefined starches [€/kg]" = feols(FEC_NON_RAF_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE  , data = sgsdata_IT_filtered, vcov = ~Identifiant)
      
      
    ) 
    
    model_summary <- modelsummary(fixed_models, output = "data.frame", 
                                  gof_omit = "LogLik|Deviance|Adj.R2|FE|R2 Within|Std.Errors",
                                  statistic = c("std.error", "p.value"), fmt = "%.2g")
    model_summary$Selection <- filter_name
    final_results <- bind_rows(final_results, model_summary)
  }
}

terms_to_exclude <- c("Intercept", "Identifiant", "Temps", "Observations", "Std.Errors",
                      "ICC", "R2 Adj.", "R2 Cond.", "R2", "AIC", "BIC", "RMSE")
final_results <- final_results[!grepl(paste(terms_to_exclude, collapse = "|"), final_results$term), ]
final_results <- final_results %>% filter(part != "gof") %>% select(-part, -Selection)

model_summary_inc2 <- as.data.frame(t(final_results))
new_header <- paste(model_summary_inc2[1, ], model_summary_inc2[2, ], sep = "_")
colnames(model_summary_inc2) <- new_header
model_summary_inc2 <- model_summary_inc2[-c(1,2), ]

dep_vars <- sapply(fixed_models, function(model) all.vars(formula(model))[1])
dep_vars_df <- data.frame(Variable_Dépendante = dep_vars, row.names = NULL)
if(nrow(model_summary_inc) == length(dep_vars)) {
  model_summary_inc2 <- cbind(dep_vars_df, model_summary_inc2)
} else {
  warning("Problème de correspondance entre model_summary_inc2 et dep_vars_df")
}
model_summary_inc2[] <- lapply(model_summary_inc2, function(x) { if(is.character(x)) gsub("[()]", "", x) else x })
model_summary_inc2$Variable_Dépendante <- sub("_diff$", "", model_summary_inc2$Variable_Dépendante)

#sgsdata0 <- subset(sgsdata_IT, Periode == 1, groupe ==0)
#existence_and_means <- sapply(model_summary_inc$Variable_Dépendante, function(var) {
#  if(var %in% colnames(sgsdata0)) {
#    mean_value <- median(sgsdata0[[var]], na.rm = TRUE)
#    return(c("OK", mean_value))
#  } else {
#    return(c("", NA))
#  }
#})
#model_summary_inc$Mean_T0 <- as.numeric(existence_and_means[2, ])
#model_summary_inc <- model_summary_inc %>% relocate(Mean_T0, .before = 1)
#
#colnames(model_summary_inc)
#columns_to_scale <- c("Mean_T0",  "as.factor(groupe)1_estimate"   , "as.factor(groupe)1_std.error"  ,                         
#                      "Income_UC_INSEE_estimate" ,                             
#                      "Income_UC_INSEE_std.error" ,
#                      "as.factor(groupe)1 × Income_UC_INSEE_estimate" , 
#                      "as.factor(groupe)1 × Income_UC_INSEE_std.error")
#rows_to_scale <- grepl("POIDS", model_summary_inc[, 2]) & model_summary_inc[, 2] != "SOMME_POIDS_KCAL"
#for(col in columns_to_scale) {
#  model_summary_inc[[col]] <- as.numeric(model_summary_inc[[col]])
#}
#model_summary_inc[rows_to_scale, columns_to_scale] <- model_summary_inc[rows_to_scale, columns_to_scale] * 1000
#
#model_summary_inc$Variable_Dépendante <- NULL
#colnames(model_summary_inc) <- gsub("estimate", "Change (%)", colnames(model_summary_inc))
#colnames(model_summary_inc) <- gsub("std.error", "Std.error (%)", colnames(model_summary_inc))
#numeric_cols <- grep("Chang|error", colnames(model_summary_inc), value = TRUE)
#for(col in numeric_cols) {
#  model_summary_inc[[col]] <-(as.numeric(model_summary_inc[[col]]) / model_summary_inc$Mean_T0) * 100
#}
#model_summary_inc[] <- lapply(model_summary_inc, function(x) { if(is.numeric(x)) format(x, scientific = FALSE) else x })

models_inc2 <- model_summary_inc2


## Affichage final des résultats------------------------
print("Résultats - Interaction Différentiel_aide")
print(models_diff)
print("Résultats - Interaction Income_UC_INSEE")
print(models_inc)

## Code de verif / Valentin---------------------------
data0<-sgsdata_IT%>%filter(Periode==0);
data1<-sgsdata_IT%>%filter(Periode==1)

data_diff<-data0%>%left_join(data1%>%select(Identifiant,FV_POIDS),by="Identifiant")%>%mutate(FV_POIDS=FV_POIDS.y-FV_POIDS.x)%>%
  mutate(aide_level=case_when(Différentiel_aide<0.5~1,Différentiel_aide>=0.5 & Différentiel_aide<1 ~2,
                              Différentiel_aide>=1 & Différentiel_aide<1.5~3,.default = 4))
summary(lm(FV_POIDS~as.factor(groupe),data=data_diff))
summary(lm(FV_POIDS~groupe*Income_UC_INSEE,data=data_diff))

summary(lm(FV_POIDS~groupe*Campagne,data=data_diff))
summary(lm(FV_POIDS~groupe*Différentiel_aide,data=data_diff))
summary(lm(FV_POIDS~groupe*Différentiel_aide,data=data_diff))



#summary(lm(FV_POIDS~groupe*as.factor(aide_level),data=data_diff%>%filter(aide_level<=2)))
#summary(lm(FV_POIDS~groupe*as.factor(aide_level),data=data_diff))
#summary(lm(FV_POIDS~groupe*(Différentiel_aide<1.4),data=data_diff))
#summary(lm(FV_POIDS~groupe*FV_POIDS.x,data=data_diff))
#summary(lm(FV_POIDS~groupe*Différentiel_aide,data=data_diff%>%filter(voie_de_recrutement=="LE")))
#summary(lm(FV_POIDS~groupe*Différentiel_aide,data=data_diff[which(data_diff$Identifiant%in%unlist(ids_combines)),]))
#summary(lm(FV_POIDS~ as.factor(groupe) + as.factor(branche) + as.factor(groupe):as.factor(branche) + Income_UC_INSEE + as.factor(groupe):Income_UC_INSEE, data=data_diff))
#summary(lm(FV_POIDS~ as.factor(groupe) + as.factor(branche) + as.factor(groupe):as.factor(branche) + Différentiel_aide + as.factor(groupe):Différentiel_aide, data=data_diff))
#summary(lm(FV_POIDS~ as.factor(groupe) + as.factor(branche) + as.factor(groupe):as.factor(branche) + Différentiel_aide + as.factor(groupe):Différentiel_aide, data=data_diff))
#
##PREDICTION ----------------------------------------------
# --- Modèles estimés (exemple) ---

# Création des sous-échantillons pour chaque valeur de 'branche'
branch_conditions <- list(
  "Branche_0" = sgsdata_IT_filtered %>% filter(branche == 0),
  "Branche_1" = sgsdata_IT_filtered %>% filter(branche == 1),
  "Branche_tot"= sgsdata_IT_filtered 
)

# Initialisation d'une liste pour stocker les résultats finaux
results_list <- list()
results_list_bis <- list()

# Boucle sur chaque sous-échantillon
for(branch_name in names(branch_conditions)){
  subset_data <- branch_conditions[[branch_name]]

M0 <- feols(M0_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
p0 <- feols(p0_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
  
FV <- feols(FV_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FRUITS <- feols(FRUITS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FRUITS_SECS <-feols(FRUITS_SECS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
NOIX <-feols(NOIX_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
LEGUMES <- feols(LEGUMES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
LEG_SECS <- feols(LEG_SECS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FRUITS_KG <- feols(FRUITS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
LEGUMES_KG <- feols(LEGUMES_prix_kg_diff ~ groupe * Income_UC_INSEE, data = subset_data, vcov = ~Identifiant)
LEG_SECS_KG <- feols(LEG_SECS_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
VIANDES <- feols(VIANDES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
VIANDE_ROUGE_PORC <- feols(VIANDE_ROUGE_PORC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
AUTRE_PDTS_ANIMAUX <- feols(AUTRE_PDTS_ANIMAUX_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
POULET_OEUFS <- feols(POULET_OEUFS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
POISSONS <- feols(POISSONS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FEC <- feols(FEC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
PDTS_LAITIERS <- feols(PDTS_LAITIERS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
MG <- feols(MG_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
PLATS_PREP <- feols(PLATS_PREP_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SSB <- feols(SSB_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
CAFE_THE_POIDS <- feols(CAFE_THE_POIDS ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
CEREALES_PD_POIDS <- feols(CEREALES_PD_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
CHARCUTERIE_HORS_JB_POIDS <- feols(CHARCUTERIE_HORS_JB_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
DESSERTS_LACTES_POIDS <- feols(DESSERTS_LACTES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FEC_NON_RAF_POIDS <- feols(FEC_NON_RAF_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FEC_RAF_POIDS <- feols(FEC_RAF_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FROMAGES_POIDS <- feols(FROMAGES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FRUITS_JUS_POIDS <- feols(FRUITS_JUS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
JAMBON_BLANC_POIDS <- feols(JAMBON_BLANC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
LAIT_POIDS <- feols(LAIT_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
LAITAGES_POIDS <- feols(LAITAGES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
MGA_POIDS <- feols(MGA_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
MGV_POIDS <- feols(MGV_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
OEUFS_POIDS <- feols(OEUFS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
PDTS_SUCRES_POIDS <- feols(PDTS_SUCRES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
PLATS_PREP_CARNES_POIDS <- feols(PLATS_PREP_CARNES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
PLATS_PREP_VEGETARIENS_POIDS <- feols(PLATS_PREP_VEGETARIENS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
POISSONS_POIDS <- feols(POISSONS_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
PORC_POIDS <- feols(PORC_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
POULET_POIDS <- feols(POULET_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
QUICHES_PIZZAS_TARTES_SALEES_POIDS <- feols(QUICHES_PIZZAS_TARTES_SALEES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SAUCES_POIDS <- feols(SAUCES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SNACKS_AUTRES_POIDS <- feols(SNACKS_AUTRES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SODAS_SUCRES_POIDS <- feols(SODAS_SUCRES_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SODAS_LIGHT_POIDS <- feols(SODAS_LIGHT_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
VIANDE_ROUGE_POIDS <- feols(VIANDE_ROUGE_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
depense_alim_uc <- feols(depense_alim_uc_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
MAR <- feols(MAR_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
MER <- feols(MER_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
energy_densite <- feols(energy_densite_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
Calories <- feols(SOMME_POIDS_KCAL_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
Climat <- feols(climat_env_diff~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SODAS_LIGHT_POIDS_diff <- feols(SODAS_LIGHT_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
EAU_POIDS <- feols(EAU_POIDS_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SODAS_SUCRES_prix_kg <- feols(SODAS_SUCRES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
CHARCUTERIE_HORS_JB_prix_kg <- feols(CHARCUTERIE_HORS_JB_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SNACKS_AUTRES_prix_kg<- feols(SNACKS_AUTRES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
POISSONS_prix_kg <- feols(POISSONS_prix_kg_diff~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
PORC_prix_kg<- feols(PORC_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FROMAGES_prix_kg <- feols(FROMAGES_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
FEC_NON_RAF_prix_kg <- feols(FEC_NON_RAF_prix_kg_diff~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
SOMME_POIDS_HORS_BOISSON <- feols(SOMME_POIDS_HORS_BOISSON_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)
NOIX_prix_kg              <- feols(NOIX_prix_kg_diff ~ as.factor(groupe) + Income_UC_INSEE  + as.factor(groupe):Income_UC_INSEE , data = subset_data, vcov = ~Identifiant)


# --- Création des combinaisons de groupe pour la prédiction ---
new_data <- expand.grid(
  groupe = c(0, 1),                       # Contrôle et traité
  Income_UC_INSEE = c(640, 2079)       # Deux niveaux spécifiques de revenus
)


# --- Obtenir les prédictions pour chacun des modèles ---
new_data$M0 <- predict(M0, newdata = new_data)
new_data$p0 <- predict(p0, newdata = new_data)

new_data$FRUITS_SECS  <- predict(FRUITS_SECS , newdata = new_data)
new_data$NOIX <- predict(NOIX, newdata = new_data)
new_data$FV <- predict(FV, newdata = new_data)
new_data$FRUITS <- predict(FRUITS, newdata = new_data)
new_data$LEGUMES <- predict(LEGUMES, newdata = new_data)
new_data$LEG_SECS <- predict(LEG_SECS, newdata = new_data)
new_data$FRUITS_KG <- predict(FRUITS_KG, newdata = new_data)
new_data$LEGUMES_KG <- predict(LEGUMES_KG, newdata = new_data)
new_data$LEG_SECS_KG <- predict(LEG_SECS_KG, newdata = new_data)
new_data$VIANDES <- predict(VIANDES, newdata = new_data)
new_data$VIANDE_ROUGE_PORC <- predict(VIANDE_ROUGE_PORC, newdata = new_data)
new_data$AUTRE_PDTS_ANIMAUX <- predict(AUTRE_PDTS_ANIMAUX, newdata = new_data)
new_data$POULET_OEUFS <- predict(POULET_OEUFS, newdata = new_data)
new_data$POISSONS <- predict(POISSONS, newdata = new_data)
new_data$FEC <- predict(FEC, newdata = new_data)
new_data$PDTS_LAITIERS <- predict(PDTS_LAITIERS, newdata = new_data)
new_data$MG <- predict(MG, newdata = new_data)
new_data$PLATS_PREP <- predict(PLATS_PREP, newdata = new_data)
new_data$SSB <- predict(SSB, newdata = new_data)
new_data$CAFE_THE_POIDS <- predict(CAFE_THE_POIDS, newdata = new_data)
new_data$CEREALES_PD_POIDS <- predict(CEREALES_PD_POIDS, newdata = new_data)
new_data$CHARCUTERIE_HORS_JB_POIDS <- predict(CHARCUTERIE_HORS_JB_POIDS, newdata = new_data)
new_data$DESSERTS_LACTES_POIDS <- predict(DESSERTS_LACTES_POIDS, newdata = new_data)
new_data$FEC_NON_RAF_POIDS <- predict(FEC_NON_RAF_POIDS, newdata = new_data)
new_data$FEC_RAF_POIDS <- predict(FEC_RAF_POIDS, newdata = new_data)
new_data$FROMAGES_POIDS <- predict(FROMAGES_POIDS, newdata = new_data)
new_data$FRUITS_JUS_POIDS <- predict(FRUITS_JUS_POIDS, newdata = new_data)
new_data$JAMBON_BLANC_POIDS <- predict(JAMBON_BLANC_POIDS, newdata = new_data)
new_data$LAIT_POIDS <- predict(LAIT_POIDS, newdata = new_data)
new_data$LAITAGES_POIDS <- predict(LAITAGES_POIDS, newdata = new_data)
new_data$MGA_POIDS <- predict(MGA_POIDS, newdata = new_data)
new_data$MGV_POIDS <- predict(MGV_POIDS, newdata = new_data)
new_data$OEUFS_POIDS <- predict(OEUFS_POIDS, newdata = new_data)
new_data$PDTS_SUCRES_POIDS <- predict(PDTS_SUCRES_POIDS, newdata = new_data)
new_data$PLATS_PREP_CARNES_POIDS <- predict(PLATS_PREP_CARNES_POIDS, newdata = new_data)
new_data$PLATS_PREP_VEGETARIENS_POIDS <- predict(PLATS_PREP_VEGETARIENS_POIDS, newdata = new_data)
new_data$POISSONS_POIDS <- predict(POISSONS_POIDS, newdata = new_data)
new_data$PORC_POIDS <- predict(PORC_POIDS, newdata = new_data)
new_data$POULET_POIDS <- predict(POULET_POIDS, newdata = new_data)
new_data$QUICHES_PIZZAS_TARTES_SALEES_POIDS <- predict(QUICHES_PIZZAS_TARTES_SALEES_POIDS, newdata = new_data)
new_data$SAUCES_POIDS <- predict(SAUCES_POIDS, newdata = new_data)
new_data$SNACKS_AUTRES_POIDS <- predict(SNACKS_AUTRES_POIDS, newdata = new_data)
new_data$SODAS_SUCRES_POIDS <- predict(SODAS_SUCRES_POIDS, newdata = new_data)
new_data$SODAS_LIGHT_POIDS <- predict(SODAS_LIGHT_POIDS, newdata = new_data)
new_data$VIANDE_ROUGE_POIDS <- predict(VIANDE_ROUGE_POIDS, newdata = new_data)
new_data$depense_alim_uc <- predict(depense_alim_uc, newdata = new_data)
new_data$MAR <- predict(MAR, newdata = new_data)
new_data$MER <- predict(MER, newdata = new_data)
new_data$energy_densite <- predict(energy_densite, newdata = new_data)
new_data$Calories <- predict(Calories, newdata = new_data)
new_data$Climat <- predict(Climat, newdata = new_data)
new_data$SODAS_LIGHT_POIDS <- predict(SODAS_LIGHT_POIDS, newdata = new_data)
new_data$EAU_POIDS <- predict(EAU_POIDS , newdata = new_data)
new_data$SODAS_SUCRES_prix_kg <- predict(SODAS_SUCRES_prix_kg, newdata = new_data)
new_data$CHARCUTERIE_HORS_JB_prix_kg <- predict(CHARCUTERIE_HORS_JB_prix_kg, newdata = new_data)
new_data$SNACKS_AUTRES_prix_kg<- predict(SNACKS_AUTRES_prix_kg , newdata = new_data)
new_data$POISSONS_prix_kg <- predict(POISSONS_prix_kg , newdata = new_data)
new_data$PORC_prix_kg<- predict(PORC_prix_kg, newdata = new_data)
new_data$FROMAGES_prix_kg <- predict(FROMAGES_prix_kg , newdata = new_data)
new_data$FEC_NON_RAF_prix_kg <- predict(FEC_NON_RAF_prix_kg , newdata = new_data)
new_data$SOMME_POIDS_HORS_BOISSON <- predict(SOMME_POIDS_HORS_BOISSON, newdata = new_data)
new_data$NOIX_prix_kg             <- predict(NOIX_prix_kg, newdata = new_data)



# --- Transformation des prédictions ---
# Pour les variables liées au poids, on multiplie par 1000
new_data_clean <- new_data %>%
  mutate(
    fit_M0 = M0,
    fit_p0 = p0,
    fit_FV = FV * 1000,
    fit_FRUITS = FRUITS * 1000,
    fit_LEGUMES = LEGUMES * 1000,
    fit_LEG_SECS = LEG_SECS * 1000,
    fit_FRUITS_SECS = FRUITS_SECS * 1000,
    fit_NOIX = NOIX * 1000,
    fit_FRUITS_KG = FRUITS_KG,
    fit_LEGUMES_KG = LEGUMES_KG,
    fit_LEG_SECS_KG = LEG_SECS_KG,
    fit_VIANDES = VIANDES * 1000,
    fit_VIANDE_ROUGE_PORC = VIANDE_ROUGE_PORC * 1000,
    fit_AUTRE_PDTS_ANIMAUX = AUTRE_PDTS_ANIMAUX * 1000,
    fit_POULET_OEUFS = POULET_OEUFS * 1000,
    fit_POISSONS = POISSONS * 1000,
    fit_FEC = FEC * 1000,
    fit_PDTS_LAITIERS = PDTS_LAITIERS * 1000,
    fit_MG = MG * 1000,
    fit_PLATS_PREP = PLATS_PREP * 1000,
    fit_SSB = SSB * 1000,
    fit_CAFE_THE_POIDS = CAFE_THE_POIDS * 1000,
    fit_CEREALES_PD_POIDS = CEREALES_PD_POIDS * 1000,
    fit_CHARCUTERIE_HORS_JB_POIDS = CHARCUTERIE_HORS_JB_POIDS * 1000,
    fit_DESSERTS_LACTES_POIDS = DESSERTS_LACTES_POIDS * 1000,
    fit_FEC_NON_RAF_POIDS = FEC_NON_RAF_POIDS * 1000,
    fit_FEC_RAF_POIDS = FEC_RAF_POIDS * 1000,
    fit_FROMAGES_POIDS = FROMAGES_POIDS * 1000,
    fit_FRUITS_JUS_POIDS = FRUITS_JUS_POIDS * 1000,
    fit_JAMBON_BLANC_POIDS = JAMBON_BLANC_POIDS * 1000,
    fit_LAIT_POIDS = LAIT_POIDS * 1000,
    fit_LAITAGES_POIDS = LAITAGES_POIDS * 1000,
    fit_MGA_POIDS = MGA_POIDS * 1000,
    fit_MGV_POIDS = MGV_POIDS * 1000,
    fit_OEUFS_POIDS = OEUFS_POIDS * 1000,
    fit_PDTS_SUCRES_POIDS = PDTS_SUCRES_POIDS * 1000,
    fit_PLATS_PREP_CARNES_POIDS = PLATS_PREP_CARNES_POIDS * 1000,
    fit_PLATS_PREP_VEGETARIENS_POIDS = PLATS_PREP_VEGETARIENS_POIDS * 1000,
    fit_POISSONS_POIDS = POISSONS_POIDS * 1000,
    fit_PORC_POIDS = PORC_POIDS * 1000,
    fit_POULET_POIDS = POULET_POIDS * 1000,
    fit_QUICHES_PIZZAS_TARTES_SALEES_POIDS = QUICHES_PIZZAS_TARTES_SALEES_POIDS * 1000,
    fit_SAUCES_POIDS = SAUCES_POIDS * 1000,
    fit_SNACKS_AUTRES_POIDS = SNACKS_AUTRES_POIDS * 1000,
    fit_SODAS_SUCRES_POIDS = SODAS_SUCRES_POIDS * 1000,
    fit_SODAS_LIGHT_POIDS = SODAS_LIGHT_POIDS * 1000,
    fit_VIANDE_ROUGE_POIDS = VIANDE_ROUGE_POIDS * 1000,
    fit_depense_alim_uc = depense_alim_uc,      # aucune transformation
    fit_MAR = MAR,                        # aucune transformation
    fit_MER = MER,                        # aucune transformation
    fit_energy_densite = energy_densite,  # aucune transformation
    fit_Calories = Calories,              # aucune transformation
    fit_Climat = Climat  ,                 # aucune transformation
    fit_SODAS_LIGHT_POIDS = SODAS_LIGHT_POIDS* 1000,,
    fit_EAU_POIDS = EAU_POIDS * 1000,,
    fit_SODAS_SUCRES_prix_kg = SODAS_SUCRES_prix_kg ,
    fit_CHARCUTERIE_HORS_JB_prix_kg = CHARCUTERIE_HORS_JB_prix_kg,
    fit_SNACKS_AUTRES_prix_kg = SNACKS_AUTRES_prix_kg,
    fit_POISSONS_prix_kg= POISSONS_prix_kg,
    fit_PORC_prix_kg = PORC_prix_kg,
    fit_FROMAGES_prix_kg = FROMAGES_prix_kg,
    fit_FEC_NON_RAF_prix_kg = FEC_NON_RAF_prix_kg,
    fit_SOMME_POIDS_HORS_BOISSON = SOMME_POIDS_HORS_BOISSON ,
    fit_NOIX_prix_kg     = NOIX_prix_kg         
    
  ) %>%
  # Supprimer les colonnes originales des prédictions
  select(-M0, p0,-FV, -FRUITS, -LEGUMES, -FRUITS_SECS, -NOIX, -LEG_SECS, -FRUITS_KG, -LEGUMES_KG, -LEG_SECS_KG, -VIANDES, -VIANDE_ROUGE_PORC,
         -AUTRE_PDTS_ANIMAUX, -POULET_OEUFS, -POISSONS, -FEC, -PDTS_LAITIERS, -MG, -PLATS_PREP, -SSB, -CAFE_THE_POIDS,
         -CEREALES_PD_POIDS, -CHARCUTERIE_HORS_JB_POIDS, -DESSERTS_LACTES_POIDS, -FEC_NON_RAF_POIDS, -FEC_RAF_POIDS,
         -FROMAGES_POIDS, -FRUITS_JUS_POIDS, -JAMBON_BLANC_POIDS, -LAIT_POIDS, -LAITAGES_POIDS, -MGA_POIDS, -MGV_POIDS,
         -OEUFS_POIDS, -PDTS_SUCRES_POIDS, -PLATS_PREP_CARNES_POIDS, -PLATS_PREP_VEGETARIENS_POIDS, -POISSONS_POIDS,
         -PORC_POIDS, -POULET_POIDS, -QUICHES_PIZZAS_TARTES_SALEES_POIDS, -SAUCES_POIDS, -SNACKS_AUTRES_POIDS,
         -SODAS_SUCRES_POIDS, -SODAS_LIGHT_POIDS, -VIANDE_ROUGE_POIDS, -depense_alim_uc, -MAR, -MER, -energy_densite,
         -Calories, -Climat,
         - SODAS_LIGHT_POIDS,
        -EAU_POIDS ,
         - SODAS_SUCRES_prix_kg ,
         - CHARCUTERIE_HORS_JB_prix_kg,
        - SNACKS_AUTRES_prix_kg,
         - POISSONS_prix_kg,
        -PORC_prix_kg,
        - FROMAGES_prix_kg,
        - FEC_NON_RAF_prix_kg,
        -  SOMME_POIDS_HORS_BOISSON ,
       - NOIX_prix_kg     )

# --- Création du tableau résumé final ---
predict_table <- new_data_clean %>%
  mutate(
    M0 =paste0(fit_M0, 2),
    p0 =paste0(fit_p0, 2),
    FV= paste0(fit_FV, 2),
    FRUITS = paste0(fit_FRUITS, 2),
    LEGUMES = paste0(fit_LEGUMES, 2),
    FRUITS_SECS = paste0(fit_FRUITS_SECS, 2),
    NOIX = paste0(fit_NOIX, 2),
    LEG_SECS = paste0(fit_LEG_SECS, 2),
    FRUITS_KG = paste0(fit_FRUITS_KG, 2),
    LEGUMES_KG = paste0(fit_LEGUMES_KG, 2),
    LEG_SECS_KG = paste0(fit_LEG_SECS_KG, 2),
    VIANDES = paste0(fit_VIANDES, 2),
    VIANDE_ROUGE_PORC = paste0(fit_VIANDE_ROUGE_PORC, 2),
    AUTRE_PDTS_ANIMAUX = paste0(fit_AUTRE_PDTS_ANIMAUX, 2),
    POULET_OEUFS = paste0(fit_POULET_OEUFS, 2),
    POISSONS = paste0(fit_POISSONS, 2),
    FEC = paste0(fit_FEC, 2),
    PDTS_LAITIERS = paste0(fit_PDTS_LAITIERS, 2),
    MG = paste0(fit_MG, 2),
    PLATS_PREP = paste0(fit_PLATS_PREP, 2),
    SSB = paste0(fit_SSB, 2),
    CAFE_THE_POIDS = paste0(fit_CAFE_THE_POIDS, 2),
    CEREALES_PD_POIDS = paste0(fit_CEREALES_PD_POIDS, 2),
    CHARCUTERIE_HORS_JB_POIDS = paste0(fit_CHARCUTERIE_HORS_JB_POIDS, 2),
    DESSERTS_LACTES_POIDS = paste0(fit_DESSERTS_LACTES_POIDS, 2),
    FEC_NON_RAF_POIDS = paste0(fit_FEC_NON_RAF_POIDS, 2),
    FEC_RAF_POIDS = paste0(fit_FEC_RAF_POIDS, 2),
    FROMAGES_POIDS = paste0(fit_FROMAGES_POIDS, 2),
    FRUITS_JUS_POIDS = paste0(fit_FRUITS_JUS_POIDS, 2),
    JAMBON_BLANC_POIDS = paste0(fit_JAMBON_BLANC_POIDS, 2),
    LAIT_POIDS = paste0(fit_LAIT_POIDS, 2),
    LAITAGES_POIDS = paste0(fit_LAITAGES_POIDS, 2),
    MGA_POIDS = paste0(fit_MGA_POIDS, 2),
    MGV_POIDS = paste0(fit_MGV_POIDS, 2),
    OEUFS_POIDS = paste0(fit_OEUFS_POIDS, 2),
    PDTS_SUCRES_POIDS = paste0(fit_PDTS_SUCRES_POIDS, 2),
    PLATS_PREP_CARNES_POIDS = paste0(fit_PLATS_PREP_CARNES_POIDS, 2),
    PLATS_PREP_VEGETARIENS_POIDS = paste0(fit_PLATS_PREP_VEGETARIENS_POIDS, 2),
    POISSONS_POIDS = paste0(fit_POISSONS_POIDS, 2),
    PORC_POIDS = paste0(fit_PORC_POIDS, 2),
    POULET_POIDS = paste0(fit_POULET_POIDS, 2),
    QUICHES_PIZZAS_TARTES_SALEES_POIDS = paste0(fit_QUICHES_PIZZAS_TARTES_SALEES_POIDS, 2),
    SAUCES_POIDS = paste0(fit_SAUCES_POIDS, 2),
    SNACKS_AUTRES_POIDS = paste0(fit_SNACKS_AUTRES_POIDS, 2),
    SODAS_SUCRES_POIDS = paste0(fit_SODAS_SUCRES_POIDS, 2),
    SODAS_LIGHT_POIDS = paste0(fit_SODAS_LIGHT_POIDS, 2),
    VIANDE_ROUGE_POIDS = paste0(fit_VIANDE_ROUGE_POIDS, 2),
    depense_alim_uc = paste0(fit_depense_alim_uc, 2),
    MAR = paste0(fit_MAR, 2),
    MER = paste0(fit_MER, 2),
    energy_densite = paste0(fit_energy_densite, 2),
    Calories = paste0(fit_Calories, 2),
    Climat = paste0(fit_Climat, 2),
    SODAS_LIGHT_POIDS = paste0(fit_SODAS_LIGHT_POIDS, 2),
    EAU_POIDS = paste0(fit_EAU_POIDS, 2),
    SODAS_SUCRES_prix_kg =  paste0(fit_SODAS_SUCRES_prix_kg, 2),
    CHARCUTERIE_HORS_JB_prix_kg = paste0(fit_CHARCUTERIE_HORS_JB_prix_kg, 2),
    SNACKS_AUTRES_prix_kg = paste0(fit_SNACKS_AUTRES_prix_kg, 2),
    POISSONS_prix_kg = paste0(fit_POISSONS_prix_kg, 2),
    PORC_prix_kg = paste0(fit_PORC_prix_kg , 2),
    FROMAGES_prix_kg = paste0(fit_FROMAGES_prix_kg , 2),
    FEC_NON_RAF_prix_kg = paste0(fit_FEC_NON_RAF_prix_kg , 2),
    SOMME_POIDS_HORS_BOISSON = paste0(fit_SOMME_POIDS_HORS_BOISSON , 2),
    NOIX_prix_kg = paste0(fit_NOIX_prix_kg , 2)
  ) %>%
  # Réorganisation des colonnes (commençant par les identifiants)
  select(groupe, Income_UC_INSEE,  M0, p0, FV, FRUITS, FRUITS_SECS, NOIX, LEGUMES, LEG_SECS, FRUITS_KG,
         LEGUMES_KG, LEG_SECS_KG, VIANDES, VIANDE_ROUGE_PORC, AUTRE_PDTS_ANIMAUX,
         POULET_OEUFS, POISSONS, FEC, PDTS_LAITIERS, MG, PLATS_PREP, SSB,
         CAFE_THE_POIDS, CEREALES_PD_POIDS, CHARCUTERIE_HORS_JB_POIDS, DESSERTS_LACTES_POIDS,
         FEC_NON_RAF_POIDS, FEC_RAF_POIDS, FROMAGES_POIDS, FRUITS_JUS_POIDS, JAMBON_BLANC_POIDS,
         LAIT_POIDS, LAITAGES_POIDS, MGA_POIDS, MGV_POIDS, OEUFS_POIDS, PDTS_SUCRES_POIDS,
         PLATS_PREP_CARNES_POIDS, PLATS_PREP_VEGETARIENS_POIDS, POISSONS_POIDS, PORC_POIDS,
         POULET_POIDS, QUICHES_PIZZAS_TARTES_SALEES_POIDS, SAUCES_POIDS, SNACKS_AUTRES_POIDS,
         SODAS_SUCRES_POIDS, SODAS_LIGHT_POIDS, VIANDE_ROUGE_POIDS, depense_alim_uc, MAR, MER,
         energy_densite, Calories, Climat,           SODAS_LIGHT_POIDS,
         EAU_POIDS ,
         SODAS_SUCRES_prix_kg ,
         CHARCUTERIE_HORS_JB_prix_kg,
         SNACKS_AUTRES_prix_kg,
         POISSONS_prix_kg,
         PORC_prix_kg,
         FROMAGES_prix_kg,
         FEC_NON_RAF_prix_kg,
         SOMME_POIDS_HORS_BOISSON ,
         NOIX_prix_kg ) %>%
  arrange(groupe, Income_UC_INSEE)


predict_table_bis <- predict_table %>% 
  filter(!( Income_UC_INSEE == 2079))

predict_table_bis <- predict_table_bis  %>% 
  # Passage en format long : on rassemble toutes les colonnes de prédiction
  pivot_longer(
    cols = -c(groupe, Income_UC_INSEE),
    names_to = "variable",
    values_to = "valeur"
  ) %>%
  # On crée des colonnes distinctes pour chaque groupe
  pivot_wider(
    names_from = groupe,
    values_from = valeur,
    names_prefix = "groupe_"
  ) %>%
  # Conversion en numérique des colonnes groupe_0 et groupe_1
  mutate(
    groupe_0 = as.numeric(groupe_0),
    groupe_1 = as.numeric(groupe_1)
  ) %>%
  # Calcul de la différence (taille d'effet)
  mutate(diff = groupe_1 - groupe_0)%>%
  # On conserve les identifiants et la variable avec sa différence
  select( Income_UC_INSEE, variable, diff) %>%
  # On repasse en format large pour obtenir une colonne par variable
  pivot_wider(
    names_from = variable,
    values_from = diff
  )

predict_table_bis <- predict_table_bis%>% 
  select(-c(Income_UC_INSEE, FV, FRUITS_KG,
            LEGUMES_KG, LEG_SECS_KG, VIANDES, VIANDE_ROUGE_PORC, AUTRE_PDTS_ANIMAUX,
            POULET_OEUFS, POISSONS, FEC, PDTS_LAITIERS, MG, PLATS_PREP, SSB,
            depense_alim_uc, MAR, MER, energy_densite, Calories,          
            EAU_POIDS ,
            SODAS_SUCRES_prix_kg ,
            CHARCUTERIE_HORS_JB_prix_kg,
            SNACKS_AUTRES_prix_kg,
            POISSONS_prix_kg,
            PORC_prix_kg,
            FROMAGES_prix_kg,
            FEC_NON_RAF_prix_kg,
            SOMME_POIDS_HORS_BOISSON ,
            NOIX_prix_kg ))

##Extraire les données pour HENI 
desired_order <- c("CAFE_THE_POIDS", "CEREALES_PD_POIDS", "CHARCUTERIE_HORS_JB_POIDS",
                   "DESSERTS_LACTES_POIDS", "FEC_NON_RAF_POIDS", "FEC_RAF_POIDS", "FROMAGES_POIDS",
                   "FRUITS", "FRUITS_JUS_POIDS", "FRUITS_SECS", "JAMBON_BLANC_POIDS",
                   "LAIT_POIDS", "LAITAGES_POIDS", "LEG_SECS", "LEGUMES", "MGA_POIDS",
                   "MGV_POIDS", "NOIX", "OEUFS_POIDS", "PDTS_SUCRES_POIDS", "PLATS_PREP_CARNES_POIDS",
                   "PLATS_PREP_VEGETARIENS_POIDS", "POISSONS_POIDS", "PORC_POIDS", "POULET_POIDS",
                   "QUICHES_PIZZAS_TARTES_SALEES_POIDS", "SAUCES_POIDS", "SNACKS_AUTRES_POIDS",
                   "SODAS_LIGHT_POIDS", "SODAS_SUCRES_POIDS", "VIANDE_ROUGE_POIDS", "Climat")

predict_table_bis <- predict_table_bis %>% 
  select(all_of(desired_order))



# Multiplier par 1000 toutes les valeurs sauf celles où Categorie vaut "Climat env"

resultat <- CALNUT %>%
  filter(!is.na(Classif_HENI)) %>%
  group_by(groupe_TI_TdC) %>%
  summarise(moyenne_HENI = mean(HENI, na.rm = TRUE)) %>%
  rename(variable = groupe_TI_TdC)



# Transposition du tableau
predict_table_long <- as.data.frame(t(predict_table_bis))

# Suppression du suffixe dans les noms de lignes
rownames(predict_table_long) <- sub("_POIDS.*$", "", rownames(predict_table_long))


predict_table_long <- data.frame(variable = rownames(predict_table_long), predict_table_long, row.names = NULL) 


# Renommage de la première colonne en "variable"

colnames(predict_table_long)[2] <- "POIDS"

predict_table_long <- predict_table_long %>% 
  left_join(resultat, by = "variable")

predict_table_long$HENI_AC <- as.numeric(predict_table_long$moyenne_HENI) * as.numeric(predict_table_long$POIDS)

# Stockage du tableau final pour ce sous-échantillon
results_list[[branch_name]] <- predict_table_long
results_list_bis[[branch_name]] <- predict_table
}

# Affichage des résultats pour chaque branche
result_TRUST <-  results_list$Branche_0
result_AC <-  results_list$Branche_1
result_TOT <-  results_list_bis$Branche_tot
result_TOT_TRUST <-  results_list_bis$Branche_0
result_TOT_AC <-  results_list_bis$Branche_1



#TABLEAU CARACTERISTIQUES SOCIO-DEMOGRAPHIQUES----------------------------------------
data  <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300", Periode==0 )


data$Age_Central <- as.numeric(data$Age_Central)
data$Combien.de.personnes.vivent.dans.votre.foyer <- as.numeric(data$Combien.de.personnes.vivent.dans.votre.foyer)
data$FV_POIDS <- as.numeric(data$FV_POIDS)
data$Part_Kcal_BIO <- as.numeric(data$Part_Kcal_BIO)
data$Part_Kcal_Epiceries <- as.numeric(data$Part_Kcal_Epiceries)
data$Part_Kcal_Supermarchés <- as.numeric(data$Part_Kcal_Supermarchés)
data$Income_UC_INSEE <- as.numeric(data$Income_UC_INSEE)

# Calculer les statistiques pour les variables continues
cont_means <- data %>%
  group_by(groupe) %>%
  summarise(
    Age_Central = mean(Age_Central, na.rm = TRUE),
    Combien.de.personnes.vivent.dans.votre.foyer  = mean(Combien.de.personnes.vivent.dans.votre.foyer, na.rm = TRUE),
    Différentiel_aide = mean(Différentiel_aide, na.rm = TRUE),
    FV_POIDS = mean(FV_POIDS, na.rm = TRUE),
    Part_Kcal_BIO = mean(Part_Kcal_BIO, na.rm = TRUE),
    Part_Kcal_Epiceries = mean(Part_Kcal_Epiceries, na.rm = TRUE),
    Part_Kcal_Supermarchés = mean(Part_Kcal_Supermarchés, na.rm = TRUE),
    Income_UC_INSEE = mean(Income_UC_INSEE, na.rm = TRUE),
    .groups = 'drop'
  )

# Effectuer le test de Wilcoxon pour chaque variable sur les données brutes
p_values <- data.frame(
  Variable = c("Age_Central", "Combien.de.personnes.vivent.dans.votre.foyer",
               "Différentiel_aide", "FV_POIDS", "Part_Kcal_BIO", "Part_Kcal_Epiceries", "Part_Kcal_Supermarchés",
               "Income_UC_INSEE"),
  p_value = c(
    t.test(Age_Central ~ groupe, data = data, var.equal = TRUE)$p.value,
    t.test(Combien.de.personnes.vivent.dans.votre.foyer ~ groupe, data = data, var.equal = TRUE)$p.value,
    t.test(Différentiel_aide ~ groupe, data = data, var.equal = TRUE)$p.value,
    t.test(FV_POIDS ~ groupe, data = data, var.equal = TRUE)$p.value,
    t.test(Part_Kcal_BIO ~ groupe, data = data, var.equal = TRUE)$p.value,
    t.test(Part_Kcal_Epiceries ~ groupe, data = data, var.equal = TRUE)$p.value,
    t.test(Part_Kcal_Supermarchés ~ groupe, data = data, var.equal = TRUE)$p.value,
    t.test(Income_UC_INSEE ~ groupe, data = data, var.equal = TRUE)$p.value
  )
)

# Joindre les p-values avec les moyennes
cont_means_long <- cont_means %>%
  pivot_longer(cols = -groupe, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = groupe, values_from = Mean, names_prefix = "groupe_") %>%
  mutate(
    Difference = groupe_1 - groupe_0
  ) %>%
  left_join(p_values, by = "Variable")


## VARIABLES QUALITATIVES -------------------------------
# Fonction pour traiter les données
process_data <- function(data, variable) {
  data %>%
    group_by(groupe, !!sym(variable)) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = groupe, values_from = Count, names_prefix = "Group_") %>%
    mutate(
      Proportion_Control_group  = ifelse(!is.na(Group_0), Group_0 / sum(Group_0, na.rm = TRUE)*100, NA),
      Proportion_Intervention_group = ifelse(!is.na(Group_1), Group_1 / sum(Group_1, na.rm = TRUE)*100, NA),
      Difference = Proportion_Intervention_group- Proportion_Control_group,
      Variable = variable  )}

# Liste des variables à analyser
variables <- c("Sexe", "Quel.est.votre.pays.de.naissance.",
               "Aide_alim","Situation_emploi", "Foyer_monoparental", "Education")

# Appliquer la fonction pour chaque variable et combiner les résultats
results <- map_df(variables, ~ process_data(data, .x))

# Transformer les valeurs des colonnes des variables en une seule colonne sans utiliser select
results$Variable <- results$Sexe
results$Variable <- ifelse(!is.na(results$Quel.est.votre.pays.de.naissance.),(results$Quel.est.votre.pays.de.naissance.),(results $Variable))
results$Variable <- ifelse(!is.na(results$Aide_alim),(results$Aide_alim),(results$Variable))
results$Variable <- ifelse(!is.na(results$Situation_emploi),(results$Situation_emploi),(results$Variable))
results$Variable <- ifelse(!is.na(results$Foyer_monoparental),(results$Foyer_monoparental),(results$Variable))
results$Variable <- ifelse(!is.na(results$Education),(results$Education),(results$Variable))

#Genre 
df_filtered <- subset(results, !is.na(Sexe))
contingency_table <- matrix(c(df_filtered$Group_0[1], df_filtered$Group_0[2], df_filtered$Group_1[1],df_filtered$Group_1[2]),nrow = 2,byrow = TRUE)
chi2_result <- chisq.test(contingency_table)
p_value_gender <- chi2_result$p.value

#Pays de naissance
df_filtered <- subset(results, !is.na(Quel.est.votre.pays.de.naissance.))
somme_autre <- df_filtered %>%
  filter(Variable != 'France') %>%  # Filtrer pour ne garder que les lignes où Variable n'est pas France
  summarise(
    Variable = 'Autre',  # Nommer la nouvelle ligne comme 'Autre'
    Group_0 = sum(Group_0, na.rm = TRUE),  # Somme des valeurs dans Group_0
    Group_1 = sum(Group_1, na.rm = TRUE)   # Somme des valeurs dans Group_1
  )
france_rows <- df_filtered %>%
  filter(Variable == 'France')
df_combined <- bind_rows(somme_autre, france_rows)
contingency_table <- matrix(c(df_combined $Group_0[1], df_combined$Group_0[2], df_combined$Group_1[1],df_combined$Group_1[2]),nrow = 2,byrow = TRUE)
chi2_result <- chisq.test(contingency_table)
p_value_pays <- chi2_result$p.value

#Aide_alim
df_filtered <- subset(results, !is.na(Aide_alim))
contingency_table <- matrix(c(df_filtered$Group_0[1], df_filtered$Group_0[2], df_filtered$Group_1[1],df_filtered$Group_1[2]),nrow = 2,byrow = TRUE)
chi2_result <- chisq.test(contingency_table)
p_value_Aide_alim <- chi2_result$p.value

#source revenu
df_filtered <- subset(results, !is.na(Situation_emploi))
somme_autre <- df_filtered %>%
  filter(Variable != 'Occupe un emploi') %>%  # Filtrer pour ne garder que les lignes où Variable n'est pas France
  summarise(
    Variable = 'Autre',  # Nommer la nouvelle ligne comme 'Autre'
    Group_0 = sum(Group_0, na.rm = TRUE),  # Somme des valeurs dans Group_0
    Group_1 = sum(Group_1, na.rm = TRUE)   # Somme des valeurs dans Group_1
  )
Situation_emploi_rows <- df_filtered %>%
  filter(Variable == 'Occupe un emploi')
df_combined <- bind_rows(somme_autre, Situation_emploi_rows)
contingency_table <- matrix(c(df_combined $Group_0[1], df_combined$Group_0[2], df_combined$Group_1[1],df_combined$Group_1[2]),nrow = 2,byrow = TRUE)
chi2_result <- chisq.test(contingency_table)
p_value_Travail <- chi2_result$p.value

#Foyer monop
df_filtered <- subset(results, !is.na(Foyer_monoparental))
contingency_table <- matrix(c(df_filtered$Group_0[1], df_filtered$Group_0[2], df_filtered$Group_1[1],df_filtered$Group_1[2]),nrow = 2,byrow = TRUE)
chi2_result <- chisq.test(contingency_table)
p_value_Foyer_monoparental <- chi2_result$p.value

#Education
df_filtered <- subset(results, !is.na(Education))
somme_autre <- df_filtered %>%
  filter(Variable != '2e ou 3e cycle universitaire, grande école') %>%  # Filtrer pour ne garder que les lignes où Variable n'est pas France
  summarise(
    Variable = 'Autre',  # Nommer la nouvelle ligne comme 'Autre'
    Group_0 = sum(Group_0, na.rm = TRUE),  # Somme des valeurs dans Group_0
    Group_1 = sum(Group_1, na.rm = TRUE)   # Somme des valeurs dans Group_1
  )
educ_rows <- df_filtered %>%
  filter(Variable == '2e ou 3e cycle universitaire, grande école')
df_combined <- bind_rows(somme_autre, educ_rows)
contingency_table <- matrix(c(df_combined $Group_0[1], df_combined$Group_0[2], df_combined$Group_1[1],df_combined$Group_1[2]),nrow = 2,byrow = TRUE)
chi2_result <- chisq.test(contingency_table)
p_value_educ <- chi2_result$p.value

results$Variable <- ifelse((results$Variable == "OUI"),("Aide alimentaire"),(results$Variable))
results$Variable <- ifelse((results$Variable == 1 & !is.na(results$ Foyer_monoparental)),("Foyer monoparental"),(results$Variable))

results <- subset(results, select = -c(Sexe, Quel.est.votre.pays.de.naissance.,  Aide_alim,
                                       Situation_emploi, Foyer_monoparental, Education))
results  <- results  %>%
  relocate(Variable, .before = everything())

results  <- results  %>%
  filter(Variable %in% c("Femme","France", 
                         "Occupe un emploi", "Inactif(ve)", "Etudiant(e)", "Retraité(e)", "Foyer monoparental", 
                         "2e ou 3e cycle universitaire, grande école","BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale",
                         "Baccalauréat", "Diplôme en dessous du baccalauréat", "Aucun diplôme", "Autre",
                         "Aide alimentaire"))

# print(unique(data$Education))
temp <- data.frame(
  Variable = c("Femme", "France", "Aide alimentaire", "Occupe un emploi", "Foyer monoparental", "2e ou 3e cycle universitaire, grande école"
  ),
  p_value = c(p_value_gender, p_value_pays, p_value_Aide_alim, p_value_Travail, p_value_Foyer_monoparental,
              p_value_educ))
results <- left_join(results, temp, by= "Variable")

#Renommer 
cont_means_long <- cont_means_long %>%
  rename(
    Control_group = groupe_0,
    Intervention_group = groupe_1)
temp <- cont_means_long[, !(colnames(cont_means_long) %in% "Difference")]

# Créer des colonnes vides avec NA
temp$Proportion_Control_group <- NA
temp$Proportion_Intervention_group<- NA

#Renommer
results <- results %>%
  rename(
    Control_group = "Group_0",
    Intervention_group = "Group_1" )
results <- results [, !(colnames(results ) %in% "Difference")]
results  <- rbind(results, temp)

results <- results %>%
  rename(
    "Control group (N)" = "Control_group",
    "Control group (%)" = "Proportion_Control_group",
    "Intervention group(N)" = "Intervention_group",
    "Intervention group (%)" = "Proportion_Intervention_group" )

results <- results
results[] <- lapply(results, function(x) if(is.numeric(x)) round(x, 2) else x)
results  <- results [c("Variable",
                       "Control group (N)",
                       "Control group (%)",
                       "Intervention group(N)",
                       "Intervention group (%)",
                       "p_value")]

# Calculer l'effectif total par groupe
group_counts <- data %>%
  group_by(groupe) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = groupe, values_from = Count, names_prefix = "groupe_")

# Ajouter la ligne au tableau final
group_totals <- tibble(
  Variable = "Total participants",
  `Control group (N)` = group_counts$groupe_0,
  `Intervention group(N)` = group_counts$groupe_1,
  `Control group (%)` = NA,
  `Intervention group (%)` = NA,
  p_value = NA
)

# Ajouter les effectifs totaux au tableau final
results <- bind_rows(results, group_totals)


results[[1]] <- gsub("[()]", "", results[[1]])
# Définir les traductions
translations <- c(
  "Femme" = "Gender-Women (%)",
  "France" = "Birth place - France (%)",
  "Aide alimentaire" = "Food aid (%)",
  "Etudiante" = "Student (%)",
  "Inactifve" = "Unemployed (%)",
  "Occupe un emploi" = "Employed (%)",
  "Retraitée" = "Retired (%)",
  "Foyer monoparental" = "Single-Parent Household (%)",
  "2e ou 3e cycle universitaire, grande école" = "Long-term studies (%)",
  "Aucun diplôme" = "No qualifications (%)",
  "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale" = "Short term studies (%)",
  "Baccalauréat" = "Baccalaureate (%)",
  "Diplôme en dessous du baccalauréat" = "Pre-baccalaureate diploma (%)",
  "Age_Central" = "Age (mean)",
  "Combien.de.personnes.vivent.dans.votre.foyer" = "Household composition (mean)",
  "Différentiel_aide" = "The amount of eligible products spent at T0 compared to the subsidie (mean)",
  "Part_Kcal_BIO" = "The amount in kilocalories of Organic food (%)",
  "Part_Kcal_Epiceries" = "The amount in kilocalories of Social grocery stores (%)",
  "Part_Kcal_Supermarchés"="The amount in kilocalories of Supermarket (%)",
  "Income_UC_INSEE" = "mean Income (€/CU/Month)"
)


# Supprimer les lignes contenant "FV_POIDS" ou "Autre" dans n'importe quelle colonne
results <- results[!apply(results, 1, function(row) any(grepl("FV_POIDS|Autre", row))), ]

# Définir l'ordre souhaité des lignes
desired_order <- c("Total participants",
                   "Femme",
                   "Age_Central",
                   "France",
                   "Combien.de.personnes.vivent.dans.votre.foyer",
                   "Foyer monoparental",
                   "Income_UC_INSEE",
                   "Occupe un emploi",
                   "Inactifve",
                   "Etudiante",
                   "Retraitée",
                   "2e ou 3e cycle universitaire, grande école",
                   "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale",
                   "Baccalauréat",
                   "Diplôme en dessous du baccalauréat",
                   "Aucun diplôme",
                   "Aide alimentaire",
                   "Part_Kcal_BIO",
                   "Part_Kcal_Epiceries",
                   "Part_Kcal_Supermarchés",
                   "Différentiel_aide"
)


# Réorganiser les lignes selon l'ordre souhaité
results <- results[match(desired_order, results$Variable), ]

results[, 3] <- ifelse(
  is.na(unlist(results[, 3])) | unlist(results[, 3]) == "",
  unlist(results[, 2]),
  unlist(results[, 3])
)

results[, 5] <- ifelse(
  is.na(unlist(results[, 5])) | unlist(results[, 5]) == "",
  unlist(results[, 4]),
  unlist(results[, 5])
)

# Supprimer les colonnes 2 et 3
results <- results[, -c(2)]
results <- results[, -c(3)]


# Suppression des lignes variables vides suite à la selection des données
results <- results %>% dplyr::filter(Variable != 'NA')

results<- as.data.frame(results)
# rownames(results) <- results$Variable
results <- results %>%
  as.data.frame() %>%
  column_to_rownames(., var = 'Variable')

tab_socio_demo <- results 


###PROP AU DESSUS DES RECOMMANDATION 
#SELECTION DU NOMBRE DE PERSONNES AVEC AU MOINS UNE PORTION EN PLUS
sgsdata_IT_filtered_bis <- sgsdata_INT %>% filter(Periode == 1)
#PROP DE FV DIFF
# Sous-ensemble des données pour Carnet
sgsdata_IT_filtered_IT <- sgsdata_IT_filtered_bis %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300")

# Sélection des groupes pour le filtrage
ids_groupe0 <- sgsdata_IT_filtered_IT %>% filter(groupe == 0, Periode == 1) %>% select(Identifiant) %>% distinct()
ids_groupe1_camp1 <- sgsdata_IT_filtered_IT %>% 
  filter(groupe == 1, Campagne == 1, utilisation2_FFQ == 1, limitation_FFQ == 1, Periode == 1) %>% 
  select(Identifiant) %>% distinct()
ids_groupe1_camp2 <- sgsdata_IT_filtered_IT %>% 
  filter(groupe == 1, Campagne == 2,
         Prop_montant_theorique_saisie >= 0.3, 
         Prop_montant_theorique_saisie <= 1.7,
         compliance >= 0.7, Periode == 1) %>% 
  select(Identifiant) %>% distinct()

ids_combines <- bind_rows(ids_groupe0, ids_groupe1_camp1, ids_groupe1_camp2)
sgsdata_IT_filtered_TR <- sgsdata_IT_filtered_IT %>% semi_join(ids_combines, by = "Identifiant")
sgsdata_IT_filtered_LE <- subset(sgsdata_IT_filtered_IT, voie_de_recrutement == "LE")

# Résumé pour le niveau "IT"
result_IT <- sgsdata_IT_filtered_IT %>%
  filter(groupe %in% c(0, 1)) %>%  # Conserver uniquement les groupes 0 et 1
  group_by(groupe) %>%
  summarise(
    nb_indiv_condition = sum(FV_POIDS_diff >= 0.08, na.rm = TRUE),
    total = n(),
    proportion = nb_indiv_condition / total
  ) %>%
  mutate(selection = "IT")

# Résumé pour le niveau "TR"
result_TR <- sgsdata_IT_filtered_TR %>%
  filter(groupe %in% c(0, 1)) %>% 
  group_by(groupe) %>%
  summarise(
    nb_indiv_condition = sum(FV_POIDS_diff >= 0.08, na.rm = TRUE),
    total = n(),
    proportion = nb_indiv_condition / total
  ) %>%
  mutate(selection = "TR")

# Résumé pour le niveau "LE"
result_LE <- sgsdata_IT_filtered_LE %>%
  filter(groupe %in% c(0, 1)) %>% 
  group_by(groupe) %>%
  summarise(
    nb_indiv_condition = sum(FV_POIDS_diff >= 0.08, na.rm = TRUE),
    total = n(),
    proportion = nb_indiv_condition / total
  ) %>%
  mutate(selection = "LE")

# Combinaison des trois résultats dans un même tableau
result_all <- bind_rows(result_IT, result_TR, result_LE, ) %>%
  select(selection, groupe, nb_indiv_condition, total, proportion)



###STAT FINALES ---------------------------------------------------------
# Sous-ensemble des données pour Carnet

sgsdata_IT <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300" & Campagne ==1  )

## Sélection des groupes pour le filtrage
#ids_groupe0 <- sgsdata_IT %>% filter(groupe == 0 ) %>% select(Identifiant) %>% distinct()
#ids_groupe1_camp1 <- sgsdata_IT %>% 
#  filter(groupe == 1, Campagne == 1, utilisation2_FFQ == 1, limitation_FFQ == 1 , Identifiant != "6348-Episourire") %>% 
#  select(Identifiant) %>% distinct()
#ids_groupe1_camp2 <- sgsdata_IT %>% 
#  filter(groupe == 1, Campagne == 2,
#         Prop_montant_theorique_saisie >= 0.3, 
#         Prop_montant_theorique_saisie <= 1.7,
#         compliance >= 0.7& Periode==1 ) %>% 
#  select(Identifiant) %>% distinct()
#
#ids_combines <- bind_rows(ids_groupe0, ids_groupe1_camp1, ids_groupe1_camp2)
#sgsdata_TR <- sgsdata_IT %>% semi_join(ids_combines, by = "Identifiant")

#sgsdata_LE <- subset(sgsdata_IT, voie_de_recrutement == "LE", Identifiant != "6348-Episourire")

sgsdata_TR <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300" & Campagne ==2  & voie_de_recrutement == "PS" )


sgsdata_LE <- sgsdata %>%
  filter(!is.na(Mesure) & Mesure == "Carnet" & Identifiant != "LE300" & Campagne ==2  & voie_de_recrutement == "LE" )


# 1) Créer deux listes de modèles, une pour TR et une pour LE
Poids_eligibleTous           <- feols(q0 ~ Traitement | Identifiant + Periode, data = sgsdata_IT,  vcov = ~Identifiant)
Prix_unitaire_eligibleTous   <- feols(p0 ~ Traitement| Identifiant + Periode, data = sgsdata_IT, vcov = ~Identifiant)
MTous   <- feols(M0 ~ Traitement| Identifiant + Periode, data = sgsdata_IT, vcov = ~Identifiant)

# 1) Créer deux listes de modèles, une pour TR et une pour LE
Poids_eligibleTR           <- feols(q0 ~ Traitement| Identifiant + Periode, data = sgsdata_TR, vcov = ~Identifiant)
Prix_unitaire_eligibleTR   <- feols( p0~ Traitement| Identifiant + Periode, data = sgsdata_TR , vcov = ~Identifiant)
MTR   <- feols(M0 ~ Traitement| Identifiant + Periode, data = sgsdata_TR, vcov = ~Identifiant)

Poids_eligibleLE           <- feols(q0 ~ Traitement| Identifiant + Periode, data = sgsdata_LE, vcov = ~Identifiant)
Prix_unitaire_eligibleLE   <- feols(p0 ~ Traitement| Identifiant + Periode, data = sgsdata_LE, vcov = ~Identifiant)
MLE   <- feols(M0 ~ Traitement| Identifiant + Periode, data = sgsdata_LE, vcov = ~Identifiant)
  


sgsdata_IT$Coeff_q0 <-   coef(Poids_eligibleTous)["Traitement"]
sgsdata_IT$Coeff_p0 <-   coef(Prix_unitaire_eligibleTous)["Traitement"]
sgsdata_IT$Coeff_M0 <-   coef(MTous)["Traitement"]

sgsdata_TR$Coeff_q0 <-   coef(Poids_eligibleTR)["Traitement"]
sgsdata_TR$Coeff_p0 <-   coef(Prix_unitaire_eligibleTR)["Traitement"]
sgsdata_TR$Coeff_M0 <-   coef(MTR)["Traitement"]

sgsdata_LE$Coeff_q0 <-   coef(Poids_eligibleLE)["Traitement"]
sgsdata_LE$Coeff_p0 <-   coef(Prix_unitaire_eligibleLE)["Traitement"]
sgsdata_LE$Coeff_M0 <-   coef(MLE)["Traitement"]


sgsdata_IT$DID_q0 <- sgsdata_IT$q0 - sgsdata_IT$Coeff_q0
sgsdata_IT$DID_p0 <- sgsdata_IT$p0 - sgsdata_IT$Coeff_p0
sgsdata_IT$DID_M0 <- sgsdata_IT$M0 - sgsdata_IT$Coeff_M0

sgsdata_TR$DID_q0 <- sgsdata_TR$q0 - sgsdata_TR$Coeff_q0
sgsdata_TR$DID_p0 <- sgsdata_TR$p0 - sgsdata_TR$Coeff_p0
sgsdata_TR$DID_M0 <- sgsdata_TR$M0 - sgsdata_TR$Coeff_M0

sgsdata_LE$DID_q0 <- sgsdata_LE$q0 - sgsdata_LE$Coeff_q0
sgsdata_LE$DID_p0 <- sgsdata_LE$p0 - sgsdata_LE$Coeff_p0
sgsdata_LE$DID_M0 <- sgsdata_LE$M0 - sgsdata_LE$Coeff_M0



#CALCUL EPI /EQI
#CALCUL DES STATS EASY -----------------------------

# 1. Regrouper les 3 jeux de données dans une liste nommée
list_data <- list(
  Tous = sgsdata_IT,
  TR   = sgsdata_TR,
  LE   = sgsdata_LE
)

# 2. Fonction enrichie pour calculer et renvoyer TOUTES les stats
calc_did <- function(data, label) {
  # moyennes par (groupe, Periode)
  stats_qp <- data %>%
    group_by(groupe, Periode) %>%
    summarise(
      Coeff_q0 = first(Coeff_q0),
      Coeff_p0 = first(Coeff_p0),
      Coeff_M0 = first(Coeff_M0),
      mean_M0 = mean(M0),
      mean_q0 = mean(q0),
      mean_p0 = mean(p0), 
      Cheque_UC = mean(cheque_theo / UC_TI_arrondi),
      Compliance_booklet = mean(compliance),
      Prop_NR = mean(pourcentage_remboursé_avant_la_fin),
      
      DID_q0= mean(DID_q0),
      DID_p0= mean(DID_p0),
      DID_M0= mean(DID_M0),
      Cov_EP = cov(
        (Coeff_p0*(q0-Coeff_q0)),
        Coeff_p0*Coeff_q0/((q0-Coeff_q0)*Coeff_p0 + (p0-Coeff_p0)*Coeff_q0)),
      
      Cov_EQ = cov(
        (Coeff_q0*(p0-Coeff_p0)),
        Coeff_p0*Coeff_q0/((q0-Coeff_q0)*Coeff_p0 + (p0-Coeff_p0)*Coeff_q0)),
      
      Cov_M1 = cov( p0, q0,),
      Cov_p1 = cov(M0, q0),
      Cov_q1 = cov(M0, p0)
    )
  
  
  # extraction des 4 stats q
  qT_pre  <- stats_qp %>% filter(groupe == 1, Periode == 0) %>% pull(mean_q0)
  qT_post <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(mean_q0)
  qC_pre  <- stats_qp %>% filter(groupe == 0, Periode == 0) %>% pull(mean_q0)
  qC_post <- stats_qp %>% filter(groupe == 0, Periode == 1) %>% pull(mean_q0)
  
  # extraction des 4 stats p
  pT_pre  <- stats_qp %>% filter(groupe == 1, Periode == 0) %>% pull(mean_p0)
  pT_post <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(mean_p0)
  pC_pre  <- stats_qp %>% filter(groupe == 0, Periode == 0) %>% pull(mean_p0)
  pC_post <- stats_qp %>% filter(groupe == 0, Periode == 1) %>% pull(mean_p0)
  
  M0T_pre  <- stats_qp %>% filter(groupe == 1, Periode == 0) %>% pull(mean_M0)
  M0T_post <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(mean_M0)
  M0C_pre  <- stats_qp %>% filter(groupe == 0, Periode == 0) %>% pull(mean_M0)
  M0C_post <- stats_qp %>% filter(groupe == 0, Periode == 1) %>% pull(mean_M0)
 

  Cov_EP <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Cov_EP)
  Cov_EQ <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Cov_EQ)
  Cov_M1 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Cov_M1) 
  Cov_p1 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull( Cov_p1)
  Cov_q1 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Cov_q1)
  
  Coeff_q0 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Coeff_q0)
  Coeff_p0 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Coeff_p0)
  Coeff_M0 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Coeff_M0)
  
  DID_q0 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(DID_q0)
  DID_p0 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(DID_p0)
  DID_M0 <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(DID_M0)

  MCHEQUE <- stats_qp %>% filter(groupe == 1, Periode == 0) %>% pull(Cheque_UC)
  Compliance_booklet <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Compliance_booklet)
  Prop_NR <- stats_qp %>% filter(groupe == 1, Periode == 1) %>% pull(Prop_NR)

  # calcul des contrefactuels DID
  
  q0_DIDbis <- qT_pre  + (qC_post - qC_pre)
  p0_DIDbis <- pT_pre  + (pC_post - pC_pre)
  M0_DIDbis <- M0T_pre  + (M0C_post - M0C_pre)
  
  # on renvoie TOUTES les valeurs dans un tibble
  tibble(
    Groupe              = label,Coeff_q0, Coeff_p0, Coeff_M0,
    qT_pre ,qT_post,qC_pre , qC_post,  pT_pre ,pT_post,pC_pre,
    pC_post,M0T_pre, M0T_post,M0C_pre,M0C_post,MCHEQUE,
    Compliance_booklet,Prop_NR,DID_q0 ,DID_p0, DID_M0,q0_DIDbis,p0_DIDbis,M0_DIDbis,
   Cov_EP, Cov_EQ ,   Cov_M1 , Cov_p1 , 
   Cov_q1 
   
    
  )
}


# 3. Appliquer à chaque sous-jeu et obtenir un data.frame de 3 lignes
did_refs <- imap_dfr(list_data, calc_did)

# 4. Joindre avec tableau final (qui a colonne Groupe = "Tous"/"TR"/"LE")
resultats <- did_refs

#TABLEAU DES DIFF--------------------------------



# Liste des filtres à appliquer
filter_conditions <- list(
  "Tous" = sgsdata_IT,
  "TR" = sgsdata_TR,
  "LE" = sgsdata_LE
)


# Initialisation d'un tableau vide pour stocker les résultats
final_results <- data.frame()

# Boucle sur les filtres
for (filter_name in names(filter_conditions)) {
  subset_data <- filter_conditions[[filter_name]]
  
  if (nrow(subset_data) > 0) {
    fixed_models <- list(
      "Prix tot eligible" = feols(M0 ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Poids_eligible"= feols( q0 ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant),
      "Prix_unitaire_eligible"= feols(p0 ~ Traitement | Identifiant + Periode, data = subset_data, vcov = ~Identifiant)
      
    )
    
    # Récupération des résultats sous forme de data.frame
    model_summary <- modelsummary(fixed_models, output = "data.frame", 
                                  gof_omit = "LogLik|Deviance|Adj.R2|FE|R2 Within|Std.Errors",
                                  statistic = c("std.error", "p.value"), fmt = "%.2g")
    # Ajout du nom du filtre
    model_summary$Selection <- filter_name
    final_results <- bind_rows(final_results, model_summary)
  }
}

# Nettoyage des résultats
terms_to_exclude <- c("Intercept", "Identifiant", "Temps", "Observations", "Std.Errors",
                      "ICC", "R2 Adj.", "R2 Cond.", "R2", "AIC", "BIC", "RMSE")
final_results <- final_results[!grepl(paste(terms_to_exclude, collapse = "|"), final_results$term), ]
final_results <- final_results %>% filter(part != "gof")
final_results <- final_results %>% 
  mutate(statistic = paste0(statistic, "_", Selection)) %>% 
  select(-part, -term, -Selection)

# Transposition et fusion de la première ligne en en-tête
model_summary <- as.data.frame(t(final_results))
new_header <- model_summary[1, ]
model_summary <- model_summary[-1, ]
colnames(model_summary) <- apply(new_header, 2, paste, collapse = " ")

# Création de la colonne Variable_Dépendante
dep_vars <- sapply(fixed_models, function(model) all.vars(formula(model))[1])
dep_vars_df <- data.frame(Variable_Dépendante = dep_vars, row.names = NULL)
if (nrow(model_summary) == length(dep_vars)) {
  model_summary <- cbind(dep_vars_df, model_summary)
} else {
  warning("Problème de correspondance entre model_summary et dep_vars_df")
}

# Nettoyage des valeurs et suppression du suffixe si présent
model_summary[] <- lapply(model_summary, function(x) { if(is.character(x)) gsub("[()]", "", x) else x })
model_summary$Variable_Dépendante <- sub("_diff$", "", model_summary$Variable_Dépendante)

# Sauvegarde du tableau final pour l'analyse Carnet
models_carnets_analyse <- model_summary


# TELECHARGEMENT ----------------------------

# Liste de vos dataframes
df_list <- list(
  tab_socio_demo = tab_socio_demo,
  models_carnets   = models_carnets,
  cohen_results    = cohen_results,
  models_ffq       = models_ffq,
  models_diff      = models_diff,
  models_inc       = models_inc,
  models_inc2       = models_inc2,
  
  result_TOT    = result_TOT,
  HENI_groupe      = models_carnets_HENI,
  result_TRUST_bis  = result_TRUST,
  result_AC_bis = result_AC,
  resultats = resultats,
  Atteinte_reco = result_all,
  models_carnets_analyse = models_carnets_analyse,
  result_TOT_TRUST = result_TOT_TRUST,
  result_TOT_AC =result_TOT_AC
)

# Fonction qui tente de convertir une colonne en numérique si possible
convert_to_numeric <- function(x) {
  # Si la colonne est character ou factor, tenter la conversion
  if (is.character(x) || is.factor(x)) {
    num <- suppressWarnings(as.numeric(as.character(x)))
    # Si au moins une valeur non-NA après conversion, on considère la conversion réussie
    if (sum(!is.na(num)) > 0) {
      return(num)
    } else {
      return(x)  # sinon, on garde la colonne originale
    }
  } else {
    return(x)  # sinon, on ne fait rien
  }
}

# Appliquer la conversion à chaque dataframe de la liste
df_list <- lapply(df_list, function(df) {
  df[] <- lapply(df, convert_to_numeric)
  return(df)
})

# Réassigner les dataframes convertis à leurs noms d'origine
tab_socio_demo  <- df_list$tab_socio_demo
models_carnets    <- df_list$models_carnets
cohen_results     <- df_list$cohen_results
models_ffq        <- df_list$models_ffq
models_diff       <- df_list$models_diff
models_inc        <- df_list$models_inc
models_inc2        <- df_list$models_inc2

result_TOT     <- df_list$result_TOT
models_carnets_HENI<- df_list$HENI_groupe
result_TRUST <-  df_list$result_TRUST_bis 
result_AC <-  df_list$result_AC_bis
result_all <-  df_list$Atteinte_reco 
resultats <- df_list$resultats
models_carnets_analyse <- df_list$models_carnets_analyse
result_TOT_TRUST<- df_list$result_TOT_TRUST 
result_TOT_AC<- df_list$result_TOT_AC


# Puis vous pouvez écrire vos dataframes convertis dans le workbook
options(scipen = 999)
wb <- createWorkbook()

addWorksheet(wb, "tab_socio_demo")
writeData(wb, sheet = "tab_socio_demo", tab_socio_demo, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "models_carnets")
writeData(wb, sheet = "models_carnets", models_carnets, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "cohen_results")
writeData(wb, sheet = "cohen_results", cohen_results, rowNames = TRUE)

addWorksheet(wb, "models_ffq")
writeData(wb, sheet = "models_ffq", models_ffq, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "models_diff")
writeData(wb, sheet = "models_diff", models_diff, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "models_inc")
writeData(wb, sheet = "models_inc", models_inc, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "models_inc2")
writeData(wb, sheet = "models_inc2", models_inc2, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "predict_table")
writeData(wb, sheet = "predict_table", result_TOT, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "HENI_groupe")
writeData(wb, sheet = "HENI_groupe", models_carnets_HENI , colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "result_AC")
writeData(wb, sheet = "result_AC", result_AC , colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "result_TRUST")
writeData(wb, sheet = "result_TRUST", result_TRUST , colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "Atteinte_reco")
writeData(wb, sheet = "Atteinte_reco", result_all, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "resultats")
writeData(wb, sheet = "resultats", resultats, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "models_carnets_analyse")
writeData(wb, sheet = "models_carnets_analyse", models_carnets_analyse, colNames = TRUE, rowNames = TRUE)


addWorksheet(wb, "result_TOT_TRUST")
writeData(wb, sheet = "result_TOT_TRUST",result_TOT_TRUST, colNames = TRUE, rowNames = TRUE)


addWorksheet(wb, "result_TOT_AC")
writeData(wb, sheet = "result_TOT_AC",result_TOT_AC, colNames = TRUE, rowNames = TRUE)



saveWorkbook(wb, "resultats_auto.xlsx", overwrite = TRUE)

saveWorkbook(wb,(paste0("Données analysées - Article N°1 chèques/Publis/resultats_auto.xlsx")))

