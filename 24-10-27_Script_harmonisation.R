#Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")


#Nov 22
Carnet_nov_22<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/Carnets_Tableaux_nov_22.xlsx", sep="")))
FFQ_nov_22<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/FFQ_Tableaux_nov_22.xlsx", sep="")))
FFQ_nov_22 <- FFQ_nov_22[!duplicated(FFQ_nov_22$Identifiant), ]

#Mars23
Carnet_mars_23<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/Carnets_Tableaux_mars_23.xlsx", sep="")))
FFQ_mars_23<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/FFQ_Tableaux_mars_23.xlsx", sep="")))
FFQ_mars_23 <- FFQ_mars_23[!duplicated(FFQ_mars_23$Identifiant), ]

#Nov23
Carnet_nov_23<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/Carnets_Tableaux_nov_23.xlsx", sep="")))
FFQ_nov_23<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/FFQ_Tableaux_nov_23.xlsx", sep="")))
FFQ_nov_23 <- FFQ_nov_23[!duplicated(FFQ_nov_23$Identifiant), ]

#Mars24
Carnet_mars_24<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/Carnets_Tableaux_mars_24.xlsx", sep="")))
FFQ_mars_24<- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichiers_prétraités/FFQ_Tableaux_mars_24.xlsx", sep="")))
FFQ_mars_24 <- FFQ_mars_24[!duplicated(FFQ_mars_24$Identifiant), ]

#Annexe
Revenu_lime_survey <- read.xlsx((paste("Données analysées - Article N°1 chèques/Tableaux_annexes/Revenus.xlsx", sep="")))


#Suppression des Opticourses
##SUPPRESSION DES OPTICOURSES
# Supprimer les identifiants qui ressortent avec un message d'alerte dans resultats_codachats

identifiants <- c("LE012", "LE017", "LE021", "LE028", "LE037", "LE040", "LE043", "LE045", "LE049", "LE058", 
                  "LE059", "LE064", "LE068", "LE076", "LE077", "LE083", "LE086", "LE087", "LE099", "LE129", 
                  "LE130", "LE142", "LE146", "LE147", "LE149", "LE152", "LE158", "LE163", "LE169", "LE170", 
                  "LE176", "LE177", "LE184", "LE191", "LE205", "PS287", "LE208", "LE210", "LE224", "LE232", 
                  "LE246", "LE249", "PS001", "PS003", "PS014", "PS016", "PS023", "PS026", "PS041", "PS044", 
                  "PS046", "PS049", "PS058", "PS059", "PS061", "PS072", "PS075", "PS094", "PS104", "PS106", 
                  "PS110", "PS116", "PS137", "PS143", "PS158", "PS164", "PS165", "PS168", "PS169", "PS178", 
                  "PS180", "PS190", "PS193", "PS203", "PS204", "PS206", "PS207", "PS210", "PS215", "PS218", 
                  "PS221", "PS237", "PS259", "PS265", "PS269", "PS272", "PS277", "PS282")

FFQ_nov_23 <- subset(FFQ_nov_23, !(Identifiant %in% identifiants))
FFQ_mars_24 <- subset(FFQ_mars_24, !(Identifiant %in% identifiants))


#Fusion et Harmonisation des FFQ---------------------------------------------------
  #Fusion des tableaux de novembre
FFQ_NOV <- rbind(FFQ_nov_22, FFQ_nov_23)

##Imputation du diplome au tableau de mars ------------------------------
temp  <- FFQ_NOV[, c("Identifiant", "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.")]
FFQ_mars_24  <- left_join(FFQ_mars_24, temp, by="Identifiant")

names(FFQ_mars_24)[ncol(FFQ_mars_24)] <- "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu"
FFQ_mars_24 <- FFQ_mars_24[, -c(which(names(FFQ_mars_24) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu..x"))] #colonne en doublon vide et .n, aussi
FFQ_mars_23 <- FFQ_mars_23[, -c(which(names(FFQ_mars_23) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu."))] #colonne en doublon vide et .n, aussi

##Fusion des tableaux de mars ----------------------------------
FFQ_MARS <- rbind(FFQ_mars_23, FFQ_mars_24)

#Déplacement du diplome après la part de budget dédiée au BIO 
FFQ_MARS <- FFQ_MARS %>% relocate(Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu, .before = Part.de.votre.budget.alimentaire.consacree.aux.produits.alimentaires.biologiques.)
names(FFQ_MARS)[9] <- "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu."

  ##Harmonisation des pays d'origine pour le tableau de mars-----------------------------------
temp  <- FFQ_NOV[, c("Identifiant", "Quel.est.votre.pays.de.naissance.")]
FFQ_MARS <- left_join(FFQ_MARS, temp, by="Identifiant")
FFQ_MARS$Quel.est.votre.pays.de.naissance..x <- ifelse((is.na(FFQ_MARS$Quel.est.votre.pays.de.naissance..x)),(FFQ_MARS$Quel.est.votre.pays.de.naissance..y), (FFQ_MARS$Quel.est.votre.pays.de.naissance..x))
FFQ_MARS <- FFQ_MARS[, -c(which(names(FFQ_MARS) == "Quel.est.votre.pays.de.naissance..y"))] #colonne en doublon vide et .n, aussi
names(FFQ_MARS)[4] <- "Quel.est.votre.pays.de.naissance."

  ##Harmonisation des colonnes aide alimentaire  pour mars ---------------------------------------------
temp  <- FFQ_NOV[, c("Identifiant", "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois.")]
FFQ_MARS <- left_join(FFQ_MARS, temp, by="Identifiant")
FFQ_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..x <- ifelse((is.na(FFQ_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..x)),(FFQ_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..y), (FFQ_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..x))
FFQ_MARS <- FFQ_MARS[, -c(which(names(FFQ_MARS) == "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..y"))] #colonne en doublon vide et .n, aussi
names(FFQ_MARS)[8] <- "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois."

  ##Imputation du groupe à FFQ NOV -----------------------------------------------------
temp  <- FFQ_MARS[, c("Identifiant", "groupe")]
FFQ_NOV <- left_join(FFQ_NOV, temp, by="Identifiant")
FFQ_NOV <- FFQ_NOV %>% relocate(groupe, .before = Periode)

colnames_nov <- colnames(FFQ_NOV)
colnames_mars <- colnames(FFQ_MARS)

#MODIF DE FFQ NOV 
temp  <- FFQ_MARS[, c("Identifiant", "Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.", "Les.avez.vous.utilises.", "Qu.avez.vous.achete.avec.",
                         "Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.","Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.")]


colonnes_a_supprimer <- c("Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.", "Les.avez.vous.utilises.", "Qu.avez.vous.achete.avec.",
                          "Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.","Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.")
FFQ_NOV <- FFQ_NOV[ , !(names(FFQ_NOV) %in% colonnes_a_supprimer)]


FFQ_NOV <- left_join(FFQ_NOV, temp, by="Identifiant")


  ## Verification que les noms des FFQ sont les mêmes et que les colonnes sont à la même place entre FFQ NOV et MARS ------------------------------------------------------------
# Colonnes présentes dans FFQ_NOV mais pas dans FFQ_MARS
diff_in_nov <- setdiff(colnames_nov, colnames_mars)

# Colonnes présentes dans FFQ_MARS mais pas dans FFQ_NOV
diff_in_mars <- setdiff(colnames_mars, colnames_nov)

# Afficher les noms des colonnes différentes
if (length(diff_in_nov) > 0) {
  cat("Colonnes présentes dans FFQ_NOV mais pas dans FFQ_MARS :\n")
  print(diff_in_nov)
} else {
  cat("Toutes les colonnes de FFQ_NOV sont également présentes dans FFQ_MARS.\n")
}

if (length(diff_in_mars) > 0) {
  cat("\nColonnes présentes dans FFQ_MARS mais pas dans FFQ_NOV :\n")
  print(diff_in_mars)
} else {
  cat("\nToutes les colonnes de FFQ_MARS sont également présentes dans FFQ_NOV.\n")
}

index_nov <- seq_along(colnames_nov)
index_mars <- seq_along(colnames_mars)
# Identifier les indices des colonnes avec des emplacements différents
diff_indices <- which(index_nov != index_mars)

if (length(diff_indices) > 0) {
  cat("Colonnes avec des emplacements différents entre FFQ_NOV et FFQ_MARS :\n")
  for (i in diff_indices) {
    cat("Index :", i, "\n")
    cat("  FFQ_NOV :", colnames_nov[i], "\n")
    cat("  FFQ_MARS :", colnames_mars[i], "\n")
  }
} else {
  cat("Toutes les colonnes de FFQ_NOV et FFQ_MARS ont les mêmes emplacements.\n")
}

#Fusion et harmonisation des carnets 
  ##Fusion des tableaux de novembre--------------------------
colonnes_nov_22 <- names(Carnet_nov_22)
colonnes_nov_23 <- names(Carnet_nov_23)
# Colonnes présentes dans Carnet_nov_22 mais pas dans Carnet_nov_23
colonnes_uniques_22 <- setdiff(colonnes_nov_22, colonnes_nov_23)
# Colonnes présentes dans Carnet_nov_23 mais pas dans Carnet_nov_22
colonnes_uniques_23 <- setdiff(colonnes_nov_23, colonnes_nov_22)
Carnet_nov_23$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois. <- NA

#Carnet_nov_22$NA_prix_kg <- NULL

# Maintenant, rbind devrait fonctionner
Carnet_NOV <- rbind(Carnet_nov_22, Carnet_nov_23)


 ##Ajout du diplome au tableau de mars ------------------------------
temp  <- Carnet_NOV[, c("Identifiant", "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.")]
Carnet_mars_24$Identifiant <- as.character(Carnet_mars_24$Identifiant)
Carnet_mars_24  <- left_join(Carnet_mars_24, temp, by="Identifiant")

#Suppression des colonnes en trop associée à l'ajout des données de SP041
Carnet_mars_23$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu <- ifelse(is.na(Carnet_mars_23$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.),(Carnet_mars_23$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu), (Carnet_mars_23$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))
Carnet_mars_23$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu <- NULL
Carnet_mars_23$Quel.est.votre.pays.de.naissance.<- NULL

  ##Fusion des tableaux de mars ----------------------------------
##Fusion des tableaux de novembre--------------------------
colonnes_mars_23 <- names(Carnet_mars_23)
colonnes_mars_24 <- names(Carnet_mars_24)
# Colonnes présentes dans Carnet_mars_23 mais pas dans Carnet_mars_24
colonnes_uniques_24 <- setdiff(colonnes_mars_23, colonnes_mars_24)
# Colonnes présentes dans Carnet_mars_24 mais pas dans Carnet_mars_23
colonnes_uniques_23 <- setdiff(colonnes_mars_24, colonnes_mars_23)

Carnet_MARS <- rbind(Carnet_mars_23, Carnet_mars_24)
cols_23 <- names(Carnet_mars_23)
cols_24 <- names(Carnet_mars_24)


  ##Déplacement du diplome après la part de budget dédiée au BIO --------------
Carnet_MARS <- Carnet_MARS %>% relocate(Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu., .before = Part.de.votre.budget.alimentaire.consacree.aux.produits.alimentaires.biologiques.)
#Carnet_MARS <- Carnet_MARS[, -c(which(names(Carnet_MARS) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu."))] #colonne en doublon vide et .n, aussi
#names(Carnet_MARS)[9] <- "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu."
#Carnet_NOV<- Carnet_NOV[, -c(which(names(Carnet_NOV) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.n"))] #colonne en doublon vide et .n, aussi

  ##Harmonisation des pays d'origine pour le tableau de mars-----------------------------------
temp  <- Carnet_NOV[, c("Identifiant", "Quel.est.votre.pays.de.naissance.")]
Carnet_MARS$Identifiant <- as.character(Carnet_MARS$Identifiant)
Carnet_MARS <- left_join(Carnet_MARS, temp, by="Identifiant")
#Carnet_MARS <- Carnet_MARS[, -c(which(names(Carnet_MARS) == "Quel.est.votre.pays.de.naissance..y"))] #colonne en doublon vide et .n, aussi
#names(Carnet_MARS)[4] <- "Quel.est.votre.pays.de.naissance."
#Carnet_MARS$Quel.est.votre.pays.de.naissance.
  ##Harmonisation des colonnes aide alimentaire  pour mars ---------------------------------------------
temp  <- Carnet_NOV[, c("Identifiant", "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois.")]
Carnet_MARS <- left_join(Carnet_MARS, temp, by="Identifiant")
#Carnet_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..x <- ifelse((is.na(Carnet_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..x)),(Carnet_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..y), (Carnet_MARS$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..x))
#Carnet_MARS <- Carnet_MARS[, -c(which(names(Carnet_MARS) == "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois..y"))] #colonne en doublon vide et .n, aussi
#names(Carnet_MARS)[8] <- "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois."

  ##Ajout colonne différentiel aide  pour mars ---------------------------------------------
temp  <- Carnet_NOV[, c("Identifiant", "Différentiel_aide", "cheque_theo")]
Carnet_MARS <- left_join(Carnet_MARS, temp, by="Identifiant")
Carnet_MARS  <- Carnet_MARS  %>%
  relocate(Différentiel_aide, .after = Mesure) 

Carnet_MARS$somme_montant_cheques <- ifelse(is.na(Carnet_MARS$somme_montant_cheques),0, Carnet_MARS$somme_montant_cheques)

  ##Ajout colonne groupe et compliant pour Nov ---------------------------------------------
temp  <- Carnet_MARS[, c("Identifiant", "groupe", "compliance","somme_montant_cheques",
                         "Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.", "Les.avez.vous.utilises.", "Qu.avez.vous.achete.avec.",
                         "Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.","Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.")]


colonnes_a_supprimer <- c("Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.", "Les.avez.vous.utilises.", "Qu.avez.vous.achete.avec.",
                          "Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.","Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.")
Carnet_NOV <- Carnet_NOV[ , !(names(Carnet_NOV) %in% colonnes_a_supprimer)]

Carnet_NOV <- left_join(Carnet_NOV, temp, by="Identifiant")

Carnet_MARS  <- Carnet_MARS  %>%
  relocate(groupe, compliance, .before = Periode) 

#Harmonisation des poids 
FFQ_NOV <- FFQ_NOV %>%
  rename_with(~ gsub("_FFQ", "_POIDS", .x), contains("_FFQ"))
FFQ_MARS <- FFQ_MARS %>%
  rename_with(~ gsub("_FFQ", "_POIDS", .x), contains("_FFQ"))
Carnet_NOV <- Carnet_NOV %>%
  rename_with(~ gsub("_CARNET", "_POIDS", .x), contains("_CARNET"))
Carnet_MARS <- Carnet_MARS%>%
  rename_with(~ gsub("_CARNET", "_POIDS", .x), contains("_CARNET"))

FFQ_NOV <- FFQ_NOV %>% rename(SOMME_POIDS = SOMME_POIDS_POIDS)
FFQ_MARS <- FFQ_MARS%>% rename(SOMME_POIDS = SOMME_POIDS_POIDS)
Carnet_NOV <- Carnet_NOV%>% rename(SOMME_POIDS = SOMME_POIDS_POIDS)
Carnet_MARS <- Carnet_MARS%>% rename(SOMME_POIDS = SOMME_POIDS_POIDS)

#Harmonisation entre carnets et FFQ -------------------------
  ## FFQ  Ajout des colonnes ""Montant.F&L" ,"Montant.Lég.secs" ,"Montant.mensuel.total" ,"Différentiel_aide" ,"compliant"--------------------------
temp  <- Carnet_NOV[, c("Identifiant", "Montant.mensuel.total", "cheque_theo","Différentiel_aide", "compliance", "somme_montant_cheques")]
FFQ_NOV <- left_join(FFQ_NOV, temp, by="Identifiant")
FFQ_MARS <- left_join(FFQ_MARS, temp, by="Identifiant")



##Retire de MARS toutes les lignes qui n'apparaissent pas en NOV
    ###supprimer les id de carnet mars qui ne se retouvent pas dans les carnets de novembre------------
initial_ids <- Carnet_MARS$Identifiant
Carnet_MARS <- Carnet_MARS %>%
  semi_join(Carnet_NOV, by = "Identifiant")
removed_ids <- setdiff(initial_ids, Carnet_MARS$Identifiant)

#print(unique(Carnet_MARS$Identifiant))

    ###supprimer les id de carnet novembre qui ne se retouvent pas dans les carnets de mars-----------------
initial_ids <- Carnet_NOV$Identifiant
Carnet_NOV <- Carnet_NOV %>%
  semi_join(Carnet_MARS, by = "Identifiant")
removed_ids <- setdiff(initial_ids, Carnet_NOV$Identifiant)





# Filtres pour exclure les FFQ abérrants----------------------------
initial_ids <- FFQ_NOV$Identifiant
FFQ_NOV<- FFQ_NOV %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ_NOV <- FFQ_NOV %>%
  filter(SOMME_POIDS_KCAL >= borne_inf & SOMME_POIDS_KCAL <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_NOV$Identifiant)
#20 CAMPAGNE 1
#7 LE
#8 PS
35
initial_ids <- FFQ_MARS$Identifiant
FFQ_MARS<- FFQ_MARS %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ_MARS <- FFQ_MARS %>%
  filter(SOMME_POIDS_KCAL >= borne_inf & SOMME_POIDS_KCAL <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)
#11 campagne 1
#3 LE

#supprimer les id de FFQ mars qui ne se retouvent pas dans les FFQ de novembre------------
  initial_ids <- FFQ_MARS$Identifiant
  
  FFQ_MARS <- FFQ_MARS %>%
  semi_join(FFQ_NOV, by = "Identifiant")
  removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)

###supprimer les id de Carnet novembre qui ne se retouvent pas dans les Carnet de mars-----------------
FFQ_NOV <- FFQ_NOV %>%
  semi_join(FFQ_MARS, by = "Identifiant")

print(unique(Carnet_NOV$Identifiant))

FFQ_NOV$borne_inf  <- NULL
FFQ_MARS$borne_inf <- NULL
FFQ_NOV$borne_sup  <- NULL
FFQ_MARS$borne_sup <- NULL

#FFQ_NOV_group1_camp1 <- sgsdata_FFQ %>%
#  filter(Campagne == 1, Periode ==1,groupe == 0)

#Ajout de colonnes supplémentaires ----------------------------------------------
  ##Ajout d'une colonne camppagne -----------------------------------------------
Carnet_NOV <- Carnet_NOV %>%
  mutate(Campagne = ifelse(substr(Identifiant, 1, 2) %in% c("PS", "LE"), 2, 1))
Carnet_MARS <- Carnet_MARS %>%
  mutate(Campagne = ifelse(substr(Identifiant, 1, 2) %in% c("PS", "LE"), 2, 1))
FFQ_NOV <- FFQ_NOV %>%
  mutate(Campagne = ifelse(substr(Identifiant, 1, 2) %in% c("PS", "LE"), 2, 1))
FFQ_MARS <- FFQ_MARS %>%
  mutate(Campagne = ifelse(substr(Identifiant, 1, 2) %in% c("PS", "LE"), 2, 1))


#Ajout d'une colonne recrutement 
  ##Ajout d'une colonne recrutement -----------------
    ###Pour les carnets de Nov---------------------------------------------------------
Carnet_NOV <- Carnet_NOV %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques
    ###Pour les carnets de Mars----------------------------------------------
Carnet_MARS <- Carnet_MARS %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques
    ###Pour les FFQ de Nov-----------------------------
FFQ_NOV <- FFQ_NOV %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques

    ####Pour les FFQ de Mars -------------------------------------------------
FFQ_MARS <- FFQ_MARS %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques

FFQ_MARS <- FFQ_MARS %>%
  mutate(voie_de_recrutement = gsub("[0-9]", "", Identifiant)) %>%  # Suppression des chiffres
  mutate(voie_de_recrutement = gsub("(^-|-$)|(?<![A-Za-z])-|-(?![A-Za-z])", "", voie_de_recrutement, perl=TRUE))  # Suppression des tirets non entourés de caractères non numériques


## Unification des dataframes carnets -------------------------------------------------------------
# Convertir la colonne en character dans les deux dataframes
Carnet_NOV$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment. <- as.character(Carnet_NOV$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.)
Carnet_MARS$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment. <- as.character(Carnet_MARS$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.)
Carnet_NOV$Les.avez.vous.utilises. <- as.character(Carnet_NOV$Les.avez.vous.utilises.)
Carnet_MARS$Les.avez.vous.utilises.<- as.character(Carnet_MARS$Les.avez.vous.utilises.)
Carnet_NOV$Qu.avez.vous.achete.avec. <- as.character(Carnet_NOV$Qu.avez.vous.achete.avec.)
Carnet_MARS$Qu.avez.vous.achete.avec.<- as.character(Carnet_MARS$Qu.avez.vous.achete.avec.)
Carnet_NOV$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits. <-  as.character(Carnet_NOV$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.)
Carnet_MARS$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.<- as.character(Carnet_MARS$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.)
Carnet_NOV$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits..1 <- NA
Carnet_MARS$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits..1 <-NA
Carnet_NOV$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.  <- as.character(Carnet_NOV$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.)
Carnet_MARS$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises. <- as.character(Carnet_MARS$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.)

sgsdata_Carnets <- bind_rows(Carnet_NOV, Carnet_MARS)
setdiff(names(Carnet_NOV), names(Carnet_MARS))
setdiff(names(Carnet_MARS), names(Carnet_NOV))


test <- sgsdata_Carnets %>%
  group_by(Identifiant) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# Unification des dataframes FFQ
FFQ_NOV$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment. <- as.character(FFQ_NOV$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.)
FFQ_MARS$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment. <- as.character(FFQ_MARS$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.)
FFQ_NOV$Les.avez.vous.utilises. <- as.character(FFQ_NOV$Les.avez.vous.utilises.)
FFQ_MARS$Les.avez.vous.utilises.<- as.character(FFQ_MARS$Les.avez.vous.utilises.)
FFQ_NOV$Qu.avez.vous.achete.avec. <- as.character(FFQ_NOV$Qu.avez.vous.achete.avec.)
FFQ_MARS$Qu.avez.vous.achete.avec.<- as.character(FFQ_MARS$Qu.avez.vous.achete.avec.)
FFQ_NOV$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits. <- as.character(FFQ_NOV$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.)
FFQ_MARS$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.<- as.character(FFQ_MARS$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.)
FFQ_NOV$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits..1 <- NA
FFQ_MARS$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits..1 <- NA
FFQ_NOV$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises. <- as.character(FFQ_NOV$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.)
FFQ_MARS$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises. <- as.character(FFQ_MARS$Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.)
sgsdata_FFQ <- bind_rows(FFQ_NOV, FFQ_MARS) 

# Remplacer revenu_mensuel à la période 1 par la valeur de la période 0 si revenu_mensuel = 0

#Si toujours 0 attribuer le revenu par UC du recrutement 
sgsdata_Carnets <- sgsdata_Carnets%>%
  left_join(Revenu_lime_survey, by = "Identifiant")  # Utilise left_join() pour garder toutes les lignes de carnet

#sgsdata_Carnets$Revenu.mensuel <- sgsdata_Carnets$Revenu_final
sgsdata_Carnets$Revenu.mensuel <- ifelse((sgsdata_Carnets$Campagne == 2 & sgsdata_Carnets$Periode == 0),(sgsdata_Carnets$Revenu_final),(sgsdata_Carnets$Revenu.mensuel))
sgsdata_Carnets$Revenu.mensuel <- ifelse((sgsdata_Carnets$Revenu.mensuel ==0),(sgsdata_Carnets$Revenu_final),(sgsdata_Carnets$Revenu.mensuel))
sgsdata_Carnets$Income_UC_INSEE <- ifelse(sgsdata_Carnets$Revenu.mensuel == "Absence de donnée" | sgsdata_Carnets$Revenu.mensuel == "Je ne souhaite pas répondre" | is.na(sgsdata_Carnets$Revenu.mensuel),  NA,  as.numeric(sgsdata_Carnets$Revenu.mensuel) / as.numeric(sgsdata_Carnets$UC_INSEE))


# Trouver les indices des colonnes de A à B
cols_to_remove <- which(names(sgsdata_Carnets) %in% c("Revenu_T1", "Revenu_T0", "Revenu_Lime_Survey", "Test", "Revenu_final"))

# Supprimer les colonnes 
sgsdata_Carnets <- sgsdata_Carnets[, -cols_to_remove]

#Appliquer les revenus de sgsdata_Carnets aux FFQ 
#Si toujours 0 attribuer le revenu par UC du recrutement 
# Trouver les indices des colonnes de A à B
cols_to_remove <- which(names(sgsdata_FFQ) %in% c("Income_UC_INSEE"))
sgsdata_FFQ <- sgsdata_FFQ[, -cols_to_remove]
sgsdata_FFQ <- sgsdata_FFQ%>%
  left_join(Revenu_lime_survey, by = "Identifiant")  # Utilise left_join() pour garder toutes les lignes de carnet


sgsdata_FFQ$Revenu.mensuel <- ifelse((sgsdata_FFQ$Campagne == 2 & sgsdata_FFQ$Periode == 0),(sgsdata_FFQ$Revenu_final),(sgsdata_FFQ$Revenu.mensuel))
sgsdata_FFQ$Revenu.mensuel <- ifelse((sgsdata_FFQ$Revenu.mensuel ==0),(sgsdata_FFQ$Revenu_final),(sgsdata_FFQ$Revenu.mensuel))
sgsdata_FFQ$Income_UC_INSEE <- ifelse(sgsdata_FFQ$Revenu.mensuel == "Absence de donnée" | sgsdata_FFQ$Revenu.mensuel == "Je ne souhaite pas répondre" | is.na(sgsdata_FFQ$Revenu.mensuel),  NA,  as.numeric(sgsdata_FFQ$Revenu.mensuel) / as.numeric(sgsdata_FFQ$UC_INSEE))



cols_to_remove <- which(names(sgsdata_FFQ) %in% c("Revenu_T1", "Revenu_T0", "Revenu_Lime_Survey", "Test", "Revenu_final"))
sgsdata_FFQ <- sgsdata_FFQ[, -cols_to_remove]



#Ajout des dernièrs colonnes à FFQ ------------------------------
temp  <- sgsdata_Carnets[, c("Identifiant", "Periode","Part_Kcal_BIO", "Part_Depense_BIO", "Part_Kcal_Supermarchés", 
                        "Part_Depense_Supermarchés", "Part_Kcal_Epiceries", "Part_Depense_Epiceries","cheque_theo")]

sgsdata_FFQ <- left_join(sgsdata_FFQ , temp, by = c("Identifiant", "Periode"))

# Obtenir les types des colonnes pour chaque dataframe
types_Carnets <- sapply(sgsdata_Carnets, class)
types_FFQ <- sapply(sgsdata_FFQ, class)

# Convertir les résultats en dataframes pour une comparaison facile
df_types_Carnets <- data.frame(Column = names(types_Carnets), Type = types_Carnets, stringsAsFactors = FALSE)
df_types_FFQ <- data.frame(Column = names(types_FFQ), Type = types_FFQ, stringsAsFactors = FALSE)

# Merge des dataframes sur les noms de colonnes pour comparaison
comparison <- merge(df_types_Carnets, df_types_FFQ, by = "Column", suffixes = c("_Carnets", "_FFQ"))

# Colonnes avec des types différents
different_types <- comparison[comparison$Type_Carnets != comparison$Type_FFQ, ]

# Afficher les colonnes avec des types différents
print("Colonnes avec des types différents :")
print(different_types)

sgsdata_Carnets$Combien.de.personnes.vivent.dans.votre.foyer<- as.character(sgsdata_Carnets$Combien.de.personnes.vivent.dans.votre.foyer)
sgsdata_FFQ$Combien.de.personnes.vivent.dans.votre.foyer<- as.character(sgsdata_FFQ$Combien.de.personnes.vivent.dans.votre.foyer)


# Combinaison des deux dataframes
sgsdata_Carnets  <- sgsdata_Carnets  %>%
  mutate(Revenu.mensuel = as.numeric(Revenu.mensuel))
sgsdata_FFQ <- sgsdata_FFQ  %>%
  mutate( Revenu.mensuel = as.numeric(Revenu.mensuel))
sgsdata <- bind_rows(sgsdata_Carnets, sgsdata_FFQ)
sgsdata$groupe <- ifelse(!is.na(sgsdata$Montant.mensuel.total), (1),(sgsdata$groupe))



sgsdata <- sgsdata %>%
  mutate(Temps = paste(Periode, Campagne, sep = "_"),
         Traitement = groupe * Periode)


#Harminisation de cheque_theo
sgsdata$cheque_theo <- ifelse(is.na(sgsdata$cheque_theo),(sgsdata$cheque_theo.x),(sgsdata$cheque_theo))
sgsdata$cheque_theo.x <- NULL
sgsdata$cheque_theo.y <- NULL
sgsdata$NA_prix_kg<- NULL
sgsdata$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits..1<- NULL
sgsdata$X7<- NULL


# Initialisation de la variable cheque_theo
sgsdata$cheque_theo_bis <- 0

  sgsdata <- sgsdata %>%
    mutate(
      UC_TI_arrondi = ceiling(UC_TI * 2) / 2,  # Arrondir au 0,5 supérieur
      cheque_theo_bis = case_when(
        # Cas "Epicerie2" ou "Epicerie1"
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 10 ~ 28,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 15 ~ 40,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 20 ~ 58,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 25 ~ 68,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 30 ~ 82,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 35 ~ 92,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 40 ~ 116,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 45 ~ 126,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 50 ~ 140,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 55 ~ 150,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 60 ~ 164,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 65 ~ 184,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 70 ~ 198,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 75 ~ 208,
        grepl("Epicerie2|Epicerie1", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 80 ~ 222,
        
        # Cas général pour les autres voie_de_recrutements
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 10 ~ 40,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 15 ~ 64,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 20 ~ 88,
        grepl("CCAS", voie_de_recrutement) &round(UC_TI_arrondi * 10) == 25 ~ 102,
        grepl("CCAS", voie_de_recrutement) &round(UC_TI_arrondi * 10) == 30 ~ 126,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 35 ~ 146,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 40 ~ 170,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 45 ~ 190,
        grepl("CCAS", voie_de_recrutement) &  round(UC_TI_arrondi * 10) == 50 ~ 214,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 55 ~ 238,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 60 ~ 258,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 65 ~ 272,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 70 ~ 296,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 75 ~ 316,
        grepl("CCAS", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 80 ~ 340,
        
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 10 ~ 44,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 15 ~ 68,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 20 ~ 88,
        grepl("PS|LE", voie_de_recrutement) &  round(UC_TI_arrondi * 10) == 25 ~ 112,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 30 ~ 132,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 35 ~ 156,
        grepl("PS|LE", voie_de_recrutement) &round(UC_TI_arrondi * 10) == 40 ~ 186,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 45 ~ 210,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 50 ~ 230,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 55 ~ 254,
        grepl("PS|LE", voie_de_recrutement) & round(UC_TI_arrondi * 10) == 60 ~ 274,
        
        # Valeur par défaut si UC_TI non reconnu
        TRUE ~ NA_real_
      )
    )

  
sgsdata$cheque_theo <- ifelse(is.na(sgsdata$cheque_theo), (sgsdata$cheque_theo_bis),(sgsdata$cheque_theo))
sgsdata$cheque_theo_bis <- NULL



#Définition des variables supplémentaires
#Verif groupe
sgsdata$FV_POIDS <- sgsdata$FRUITS_POIDS + sgsdata$FRUITS_SECS_POIDS  + sgsdata$NOIX_POIDS + sgsdata$LEGUMES_POIDS 
sgsdata$FEC_POIDS <- sgsdata$FEC_NON_RAF_POIDS + sgsdata$FEC_RAF_POIDS
sgsdata$PDTS_LAITIERS_POIDS <- sgsdata$LAIT_POIDS + sgsdata$LAITAGES_POIDS + sgsdata$FROMAGES_POIDS
sgsdata$POULET_OEUFS_POIDS <- sgsdata$POULET_POIDS + sgsdata$OEUFS_POIDS
sgsdata$AUTRE_PDTS_ANIMAUX_POIDS <- sgsdata$CHARCUTERIE_HORS_JB_POIDS  + sgsdata$JAMBON_BLANC_POIDS 
sgsdata$PLATS_PREP_POIDS <- sgsdata$PLATS_PREP_CARNES_POIDS + sgsdata$PLATS_PREP_VEGETARIENS_POIDS + sgsdata$QUICHES_PIZZAS_TARTES_SALEES_POIDS
sgsdata$VIANDE_ROUGE_PORC_POIDS <- sgsdata$VIANDE_ROUGE_POIDS+ sgsdata$PORC_POIDS
sgsdata$VIANDES_POIDS <- sgsdata$VIANDE_ROUGE_POIDS + sgsdata$POULET_POIDS + sgsdata$PLATS_PREP_CARNES_POIDS + sgsdata$JAMBON_BLANC_POIDS + sgsdata$PORC_POIDS + sgsdata$CHARCUTERIE_HORS_JB_POIDS
sgsdata$MG_POIDS <- sgsdata$MGA_POIDS + sgsdata$MGV_POIDS
sgsdata$PDTS_DISCRETIONNAIRES_POIDS <- sgsdata$SNACKS_AUTRES_POIDS +  sgsdata$CEREALES_PD_POIDS + sgsdata$DESSERTS_LACTES_POIDS + sgsdata$PDTS_SUCRES_POIDS + sgsdata$SAUCES_POIDS
sgsdata$SSB_POIDS <-  sgsdata$SODAS_SUCRES_POIDS + sgsdata$SODAS_LIGHT_POIDS +sgsdata$FRUITS_JUS_POIDS 


sgsdata$Aide_alim <- ifelse(is.na(sgsdata$Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois.), ("NON"), ("OUI"))
sgsdata$energy_densite <- (sgsdata$KCAL_SANS_BOISSON/(sgsdata$SOMME_POIDS_HORS_BOISSON*1000))
sgsdata$energy_densite <- sgsdata$energy_densite*100
sgsdata$Prop_montant_theorique_saisie <-  sgsdata$somme_montant_cheques/sgsdata$cheque_theo
sgsdata$depense_alim_uc <- sgsdata$Dépense_alim/ sgsdata$UC_TI
sgsdata$PVS <- sgsdata$FV_POIDS + sgsdata$LEG_SECS_POIDS

sgsdata$Situation_emploi <- ifelse(( sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.== "Autre inactif (invalide, handicapé, en congé maladie > 3 mois, titulaire d’une pension de réversion)"|
                            sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.== "Chômeur inscrit ou non au Pôle-Emploi"|  sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.=="Femme ou homme au foyer (y compris congé parental)" |
                             sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.==  "Situation administrative ne permettant pas de travailler" ),
                           ("Inactif(ve)"), (sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.))

sgsdata$Situation_emploi <- ifelse(( sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.== "Refuse de répondre"|
                                       sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.==  "Ne sait pas" ),
                                   ("Autre"), (sgsdata$Situation_emploi))

sgsdata$Situation_emploi <- ifelse(sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.== "Etudiant, élève, en formation, en stage non rémunéré",
                                   ("Etudiant(e)"), (sgsdata$Situation_emploi))

sgsdata$Situation_emploi <- ifelse(sgsdata$Quelle.est.votre.situation.professionnelle.actuelle.== "Retraitée ancien salarié ou préretraitée",
                                   ("Retraité(e)"), (sgsdata$Situation_emploi))

sgsdata$Source_revenu <- ifelse(( sgsdata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Minimas sociaux (RSA, Allocations familiales...)"|
                                  sgsdata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Indemnités chômage" ),
                                   ("Aides sociales"), (sgsdata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))

sgsdata$Source_revenu <- ifelse(( sgsdata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Ne sait pas"|
                                    sgsdata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Ne veut pas répondre" |
                                    sgsdata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Autre" |
                                    sgsdata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Refuse de répondre" ),
                                ("Autre"), (sgsdata$Source_revenu))

sgsdata$Source_revenu <- ifelse(( sgsdata$Source_revenu== "Travail (salarié, autoentrepreneur...)"),
                                ("Travail"), (sgsdata$Source_revenu))


sgsdata$Education <- ifelse(( sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale"|
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "2e ou 3e cycle universitaire, grande école") ,
                            ("Université"), (sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))


sgsdata$Education <- ifelse(( sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Baccalauréat"|
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Baccalauréat technologique ou professionnel" ) ,
                                ("Baccalauréat"), (sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))

sgsdata$Education <- ifelse(( sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Diplôme en-dessous du baccalauréat"|
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "CAP, BEP, BEPC, brevet élémentaire, BEPS" |
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Brevet de technicien, Brevet Professionnel (BP), BEI, BEC, BEA" |
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Certificat d'études primaires (CEP), diplôme de fin d'études obligatoire") ,
                            ("Diplôme en dessous du baccalauréat"), (sgsdata$Education))

sgsdata$Education <- ifelse(( sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Ne sait pas"|
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Refuse de répondre" |
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Autre" |
                                sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Ne veut pas répondre"|
                                is.na(sgsdata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.)) ,
                            ("Autre"), (sgsdata$Education))

print(unique(sgsdata$Situation_emploi))

#COMPLIANCE
#SI valeur manquante attribuer 1 si groupe = 0 
sgsdata$Compliant_booklet <- ifelse((is.na(sgsdata$compliance) & sgsdata$groupe==0),(1),(sgsdata$compliance))
sgsdata$reception_FFQ <- ifelse((sgsdata$Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.=="Oui"),(1),(0))
sgsdata$utilisation_FFQ <- ifelse((sgsdata$Les.avez.vous.utilises.=="Oui tous" |
                                         sgsdata$Les.avez.vous.utilises.=="Oui certains"),(1),(0))

sgsdata$utilisation2_FFQ <- ifelse(( sgsdata$Qu.avez.vous.achete.avec.=="Fruits;Légumes;Légumineuses"|
                                          sgsdata$Qu.avez.vous.achete.avec.=="Fruits;Légumes"|
                                          sgsdata$Qu.avez.vous.achete.avec.=="Légumes;Légumineuses"|
                                          sgsdata$Qu.avez.vous.achete.avec.=="Fruits;Légumineuses"|
                                          sgsdata$Qu.avez.vous.achete.avec.=="Fruits"|
                                          sgsdata$Qu.avez.vous.achete.avec.=="Légumes"|
                                          sgsdata$Qu.avez.vous.achete.avec.=="Légumineuses"),(1),(0))

sgsdata$limitation_FFQ <- ifelse((sgsdata$Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.=="Oui"),(1),(0))
sgsdata$Compliant_FFQ <- ifelse(( sgsdata$utilisation2_FFQ==1 & sgsdata$limitation_FFQ==1),(1),(0))
sgsdata$Compliant_FFQ <- ifelse ((sgsdata$groupe==0),(1), sgsdata$Compliant_FFQ)
sgsdata$Compliant_FFQ <- ifelse (is.na(sgsdata$Compliant_FFQ),(0),(sgsdata$Compliant_FFQ))



#Dernière réorganisation 
#Réorganisation des dataframes---------------------------------------------
#Carnet_NOV
sgsdata$Foyer_monoparental <- ifelse((sgsdata$Avez.vous.des.enfants.a.charge.=="Oui" & (sgsdata$Quelle.est.votre.situation.matrimoniale. == "Veuf(ve)" |sgsdata$Quelle.est.votre.situation.matrimoniale. == "Célibataire" |sgsdata$Quelle.est.votre.situation.matrimoniale. == "Divorce(e) ou séparé(e)")), (1), (0))
sgsdata$branche <- ifelse((sgsdata$voie_de_recrutement=="LE"),(1),(0))

sgsdata <- sgsdata  %>%
  relocate( Campagne, voie_de_recrutement, groupe, Compliant_booklet, Compliant_FFQ, Periode, Temps, Traitement, Mesure, .after  = Identifiant)

sgsdata <- sgsdata %>%
  relocate(`Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.`, `Qu.avez.vous.achete.avec.`, `Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.`, cheque_theo, `Différentiel_aide`,  .after = Mesure)

sgsdata$Part_Kcal_BIO <- ifelse(is.na(sgsdata$Part_Kcal_BIO), 0, as.numeric(sgsdata$Part_Kcal_BIO))
sgsdata$Part_Kcal_Epiceries <- ifelse(is.na(sgsdata$Part_Kcal_Epiceries), 0, as.numeric(sgsdata$Part_Kcal_Epiceries))
sgsdata$Part_Kcal_Supermarchés <- ifelse(is.na(sgsdata$Part_Kcal_Supermarchés), 0, as.numeric(sgsdata$Part_Kcal_Supermarchés))
sgsdata$Part_Depense_BIO <- ifelse(is.na(sgsdata$Part_Depense_BIO), 0, as.numeric(sgsdata$Part_Depense_BIO))
sgsdata$Part_Depense_Epiceries <- ifelse(is.na(sgsdata$Part_Depense_Epiceries), 0, as.numeric(sgsdata$Part_Depense_Epiceries))
sgsdata$Part_Depense_Supermarchés <- ifelse(is.na(sgsdata$Part_Depense_Supermarchés), 0, as.numeric(sgsdata$Part_Depense_Supermarchés))

##HENI---------------------------------------------------------------------------
CALNUT  <- read.xlsx((paste("Données analysées - Article N°1 chèques/Tableaux_annexes/Alim_CALNUT_CODAPPRO_CARNET.xlsx", sep="")))


# Ajouter le suffixe _POIDS à toutes les colonnes de 'résultats'
# Modifier toutes les valeurs de la première colonne en ajoutant "_HENI"
resultat_bis <- CALNUT %>%
  filter(!is.na(Classif_HENI)) %>%      # On garde uniquement les lignes où Classif_HENI est "TI"
  group_by(groupe_TI_TdC) %>%           # On groupe par la colonne groupe_TI_TdC
  summarise(moyenne_HENI = mean(HENI, na.rm = TRUE))
resultat_bis$moyenne_HENI <- resultat_bis$moyenne_HENI * 1000
resultat_bis[[1]] <- paste0(resultat_bis[[1]], "_HENI")
# Transposer sgsdata_IT et convertir le résultat en data.frame


resultat_bis_transposed <- as.data.frame(t(resultat_bis))
colnames(resultat_bis_transposed) <- as.character(resultat_bis_transposed[1, ])
resultat_bis_transposed <- resultat_bis_transposed[-1, ]
# Copier la première ligne 813 fois
resultat_bis_transposed <- resultat_bis_transposed[rep(1, 1688), ]


# Ajouter les colonnes de resultat_bis_transposed à sgsdata_IT
sgsdata <- cbind(sgsdata, resultat_bis_transposed)

# Définir la liste des catégories
categories <- c("CAFE_THE","CEREALES_PD", "CHARCUTERIE_HORS_JB", "DESSERTS_LACTES", "FEC_NON_RAF", 
                "FEC_RAF", "FROMAGES", "FRUITS", "FRUITS_JUS", "FRUITS_SECS", 
                "JAMBON_BLANC", "LAIT", "LAITAGES", "LEG_SECS", "LEGUMES", 
                "MGA", "MGV", "NOIX", "OEUFS", "PDTS_SUCRES", "PLATS_PREP_CARNES", 
                "PLATS_PREP_VEGETARIENS", "POISSONS", "PORC", "POULET", 
                "QUICHES_PIZZAS_TARTES_SALEES", "SAUCES", "SNACKS_AUTRES", 
                "SODAS_LIGHT", "SODAS_SUCRES", "VIANDE_ROUGE")

# Pour chaque catégorie, multiplier la variable poids par le score HENI correspondant
for (cat in categories) {
  poids_col <- paste0(cat, "_POIDS")
  heni_col  <- paste0(cat, "_HENI")
  
  # Vérifier que les deux colonnes existent dans le dataframe
  if (poids_col %in% names(sgsdata) && heni_col %in% names(sgsdata)) {
    # S'assurer que les colonnes sont numériques
    sgsdata[[poids_col]] <- as.numeric(as.character(sgsdata[[poids_col]]))
    sgsdata[[heni_col]]  <- as.numeric(as.character(sgsdata[[heni_col]]))
    
    # Multiplier le poids (déjà converti en grammes) par le score HENI
    sgsdata[[heni_col]] <- sgsdata[[poids_col]] * sgsdata[[heni_col]]
  } else {
    warning(paste("Les colonnes", poids_col, "ou", heni_col, "n'existent pas dans sgsdata"))
  }
}

sgsdata$HENI_TOT <- 0
sgsdata$HENI_TOT <- sgsdata$CAFE_THE_HENI + sgsdata$CEREALES_PD_HENI + sgsdata$CHARCUTERIE_HORS_JB_HENI + sgsdata$DESSERTS_LACTES_HENI + sgsdata$FEC_NON_RAF_HENI +
  sgsdata$FEC_RAF_HENI + sgsdata$FROMAGES_HENI + sgsdata$FRUITS_HENI + sgsdata$FRUITS_JUS_HENI + sgsdata$FRUITS_SECS_HENI +
  sgsdata$JAMBON_BLANC_HENI + sgsdata$LAIT_HENI + sgsdata$LAITAGES_HENI + sgsdata$LEG_SECS_HENI + sgsdata$LEGUMES_HENI +
  sgsdata$MGA_HENI + sgsdata$MGV_HENI + sgsdata$NOIX_HENI + sgsdata$OEUFS_HENI + sgsdata$PDTS_SUCRES_HENI + sgsdata$PLATS_PREP_CARNES_HENI +
  sgsdata$PLATS_PREP_VEGETARIENS_HENI + sgsdata$POISSONS_HENI + sgsdata$PORC_HENI + sgsdata$POULET_HENI +
  sgsdata$QUICHES_PIZZAS_TARTES_SALEES_HENI + sgsdata$SAUCES_HENI + sgsdata$SNACKS_AUTRES_HENI + sgsdata$SODAS_LIGHT_HENI + sgsdata$SODAS_SUCRES_HENI + sgsdata$VIANDE_ROUGE_HENI




#Part de non recours   ---------------------------------------------------------
#Definition fichier up 24
up <- read.xlsx(paste("usage_cheques/24-12-20_Etat des titres simplifiés_millesime2024.xlsx"),cols = 19:29,startRow = 30,sheet = "Etat des titres simplifiés",detectDates = T)
colnames(up)<-gsub('xml:space="preserve">',"",colnames(up));up$Statut<-gsub('xml:space="preserve">',"",up$Statut);up$RS.Affilié<-gsub('xml:space="preserve">',"",up$RS.Affilié)
up<-up%>%mutate(Titre = if_else(Montant.Emission == 5, paste0("fl_",Titre),paste0("ls_",Titre)))

#Bordereau up 24
bordereau_9_janvier <- read.xlsx(paste("usage_cheques/24-03-26-bordereau_envoi_cheques.xlsx"),sheet = "bordereau_9_janvier",cols = 1:18,detectDates = T)
bordereau_9_janvier$date<-as.Date("2024-01-09")
bordereau_23_janvier <- read.xlsx(paste("usage_cheques/24-03-26-bordereau_envoi_cheques.xlsx"),sheet = "bordereau_23_janvier",cols = 1:18,detectDates = T)
bordereau_23_janvier$date<-as.Date("2024-01-23")
bordereau_6_fevrier <- read.xlsx(paste("usage_cheques/24-03-26-bordereau_envoi_cheques.xlsx"),sheet = "bordereau_6_fevrier",cols = 1:18,detectDates = T)
bordereau_6_fevrier$date<-as.Date("2024-02-06")
bordereau_20_fevrier <- read.xlsx(paste("usage_cheques/24-03-26-bordereau_envoi_cheques.xlsx"),sheet = "bordereau_20_fevrier",cols = 1:18,detectDates = T)
bordereau_20_fevrier$date<-as.Date("2024-02-20")
bordereau_5_mars <- read.xlsx(paste("usage_cheques/24-03-26-bordereau_envoi_cheques.xlsx"),sheet = "bordereau_5_mars",cols = 1:18,detectDates = T)
bordereau_5_mars$date<-as.Date("2024-03-05")
bordereau_19_mars <- read.xlsx(paste("usage_cheques/24-03-26-bordereau_envoi_cheques.xlsx"),sheet = "bordereau_19_mars",cols = 1:18,detectDates = T)
bordereau_19_mars$date<-as.Date("2024-03-19")
bordereau_2_avril <- read.xlsx(paste("usage_cheques/24-03-26-bordereau_envoi_cheques.xlsx"),sheet = "bordereau_2_avril",cols = 1:18,detectDates = T)
bordereau_2_avril$date<-as.Date("2024-04-02")
bordereau <- rbind (bordereau_9_janvier, bordereau_23_janvier,bordereau_6_fevrier,
                    bordereau_20_fevrier, bordereau_5_mars, bordereau_19_mars, bordereau_2_avril)
bordereau$date_inclusion<-as.Date("2024-04-02")
liste_id<-unique(bordereau$id)

up$id_fl<-NA;up$id_ls<-NA;up$date_envoi<-NA
liste_no_cheques<-list("id"=liste_id,"id_fl"=vector("list", length = length(liste_id)),"date_fl"=vector("list", length = length(liste_id)),
                       "id_ls"=vector("list", length = length(liste_id)),"date_ls"=vector("list", length = length(liste_id)))
for (i in 1:length(liste_id)) {
  temp<-bordereau[which(bordereau$id==liste_id[i]),c("no_premier_cheque_FruitsLegs","no_dernier_cheque_FruitsLegs","date")]
  for (j in 1:nrow(temp)) {
    if (temp[j,1]>0 & j == 1) {
      # liste_no_cheques$id_fl[[i]]<-unlist(Map(seq, temp[j,1], temp[j,2]))
      liste_no_cheques$id_fl[[i]]<-paste0("fl_",unlist(Map(seq, temp[j,1], temp[j,2])))
      liste_no_cheques$date_fl[[i]]<-rep(temp$date[j],length(unlist(Map(seq, temp[j,1], temp[j,2]))),each=T)
    }
    if (temp[j,1]>0 & j > 1) {
      liste_no_cheques$id_fl[[i]]<-c(liste_no_cheques$id_fl[[i]],paste0("fl_",unlist(Map(seq, temp[j,1], temp[j,2]))))
      liste_no_cheques$date_fl[[i]]<-c(liste_no_cheques$date_fl[[i]],rep(temp$date[j],length(unlist(Map(seq, temp[j,1], temp[j,2]))),each=T))
    }
  }
  temp<-bordereau[which(bordereau$id==liste_id[i]),c("no_premier_cheque_LegSecs","no_dernier_cheque_LegSecs","date")]
  for (j in 1:nrow(temp)) {
    if (temp[j,1]>0 & j == 1) {
      liste_no_cheques$id_ls[[i]]<-paste0("ls_",unlist(Map(seq, temp[j,1], temp[j,2])))
      liste_no_cheques$date_ls[[i]]<-rep(temp$date[j],length(unlist(Map(seq, temp[j,1], temp[j,2]))),each=T)
    }
    if (temp[j,1]>0 & j > 1) {
      liste_no_cheques$id_ls[[i]]<-c(liste_no_cheques$id_ls[[i]],paste0("ls_",unlist(Map(seq, temp[j,1], temp[j,2]))))
      liste_no_cheques$date_ls[[i]]<-c(liste_no_cheques$date_ls[[i]],rep(temp$date[j],length(unlist(Map(seq, temp[j,1], temp[j,2]))),each=T))
    }
  }
}

for (i in 1:nrow(up)) {
  for (j in 1:length(liste_id)) {
    temp<-which(liste_no_cheques$id_fl[[j]]==up$Titre[i])
    if (length(temp)>0) {
      # if (up$Numero.Titre.Source[i]%in%liste_no_cheques$id_fl[[j]]) {
      up$id_fl[i]<-liste_id[j]
      up$date_envoi[i]<-liste_no_cheques$date_fl[[j]][temp]
    }
    temp<-which(liste_no_cheques$id_ls[[j]]==up$Titre[i])
    if (length(temp)>0) {
      up$id_ls[i]<-liste_id[j]
      up$date_envoi[i]<-liste_no_cheques$date_ls[[j]][temp]
    }
  }
}
up<-up %>% mutate(date_envoi = as.Date(date_envoi,origin=as.Date("1970-01-01")))


#Extraire de sgsdata les dates de saisie des questionnaires
extracted_data <- sgsdata[, c("Campagne", "Periode", "Identifiant", "Date.de.saisie")]

# Supprimer les doublons de tout le dataframe
extracted_data <- extracted_data %>% filter(Periode != 0)
extracted_data <- extracted_data[, -which(names(extracted_data) == "Campagne")]
extracted_data <- extracted_data[, -which(names(extracted_data) == "Periode")]
extracted_data_unique <- distinct(extracted_data)
up <- up %>%
  mutate(Identifiant = coalesce(id_fl, id_ls))
up <- up %>%
  left_join(extracted_data_unique, by = c("Identifiant" = "Identifiant"))

temp <- up  # %>%
temp$montant_remboursé<- ifelse((temp$Statut=="Remboursé"),(temp$Montant.Emission),(0))
temp$date_fin_remboursement <- as.Date("2024-05-21")
temp$montant.remboursé_vf <- ifelse(
  temp$Date.Statut > temp$date_fin_remboursement,
  0,
  temp$montant_remboursé
)

# Calculer le pourcentage pour chaque Identifiant
result <- temp %>%
  group_by(Identifiant) %>%
  summarise(
    Montant.Emission = sum(Montant.Emission, na.rm = TRUE),
    montant.remboursé_vf = sum(montant.remboursé_vf, na.rm = TRUE),
    pourcentage_remboursé_avant_la_fin = montant.remboursé_vf / Montant.Emission
  ) %>%
  distinct()

mean(result$pourcentage_remboursé_avant_la_fin)


#Fusion avec sgsdata
sgsdata <- left_join(sgsdata, result, by="Identifiant")
temp <- up

#Non recours 1 ère campagne ---------------------------------
#Analyse en prenant uniquement les personnes de l'analyse------------------------------------
bordereau_2_decembre <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 02-12-2022",cols = 1:11, startRow = 3, detectDates = T)
bordereau_2_decembre$date<-as.Date("2024-01-09")
bordereau_9_decembre <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 09-12-2022",cols = 1:11, startRow = 3, detectDates = T)
bordereau_9_decembre$date<-as.Date("2024-01-09")
bordereau_16_decembre <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 16-12-2022",cols = 1:11, startRow = 3, detectDates = T)
bordereau_16_decembre$date<-as.Date("2024-01-09")
bordereau_23_decembre <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 23-12-2022",cols = 1:11, startRow = 3, detectDates = T)
bordereau_23_decembre$date<-as.Date("2024-01-09")
bordereau_30_decembre <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 30-12-2022",cols = 1:11, startRow = 3, detectDates = T)
bordereau_30_decembre$date<-as.Date("2024-01-09")
bordereau_6_janvier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 06-01-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_6_janvier$date<-as.Date("2024-01-09")
bordereau_13_janvier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 13-01-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_13_janvier$date<-as.Date("2024-01-09")
bordereau_20_janvier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 20-01-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_20_janvier$date<-as.Date("2024-01-09")
bordereau_27_janvier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 27-01-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_27_janvier$date<-as.Date("2024-01-09")
bordereau_03_fevrier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 03-02-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_03_fevrier$date<-as.Date("2024-01-09")
bordereau_10_fevrier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 10-02-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_10_fevrier$date<-as.Date("2024-01-09")
bordereau_17_fevrier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 17-02-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_17_fevrier$date<-as.Date("2024-01-09")
bordereau_24_fevrier <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 24-02-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_24_fevrier$date<-as.Date("2024-01-09")
bordereau_3_mars <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 03-03-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_3_mars$date<-as.Date("2024-01-09")
bordereau_10_mars <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 10-03-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_10_mars$date<-as.Date("2024-01-09")
bordereau_17_mars <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 17-03-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_17_mars$date<-as.Date("2024-01-09")
bordereau_24_mars <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 24-03-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_24_mars$date<-as.Date("2024-01-09")
bordereau_31_mars <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 31-03-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_31_mars$date<-as.Date("2024-01-09")
bordereau_7_avril <- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 07-04-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_7_avril$date<-as.Date("2024-01-09")
bordereau_14_avril<- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 14-04-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_14_avril$date<-as.Date("2024-01-09")
bordereau_21_avril<- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 21-04-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_21_avril$date<-as.Date("2024-01-09")
bordereau_28_avril<- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 28-04-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_28_avril$date<-as.Date("2024-01-09")
bordereau_05_mai<- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 05-05-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_05_mai$date<-as.Date("2024-01-09")
bordereau_12_mai<- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 12-05-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_12_mai$date<-as.Date("2024-01-09")
bordereau_19_mai<- read.xlsx(paste("usage_cheques/Bordereaux_2022/Envoi cheque-22-11_metadata.xlsx"),sheet = "Envoi - T - 19-05-2023",cols = 1:11, startRow = 3, detectDates = T)
bordereau_19_mai$date<-as.Date("2024-01-09")
bordereau <- rbind (bordereau_2_decembre ,bordereau_9_decembre,bordereau_16_decembre, bordereau_23_decembre,
                    bordereau_30_decembre, bordereau_6_janvier, bordereau_13_janvier, bordereau_20_janvier,
                    bordereau_27_janvier, bordereau_03_fevrier, bordereau_10_fevrier, bordereau_17_fevrier,
                    bordereau_24_fevrier,bordereau_3_mars, bordereau_10_mars, bordereau_17_mars)

bordereau_bis <- rbind ( bordereau_24_mars,bordereau_31_mars,bordereau_7_avril,bordereau_14_avril,bordereau_21_avril,bordereau_28_avril,
                    bordereau_05_mai, bordereau_12_mai, bordereau_19_mai)

bordereau$Nb_cheques <- bordereau$`Nb.ch.F&L.2.€` + bordereau$`Nb.ch.F&L.5.€` + bordereau$`Nb.ch.Lég.secs.2.€`
bordereau$Montant <- bordereau$`Montant.F&L` + bordereau$Montant.Lég.secs
colnames(bordereau)[1] <- "Identifiant"
#bordereau<- bordereau[bordereau$Date.envoi.1 >= "2022-11-02" & bordereau$Date.envoi.1 <= "2022-12-03", ]
sum_bordereau <- sum(bordereau$Montant, na.rm = TRUE) # 28037€

bordereau_bis$Nb_cheques <- bordereau_bis$`Nb.ch.F&L.2.€` + bordereau_bis$`Nb.ch.F&L.5.€` + bordereau_bis$`Nb.ch.Lég.secs.2.€`
bordereau_bis$Montant <- bordereau_bis$`Montant.F&L` + bordereau_bis$Montant.Lég.secs
colnames(bordereau_bis)[1] <- "Identifiant"
#bordereau_bis<- bordereau_bis[bordereau_bis$Date.envoi.1 >= "2022-11-02" & bordereau_bis$Date.envoi.1 <= "2022-12-03", ]
sum_bordereau_bis <- sum(bordereau_bis$Montant, na.rm = TRUE) # 5962€
#28037+ 5962 = 33999 
#28037/33999 = 0.8246419
Cheques_22 <- read.xlsx(paste("usage_cheques/23-12-22_Etat des titres simplifiés_millesime2022.xlsx"),cols = 15:25,startRow = 30,sheet = "Etat des titres simplifiés",detectDates = T)
colnames(Cheques_22)<-gsub('xml:space="preserve">',"",colnames(Cheques_22));Cheques_22$Statut<-gsub('xml:space="preserve">',"",Cheques_22$Statut);Cheques_22$RS.Affilié<-gsub('xml:space="preserve">',"",Cheques_22$RS.Affilié)
Cheques_23 <- read.xlsx(paste("usage_cheques/24-12-20_Etat des titres simplifiés_millesime2023.xlsx"),cols = 15:25,startRow = 30,sheet = "Etat des titres simplifiés",detectDates = T)
#colnames(Cheques_23)<-gsub('xml:space="preserve">',"",colnames(Cheques_23));Cheques_23$Statut<-gsub('xml:space="preserve">',"",Cheques_23$Statut);Cheques_23$RS.Affilié<-gsub('xml:space="preserve">',"",Cheques_23$RS.Affilié)
colnames(Cheques_22)
colnames(Cheques_23)
Cheques_22$RS.Affilié<- NULL
Cheques_22$Code.Affilie<- NULL
Cheques_23$Reference.Client <- NULL
Cheques_23$Reference.Mois.Externe <- NULL
Cheques_23$Reference.Externe.Bénéficiaire <- NULL
Cheques_23$Titre <- Cheques_23$X4
Cheques_23$X4 <- NULL
Cheques <- rbind(Cheques_22, Cheques_23)

#Liste des chèques distribués
df <- read.xlsx(paste("usage_cheques/Cheques_202223.xlsx"))
df$DATE <- as.Date(df$DATE, origin = "1899-12-30")
#df$DATE <- NULL
fill_gaps_titre_date <- function(df, threshold = 60) {
  # df doit contenir au moins deux colonnes :
  #  - df$titre  (numérique)
  #  - df$DATE   (Date)
  
  # 1) Construire une table d'association titre → dates (agrégées)
  #    Si un titre a plusieurs dates, on les fusionne en "2023-01-05;2023-01-12", etc.
  agg_dates <- tapply(df$DATE, df$titre, function(x) {
    ux <- sort(unique(x))
    paste0(ux, collapse = ";")
  })
  # agg_dates est un vecteur nommé : names(agg_dates) = titres, values = "date1;date2;..."
  
  # 2) Générer la séquence complète des titres (avec comblements < threshold)
  titres_existants <- sort(unique(df$titre))
  full_seq <- numeric(0)
  for (i in seq_along(titres_existants)) {
    t0 <- titres_existants[i]
    full_seq <- c(full_seq, t0)
    if (i < length(titres_existants)) {
      gap <- titres_existants[i+1] - t0
      if (gap > 1 && gap < threshold) {
        full_seq <- c(full_seq, seq(t0 + 1, titres_existants[i+1] - 1))
      }
    }
  }
  
  # 3) Pour chaque titre de la séquence, retrouver la/les dates (NA si titre "inséré")
  DATE <- unname(agg_dates[as.character(full_seq)])
  # les titres non présents dans agg_dates retournent NA
  
  # 4) Retourner le data.frame
  data.frame(
    titre = full_seq,
    DATE  = DATE,
    stringsAsFactors = FALSE
  )
}


# 1. Forcer en numérique
df$Titre <- as.numeric(df$Titre)
# 2. Renommer
names(df)[names(df) == "Titre"] <- "titre"
# 3. Appel de la fonction
Numeros <- fill_gaps_titre_date(df)
#Si une date est manquante, on reprend la date de la ligne précédente 
for (i in seq_len(nrow(Numeros))) {
  if (is.na(Numeros$DATE[i]) && i > 1) {
    Numeros$DATE[i] <- Numeros$DATE[i - 1]
  }
}
#Supprimer tous les titres envoyés après le 18 mars (envoi)
Numeros <- subset(Numeros, DATE <= as.Date("2023-03-18"))

Numeros  <- Numeros  %>%
  filter(!if_all(everything(), ~ is.na(.) | . == ""))
names(Numeros)[names(Numeros) == "titre"] <- "Titre"
Numeros$Titre <- as.numeric(Numeros$Titre)
Cheques$Titre <- as.numeric(Cheques$Titre)

Total <- left_join(Numeros, Cheques, by = "Titre")
Total <- Total %>% filter(!if_all(-Titre, ~ is.na(.) | . == ""))
Total <- Total %>% filter(!if_all(-Montant.Emission, ~ is.na(.) | . == ""))
Total <- Total %>% filter(Statut=="Remboursé")
Total$date_fin_remboursement <- as.Date("2023-05-02") #On regarde ce qui est remboursé 50 jours jusqu'au dernier envoi
Total$montant.remboursé_vf <- ifelse(Total$Date.Statut > Total$date_fin_remboursement, 0,Total$Montant.Emission)
# Étape 1 : initialiser la colonne
# Étape 2 : calcul du ratio
ratio_rembourse <- sum(Total$Montant.Emission, na.rm = TRUE) / sum_bordereau
# Étape 3 : remplir conditionnellement
sgsdata$pourcentage_remboursé_avant_la_fin[sgsdata$Campagne == 1 & sgsdata$groupe == 1] <- ratio_rembourse


#distribution
temp <- up
temp$duree_remboursement <- as.Date(temp$Date.Statut) - as.Date(temp$date_envoi)
temp_rembourse <- temp %>% 
  filter(Statut == "Remboursé")
temp_rembourse$duree_remboursement = as.period(temp_rembourse$duree_remboursement)
temp_rembourse <- temp_rembourse[, c("Titre", "duree_remboursement")]

summary(temp_rembourse$duree_remboursement)

temp2 <- Total
temp2$duree_remboursement <- as.Date(temp2$Date.Statut) - as.Date(temp2$DATE)
temp_rembourse2 <- temp2 %>% 
  filter(Statut == "Remboursé")
temp_rembourse2$duree_remboursement = as.period(temp_rembourse2$duree_remboursement)
temp_rembourse2 <- temp_rembourse2[, c("Titre", "duree_remboursement")]
Temp_vf <- rbind(temp_rembourse, temp_rembourse2)

summary(temp_rembourse$duree_remboursement)
summary(temp_rembourse2$duree_remboursement)
summary(Temp_vf$duree_remboursement)



#Téléchargement des tableaux finaux -----------------------------------------
# Créer un nouvel objet workbook
wb <- createWorkbook()

addWorksheet(wb, "sgsdata")
writeData(wb, sheet = "sgsdata", sgsdata  )

saveWorkbook(wb,(paste0("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichier_traitement/sgsdata.xlsx")))



###
carnet_data <- sgsdata %>%
  filter(Mesure == "Carnet") %>%
  group_by(Identifiant) %>%
  mutate(
    across(
      .cols = c(
        FV_POIDS, FRUITS_POIDS, FRUITS_SECS_POIDS, NOIX_POIDS, LEGUMES_POIDS, 
        LEG_SECS_POIDS, FRUITS_prix_kg, LEGUMES_prix_kg, FRUITS_SECS_prix_kg, 
        NOIX_prix_kg, LEG_SECS_prix_kg, VIANDES_POIDS, VIANDE_ROUGE_PORC_POIDS, 
        AUTRE_PDTS_ANIMAUX_POIDS, POULET_OEUFS_POIDS, POISSONS_POIDS, POISSONS_prix_kg, 
        FEC_POIDS, PDTS_LAITIERS_POIDS, PDTS_DISCRETIONNAIRES_POIDS, PLATS_PREP_POIDS, 
        MG_POIDS, SSB_POIDS, ALCOOL_POIDS, depense_alim_uc, MER, MAR, energy_densite, 
        SOMME_POIDS, SOMME_POIDS_HORS_BOISSON, SOMME_POIDS_KCAL, KCAL_SANS_BOISSON, 
        climat_env, couche_ozone_env, ions_env, ozone_env, partic_env, acid_env, 
        eutro_terr_env, eutro_eau_env, eutro_mer_env, sol_env, toxi_eau_env, ress_eau_env, 
        ress_ener_env, ress_min_env, ALCOOL_prix_kg, AUTRE_prix_kg, CAFE_THE_prix_kg, 
        CEREALES_PD_prix_kg, CHARCUTERIE_HORS_JB_prix_kg, DESSERTS_LACTES_prix_kg, 
        EAU_prix_kg, EPICES_CONDIMENTS_prix_kg, FEC_NON_RAF_prix_kg, FEC_RAF_prix_kg, 
        FROMAGES_prix_kg, FRUITS_JUS_prix_kg, JAMBON_BLANC_prix_kg, LAIT_prix_kg, 
        LAITAGES_prix_kg, MGA_prix_kg, MGV_prix_kg, OEUFS_prix_kg, PDTS_SUCRES_prix_kg, 
        PLATS_PREP_CARNES_prix_kg, PLATS_PREP_VEGETARIENS_prix_kg, PORC_prix_kg, 
        QUICHES_PIZZAS_TARTES_SALEES_prix_kg, SAUCES_prix_kg, SNACKS_AUTRES_prix_kg, 
        SODAS_LIGHT_prix_kg, SODAS_SUCRES_prix_kg, VIANDE_ROUGE_prix_kg, 
        CAFE_THE_POIDS, CEREALES_PD_POIDS, CHARCUTERIE_HORS_JB_POIDS, DESSERTS_LACTES_POIDS, 
        FEC_NON_RAF_POIDS, FEC_RAF_POIDS, FROMAGES_POIDS, FRUITS_JUS_POIDS, JAMBON_BLANC_POIDS, 
        LAIT_POIDS, LAITAGES_POIDS, MGA_POIDS, MGV_POIDS, OEUFS_POIDS, PDTS_SUCRES_POIDS, 
        PLATS_PREP_CARNES_POIDS, PLATS_PREP_VEGETARIENS_POIDS, PORC_POIDS, POULET_POIDS, 
        QUICHES_PIZZAS_TARTES_SALEES_POIDS, SAUCES_POIDS, SNACKS_AUTRES_POIDS, 
        SODAS_LIGHT_POIDS, SODAS_SUCRES_POIDS, VIANDE_ROUGE_POIDS, EAU_POIDS, 
        EPICES_CONDIMENTS_POIDS,
        CAFE_THE_HENI, CEREALES_PD_HENI, CHARCUTERIE_HORS_JB_HENI, DESSERTS_LACTES_HENI, 
        FEC_NON_RAF_HENI, FEC_RAF_HENI, FROMAGES_HENI, FRUITS_JUS_HENI, JAMBON_BLANC_HENI, 
        LAIT_HENI, LAITAGES_HENI, MGA_HENI, MGV_HENI, OEUFS_HENI, PDTS_SUCRES_HENI, 
        PLATS_PREP_CARNES_HENI, PLATS_PREP_VEGETARIENS_HENI, PORC_HENI, POULET_HENI, 
        QUICHES_PIZZAS_TARTES_SALEES_HENI, SAUCES_HENI, SNACKS_AUTRES_HENI, 
        SODAS_LIGHT_HENI, SODAS_SUCRES_HENI, VIANDE_ROUGE_HENI, HENI_TOT
      ),
      .names = "{.col}_diff",
      .fns = ~ .[Periode == "1"] - .[Periode == "0"]
    ),
    across(
      .cols = c(
        FV_POIDS, FRUITS_POIDS, FRUITS_SECS_POIDS, NOIX_POIDS, LEGUMES_POIDS, 
        LEG_SECS_POIDS, FRUITS_prix_kg, LEGUMES_prix_kg, FRUITS_SECS_prix_kg, 
        NOIX_prix_kg, LEG_SECS_prix_kg, VIANDES_POIDS, VIANDE_ROUGE_PORC_POIDS, 
        AUTRE_PDTS_ANIMAUX_POIDS, POULET_OEUFS_POIDS, POISSONS_POIDS, POISSONS_prix_kg, 
        FEC_POIDS, PDTS_LAITIERS_POIDS, PDTS_DISCRETIONNAIRES_POIDS, PLATS_PREP_POIDS, 
        MG_POIDS, SSB_POIDS, ALCOOL_POIDS, depense_alim_uc, MER, MAR, energy_densite, 
        SOMME_POIDS, SOMME_POIDS_HORS_BOISSON, SOMME_POIDS_KCAL, KCAL_SANS_BOISSON, 
        climat_env, couche_ozone_env, ions_env, ozone_env, partic_env, acid_env, 
        eutro_terr_env, eutro_eau_env, eutro_mer_env, sol_env, toxi_eau_env, ress_eau_env, 
        ress_ener_env, ress_min_env, ALCOOL_prix_kg, AUTRE_prix_kg, CAFE_THE_prix_kg, 
        CEREALES_PD_prix_kg, CHARCUTERIE_HORS_JB_prix_kg, DESSERTS_LACTES_prix_kg, 
        EAU_prix_kg, EPICES_CONDIMENTS_prix_kg, FEC_NON_RAF_prix_kg, FEC_RAF_prix_kg, 
        FROMAGES_prix_kg, FRUITS_JUS_prix_kg, JAMBON_BLANC_prix_kg, LAIT_prix_kg, 
        LAITAGES_prix_kg, MGA_prix_kg, MGV_prix_kg, OEUFS_prix_kg, PDTS_SUCRES_prix_kg, 
        PLATS_PREP_CARNES_prix_kg, PLATS_PREP_VEGETARIENS_prix_kg, PORC_prix_kg, 
        QUICHES_PIZZAS_TARTES_SALEES_prix_kg, SAUCES_prix_kg, SNACKS_AUTRES_prix_kg, 
        SODAS_LIGHT_prix_kg, SODAS_SUCRES_prix_kg, VIANDE_ROUGE_prix_kg, 
        CAFE_THE_POIDS, CEREALES_PD_POIDS, CHARCUTERIE_HORS_JB_POIDS, DESSERTS_LACTES_POIDS, 
        FEC_NON_RAF_POIDS, FEC_RAF_POIDS, FROMAGES_POIDS, FRUITS_JUS_POIDS, JAMBON_BLANC_POIDS, 
        LAIT_POIDS, LAITAGES_POIDS, MGA_POIDS, MGV_POIDS, OEUFS_POIDS, PDTS_SUCRES_POIDS, 
        PLATS_PREP_CARNES_POIDS, PLATS_PREP_VEGETARIENS_POIDS, PORC_POIDS, POULET_POIDS, 
        QUICHES_PIZZAS_TARTES_SALEES_POIDS, SAUCES_POIDS, SNACKS_AUTRES_POIDS, 
        SODAS_LIGHT_POIDS, SODAS_SUCRES_POIDS, VIANDE_ROUGE_POIDS, EAU_POIDS, 
        EPICES_CONDIMENTS_POIDS,         CAFE_THE_HENI, CEREALES_PD_HENI, CHARCUTERIE_HORS_JB_HENI, DESSERTS_LACTES_HENI, 
        FEC_NON_RAF_HENI, FEC_RAF_HENI, FROMAGES_HENI, FRUITS_JUS_HENI, JAMBON_BLANC_HENI, 
        LAIT_HENI, LAITAGES_HENI, MGA_HENI, MGV_HENI, OEUFS_HENI, PDTS_SUCRES_HENI, 
        PLATS_PREP_CARNES_HENI, PLATS_PREP_VEGETARIENS_HENI, PORC_HENI, POULET_HENI, 
        QUICHES_PIZZAS_TARTES_SALEES_HENI, SAUCES_HENI, SNACKS_AUTRES_HENI, 
        SODAS_LIGHT_HENI, SODAS_SUCRES_HENI, VIANDE_ROUGE_HENI, HENI_TOT
      ),
      .names = "{.col}_P0",
      .fns = ~ .[Periode == "0"]
    )
  ) %>%
  ungroup()

ffq_data <- sgsdata %>%
  filter(Mesure == "FFQ") %>%
  group_by(Identifiant) %>%
  mutate(
    across(
      .cols = c(
        FV_POIDS, FRUITS_POIDS, FRUITS_SECS_POIDS, NOIX_POIDS, LEGUMES_POIDS, 
        LEG_SECS_POIDS, FRUITS_prix_kg, LEGUMES_prix_kg, FRUITS_SECS_prix_kg, 
        NOIX_prix_kg, LEG_SECS_prix_kg, VIANDES_POIDS, VIANDE_ROUGE_PORC_POIDS, 
        AUTRE_PDTS_ANIMAUX_POIDS, POULET_OEUFS_POIDS, POISSONS_POIDS, POISSONS_prix_kg, 
        FEC_POIDS, PDTS_LAITIERS_POIDS, PDTS_DISCRETIONNAIRES_POIDS, PLATS_PREP_POIDS, 
        MG_POIDS, SSB_POIDS, ALCOOL_POIDS, depense_alim_uc, MER, MAR, energy_densite, 
        SOMME_POIDS, SOMME_POIDS_HORS_BOISSON, SOMME_POIDS_KCAL, KCAL_SANS_BOISSON, 
        climat_env, couche_ozone_env, ions_env, ozone_env, partic_env, acid_env, 
        eutro_terr_env, eutro_eau_env, eutro_mer_env, sol_env, toxi_eau_env, ress_eau_env, 
        ress_ener_env, ress_min_env, ALCOOL_prix_kg, AUTRE_prix_kg, CAFE_THE_prix_kg, 
        CEREALES_PD_prix_kg, CHARCUTERIE_HORS_JB_prix_kg, DESSERTS_LACTES_prix_kg, 
        EAU_prix_kg, EPICES_CONDIMENTS_prix_kg, FEC_NON_RAF_prix_kg, FEC_RAF_prix_kg, 
        FROMAGES_prix_kg, FRUITS_JUS_prix_kg, JAMBON_BLANC_prix_kg, LAIT_prix_kg, 
        LAITAGES_prix_kg, MGA_prix_kg, MGV_prix_kg, OEUFS_prix_kg, PDTS_SUCRES_prix_kg, 
        PLATS_PREP_CARNES_prix_kg, PLATS_PREP_VEGETARIENS_prix_kg, PORC_prix_kg, 
        QUICHES_PIZZAS_TARTES_SALEES_prix_kg, SAUCES_prix_kg, SNACKS_AUTRES_prix_kg, 
        SODAS_LIGHT_prix_kg, SODAS_SUCRES_prix_kg, VIANDE_ROUGE_prix_kg, 
        CAFE_THE_POIDS, CEREALES_PD_POIDS, CHARCUTERIE_HORS_JB_POIDS, DESSERTS_LACTES_POIDS, 
        FEC_NON_RAF_POIDS, FEC_RAF_POIDS, FROMAGES_POIDS, FRUITS_JUS_POIDS, JAMBON_BLANC_POIDS, 
        LAIT_POIDS, LAITAGES_POIDS, MGA_POIDS, MGV_POIDS, OEUFS_POIDS, PDTS_SUCRES_POIDS, 
        PLATS_PREP_CARNES_POIDS, PLATS_PREP_VEGETARIENS_POIDS, PORC_POIDS, POULET_POIDS, 
        QUICHES_PIZZAS_TARTES_SALEES_POIDS, SAUCES_POIDS, SNACKS_AUTRES_POIDS, 
        SODAS_LIGHT_POIDS, SODAS_SUCRES_POIDS, VIANDE_ROUGE_POIDS, EAU_POIDS, 
        EPICES_CONDIMENTS_POIDS,         CAFE_THE_HENI, CEREALES_PD_HENI, CHARCUTERIE_HORS_JB_HENI, DESSERTS_LACTES_HENI, 
        FEC_NON_RAF_HENI, FEC_RAF_HENI, FROMAGES_HENI, FRUITS_JUS_HENI, JAMBON_BLANC_HENI, 
        LAIT_HENI, LAITAGES_HENI, MGA_HENI, MGV_HENI, OEUFS_HENI, PDTS_SUCRES_HENI, 
        PLATS_PREP_CARNES_HENI, PLATS_PREP_VEGETARIENS_HENI, PORC_HENI, POULET_HENI, 
        QUICHES_PIZZAS_TARTES_SALEES_HENI, SAUCES_HENI, SNACKS_AUTRES_HENI, 
        SODAS_LIGHT_HENI, SODAS_SUCRES_HENI, VIANDE_ROUGE_HENI, HENI_TOT
      ),
      .names = "{.col}_diff",
      .fns = ~ .[Periode == "1"] - .[Periode == "0"]
    ),
    across(
      .cols = c(
        FV_POIDS, FRUITS_POIDS, FRUITS_SECS_POIDS, NOIX_POIDS, LEGUMES_POIDS, 
        LEG_SECS_POIDS, FRUITS_prix_kg, LEGUMES_prix_kg, FRUITS_SECS_prix_kg, 
        NOIX_prix_kg, LEG_SECS_prix_kg, VIANDES_POIDS, VIANDE_ROUGE_PORC_POIDS, 
        AUTRE_PDTS_ANIMAUX_POIDS, POULET_OEUFS_POIDS, POISSONS_POIDS, POISSONS_prix_kg, 
        FEC_POIDS, PDTS_LAITIERS_POIDS, PDTS_DISCRETIONNAIRES_POIDS, PLATS_PREP_POIDS, 
        MG_POIDS, SSB_POIDS, ALCOOL_POIDS, depense_alim_uc, MER, MAR, energy_densite, 
        SOMME_POIDS, SOMME_POIDS_HORS_BOISSON, SOMME_POIDS_KCAL, KCAL_SANS_BOISSON, 
        climat_env, couche_ozone_env, ions_env, ozone_env, partic_env, acid_env, 
        eutro_terr_env, eutro_eau_env, eutro_mer_env, sol_env, toxi_eau_env, ress_eau_env, 
        ress_ener_env, ress_min_env, ALCOOL_prix_kg, AUTRE_prix_kg, CAFE_THE_prix_kg, 
        CEREALES_PD_prix_kg, CHARCUTERIE_HORS_JB_prix_kg, DESSERTS_LACTES_prix_kg, 
        EAU_prix_kg, EPICES_CONDIMENTS_prix_kg, FEC_NON_RAF_prix_kg, FEC_RAF_prix_kg, 
        FROMAGES_prix_kg, FRUITS_JUS_prix_kg, JAMBON_BLANC_prix_kg, LAIT_prix_kg, 
        LAITAGES_prix_kg, MGA_prix_kg, MGV_prix_kg, OEUFS_prix_kg, PDTS_SUCRES_prix_kg, 
        PLATS_PREP_CARNES_prix_kg, PLATS_PREP_VEGETARIENS_prix_kg, PORC_prix_kg, 
        QUICHES_PIZZAS_TARTES_SALEES_prix_kg, SAUCES_prix_kg, SNACKS_AUTRES_prix_kg, 
        SODAS_LIGHT_prix_kg, SODAS_SUCRES_prix_kg, VIANDE_ROUGE_prix_kg, 
        CAFE_THE_POIDS, CEREALES_PD_POIDS, CHARCUTERIE_HORS_JB_POIDS, DESSERTS_LACTES_POIDS, 
        FEC_NON_RAF_POIDS, FEC_RAF_POIDS, FROMAGES_POIDS, FRUITS_JUS_POIDS, JAMBON_BLANC_POIDS, 
        LAIT_POIDS, LAITAGES_POIDS, MGA_POIDS, MGV_POIDS, OEUFS_POIDS, PDTS_SUCRES_POIDS, 
        PLATS_PREP_CARNES_POIDS, PLATS_PREP_VEGETARIENS_POIDS, PORC_POIDS, POULET_POIDS, 
        QUICHES_PIZZAS_TARTES_SALEES_POIDS, SAUCES_POIDS, SNACKS_AUTRES_POIDS, 
        SODAS_LIGHT_POIDS, SODAS_SUCRES_POIDS, VIANDE_ROUGE_POIDS, EAU_POIDS, 
        EPICES_CONDIMENTS_POIDS,         CAFE_THE_HENI, CEREALES_PD_HENI, CHARCUTERIE_HORS_JB_HENI, DESSERTS_LACTES_HENI, 
        FEC_NON_RAF_HENI, FEC_RAF_HENI, FROMAGES_HENI, FRUITS_JUS_HENI, JAMBON_BLANC_HENI, 
        LAIT_HENI, LAITAGES_HENI, MGA_HENI, MGV_HENI, OEUFS_HENI, PDTS_SUCRES_HENI, 
        PLATS_PREP_CARNES_HENI, PLATS_PREP_VEGETARIENS_HENI, PORC_HENI, POULET_HENI, 
        QUICHES_PIZZAS_TARTES_SALEES_HENI, SAUCES_HENI, SNACKS_AUTRES_HENI, 
        SODAS_LIGHT_HENI, SODAS_SUCRES_HENI, VIANDE_ROUGE_HENI, HENI_TOT
      ),
      .names = "{.col}_P0",
      .fns = ~ .[Periode == "0"]
    )
  ) %>%
  ungroup()

sgsdata_bis <- bind_rows(carnet_data, ffq_data)


sgsdata_bis <- sgsdata_bis %>% filter(Periode != "0")


colonnes_a_supprimer <- c("FV_POIDS", "FRUITS_POIDS", "FRUITS_SECS_POIDS", "NOIX_POIDS", "LEGUMES_POIDS", 
                          "LEG_SECS_POIDS", "FRUITS_prix_kg", "LEGUMES_prix_kg", "FRUITS_SECS_prix_kg", 
                          "NOIX_prix_kg", "LEG_SECS_prix_kg", "VIANDES_POIDS", "VIANDE_ROUGE_PORC_POIDS", 
                          "AUTRE_PDTS_ANIMAUX_POIDS", "POULET_OEUFS_POIDS", "POISSONS_POIDS", "POISSONS_prix_kg", 
                          "FEC_POIDS", "PDTS_LAITIERS_POIDS", "PDTS_DISCRETIONNAIRES_POIDS", "PLATS_PREP_POIDS", 
                          "MG_POIDS", "SSB_POIDS", "ALCOOL_POIDS", "depense_alim_uc", "MER", "MAR", "energy_densite", 
                          "SOMME_POIDS", "SOMME_POIDS_HORS_BOISSON", "SOMME_POIDS_KCAL", "KCAL_SANS_BOISSON", 
                          "climat_env", "couche_ozone_env", "ions_env", "ozone_env", "partic_env", "acid_env", 
                          "eutro_terr_env", "eutro_eau_env", "eutro_mer_env", "sol_env", "toxi_eau_env", "ress_eau_env", 
                          "ress_ener_env", "ress_min_env", "ALCOOL_prix_kg", "AUTRE_prix_kg", "CAFE_THE_prix_kg", 
                          "CEREALES_PD_prix_kg", "CHARCUTERIE_HORS_JB_prix_kg", "DESSERTS_LACTES_prix_kg", 
                          "EAU_prix_kg", "EPICES_CONDIMENTS_prix_kg", "FEC_NON_RAF_prix_kg", "FEC_RAF_prix_kg", 
                          "FROMAGES_prix_kg", "FRUITS_JUS_prix_kg", "JAMBON_BLANC_prix_kg", "LAIT_prix_kg", 
                          "LAITAGES_prix_kg", "MGA_prix_kg", "MGV_prix_kg", "OEUFS_prix_kg", "PDTS_SUCRES_prix_kg", 
                          "PLATS_PREP_CARNES_prix_kg", "PLATS_PREP_VEGETARIENS_prix_kg", "PORC_prix_kg", 
                          "QUICHES_PIZZAS_TARTES_SALEES_prix_kg", "SAUCES_prix_kg", "SNACKS_AUTRES_prix_kg", 
                          "SODAS_LIGHT_prix_kg", "SODAS_SUCRES_prix_kg", "VIANDE_ROUGE_prix_kg", 
                          "CAFE_THE_POIDS", "CEREALES_PD_POIDS", "CHARCUTERIE_HORS_JB_POIDS", "DESSERTS_LACTES_POIDS", 
                          "FEC_NON_RAF_POIDS", "FEC_RAF_POIDS", "FROMAGES_POIDS", "FRUITS_JUS_POIDS", "JAMBON_BLANC_POIDS", 
                          "LAIT_POIDS", "LAITAGES_POIDS", "MGA_POIDS", "MGV_POIDS", "OEUFS_POIDS", "PDTS_SUCRES_POIDS", 
                          "PLATS_PREP_CARNES_POIDS", "PLATS_PREP_VEGETARIENS_POIDS", "PORC_POIDS", "POULET_POIDS", 
                          "QUICHES_PIZZAS_TARTES_SALEES_POIDS", "SAUCES_POIDS", "SNACKS_AUTRES_POIDS", 
                          "SODAS_LIGHT_POIDS", "SODAS_SUCRES_POIDS", "VIANDE_ROUGE_POIDS", "EAU_POIDS", 
                          "EPICES_CONDIMENTS_POIDS",         "CAFE_THE_HENI", "CEREALES_PD_HENI", "CHARCUTERIE_HORS_JB_HENI", "DESSERTS_LACTES_HENI", 
                          "FEC_NON_RAF_HENI", "FEC_RAF_HENI", "FROMAGES_HENI", "FRUITS_JUS_HENI", "JAMBON_BLANC_HENI", 
                          "LAIT_HENI", "LAITAGES_HENI", "MGA_HENI", "MGV_HENI", "OEUFS_HENI", "PDTS_SUCRES_HENI", 
                          "PLATS_PREP_CARNES_HENI", "PLATS_PREP_VEGETARIENS_HENI", "PORC_HENI", "POULET_HENI", 
                          "QUICHES_PIZZAS_TARTES_SALEES_HENI", "SAUCES_HENI", "SNACKS_AUTRES_HENI", 
                          "SODAS_LIGHT_HENI", "SODAS_SUCRES_HENI", "VIANDE_ROUGE_HENI", "HENI_TOT")

sgsdata_bis <- sgsdata_bis[, !(names(sgsdata_bis) %in% colonnes_a_supprimer)]
sgsdata_bis <- sgsdata_bis%>% 
  filter(Identifiant != "LE300")



#GARDER LES CARACTERISTIQUES SOCIO DEMO A T0
sgsdata_socio_demo <- sgsdata %>% 
  filter(Periode == 0 & Identifiant != "LE300")

sgsdata_selected <- sgsdata_socio_demo[, c(
  "Identifiant", "Mesure","Sexe","Quel.age.avez.vous." ,"Age_Central", "Quel.est.votre.pays.de.naissance.",
  "Combien.de.personnes.vivent.dans.votre.foyer", "Foyer_monoparental", "Avez.vous.des.enfants.a.charge.",
  "Income_UC_INSEE", "Situation_emploi",
  "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.",
  "Aide_alim", "Part_Kcal_BIO", "Part_Kcal_Epiceries", "Part_Kcal_Supermarchés",
  "Part_Depense_BIO", "Part_Depense_Epiceries", "Part_Depense_Supermarchés",
  "Différentiel_aide"
)]


sgsdata_selected <- sgsdata_selected %>%
  mutate(across(
    c(Part_Kcal_BIO, Part_Kcal_Epiceries, Part_Kcal_Supermarchés,
      Part_Depense_BIO, Part_Depense_Epiceries, Part_Depense_Supermarchés),
    ~ . * 100
  ))

sgsdata_selected$Avez.vous.des.enfants.a.charge. <- ifelse(
  sgsdata_selected$Avez.vous.des.enfants.a.charge. == "Refuse de répondre", 
  NA, 
  sgsdata_selected$Avez.vous.des.enfants.a.charge.
)


# Fusionner `sgsdata_bis` et `sgsdata_selected` sur `Identifiant`
sgsdata_bis <- sgsdata_bis %>%
  left_join(sgsdata_selected, by = c("Identifiant", "Mesure"))


sgsdata_bis$Sexe.x <- sgsdata_bis$Sexe.y
sgsdata_bis$Age_Central.x <- as.numeric(sgsdata_bis$Age_Central.y)
sgsdata_bis$Quel.est.votre.pays.de.naissance..x <- sgsdata_bis$Quel.est.votre.pays.de.naissance..y
sgsdata_bis$Quel.age.avez.vous.x <- sgsdata_bis$Quel.age.avez.vous.y
sgsdata_bis$Combien.de.personnes.vivent.dans.votre.foyer.x <- as.numeric(sgsdata_bis$Combien.de.personnes.vivent.dans.votre.foyer.y) 
sgsdata_bis$Foyer_monoparental.x <- as.numeric(sgsdata_bis$Foyer_monoparental.y) 
sgsdata_bis$Avez.vous.des.enfants.a.charge..x <- sgsdata_bis$Avez.vous.des.enfants.a.charge..y 
sgsdata_bis$Income_UC_INSEE.x <- as.numeric(sgsdata_bis$Income_UC_INSEE.y) 
sgsdata_bis$Situation_emploi.x <- sgsdata_bis$Situation_emploi.y 
sgsdata_bis$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu..x <- sgsdata_bis$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu..y 
sgsdata_bis$Aide_alim.x <- sgsdata_bis$Aide_alim.y 
sgsdata_bis$Part_Kcal_BIO.x <- as.numeric(sgsdata_bis$Part_Kcal_BIO.y )
sgsdata_bis$Part_Kcal_Epiceries.x <- as.numeric(sgsdata_bis$Part_Kcal_Epiceries.y )
sgsdata_bis$Part_Kcal_Supermarchés.x <- as.numeric(sgsdata_bis$Part_Kcal_Supermarchés.y )
sgsdata_bis$Part_Depense_BIO.x <- as.numeric(sgsdata_bis$Part_Depense_BIO.y )
sgsdata_bis$Part_Depense_Epiceries.x <- as.numeric(sgsdata_bis$Part_Depense_Epiceries.y )
sgsdata_bis$Part_Depense_Supermarchés.x <- as.numeric(sgsdata_bis$Part_Depense_Supermarchés.y )
sgsdata_bis$Différentiel_aide.x <- as.numeric(sgsdata_bis$Différentiel_aide.y )

#Si valeur nulle indiquer zeo
sgsdata_bis$Part_Kcal_BIO.x <- ifelse(is.na(sgsdata_bis$Part_Kcal_BIO.x), 0, as.numeric(sgsdata_bis$Part_Kcal_BIO.x))
sgsdata_bis$Part_Kcal_Epiceries.x <- ifelse(is.na(sgsdata_bis$Part_Kcal_Epiceries.x), 0, as.numeric(sgsdata_bis$Part_Kcal_Epiceries.x))
sgsdata_bis$Part_Kcal_Supermarchés.x <- ifelse(is.na(sgsdata_bis$Part_Kcal_Supermarchés.x), 0, as.numeric(sgsdata_bis$Part_Kcal_Supermarchés.x))
sgsdata_bis$Part_Depense_BIO.x <- ifelse(is.na(sgsdata_bis$Part_Depense_BIO.x), 0, as.numeric(sgsdata_bis$Part_Depense_BIO.x))
sgsdata_bis$Part_Depense_Epiceries.x <- ifelse(is.na(sgsdata_bis$Part_Depense_Epiceries.x), 0, as.numeric(sgsdata_bis$Part_Depense_Epiceries.x))
sgsdata_bis$Part_Depense_Supermarchés.x <- ifelse(is.na(sgsdata_bis$Part_Depense_Supermarchés.x), 0, as.numeric(sgsdata_bis$Part_Depense_Supermarchés.x))


# Supprimer les colonnes se terminant par ".y"
sgsdata_bis <- sgsdata_bis[, !grepl("\\.y$", names(sgsdata_bis))]

# Renommer les colonnes se terminant par ".y"
names(sgsdata_bis) <- gsub("\\.x$", "", names(sgsdata_bis))




# Créer un nouvel objet workbook
wb <- createWorkbook()

addWorksheet(wb, "sgsdata_bis")
writeData(wb, sheet = "sgsdata_bis", sgsdata_bis  )

saveWorkbook(wb,(paste0("Données analysées - Article N°1 chèques/Fichiers_nettoyés/Fichier_traitement/sgsdata_bis.xlsx")))



