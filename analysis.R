# =============================================================================
# Code_clean.R
# Mémoire – Écarts de genre en mathématiques : données ELFE (INED)
# =============================================================================


# 0. PACKAGES & DATA IMPORT ====================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  here,
  haven, dplyr, tidyr, tidyverse,
  summarytools, skimr,
  ggplot2, patchwork, scales,
  stargazer, knitr,
  gmodels, purrr,
  quantreg, broom,
  lmtest, sandwich, margins,
  MASS, oaxaca, forcats,
  lme4, plm, fixest, erer
)

# Import datasets (paths relative to project root via here)
data                  <- read.csv(here("data", "DATA_DEM_1055_RG.csv"),        fileEncoding = "latin1")
variables_construites <- read.csv(here("data", "EQR12_VARIABLESOCIODEMO.csv"), fileEncoding = "latin1")


# 1. DATA PREPARATION ==========================================================

## 1a. Toy and game data --------------------------------------------------------
# prop_table        → Figure 10 (parent-reported toy play by sex)
# toy_chosen_props  → Figure 11 (child-chosen toys at age 6)
# participants_data → also merged into regression_data and transition_data
#                     to supply toy choice dummies used in Table 7

parent_types <- c("A05R", "A05R2", "A05C", "A05N")
game_vars    <- c("JPOUP", "JPELUCH", "JVOIT", "JBAL", "JCONS",
                  "JMUZ", "JSOCART", "JEDUC", "JDEGUIS", "JDINET", "JFIGUR")

# Long format: toy play by parent type
long_games <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")
  if (!quirep_var %in% names(data)) return(NULL)
  map_dfr(game_vars, function(game) {
    game_var <- paste0(prefix, "_", game)
    if (!game_var %in% names(data)) return(NULL)
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[game_var]])
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],
      sexe_parent = data[[quirep_var]][valid_rows],
      jeu         = game,
      valeur      = data[[game_var]][valid_rows]
    )
  })
})

# clean_games: used by Figure 10
clean_games <- long_games %>%
  filter(sexe_parent %in% c(1, 2),
         sexe_enfant %in% c(1, 2),
         valeur      %in% c(1, 2)) %>%
  mutate(
    sexe_parent = recode(sexe_parent, `1` = "Père",   `2` = "Mère"),
    sexe_enfant = recode(sexe_enfant, `1` = "Garçon", `2` = "Fille"),
    jeu         = factor(jeu, levels = game_vars),
    valeur      = recode(valeur, `1` = 1, `2` = 0)   # 1=Yes, 0=No
  )

# prop_table: used by Figure 10
prop_table <- clean_games %>%
  group_by(jeu, sexe_parent, sexe_enfant) %>%
  summarise(n = n(), proportion_yes = mean(valeur, na.rm = TRUE), .groups = "drop")

# participants_data / toy_chosen_props: children's toy choices at age 6 (Figure 11)
toy_vars   <- paste0("A06E_JOUETS", 1:10)
played_game <- apply(!is.na(data[toy_vars]), 1, any)
data[played_game, toy_vars] <- lapply(data[played_game, toy_vars],
                                       function(x) ifelse(is.na(x), 0, x))
data[toy_vars] <- lapply(data[toy_vars], as.integer)
participants_data <- data[played_game, ]

participants_data$SEXE_ENF <- factor(participants_data$SEXE_ENF, labels = c("Boy", "Girl"))

toy_long <- participants_data %>%
  dplyr::select(SEXE_ENF, all_of(toy_vars)) %>%
  pivot_longer(cols = all_of(toy_vars), names_to = "Toy", values_to = "Chosen")

gender_totals <- participants_data %>% count(SEXE_ENF, name = "total")

toy_chosen_props <- toy_long %>%
  filter(Chosen == 1) %>%
  group_by(SEXE_ENF, Toy) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(gender_totals, by = "SEXE_ENF") %>%
  mutate(prop = n / total * 100)


## 1b. regression_data – main cross-sectional analysis dataframe ---------------
# Used by ALL regression tables (Tables 1–7) and ordered probit (Table 5).
# Key variables and their downstream use:
#   sexe_dummy, A06X_SCMOYMATH_rescaled   → Tables 1, 3, 4, 7 (OLS — main DV and gender var)
#   mère/père_educ_5ans, emploi, revenu   → Tables 1, 3, 4, 7 (socioeconomic controls)
#   type_classe_cp, A06X_NBELEVES         → Table 3 (school characteristics)
#   sexe_enseignant, experience_std_1,
#   A06X_ENSDISCI3                        → Tables 3 & 5 (teacher characteristics)
#   perceived_math/french_skill_f         → Table 5 (ordered probit DV — teacher bias)
#   A06X_SCORE_SDQ_* (SDQ scores)         → Table 6 (pro-social skills)
#   mère/père_educ_3ans                   → Table 6 (maternelle SDQ robustness)
#   A05C_J..._dummy (parent toy dummies)  → Table 7 (parent-reported toy use)
#   A06E_JOUETS 1–10 (child toy choices)  → Table 7 (child-chosen toy regressions)
#   mere/pere_emploi_3ans, chez_qui_vit_3ans,
#   log_revenu_*                          → Section 4.8 (anxiety longitudinal)

# Merge main data with constructed variables
regression_data <- data %>%
  left_join(variables_construites, by = "id_DEM_1055_RG")

# Gender dummy (0=Boy, 1=Girl)
regression_data <- regression_data %>%
  mutate(sexe_dummy = case_when(
    SEXE_ENF == 1 ~ 0,
    SEXE_ENF == 2 ~ 1,
    TRUE ~ NA_real_
  ))

# Class level
regression_data <- regression_data %>%
  mutate(
    niveau_classe = case_when(
      A06X_NIVECLASS1 == 1 ~ "Classe simple : GS",
      A06X_NIVECLASS2 == 1 ~ "Classe simple : CP",
      A06X_NIVECLASS3 == 1 ~ "Classe simple : CE1",
      A06X_NIVECLASS4 == 1 ~ "Double niveau : GS-CP",
      A06X_NIVECLASS5 == 1 ~ "Double niveau : CP-CE1",
      A06X_NIVECLASS6 == 1 ~ "Autre"
    ),
    niveau_classe = factor(niveau_classe),
    type_classe_cp = case_when(
      A06X_NIVECLASS2 == 1 ~ "Classe simple : CP",
      A06X_NIVECLASS4 == 1 ~ "Double niveau : GS-CP",
      A06X_NIVECLASS5 == 1 ~ "Double niveau : CP-CE1",
      TRUE ~ NA_character_
    ),
    type_classe_cp = factor(type_classe_cp, levels = c(
      "Classe simple : CP", "Double niveau : GS-CP", "Double niveau : CP-CE1"
    ))
  )

# Private/public school (numeric)
regression_data <- regression_data %>%
  mutate(A06X_PUBLPRIVc_num = case_when(
    A06X_PUBLPRIVc == "PR" ~ 1,
    A06X_PUBLPRIVc == "PU" ~ 0,
    TRUE ~ NA_real_
  ))

# Teacher sex
regression_data <- regression_data %>%
  mutate(A06X_ENSSEXE = na_if(A06X_ENSSEXE, 0))

# Teacher sex dummies
regression_data$sexe_enseignant      <- ifelse(regression_data$A06X_ENSSEXE == 2, 1, 0)
regression_data$sexe_enseignant_4ans <- ifelse(regression_data$A04X_ENSSEXE == 2, 1, 0)

# Teacher experience (standardised)
regression_data$experience_std   <- scale(regression_data$A04X_ENSDUREE, center = TRUE, scale = TRUE)
regression_data$experience_std_1 <- scale(regression_data$A06X_ENSDUREE, center = TRUE, scale = TRUE)

# Parental employment
regression_data$mere_emploi_3ans <- factor(regression_data$mother_occup_status_3y,
  levels = c(1,2,3,4), labels = c("En_activité","Chômage","Autre","Étudiante"))
regression_data$pere_emploi_3ans <- factor(regression_data$father_occup_status_3y,
  levels = c(1,2,3,4), labels = c("En_activité","Chômage","Autre","Étudiante"))
regression_data$mere_emploi      <- factor(regression_data$mother_occup_status_5y,
  levels = c(1,2,3,4), labels = c("En_activité","Chômage","Autre","Étudiante"))
regression_data$pere_emploi      <- factor(regression_data$father_occup_status_5y,
  levels = c(1,2,3,4), labels = c("En_activité","Chômage","Autre","Étudiante"))

# Parental education (age 3)
regression_data$mère_educ_3ans <- factor(regression_data$meduc_3y, levels = 0:6,
  labels = c("Aucun","Enseignement primaire","Enseignement primaire",
             "Enseignements secondaire niveau lycée",
             "Enseignement supérieur 1er cycle",
             "Enseignement supérieur diplôme de 2ème cycle",
             "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))
regression_data$père_educ_3ans <- factor(regression_data$feduc_3y, levels = 0:6,
  labels = c("Aucun","Enseignement primaire","Enseignement primaire",
             "Enseignements secondaire niveau lycée",
             "Enseignement supérieur 1er cycle",
             "Enseignement supérieur diplôme de 2ème cycle",
             "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))

# Parental education (age 5, collapsed)
regression_data$mère_educ_5ans <- factor(regression_data$meduc_5y, levels = 0:6,
  labels = c("Aucun","Enseignement primaire",
             "Enseignement secondaire niveau collège(Brevet)",
             "Enseignements secondaire niveau lycée",
             "Enseignement supérieur 1er cycle",
             "Enseignement supérieur diplôme de 2ème cycle",
             "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))
regression_data$père_educ_5ans <- factor(regression_data$feduc_5y, levels = 0:6,
  labels = c("Aucun","Enseignement primaire",
             "Enseignement secondaire niveau collège(Brevet)",
             "Enseignements secondaire niveau lycée",
             "Enseignement supérieur 1er cycle",
             "Enseignement supérieur diplôme de 2ème cycle",
             "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))

regression_data$mère_educ_5ans <- fct_collapse(regression_data$mère_educ_5ans,
  "Primaire ou Brevet"  = c("Enseignement primaire","Enseignement secondaire niveau collège(Brevet)"),
  "Lycée"               = "Enseignements secondaire niveau lycée",
  "Bac + 2"             = "Enseignement supérieur 1er cycle",
  "Licence ou Master"   = "Enseignement supérieur diplôme de 2ème cycle",
  "Grandes Écoles"      = "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"
)
regression_data$père_educ_5ans <- fct_collapse(regression_data$père_educ_5ans,
  "Primaire ou Brevet"  = c("Enseignement primaire","Enseignement secondaire niveau collège(Brevet)"),
  "Lycée"               = "Enseignements secondaire niveau lycée",
  "Bac + 2"             = "Enseignement supérieur 1er cycle",
  "Licence ou Master"   = "Enseignement supérieur diplôme de 2ème cycle",
  "Grandes Écoles"      = "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"
)

# Household structure
regression_data <- regression_data %>%
  mutate(
    chez_qui_vit_3ans = case_when(
      child_hhld_3y == 1        ~ "Avec ses deux parents",
      child_hhld_3y %in% c(2,3) ~ "Famille monoparentale",
      child_hhld_3y == 4        ~ "Garde alternée",
      child_hhld_3y == 5        ~ "Enfant placé",
      TRUE ~ NA_character_
    ),
    chez_qui_vit_3ans = factor(chez_qui_vit_3ans,
      levels = c("Avec ses deux parents","Famille monoparentale","Garde alternée","Enfant placé")),
    chez_qui_vit_5ans = case_when(
      child_hhld_5y == 1        ~ "Avec ses deux parents",
      child_hhld_5y %in% c(2,3) ~ "Famille monoparentale",
      child_hhld_5y == 4        ~ "Garde alternée",
      child_hhld_5y == 5        ~ "Enfant placé",
      TRUE ~ NA_character_
    ),
    chez_qui_vit_5ans = factor(chez_qui_vit_5ans,
      levels = c("Avec ses deux parents","Famille monoparentale","Garde alternée","Enfant placé"))
  )

# Migration background
regression_data$migration_mère <- factor(regression_data$mimm, levels = c(1,2,3,4),
  labels = c("Mère qui n'est pas de nationalité française",
             "Mère de nationalité Française descendante de deux parents immigrés",
             "Mère de nationalité Française descendante d'un parent immigré",
             "Autre mère Française"))
regression_data$migration_père <- factor(regression_data$fimm, levels = c(1,2,3,4),
  labels = c("Père qui n'est pas de nationalité française",
             "Père de nationalité Française descendante de deux parents immigrés",
             "Père de nationalité Française descendante d'un parent immigré",
             "Autre Père Française"))
regression_data$migration_mère <- relevel(regression_data$migration_mère, ref = "Autre mère Française")
regression_data$migration_père <- relevel(regression_data$migration_père, ref = "Autre Père Française")

# Toy variables (recode Yes/No)
jueguetes_vars <- c("A05C_JFIGUR","A05C_JVOIT","A05C_JPOUP","A05C_JBAL",
                    "A05C_JCONS","A05C_JDINET","A05C_JMUZ",
                    "A05C_JSOCART","A05C_JPELUCH","A05C_JDEGUIS","A05C_JEDUC")
regression_data <- regression_data %>%
  mutate(across(all_of(jueguetes_vars),
                .fns  = ~ case_when(. == 1 ~ 1, . == 2 ~ 0, TRUE ~ NA_real_),
                .names = "{.col}_dummy"))

# Dependent variable: rescaled math score [0,1]
min_val <- min(regression_data$A06X_SCMOYMATH, na.rm = TRUE)
max_val <- max(regression_data$A06X_SCMOYMATH, na.rm = TRUE)
regression_data$A06X_SCMOYMATH_rescaled <- (regression_data$A06X_SCMOYMATH - min_val) / (max_val - min_val)

# Teacher perception variables (inverted so high = good)
regression_data$A06X_MATHEMATIQUES[regression_data$A06X_MATHEMATIQUES == 0] <- NA
regression_data$perceived_math_skill    <- 6 - regression_data$A06X_MATHEMATIQUES
regression_data$perceived_math_skill_4ans <- 6 - regression_data$A04X_NOMBFORM

regression_data$A06X_FRANCAIS[regression_data$A06X_FRANCAIS == 0] <- NA
regression_data$perceived_french_skill      <- 6 - regression_data$A06X_FRANCAIS
regression_data$perceived_french_skill_4ans <- 6 - regression_data$A04X_LANGAGE

regression_data$perceived_math_skill_f <- factor(
  regression_data$perceived_math_skill, levels = 1:5,
  labels = c("VBA","BA","A","AA","VAB"), ordered = TRUE)
regression_data$perceived_math_skill_4ans_f <- factor(
  regression_data$perceived_math_skill_4ans, levels = 1:5,
  labels = c("Very Below Average","Below Average","Average","Above Average","Very Above Average"),
  ordered = TRUE)
regression_data$perceived_french_skill_f <- factor(
  regression_data$perceived_french_skill, levels = 1:5,
  labels = c("Very Below Average","Below Average","Average","Above Average","Very Above Average"),
  ordered = TRUE)
regression_data$perceived_french_skill_4ans_f <- factor(
  regression_data$perceived_french_skill_4ans, levels = 1:5,
  labels = c("Very Below Average","Below Average","Average","Above Average","Very Above Average"),
  ordered = TRUE)

regression_data$A06X_MATHEMATIQUES_f <- factor(
  regression_data$A06X_MATHEMATIQUES, levels = 1:5,
  labels = c("très en dessous","en dessous","moyen","au dessus","très en dessus"))

# SDQ factors
regression_data$pro_social_rating <- factor(regression_data$A06X_SCORE_CAT_SDQ_PRO_6ANS,
  levels = c(1,2,3), labels = c("normal","limite","anormal"))
regression_data$hyperactivity_rating <- factor(regression_data$A06X_SCORE_CAT_SDQ_HYPER_6ANS,
  levels = c(1,2,3), labels = c("normal","limite","anormal"))
regression_data$emotional_regulation_rating <- factor(regression_data$A06X_SCORE_CAT_SDQ_EMO_6ANS,
  levels = c(1,2,3), labels = c("normal","limite","anormal"))
regression_data$pro_social_rating            <- relevel(regression_data$pro_social_rating,            ref = "limite")
regression_data$emotional_regulation_rating  <- relevel(regression_data$emotional_regulation_rating,  ref = "limite")
regression_data$hyperactivity_rating         <- relevel(regression_data$hyperactivity_rating,         ref = "limite")

# Anxiety (clean zeros)
regression_data$A06X_ANXIEU  <- ifelse(regression_data$A06X_ANXIEU  == 0, NA, regression_data$A06X_ANXIEU)
regression_data$A06X_INQUIE  <- ifelse(regression_data$A06X_INQUIE  == 0, NA, regression_data$A06X_INQUIE)
regression_data$A06X_ATTENTI <- ifelse(regression_data$A06X_ATTENTI == 0, NA, regression_data$A06X_ATTENTI)
regression_data$A06X_MATHEMATIQUES <- ifelse(regression_data$A06X_MATHEMATIQUES == 0, NA, regression_data$A06X_MATHEMATIQUES)

regression_data$A06X_ANXIEU_f <- factor(regression_data$A06X_ANXIEU,
  levels = c(1,2,3), labels = c("very true","a little bit true","not true"))
regression_data$A06X_INQUIE_f <- factor(regression_data$A06X_INQUIE,
  levels = c(1,2,3), labels = c("very true","a little bit true","not true"))
regression_data$A04X_ANXIEU_f <- factor(regression_data$A04X_ANXIEU,
  levels = c(1,2,3), labels = c("very true","a little bit true","not true"))

# Income (log)
regression_data$log_revenu_cp          <- log(regression_data$revenu_part_5y + 1)
regression_data$log_revenu_maternelle  <- log(regression_data$revenu_part_3y + 1)

# Merge toy choices into regression_data
regression_data <- regression_data %>%
  left_join(participants_data %>%
              dplyr::select(id_DEM_1055_RG, all_of(paste0("A06E_JOUETS", 1:10))),
            by = "id_DEM_1055_RG")


## 1c. Derived objects for thesis figures --------------------------------------
# gender_gap_long                            → Figure 2 (gender gap bar chart)
# gender_curve                               → Figure 4 (proportion girls by percentile)
# long_math_data                             → Figure 1.1 (math sub-test decomposition)
# distribution_perceived_skills_teacher_6ans → Figure 8 (teacher perception bar chart)

# gender_gap_long: used by Figure 2
gender_gap_long <- data %>%
  filter(SEXE_ENF %in% c(1,2)) %>%
  pivot_longer(cols = c(A04X_SCmoymath, A04X_SCmoylect, A06X_SCMOYMATH, A06X_SCmoylect),
               names_to = "variable", values_to = "score") %>%
  mutate(
    subject = case_when(variable %in% c("A04X_SCmoymath","A06X_SCMOYMATH") ~ "Mathématiques",
                        TRUE ~ "Français"),
    time    = case_when(variable %in% c("A04X_SCmoymath","A04X_SCmoylect") ~ "Âge 4",
                        TRUE ~ "Âge 6")
  ) %>%
  group_by(subject, time, SEXE_ENF) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SEXE_ENF, values_from = mean_score, names_prefix = "sex_") %>%
  mutate(gender_gap = sex_1 - sex_2)   # Boy minus Girl

# gender_curve: used by Figure 4
get_percentile_gender_dist <- function(dat, score_var, sexe_var = "SEXE_ENF", label = "Timepoint") {
  dat %>%
    filter(!is.na(.data[[score_var]]), .data[[sexe_var]] %in% c(1,2)) %>%
    mutate(Percentile = ntile(.data[[score_var]], 100),
           IsGirl     = ifelse(.data[[sexe_var]] == 2, 1, 0)) %>%
    group_by(Percentile) %>%
    summarise(ProportionGirls = mean(IsGirl) * 100, .groups = "drop") %>%
    mutate(Time = label)
}

girls_maternelle <- get_percentile_gender_dist(data, "A04X_SCmoymath", label = "Maternelle")
girls_cp         <- get_percentile_gender_dist(data, "A06X_SCMOYMATH", label = "CP (début)")
gender_curve     <- bind_rows(girls_maternelle, girls_cp)

# long_math_data: used by Figure 1.1 (math sub-test decomposition)
CPtest_math_vars <- c("A06X_SCCALCD","A06X_SCPROB","A06X_SCCALCM",
                      "A06X_SCSUITE","A06X_SCCOMPA","A06X_SCMOYMATH")
long_math_data <- data %>%
  dplyr::select(SEXE_ENF, all_of(CPtest_math_vars)) %>%
  pivot_longer(cols = all_of(CPtest_math_vars), names_to = "Variable", values_to = "Score") %>%
  drop_na(SEXE_ENF, Score) %>%
  mutate(Variable = gsub("A06X_", "", Variable))

# distribution_perceived_skills_teacher_6ans: used by Figure 8
distribution_perceived_skills_teacher_6ans <- data %>%
  dplyr::select(SEXE_ENF,
         A06X_FRANCAIS, A06X_MATHEMATIQUES, A06X_ACTIPHYS,
         A06X_LANGVIV, A06X_ACTIARTIS, A06X_QUESTMONDE, A06X_MORALCIVIC) %>%
  pivot_longer(-SEXE_ENF, names_to = "Subject", values_to = "Score") %>%
  filter(!is.na(Score), Score %in% 1:5) %>%
  group_by(Subject, SEXE_ENF, Score) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Subject, SEXE_ENF) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  ungroup() %>%
  mutate(Gender = case_when(SEXE_ENF == 1 ~ "Boy", SEXE_ENF == 2 ~ "Girl", TRUE ~ NA_character_))


## 1d. transition_data – longitudinal panel (age 4 → 6) ------------------------
# Links each child's score at age 4 (maternelle) to their score at age 6 (CP).
# → Figure 5  (average percentile change by initial position)
# → Figure 6  (decile mobility stacked bar)
# → Table 2   (comparative advantage regressions: percentile_change as DV)
# → Table 3   (school characteristics with percentile_change as DV)
# → Section 4.7 (reading percentile mobility plot)

transition_data <- data %>%
  dplyr::select(id_DEM_1055_RG, SEXE_ENF,
                A04X_SCmoymath, A06X_SCMOYMATH,
                A04X_SCmoylect, A06X_SCmoylect) %>%
  filter(!is.na(A04X_SCmoymath), !is.na(A06X_SCMOYMATH),
         !is.na(A04X_SCmoylect), !is.na(A06X_SCmoylect), !is.na(SEXE_ENF)) %>%
  left_join(
    regression_data %>%
      dplyr::select(id_DEM_1055_RG, A06X_AGEM, A06X_PUBLPRIVc_num,
                    mère_educ_5ans, père_educ_5ans, mere_emploi, pere_emploi,
                    chez_qui_vit_5ans, migration_père, migration_mère,
                    revenu_part_dec_5y, A06X_ENSSEXE, A06X_ENSDUREE,
                    A06X_ENSDISCI3, A06X_ENSAGE, type_classe_cp,
                    A06X_REPPLUSc, A06X_NBELEVES),
    by = "id_DEM_1055_RG"
  ) %>%
  mutate(
    sexe_dummy  = case_when(SEXE_ENF == 1 ~ 0, SEXE_ENF == 2 ~ 1, TRUE ~ NA_real_),
    sexe_enfant = case_when(SEXE_ENF == 1 ~ "Boy", SEXE_ENF == 2 ~ "Girl", TRUE ~ NA_character_),
    # Percentile ranks
    percentile_4y   = percent_rank(A04X_SCmoymath) * 100,
    percentile_6y   = percent_rank(A06X_SCMOYMATH) * 100,
    percentile_change = percentile_6y - percentile_4y,
    percentile_4y_read  = percent_rank(A04X_SCmoylect) * 100,
    percentile_6y_read  = percent_rank(A06X_SCmoylect) * 100,
    percentile_change_read = percentile_6y_read - percentile_4y_read,
    # Comparative advantage
    math_advantage_4y    = percentile_4y     - percentile_4y_read,
    reading_advantage_4y = percentile_4y_read - percentile_4y,
    # Quartiles
    math_quartile_4y = ntile(A04X_SCmoymath, 4),
    math_initial_quartile_f = factor(math_quartile_4y, labels = c("Q1","Q2","Q3","Q4")),
    reading_quartile_4y = ntile(A04X_SCmoylect, 4),
    reading_initial_quartile_f = factor(reading_quartile_4y, labels = c("Q1","Q2","Q3","Q4")),
    # Deciles
    math_decile_4y = ntile(A04X_SCmoymath, 10),
    math_decile_6y = ntile(A06X_SCMOYMATH, 10)
  )

# Reference level for quartile factors
transition_data$math_initial_quartile_f    <- relevel(transition_data$math_initial_quartile_f,    ref = "Q2")
transition_data$reading_initial_quartile_f <- relevel(transition_data$reading_initial_quartile_f, ref = "Q2")

# Merge toy choice and teacher-rated traits into transition_data
transition_data <- transition_data %>%
  left_join(participants_data %>%
              dplyr::select(id_DEM_1055_RG, all_of(paste0("A06E_JOUETS", 1:10))),
            by = "id_DEM_1055_RG")

traits <- c("ATTENTA","PLAINT","PARTAG","OBEIS","INQUIE",
            "NTIENTP","AAMI","DISTRA","ANXIEU","AIDE","REFLECHI","ATTENTI")
traits_data <- data %>%
  dplyr::select(id_DEM_1055_RG,
                all_of(paste0("A04X_", traits)),
                all_of(paste0("A06X_", traits)))
transition_data <- transition_data %>% left_join(traits_data, by = "id_DEM_1055_RG")

# SDQ merge into transition_data
transition_data <- transition_data %>%
  left_join(
    regression_data %>%
      dplyr::select(id_DEM_1055_RG, pro_social_rating,
                    A06X_SCORE_CAT_SDQ_HYPER_6ANS, A06X_SCORE_CAT_SDQ_EMO_6ANS),
    by = "id_DEM_1055_RG"
  )


## 1e. Mobility objects (computed from transition_data) ------------------------
# stickiness_table    → Table 7  (share of children stuck in bottom/top deciles)
# mobility_summary    → Figure 6 (stacked bar: upward/downward/stable by decile)
# percentile_mobility → Figure 5 (smoothed average percentile change by starting position)

# mobility_data: intermediate — used to compute stickiness_table and mobility_summary
mobility_data <- transition_data %>%
  mutate(
    sticky_low  = ifelse(math_decile_4y <= 3 & math_decile_6y <= 3, 1,
                  ifelse(math_decile_4y <= 3, 0, NA)),
    sticky_high = ifelse(math_decile_4y >= 8 & math_decile_6y >= 8, 1,
                  ifelse(math_decile_4y >= 8, 0, NA)),
    decile_change  = math_decile_6y - math_decile_4y,
    mobility_type  = case_when(decile_change > 0 ~ "Upward",
                               decile_change < 0 ~ "Downward",
                               TRUE ~ "Same")
  )

# stickiness_table: used by Table 7
stickiness_table <- mobility_data %>%
  group_by(sexe_enfant) %>%
  summarise(
    `Stickiness in Bottom Deciles (1–3)` = round(mean(sticky_low,  na.rm = TRUE) * 100, 1),
    `Stickiness in Top Deciles (8–10)`   = round(mean(sticky_high, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(sexe_enfant = ifelse(sexe_enfant == "Girl", "Girls", "Boys"))

# mobility_summary: share of upward/downward/stable by decile and sex
mobility_summary <- mobility_data %>%
  group_by(sexe_enfant, math_decile_4y, mobility_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe_enfant, math_decile_4y) %>%
  mutate(share = round(100 * n / sum(n), 1)) %>%
  dplyr::select(-n)

# percentile_mobility: smoothed average percentile change by initial position
percentile_mobility <- transition_data %>%
  mutate(percentile_4y_bucket = floor(percentile_4y) + 1) %>%
  group_by(sexe_enfant, percentile_4y_bucket) %>%
  summarise(avg_percentile_change = mean(percentile_change, na.rm = TRUE), .groups = "drop")


# 2. THESIS FIGURES & TABLES ===================================================

## Figure 1 – Scores standardisés par sexe (boîtes à moustaches) ---------------
# Output: boxplot_scores_sexe_age.pdf

boxplot_data <- data %>%
  filter(!is.na(SEXE_ENF)) %>%
  dplyr::select(SEXE_ENF, A04X_SCmoymath, A04X_SCmoylect, A06X_SCMOYMATH, A06X_SCmoylect) %>%
  pivot_longer(cols = -SEXE_ENF, names_to = "var", values_to = "score") %>%
  mutate(
    subject = case_when(var %in% c("A04X_SCmoymath","A06X_SCMOYMATH") ~ "Mathématiques",
                        TRUE ~ "Français"),
    age_group   = case_when(var %in% c("A04X_SCmoymath","A04X_SCmoylect") ~ "4 ans", TRUE ~ "6 ans"),
    sexe_enfant = recode_factor(SEXE_ENF, `1`="Garçon", `2`="Fille")
  )

p_boxplot <- ggplot(boxplot_data, aes(x = sexe_enfant, y = score, fill = sexe_enfant)) +
  geom_boxplot() +
  facet_grid(age_group ~ subject) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  labs(x = "Sexe de l'enfant", y = "Score standardisé", fill = "Sexe") +
  scale_fill_manual(values = c("Garçon" = "#A6CEE3", "Fille" = "#B2DF8A")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p_boxplot)
ggsave(here("figures", "boxplot_scores_sexe_age.pdf"), plot = p_boxplot, width = 8, height = 6)


## Figure 1.1 – Décomposition du score de maths (sous-tests) -------------------

p_math_subtests <- ggplot(long_math_data,
                           aes(x = as.factor(SEXE_ENF), y = Score,
                               fill = as.factor(SEXE_ENF))) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.6, alpha = 0.7) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.25) +
  facet_wrap(~ Variable, scales = "free_y",
             labeller = as_labeller(c(
               "SCCALCD"   = "Calcul mental dicté",
               "SCPROB"    = "Résolution de problèmes",
               "SCCALCM"   = "Calcul mental pratique",
               "SCSUITE"   = "Suites numériques",
               "SCCOMPA"   = "Comparaison de nombres",
               "SCMOYMATH" = "Score moyen de maths"
             ))) +
  scale_fill_manual(values = c("skyblue","salmon"), labels = c("Garçons","Filles")) +
  labs(title = "Distribution des scores de mathématiques par sexe",
       x = "Sexe (1 = Garçon, 2 = Fille)", y = "Score", fill = "Sexe") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

print(p_math_subtests)


## Figure 2 – Écart de genre en mathématiques et en français -------------------
# Output: gender_gap_plot.pdf

p_gap <- ggplot(gender_gap_long, aes(x = time, y = gender_gap, fill = subject)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, alpha = 0.7, color = "gray95") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("Mathématiques" = "#A6D854", "Français" = "#BC80BD")) +
  labs(x = "Âge de passation", y = "Différence moyenne de score", fill = "Matière") +
  coord_cartesian(ylim = c(-0.07, 0.15)) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        axis.title.y  = element_text(margin = margin(r = 10)),
        axis.title.x  = element_text(margin = margin(t = 10)),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

print(p_gap)
ggsave(here("figures", "gender_gap_plot.pdf"), plot = p_gap, width = 6.5, height = 4.5, dpi = 300)


## Figure 3 – Distribution des scores (densités 2×2) ---------------------------
# Output: combined_plot.pdf

common_theme <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.direction = "vertical", legend.key.size = unit(0.6, "cm"))

p1 <- ggplot(regression_data, aes(x = A04X_SCmoymath, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0","#55A868"), labels = c("Garçon","Fille")) +
  labs(title = "Mathématiques à 4 ans", x = "Score standardisé", y = "Densité", fill = "Sexe") +
  scale_x_continuous(breaks = seq(-2,1,1), labels = scales::number_format(accuracy = 0.1)) +
  common_theme + theme(axis.title.x = element_blank())

p2 <- ggplot(regression_data, aes(x = A06X_SCMOYMATH, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0","#55A868"), labels = c("Garçon","Fille")) +
  labs(title = "Mathématiques à 6 ans", x = "Score standardisé", y = "Densité", fill = "Sexe") +
  scale_x_continuous(breaks = seq(-2,1,1), labels = scales::number_format(accuracy = 0.1)) +
  common_theme + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

p3 <- ggplot(regression_data, aes(x = A04X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0","#55A868"), labels = c("Garçon","Fille")) +
  labs(title = "Français à 4 ans", x = "Score standardisé", y = "Densité", fill = "Sexe") +
  common_theme

p4 <- ggplot(regression_data, aes(x = A06X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0","#55A868"), labels = c("Garçon","Fille")) +
  labs(title = "Français à 6 ans", x = "Score standardisé", y = "Densité", fill = "Sexe") +
  common_theme + theme(axis.title.y = element_blank())

combined_plot <- (p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")
print(combined_plot)
ggsave(here("figures", "combined_plot.pdf"), plot = combined_plot, width = 12, height = 8, dpi = 300)


## Figure 4 – Proportion de filles par centile en maths ------------------------
# Output: proportion_girls_percentiles.pdf

p_percentile <- ggplot(gender_curve, aes(x = Percentile, y = ProportionGirls, color = Time)) +
  geom_line(linewidth = 0.5, alpha = 0.9) +
  geom_point(size = 1.5, shape = 21, fill = "white", stroke = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = "Percentile in Mathematics", y = "Proportion of Girls (%)", color = "Timepoint") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 10),
        axis.text  = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20)))

print(p_percentile)
ggsave(here("figures", "proportion_girls_percentiles.pdf"), plot = p_percentile, width = 9, height = 7)


## Figure 5 – Évolution moyenne du centile (4 → 6 ans) -------------------------
# Output: percentile_change_math.pdf

p_mob <- ggplot(percentile_mobility,
                aes(x = percentile_4y_bucket, y = avg_percentile_change, color = sexe_enfant)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Girl" = "#55A868", "Boy" = "#4C72B0")) +
  labs(x = "Initial Percentile Rank at Age 4", y = "Average Percentile Change", color = "Sexe") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right",
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)))

print(p_mob)
ggsave(here("figures", "percentile_change_math.pdf"), plot = p_mob, width = 10, height = 5)


## Figure 6 – Mobilité décile (4 → 6 ans) --------------------------------------
# Output: mobility_by_decile.pdf

p_decile <- ggplot(mobility_summary,
                   aes(x = factor(math_decile_4y), y = share, fill = mobility_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  facet_wrap(~ sexe_enfant,
             labeller = labeller(sexe_enfant = c("Girl" = "Girls", "Boy" = "Boys"))) +
  scale_fill_manual(
    values = c("Upward" = "#1b9e77", "Same" = "#bdbdbd", "Downward" = "#d95f02"),
    labels = c("Upward" = "Mobilité ascendante", "Same" = "Stable", "Downward" = "Mobilité descendante")
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Décile initial en mathématiques (âge 4)",
       y = "Pourcentage des enfants", fill = "Type de mobilité") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

print(p_decile)
ggsave(here("figures", "mobility_by_decile.pdf"), plot = p_decile, width = 8, height = 6)


## Table 7 – Stickiness (degré de persistence en haut/bas) ---------------------

stickiness_table %>%
  kable(caption = "Stickiness in Math Performance by Gender",
        col.names = c("Genre","Bas de la distribution (1–3)","Haut de la distribution (8–10)"))


## Figure 8 – Perception des compétences par l'enseignant ----------------------

distribution_perceived_skills_teacher_6ans %>%
  filter(Subject %in% c("A06X_FRANCAIS","A06X_MATHEMATIQUES","A06X_LANGVIV")) %>%
  ggplot(aes(x = factor(Score), y = Percent, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Subject, scales = "free_y", ncol = 3,
             labeller = as_labeller(c(A06X_FRANCAIS="Français",
                                      A06X_MATHEMATIQUES="Mathématiques",
                                      A06X_LANGVIV="Langues vivantes"))) +
  labs(x = "Score attribué (1 = Très au-dessus de la moyenne, 5 = Très en dessous de la moyenne)",
       y = "Pourcentage (%)", fill = "Sexe de l'enfant") +
  scale_fill_manual(values = c("Boy" = "#A6CEE3", "Girl" = "#FDB462")) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        strip.text    = element_text(face = "bold", size = 12),
        legend.position = "bottom")


## Figure 10 – Préférences de jouets déclarées par les parents -----------------
# Output: jouets_enfants.pdf

p_toys_parent <- ggplot(prop_table,
                        aes(x = reorder(jeu, proportion_yes), y = proportion_yes, fill = sexe_enfant)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ sexe_parent, labeller = label_value) +
  coord_flip() +
  labs(x = "Type de jouet", y = "Proportion (Oui)", fill = "Sexe de l'enfant") +
  scale_x_discrete(labels = c(JPOUP="Poupée", JPELUCH="Peluche", JVOIT="Voitures",
                               JBAL="Ballon", JCONS="Jeux de construction",
                               JMUZ="Instrument musical", JSOCART="Jeux de société",
                               JEDUC="Jeu éducatif", JDEGUIS="Costume",
                               JDINET="Dinette", JFIGUR="Figurines d'action")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Garçon" = "#A6CEE3", "Fille" = "#B2DF8A")) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        strip.text = element_text(size = 11), panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), panel.spacing = unit(2, "lines"))

print(p_toys_parent)
ggsave(here("figures", "jouets_enfants.pdf"), plot = p_toys_parent, width = 9, height = 6)


## Figure 11 – Jouets préférés de l'enfant (âge 6) -----------------------------
# Output: jouet_prefere_6ans.pdf

p_toys_child <- ggplot(toy_chosen_props,
                       aes(x = reorder(Toy, -prop), y = prop, fill = SEXE_ENF)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("A06E_JOUETS1"="Livre",    "A06E_JOUETS2"="Poupée",
                               "A06E_JOUETS3"="Voitures", "A06E_JOUETS4"="Jeux de construction",
                               "A06E_JOUETS5"="Jeux vidéo", "A06E_JOUETS6"="Costume de princesse",
                               "A06E_JOUETS7"="Costume de pirate", "A06E_JOUETS8"="Dinette",
                               "A06E_JOUETS9"="Figurines d'action", "A06E_JOUETS10"="Jeux de société")) +
  scale_fill_manual(values = c("Boy" = "#A6CEE3", "Girl" = "#B2DF8A"),
                    labels = c("Boy" = "Boys", "Girl" = "Girls")) +
  labs(x = "Jouet", y = "Proportion (par sexe)", fill = "Genre") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        legend.position = "bottom")

print(p_toys_child)
ggsave(here("figures", "jouet_prefere_6ans.pdf"), plot = p_toys_child, width = 8, height = 5)


# 3. REGRESSION TABLES =========================================================
# Global variable labels — inherited by all etable() calls via setFixest_dict()
setFixest_dict(c(
  A06X_SCMOYMATH_rescaled  = "Score en maths (6 ans)",
  percentile_change        = "Progrès en maths",
  percentile_change_read   = "Progrès en français",
  A06X_SCMOYMATH           = "Score en maths (6 ans, brut)",
  A06X_SCCOMPA             = "Comparaison de nombres",
  A06X_SCCALCD             = "Calcul mental dicté",
  A06X_SCCALCM             = "Calcul mental pratique",
  A06X_SCPROB              = "Résolution de problèmes",
  A06X_SCSUITE             = "Suites numériques",
  A04X_SCmoymath           = "Score maths (4 ans)",
  A04X_SCmoylect           = "Score français (4 ans)",
  sexe_dummy               = "Fille",
  A06X_AGEM                = "Âge",
  A06X_PUBLPRIVc_num       = "École Privée",
  A06X_NBELEVES            = "Taille classe",
  sexe_enseignant          = "Enseignant Homme",
  A06X_ENSDISCI3           = "Formation scientifique",
  experience_std_1         = "Expérience enseignant",
  math_advantage_4y        = "Avantage comparatif en maths",
  reading_advantage_4y     = "Avantage comparatif en français",
  A06X_SCORE_SDQ_PRO_6ANS  = "Score Pro Social",
  A06X_SCORE_SDQ_EMO_6ANS  = "Score Troubles Émotionnels",
  A06X_SCORE_SDQ_HYPER_6ANS = "Score Hyperactivité",
  A04X_SCmoymath           = "Score maths (4 ans)"
))

## Table 1 – Main gender-gap regressions (math score at 6) --------------------

regression_naive <- feols(A06X_SCMOYMATH_rescaled ~ sexe_dummy,
                          data = regression_data)

regression_parent_educ <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06X_AGEM + A06X_PUBLPRIVc_num +
    mère_educ_5ans + père_educ_5ans,
  data = regression_data)

regression_house_income <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06X_AGEM + A06X_PUBLPRIVc_num +
    mère_educ_5ans + père_educ_5ans + mere_emploi + pere_emploi +
    chez_qui_vit_5ans + migration_père + migration_mère + revenu_part_dec_5y,
  data = regression_data)

regression_past_lecture_score <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06X_AGEM + A06X_PUBLPRIVc_num +
    mère_educ_5ans + père_educ_5ans + mere_emploi + pere_emploi +
    chez_qui_vit_5ans + migration_père + migration_mère + revenu_part_dec_5y +
    A04X_SCmoymath + A04X_SCmoylect,
  data = regression_data)

table1_drop <- c("mère_educ_5ans", "père_educ_5ans", "mere_emploi", "pere_emploi",
                 "chez_qui_vit_5ans", "migration_père", "migration_mère", "revenu_part_dec_5y")

# Console output
etable(list("(1)" = regression_naive, "(2)" = regression_parent_educ,
            "(3)" = regression_house_income),
       drop = table1_drop,
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = regression_naive, "(2)" = regression_parent_educ,
            "(3)" = regression_house_income),
       drop = table1_drop,
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table1_gender_gap_naive.tex"))

# Console output
etable(list("(1)" = regression_past_lecture_score),
       drop = table1_drop,
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = regression_past_lecture_score),
       drop = table1_drop,
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table1_gender_gap_with_prior.tex"))


## Table 2 – Comparative advantage and math progress ---------------------------

lm_math_gap <- feols(percentile_change ~ sexe_dummy, data = transition_data)

lm_math_adv_1 <- feols(percentile_change ~ sexe_dummy * math_advantage_4y,
                        data = transition_data)

lm_math_adv_2 <- feols(percentile_change ~ sexe_dummy * math_advantage_4y +
                          sexe_dummy * math_initial_quartile_f +
                          math_advantage_4y * math_initial_quartile_f,
                        data = transition_data)

lm_math_adv_3 <- feols(percentile_change ~ sexe_dummy * math_advantage_4y +
                          sexe_dummy * math_initial_quartile_f +
                          sexe_dummy * percentile_change_read,
                        data = transition_data)

lm_math_adv_4 <- feols(percentile_change ~ sexe_dummy * math_advantage_4y +
                          sexe_dummy * math_initial_quartile_f +
                          sexe_dummy * percentile_change_read +
                          A06X_AGEM + père_educ_5ans + mère_educ_5ans,
                        data = transition_data)


table2_drop <- c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect",
                 "mère_educ_5ans", "père_educ_5ans")

# Console output
etable(list("(1)" = lm_math_gap, "(2)" = lm_math_adv_1, "(3)" = lm_math_adv_2,
            "(4)" = lm_math_adv_3, "(5)" = lm_math_adv_4),
       drop = table2_drop,
       extralines = list("Contrôles individuels" = c("Non","Non","Non","Non","Oui")),
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = lm_math_gap, "(2)" = lm_math_adv_1, "(3)" = lm_math_adv_2,
            "(4)" = lm_math_adv_3, "(5)" = lm_math_adv_4),
       drop = table2_drop,
       extralines = list("Contrôles individuels" = c("Non","Non","Non","Non","Oui")),
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table2_comparative_advantage_math.tex"))


# Reading comparative advantage (robustness)
lm_reading_gap <- feols(percentile_change_read ~ sexe_dummy, data = transition_data)

lm_read_adv_1 <- feols(percentile_change_read ~ sexe_dummy * reading_advantage_4y,
                        data = transition_data)

lm_read_adv_2 <- feols(percentile_change_read ~ sexe_dummy * reading_advantage_4y +
                          sexe_dummy * reading_initial_quartile_f,
                        data = transition_data)

# Console output
etable(list("(1)" = lm_reading_gap, "(2)" = lm_read_adv_1, "(3)" = lm_read_adv_2),
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = lm_reading_gap, "(2)" = lm_read_adv_1, "(3)" = lm_read_adv_2),
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table2_comparative_advantage_read.tex"))


## Table 3 – School and teacher characteristics --------------------------------

# School interactions
regression_school__01 <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy * A06X_PUBLPRIVc_num +
    sexe_dummy * A06X_NBELEVES + sexe_dummy * type_classe_cp +
    A06X_AGEM + A04X_SCmoymath + A04X_SCmoylect + mère_educ_5ans + père_educ_5ans,
  data = regression_data)

regression_school_2 <- feols(
  percentile_change ~ sexe_dummy * A06X_PUBLPRIVc_num +
    sexe_dummy * A06X_NBELEVES + sexe_dummy * type_classe_cp +
    A06X_AGEM + mère_educ_5ans + A04X_SCmoymath + A04X_SCmoylect + père_educ_5ans,
  data = transition_data)


table3_school_drop <- c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect",
                        "mère_educ_5ans", "père_educ_5ans")

# Console output
etable(list("(1)" = regression_school__01),
       drop = table3_school_drop,
       extralines = list("Contrôles individuels" = c("Oui")),
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = regression_school__01),
       drop = table3_school_drop,
       extralines = list("Contrôles individuels" = c("Oui")),
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table3_school_characteristics.tex"))

# Teacher interactions
regression_teacher_21 <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy * sexe_enseignant +
    sexe_dummy * A06X_ENSDUREE + sexe_dummy * A06X_ENSDISCI3 +
    A06X_AGEM + A04X_SCmoymath + A04X_SCmoylect + mère_educ_5ans + père_educ_5ans,
  data = regression_data)

regression_teacher_2_base <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy * sexe_enseignant +
    sexe_dummy * A06X_ENSDUREE + sexe_dummy * A06X_ENSDISCI3 +
    A06X_AGEM + mère_educ_5ans + père_educ_5ans,
  data = regression_data)


table3_teacher_drop <- c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect",
                         "mère_educ_5ans", "père_educ_5ans")

# Console output
etable(list("(1)" = regression_teacher_2_base, "(2)" = regression_teacher_21),
       drop = table3_teacher_drop,
       extralines = list("Contrôles individuels" = c("Non","Oui")),
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = regression_teacher_2_base, "(2)" = regression_teacher_21),
       drop = table3_teacher_drop,
       extralines = list("Contrôles individuels" = c("Non","Oui")),
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table3_teacher_characteristics.tex"))


## Table 4 – Interaction terms (family background × gender) --------------------

regression_interac_migration <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy * migration_père + sexe_dummy * migration_mère +
    sexe_dummy * mère_educ_5ans + sexe_dummy * père_educ_5ans +
    sexe_dummy * chez_qui_vit_5ans + A06X_AGEM + revenu_part_dec_5y,
  data = regression_data)

regression_interac_migration_1 <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy * migration_père + sexe_dummy * migration_mère +
    sexe_dummy * mère_educ_5ans + sexe_dummy * père_educ_5ans +
    sexe_dummy * chez_qui_vit_5ans + A06X_AGEM + revenu_part_dec_5y +
    A04X_SCmoymath + A04X_SCmoylect,
  data = regression_data)

# Console output
etable(list("(1)" = regression_interac_migration, "(2)" = regression_interac_migration_1),
       title = "Tableau – Interactions entre genre et caractéristiques familiales",
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = regression_interac_migration, "(2)" = regression_interac_migration_1),
       title = "Tableau – Interactions entre genre et caractéristiques familiales",
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table4_interactions_family.tex"))


## Table 5 – Teacher bias: ordered probit --------------------------------------

# Main model (6 ans, maths)
oprobit_2 <- polr(
  perceived_math_skill_f ~ sexe_dummy + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect +
    sexe_enseignant + experience_std_1,
  data = regression_data, method = "probit", Hess = TRUE)
summary(oprobit_2)
stargazer(oprobit_2, type = "text",
          dep.var.labels = "Perception niveau en maths (6 ans)",
          keep.stat = c("n"))

# Marginal effects
ME <- ocME(oprobit_2)
colnames(ME$out$ME.all) <- c("Très en dessous","En dessous","Moyen","En dessus","Très en dessus")
stargazer(ME$out$ME.all, type = "text", summary = FALSE, digits = 3,
          title = "Marginal Effects: Teacher's Perceived Math Ability")

# Interaction: gender × teacher sex × teacher background
oprobit_3 <- polr(
  perceived_math_skill_f ~ sexe_dummy * sexe_enseignant * A06X_ENSDISCI3 +
    A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data, method = "probit", Hess = TRUE)

# By teacher sex – boys only
oprobit_12 <- polr(
  perceived_math_skill_f ~ sexe_enseignant * A06X_ENSDISCI3 + A06X_SCMOYMATH +
    A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data %>% filter(sexe_dummy == 0), method = "probit", Hess = TRUE)

# By teacher sex – girls only
oprobit_13 <- polr(
  perceived_math_skill_f ~ sexe_enseignant * A06X_ENSDISCI3 + A06X_SCMOYMATH +
    A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data %>% filter(sexe_dummy == 1), method = "probit", Hess = TRUE)

summary(oprobit_3)
summary(oprobit_12)
summary(oprobit_13)
stargazer(oprobit_3, oprobit_12, oprobit_13, type = "latex",
          dep.var.labels = "Perception de l'enseignant",
          column.labels = c("Tous","Garçons","Filles"),
          covariate.labels = c("Fille","Enseignant Homme","Formation scientifique",
                               "Score en maths","Âge de l'enfant","Score en français",
                               "Expérience de l'enseignant",
                               "Fille × Enseignant Homme","Fille × Formation scientifique",
                               "Enseignant Homme × Formation scientifique",
                               "Fille × Enseignant Homme × Formation scientifique"),
          keep.stat = c("n"))

# Robustness: teacher bias at age 4 and for French
oprobit_4ans_maths <- polr(
  perceived_math_skill_4ans_f ~ sexe_dummy + A04X_SCmoymath + A04X_AGE4A +
    A04X_SCmoylect + sexe_enseignant_4ans + experience_std,
  data = regression_data, method = "probit", Hess = TRUE)

oprobit_6ans_french <- polr(
  perceived_french_skill_f ~ sexe_dummy + A06X_SCMOYMATH + A06X_AGEM +
    A06X_SCmoylect + sexe_enseignant + experience_std_1,
  data = regression_data, method = "probit", Hess = TRUE)

oprobit_4ans_french <- polr(
  perceived_french_skill_4ans_f ~ sexe_dummy + A04X_SCmoymath + A04X_AGE4A +
    A04X_SCmoylect + sexe_enseignant_4ans + experience_std,
  data = regression_data, method = "probit", Hess = TRUE)

summary(oprobit_2)
summary(oprobit_4ans_maths)
summary(oprobit_6ans_french)
summary(oprobit_4ans_french)
stargazer(oprobit_2, oprobit_4ans_maths, oprobit_6ans_french, oprobit_4ans_french,
          type = "latex", keep.stat = c("n"))


## Table 6 – Pro-social skills and math sub-scores -----------------------------


sdq_drop_base  <- c("A06X_AGEM", "mère_educ_5ans", "père_educ_5ans")
sdq_drop_score <- c("A06X_AGEM", "A04X_SCmoymath", "mère_educ_5ans", "père_educ_5ans")
sdq_drop_mat   <- c("A04X_AGE4A", "mère_educ_3ans", "père_educ_3ans")

# Without prior scores
hihi <- feols(
  A06X_SCMOYMATH ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)

hihi_1 <- feols(A06X_SCCOMPA ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                  sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                  A06X_AGEM + père_educ_5ans + mère_educ_5ans, data = regression_data)

hihi_2 <- feols(A06X_SCCALCD ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                  sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                  A06X_AGEM + père_educ_5ans + mère_educ_5ans, data = regression_data)

hihi_3 <- feols(A06X_SCCALCM ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                  sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                  A06X_AGEM + père_educ_5ans + mère_educ_5ans, data = regression_data)

hihi_4 <- feols(A06X_SCPROB ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                  sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                  A06X_AGEM + père_educ_5ans + mère_educ_5ans, data = regression_data)

hihi_5 <- feols(A06X_SCSUITE ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                  sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                  A06X_AGEM + père_educ_5ans + mère_educ_5ans, data = regression_data)

# With prior math score
pro_social_generalmaths <- feols(
  A06X_SCMOYMATH ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath,
  data = regression_data)

pro_social_numbcomp <- feols(A06X_SCCOMPA ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                               sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                               A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, data = regression_data)

pro_social_mentalcalcul <- feols(A06X_SCCALCD ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                                   sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                                   A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, data = regression_data)

pro_social_mentalpract <- feols(A06X_SCCALCM ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                                  sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                                  A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, data = regression_data)

pro_social_probresolu <- feols(A06X_SCPROB ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                                 sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                                 A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, data = regression_data)

pro_social_suitnum <- feols(A06X_SCSUITE ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS +
                              sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS +
                              A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, data = regression_data)

# At kindergarten (age 4)
hihi_maternelle <- feols(
  A04X_SCmoymath ~ sexe_dummy * A04X_Score_sdq_pro_4ans +
    sexe_dummy * A04X_Score_cat_sdq_emo_4ans + sexe_dummy * A04X_Score_sdq_hyper_4ans +
    mère_educ_3ans + père_educ_3ans + A04X_AGE4A,
  data = regression_data)

# Console output
etable(list("Général" = hihi, "Comparaison de nombres" = hihi_1,
            "Calcul mental dicté" = hihi_2),
       drop = sdq_drop_base,
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("Général" = hihi, "Comparaison de nombres" = hihi_1,
            "Calcul mental dicté" = hihi_2),
       drop = sdq_drop_base,
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table6_sdq_no_prior_A.tex"))

# Console output
etable(list("Calcul mental pratique" = hihi_3, "Résolution de problèmes" = hihi_4,
            "Suite numérique" = hihi_5),
       drop = sdq_drop_base,
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("Calcul mental pratique" = hihi_3, "Résolution de problèmes" = hihi_4,
            "Suite numérique" = hihi_5),
       drop = sdq_drop_base,
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table6_sdq_no_prior_B.tex"))

# Console output
etable(list("Général" = pro_social_generalmaths, "Comparaison de nombres" = pro_social_numbcomp,
            "Calcul mental dicté" = pro_social_mentalcalcul),
       drop = sdq_drop_score,
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("Général" = pro_social_generalmaths, "Comparaison de nombres" = pro_social_numbcomp,
            "Calcul mental dicté" = pro_social_mentalcalcul),
       drop = sdq_drop_score,
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table6_sdq_with_prior_A.tex"))

# Console output
etable(list("Calcul mental pratique" = pro_social_mentalpract,
            "Résolution de problèmes" = pro_social_probresolu,
            "Suite numérique" = pro_social_suitnum),
       drop = sdq_drop_score,
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("Calcul mental pratique" = pro_social_mentalpract,
            "Résolution de problèmes" = pro_social_probresolu,
            "Suite numérique" = pro_social_suitnum),
       drop = sdq_drop_score,
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table6_sdq_with_prior_B.tex"))

# Console output
etable(list("(1)" = hihi_maternelle),
       drop = sdq_drop_mat,
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("(1)" = hihi_maternelle),
       drop = sdq_drop_mat,
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table6_sdq_maternelle.tex"))


## Table 7 – Toy use and math score --------------------------------------------

toy_choice_vars <- paste0("A06E_JOUETS", 1:10)

lm_toys_raw_2 <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy +
    A06E_JOUETS1 + A06E_JOUETS2 + A06E_JOUETS3 + A06E_JOUETS4 + A06E_JOUETS5 +
    A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS10 +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data)

lm_toys_raw_2_boys <- feols(
  A06X_SCMOYMATH_rescaled ~
    A06E_JOUETS1 + A06E_JOUETS2 + A06E_JOUETS3 + A06E_JOUETS4 + A06E_JOUETS5 +
    A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS10 +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 0))

lm_toys_raw_2_girls <- feols(
  A06X_SCMOYMATH_rescaled ~
    A06E_JOUETS1 + A06E_JOUETS2 + A06E_JOUETS3 + A06E_JOUETS4 + A06E_JOUETS5 +
    A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS10 +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1))


toy_drop <- c("A06X_AGEM", "mère_educ_5ans", "père_educ_5ans", "revenu_part_dec_5y")

# Console output
etable(list("Tous" = lm_toys_raw_2, "Garçons" = lm_toys_raw_2_boys,
            "Filles" = lm_toys_raw_2_girls),
       drop = toy_drop,
       extralines = list("Contrôles individuels" = c("Oui","Oui","Oui")),
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("Tous" = lm_toys_raw_2, "Garçons" = lm_toys_raw_2_boys,
            "Filles" = lm_toys_raw_2_girls),
       drop = toy_drop,
       extralines = list("Contrôles individuels" = c("Oui","Oui","Oui")),
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table7_toy_choice_child.tex"))

# Parent-reported toy use
prudencio <- feols(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy +
    A05C_JCONS_dummy + A05C_JDINET_dummy + A05C_JPELUCH_dummy + A05C_JMUZ_dummy +
    A05C_JBAL_dummy + A05C_JSOCART_dummy + A05C_JPOUP_dummy + A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy + A06X_PUBLPRIVc_num + A06X_AGEM +
    mère_educ_5ans + père_educ_5ans + mere_emploi + pere_emploi +
    migration_père + migration_mère + revenu_part_dec_5y,
  data = regression_data)

prudencio_filles <- feols(
  A06X_SCMOYMATH_rescaled ~
    A05C_JCONS_dummy + A05C_JDINET_dummy + A05C_JPELUCH_dummy + A05C_JMUZ_dummy +
    A05C_JBAL_dummy + A05C_JSOCART_dummy + A05C_JPOUP_dummy + A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy + A06X_PUBLPRIVc_num + A06X_AGEM +
    mère_educ_5ans + père_educ_5ans + mere_emploi + pere_emploi +
    migration_père + migration_mère + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1))

prudencio_boys <- feols(
  A06X_SCMOYMATH_rescaled ~
    A05C_JCONS_dummy + A05C_JDINET_dummy + A05C_JPELUCH_dummy + A05C_JMUZ_dummy +
    A05C_JBAL_dummy + A05C_JSOCART_dummy + A05C_JPOUP_dummy + A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy + A06X_PUBLPRIVc_num + A06X_AGEM +
    mère_educ_5ans + père_educ_5ans + mere_emploi + pere_emploi +
    migration_père + migration_mère + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 0))

prudencio_boys_score <- feols(
  A06X_SCMOYMATH_rescaled ~
    A05C_JCONS_dummy + A05C_JDINET_dummy + A05C_JPELUCH_dummy + A05C_JMUZ_dummy +
    A05C_JBAL_dummy + A05C_JSOCART_dummy + A05C_JPOUP_dummy + A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy + A06X_PUBLPRIVc_num + A06X_AGEM +
    mère_educ_5ans + père_educ_5ans + mere_emploi + pere_emploi +
    migration_père + migration_mère + A04X_SCmoymath + A04X_SCmoylect + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 0))

prudencio_filles_score <- feols(
  A06X_SCMOYMATH_rescaled ~
    A05C_JCONS_dummy + A05C_JDINET_dummy + A05C_JPELUCH_dummy + A05C_JMUZ_dummy +
    A05C_JBAL_dummy + A05C_JSOCART_dummy + A05C_JPOUP_dummy + A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy + A06X_PUBLPRIVc_num + A06X_AGEM +
    mère_educ_5ans + père_educ_5ans + mere_emploi + pere_emploi +
    migration_père + migration_mère + A04X_SCmoymath + A04X_SCmoylect + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1))


parent_toy_drop <- c("A06X_AGEM", "mère_educ_5ans", "A04X_SCmoylect", "A04X_SCmoymath",
                     "père_educ_5ans", "revenu_part_dec_5y", "mere_emploi", "pere_emploi",
                     "migration_père", "migration_mère", "A06X_PUBLPRIVc_num")

# Console output
etable(list("Tous" = prudencio, "Filles" = prudencio_filles,
            "Garçons" = prudencio_boys, "Garçons + Score" = prudencio_boys_score),
       drop = parent_toy_drop,
       extralines = list("Contrôles individuels" = c("Oui","Oui","Oui","Oui")),
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("Tous" = prudencio, "Filles" = prudencio_filles,
            "Garçons" = prudencio_boys, "Garçons + Score" = prudencio_boys_score),
       drop = parent_toy_drop,
       extralines = list("Contrôles individuels" = c("Oui","Oui","Oui","Oui")),
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table7_toy_choice_parent.tex"))

# parent_toy_dict without sexe_dummy (for filles_score which has no sexe_dummy regressor)
parent_toy_dict_no_sexe <- parent_toy_dict[names(parent_toy_dict) != "sexe_dummy"]

# Console output
etable(list("Filles + Score" = prudencio_filles_score),
       drop = parent_toy_drop,
       extralines = list("Contrôles individuels" = c("Oui")),
       fitstat = c("n", "r2", "ar2", "f"))

# LaTeX output
etable(list("Filles + Score" = prudencio_filles_score),
       drop = parent_toy_drop,
       extralines = list("Contrôles individuels" = c("Oui")),
       fitstat = c("n", "r2", "ar2", "f"),
       tex = TRUE,
       file = here("tables", "table7_toy_choice_parent_girls.tex"))



# 4. OTHER ANALYSES ============================================================

## 4.1 Activités éducatives (déclaration parentale) ----------------------------

activity_vars <- paste0("ACT", 1:14)

activity_data <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")
  if (!quirep_var %in% names(data)) return(NULL)
  map_dfr(activity_vars, function(act) {
    act_var <- paste0(prefix, "_", act)
    if (!act_var %in% names(data)) return(NULL)
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[act_var]])
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],
      sexe_parent = data[[quirep_var]][valid_rows],
      activité    = act,
      valeur      = data[[act_var]][valid_rows]
    )
  })
})

clean_activity <- activity_data %>%
  filter(sexe_parent %in% c(1, 2),
         sexe_enfant %in% c(1, 2),
         valeur      %in% c(1, 2)) %>%
  mutate(
    sexe_parent = recode(sexe_parent, `1` = "Father", `2` = "Mother"),
    sexe_enfant = recode(sexe_enfant, `1` = "Boy",    `2` = "Girl"),
    activité    = factor(activité, levels = paste0("ACT", 1:14))
  )

mean_table <- clean_activity %>%
  group_by(activité, sexe_parent, sexe_enfant) %>%
  summarise(mean_value = mean(valeur, na.rm = TRUE), .groups = "drop") %>%
  arrange(activité, sexe_parent, sexe_enfant)
print(mean_table, n = 56)

# Plot: mean participation in educational activities
ggplot(mean_table, aes(x = activité, y = mean_value, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ sexe_parent) +
  labs(title = "Mean participation in educational activities",
       x = "Activity", y = "Mean response", fill = "Child sex") +
  coord_cartesian(ylim = c(1, 2)) +
  scale_fill_manual(values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Selected activity breakdowns (by parent and child gender)
# ACT7: Recite numbers
tapply(clean_activity$valeur[clean_activity$activité == "ACT7"],
       list(Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT7"],
            Child  = clean_activity$sexe_enfant[clean_activity$activité == "ACT7"]),
       function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x))))

# ACT8: Copy letters/words
tapply(clean_activity$valeur[clean_activity$activité == "ACT8"],
       list(Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT8"],
            Child  = clean_activity$sexe_enfant[clean_activity$activité == "ACT8"]),
       function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x))))

# ACT10: Memory games
tapply(clean_activity$valeur[clean_activity$activité == "ACT10"],
       list(Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT10"],
            Child  = clean_activity$sexe_enfant[clean_activity$activité == "ACT10"]),
       function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x))))

# ACT9: Puzzles
tapply(clean_activity$valeur[clean_activity$activité == "ACT9"],
       list(Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT9"],
            Child  = clean_activity$sexe_enfant[clean_activity$activité == "ACT9"]),
       function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x))))

# ACT12: Play with a ball
tapply(clean_activity$valeur[clean_activity$activité == "ACT12"],
       list(Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT12"],
            Child  = clean_activity$sexe_enfant[clean_activity$activité == "ACT12"]),
       function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x))))


## 4.2 CDI – Inventaire développement cognitif ---------------------------------

cdi_vars <- paste0("CDI", 1:52)

long_cdi <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")
  if (!quirep_var %in% names(data)) return(NULL)
  map_dfr(cdi_vars, function(cdi) {
    cdi_var <- paste0(prefix, "_", cdi)
    if (!cdi_var %in% names(data)) return(NULL)
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[cdi_var]])
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],
      sexe_parent = data[[quirep_var]][valid_rows],
      item        = cdi,
      valeur      = data[[cdi_var]][valid_rows]
    )
  })
})

clean_cdi <- long_cdi %>%
  filter(sexe_enfant %in% c(1, 2), sexe_parent %in% c(1, 2), valeur %in% c(1, 2)) %>%
  mutate(
    sexe_enfant   = recode(sexe_enfant, `1` = "Boy",    `2` = "Girl"),
    sexe_parent   = recode(sexe_parent, `1` = "Father", `2` = "Mother"),
    valeur_binary = 2 - valeur   # 1 → 1 (yes), 2 → 0 (no)
  )

prop_cdi <- clean_cdi %>%
  group_by(item, sexe_parent, sexe_enfant) %>%
  summarise(proportion_yes = mean(valeur_binary, na.rm = TRUE), n = n(), .groups = "drop")
print(prop_cdi, n = 500)

# Math-related CDI items
math_cognitive_cdi <- c("CDI16", "CDI17", "CDI18", "CDI20", "CDI21",
                        "CDI23", "CDI24", "CDI36", "CDI41", "CDI42",
                        "CDI43", "CDI44", "CDI46", "CDI50", "CDI51", "CDI52")

math_prop <- prop_cdi %>%
  filter(item %in% math_cognitive_cdi) %>%
  mutate(label = recode(item,
    CDI16 = "4+ word sentences",   CDI17 = "Uses 'because'",
    CDI18 = "Clear speech",        CDI20 = "Asks 'why', 'how'",
    CDI21 = "20+ words",           CDI23 = "Says 'big/small'",
    CDI24 = "Knows object use",    CDI36 = "Tells structured story",
    CDI41 = "Writes letters/numbers", CDI42 = "Writes name",
    CDI43 = "Counts 10+ objects",  CDI44 = "Compares size",
    CDI46 = "Draws square",        CDI50 = "Knows alphabet",
    CDI51 = "Counts 1–30",         CDI52 = "Simple arithmetic"
  ))
print(math_prop, n = 70)


## 4.3 SDQ – Questionnaire forces et difficultés -------------------------------

sqi_vars <- c("AGITE", "PLAINT", "CRISES", "SOLITA", "OBEIS", "INQUIE", "NTIENTP",
              "AAMI", "BAGAR", "PLEURE", "ESTAIME", "DISTRA", "ANXIEU", "MENTTRI",
              "HARCEL", "REFLECH", "VOLEMA", "PREADUL", "PEURFREQ", "ATTENTI",
              "ESTIMDIF", "ESTIMDIFD", "DERANDIF", "VIEMDIF", "AMITDIF", "APPRDIF")

clean_sqi <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")
  if (!quirep_var %in% names(data)) return(NULL)
  map_dfr(sqi_vars, function(var) {
    sqi_var <- paste0(prefix, "_", var)
    if (!sqi_var %in% names(data)) return(NULL)
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[sqi_var]])
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],
      sexe_parent = data[[quirep_var]][valid_rows],
      item        = var,
      valeur      = data[[sqi_var]][valid_rows]
    )
  })
}) %>%
  filter(valeur %in% c(1, 2, 3, 4), sexe_enfant %in% c(1, 2), sexe_parent %in% c(1, 2)) %>%
  mutate(
    sexe_enfant = recode(sexe_enfant, `1` = "Boy",    `2` = "Girl"),
    sexe_parent = recode(sexe_parent, `1` = "Father", `2` = "Mother")
  )


## 4.4 Tempérament (déclaration parentale) -------------------------------------

temperament_vars <- c("ENERGIQ", "CALM", "ATTENT", "ADROIT", "ENDUR", "SPORTIF")

clean_temperament <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")
  if (!quirep_var %in% names(data)) return(NULL)
  map_dfr(temperament_vars, function(var) {
    temp_var <- paste0(prefix, "_", var)
    if (!temp_var %in% names(data)) return(NULL)
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[temp_var]]) &
                  data[[temp_var]] %in% c(1, 2)
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],
      sexe_parent = data[[quirep_var]][valid_rows],
      item        = var,
      valeur      = data[[temp_var]][valid_rows]
    )
  })
}) %>%
  filter(sexe_enfant %in% c(1, 2), sexe_parent %in% c(1, 2)) %>%
  mutate(
    sexe_enfant   = recode(sexe_enfant, `1` = "Boy",    `2` = "Girl"),
    sexe_parent   = recode(sexe_parent, `1` = "Father", `2` = "Mother"),
    valeur_binary = ifelse(valeur == 1, 1, 0)
  )

personality_prop <- clean_temperament %>%
  group_by(item, sexe_parent, sexe_enfant) %>%
  summarise(proportion_yes = mean(valeur_binary, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(item, sexe_parent, sexe_enfant)
print(personality_prop, n = 50)


## 4.5 Questionnaire enseignant maternelle (4 ans) -----------------------------

competency_vars <- c("A04X_LANGAGE", "A04X_ACTIPHYS", "A04X_ACTIARTIS",
                     "A04X_NOMBFORM", "A04X_TEMPESPA")
behavior_vars   <- c("A04X_ATTENTA", "A04X_PLAINT", "A04X_PARTAG", "A04X_OBEIS",
                     "A04X_INQUIE", "A04X_NTIENTP", "A04X_AAMI", "A04X_DISTRA",
                     "A04X_ANXIEU", "A04X_AIDE", "A04X_REFLECHI", "A04X_ATTENTI")
teacher_vars    <- c(competency_vars, behavior_vars)

clean_maternelle <- map_dfr(teacher_vars, function(var) {
  if (!var %in% names(data)) return(NULL)
  valid_rows <- !is.na(data[[var]])
  tibble(
    sexe_enfant = data[["SEXE_ENF"]][valid_rows],
    item        = var,
    valeur      = as.numeric(data[[var]][valid_rows])
  )
}) %>%
  filter(sexe_enfant %in% c(1, 2)) %>%
  mutate(sexe_enfant = recode(sexe_enfant, `1` = "Boy", `2` = "Girl"))


## 4.6 Questionnaire enfant (6 ans) – jeux et aspirations ----------------------

jeu_6ans <- c(
  "A06E_AIMECOLE1","A06E_AIMECOLE2","A06E_AIMECOLE3","A06E_AIMECOLE4","A06E_AIMECOLE5","A06E_AIMECOLE6",
  "A06E_ACTIMATIN1","A06E_ACTIMATIN2","A06E_ACTIMATIN3","A06E_ACTIMATIN4","A06E_ACTIMATIN5",
  "A06E_ACTIMATIN6","A06E_ACTIMATIN7","A06E_ACTIMATIN8",
  "A06E_ACTICLASSE1","A06E_ACTICLASSE2","A06E_ACTICLASSE3","A06E_ACTICLASSE4","A06E_ACTICLASSE5",
  "A06E_ACTICLASSE7","A06E_ACTICLASSE8",
  "A06E_BONBONS",
  paste0("A06E_JOUETS", 1:10), "A06E_JOUETSTV", "A06E_MANGER",
  "A06E_ACTCHA","A06E_ACTCHI","A06E_ACTLIO","A06E_ACTOIS","A06E_ACTOUR",
  "A06E_BEAU","A06E_BETE","A06E_FORT","A06E_FRAGILE","A06E_GENTIL",
  "A06E_MALIN","A06E_MECHANT","A06E_MOCHE",
  "A06E_PASETREANIMAL","A06E_ETREANIMAL","A06E_OBJET",
  "A06E_BEAUPLACE","A06E_BETEPLACE","A06E_FORTPLACE","A06E_FRAGILEPLACE","A06E_GENTILPLACE",
  "A06E_MALINPLACE","A06E_MECHANTPLACE","A06E_MOCHEPLACE",
  "A06E_PASETREANIMALPLACE","A06E_ETREANIMALPLACE",
  "A06E_SANCTION",
  paste0("A06E_MET", 1:6)
)

clean_jeu_6ans <- data %>%
  dplyr::select(SEXE_ENF, all_of(jeu_6ans)) %>%
  pivot_longer(cols = -SEXE_ENF, names_to = "item", values_to = "valeur") %>%
  filter(!is.na(valeur)) %>%
  mutate(sexe_enfant = recode(SEXE_ENF, `1` = "Boy", `2` = "Girl"))


## 4.7 Lecture moyenne du centile – évolution en lecture (4 → 6 ans) -----------

# Class distribution (descriptive)
table(recode(as.character(data$A06X_SCOL),
             "1"="GS","2"="CP","3"="CE1","4"="CLIS","5"="Autre"))

# Mean math scores at 6 by gender
data %>%
  filter(!is.na(A06X_SCMOYMATH), !is.na(SEXE_ENF)) %>%
  mutate(sexe_enfant = recode(SEXE_ENF, `1`="Boy", `2`="Girl")) %>%
  group_by(sexe_enfant) %>%
  summarise(n = n(), mean_math = mean(A06X_SCMOYMATH, na.rm = TRUE), .groups = "drop")

# T-test
t.test(A06X_SCMOYMATH ~ factor(SEXE_ENF, levels = c(1,2), labels = c("Boy","Girl")), data = data)

# Average change in reading percentile (age 4 → 6)
reading_percentile_mobility <- transition_data %>%
  mutate(percentile_4y_bucket = floor(percentile_4y_read) + 1) %>%
  group_by(sexe_enfant, percentile_4y_bucket) %>%
  summarise(avg_percentile_change = mean(percentile_change_read, na.rm = TRUE), .groups = "drop")

p_mob_read <- ggplot(reading_percentile_mobility,
                     aes(x = percentile_4y_bucket, y = avg_percentile_change, color = sexe_enfant)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Girl" = "#55A868", "Boy" = "#4C72B0")) +
  labs(x = "Initial Reading Percentile Rank at Age 4", y = "Average Percentile Change", color = "Sexe") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right",
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)))

print(p_mob_read)


## 4.8 Analyse de l'anxiété (longitudinale) ------------------------------------

long_panel <- regression_data %>%
  rename(
    id                  = id_DEM_1055_RG,
    math_maternelle     = A04X_SCmoymath,
    french_maternelle   = A04X_SCmoylect,
    age_maternelle      = A04X_AGE4A,
    math_cp             = A06X_SCMOYMATH,
    french_cp           = A06X_SCmoylect,
    age_cp              = A06X_AGEM,
    nbeleves_maternelle = A04X_NBELEVES,
    nbeleves_cp         = A06X_NBELEVES,
    chez_qui_vit_maternelle = chez_qui_vit_3ans,
    chez_qui_vit_cp         = chez_qui_vit_5ans,
    père_emploi_maternelle  = pere_emploi_3ans,
    père_emploi_cp          = pere_emploi,
    mère_emploi_maternelle  = mere_emploi_3ans,
    mère_emploi_cp          = mere_emploi,
    ATTENTA_maternelle = A04X_ATTENTA, ATTENTA_cp = A06X_ATTENTA,
    PLAINT_maternelle  = A04X_PLAINT,  PLAINT_cp  = A06X_PLAINT,
    PARTAG_maternelle  = A04X_PARTAG,  PARTAG_cp  = A06X_PARTAG,
    OBEIS_maternelle   = A04X_OBEIS,   OBEIS_cp   = A06X_OBEIS,
    INQUIE_maternelle  = A04X_INQUIE,  INQUIE_cp  = A06X_INQUIE,
    NTIENTP_maternelle = A04X_NTIENTP, NTIENTP_cp = A06X_NTIENTP,
    AAMI_maternelle    = A04X_AAMI,    AAMI_cp    = A06X_AAMI,
    DISTRA_maternelle  = A04X_DISTRA,  DISTRA_cp  = A06X_DISTRA,
    ANXIEU_maternelle  = A04X_ANXIEU,  ANXIEU_cp  = A06X_ANXIEU,
    AIDE_maternelle    = A04X_AIDE,    AIDE_cp    = A06X_AIDE,
    REFLECHI_maternelle = A04X_REFLECHI, REFLECHI_cp = A06X_REFLECHI,
    ATTENTI_maternelle = A04X_ATTENTI, ATTENTI_cp = A06X_ATTENTI,
    ensex_maternelle   = A04X_ENSSEXE, ensex_cp   = A06X_ENSSEXE,
    ensduree_maternelle = A04X_ENSDUREE, ensduree_cp = A06X_ENSDUREE
  ) %>%
  dplyr::select(
    id, sexe_enfant = SEXE_ENF,
    math_maternelle, french_maternelle, age_maternelle,
    math_cp, french_cp, age_cp,
    log_revenu_maternelle, log_revenu_cp,
    chez_qui_vit_maternelle, chez_qui_vit_cp,
    père_emploi_maternelle, père_emploi_cp,
    mère_emploi_maternelle, mère_emploi_cp,
    nbeleves_maternelle, nbeleves_cp,
    ATTENTA_maternelle, ATTENTA_cp, PLAINT_maternelle, PLAINT_cp,
    PARTAG_maternelle, PARTAG_cp, OBEIS_maternelle, OBEIS_cp,
    INQUIE_maternelle, INQUIE_cp, NTIENTP_maternelle, NTIENTP_cp,
    AAMI_maternelle, AAMI_cp, DISTRA_maternelle, DISTRA_cp,
    ANXIEU_maternelle, ANXIEU_cp, AIDE_maternelle, AIDE_cp,
    REFLECHI_maternelle, REFLECHI_cp, ATTENTI_maternelle, ATTENTI_cp,
    ensex_maternelle, ensex_cp, ensduree_maternelle, ensduree_cp
  ) %>%
  pivot_longer(
    cols = c(math_maternelle, french_maternelle, age_maternelle,
             math_cp, french_cp, age_cp, nbeleves_maternelle, nbeleves_cp,
             ATTENTA_maternelle, ATTENTA_cp, PLAINT_maternelle, PLAINT_cp,
             PARTAG_maternelle, PARTAG_cp, OBEIS_maternelle, OBEIS_cp,
             INQUIE_maternelle, INQUIE_cp, NTIENTP_maternelle, NTIENTP_cp,
             AAMI_maternelle, AAMI_cp, DISTRA_maternelle, DISTRA_cp,
             ANXIEU_maternelle, ANXIEU_cp, AIDE_maternelle, AIDE_cp,
             REFLECHI_maternelle, REFLECHI_cp, ATTENTI_maternelle, ATTENTI_cp,
             ensex_maternelle, ensex_cp, ensduree_maternelle, ensduree_cp),
    names_to  = c(".value","time"),
    names_pattern = "(.*)_(maternelle|cp)"
  ) %>%
  mutate(
    time        = factor(time, levels = c("maternelle","cp")),
    time_num    = ifelse(time == "maternelle", 0, 1),
    sexe_enfant = factor(sexe_enfant, levels = c(1,2), labels = c("Garçon","Fille")),
    revenu_part = ifelse(time == "maternelle", log_revenu_maternelle, log_revenu_cp),
    chez_qui_vit = factor(case_when(
      time == "maternelle" ~ as.character(chez_qui_vit_maternelle),
      time == "cp"         ~ as.character(chez_qui_vit_cp))),
    père_emploi = factor(case_when(
      time == "maternelle" ~ as.character(père_emploi_maternelle),
      time == "cp"         ~ as.character(père_emploi_cp))),
    mère_emploi = factor(case_when(
      time == "maternelle" ~ as.character(mère_emploi_maternelle),
      time == "cp"         ~ as.character(mère_emploi_cp)))
  )

# Growth model (LME)
growth_model <- lmer(math ~ time_num * sexe_enfant + (1 | id), data = long_panel)
summary(growth_model)

growth_model_controls <- lmer(
  math ~ time_num * sexe_enfant + french + revenu_part + mère_emploi + père_emploi +
    chez_qui_vit + (1 | id),
  data = long_panel)
summary(growth_model_controls)

# Fixed effects model (feols)
panel_data <- pdata.frame(long_panel, index = c("id","time"))
panel_data$ANXIEU   <- as.numeric(panel_data$ANXIEU)
panel_data$ATTENTI  <- as.numeric(panel_data$ATTENTI)
panel_data$REFLECHI <- as.numeric(panel_data$REFLECHI)
panel_data$DISTRA   <- as.numeric(panel_data$DISTRA)
panel_data <- panel_data %>% mutate(ensduree_z = as.numeric(scale(ensduree)))

fe_model <- feols(
  math ~ sexe_enfant * (ensduree_z + ANXIEU + ATTENTI) | id,
  data = panel_data)
summary(fe_model)

# Oaxaca-Blinder decomposition
regression_data <- regression_data %>%
  mutate(math_quantile = factor(ntile(A06X_SCMOYMATH, 10)))

oaxaca_data <- regression_data %>%
  dplyr::select(A06X_SCMOYMATH, A06X_AGEM, mère_educ_5ans, père_educ_5ans,
         revenu_part_dec_5y, SEXE_ENF, A04X_SCmoymath, A04X_SCmoylect) %>%
  filter(!is.na(A06X_SCMOYMATH), !is.na(A06X_AGEM), !is.na(mère_educ_5ans),
         !is.na(père_educ_5ans), !is.na(revenu_part_dec_5y), !is.na(SEXE_ENF),
         !is.na(A04X_SCmoymath), !is.na(A04X_SCmoylect)) %>%
  mutate(
    SEXE_ENF = ifelse(SEXE_ENF == 1, 0, ifelse(SEXE_ENF == 2, 1, NA)),
    mère_educ_5ans = fct_collapse(mère_educ_5ans,
      Faible = c("Primaire ou Brevet"),
      Moyen  = "Lycée",
      Élevé  = c("Bac + 2","Licence ou Master","Grandes Écoles")),
    père_educ_5ans = fct_collapse(père_educ_5ans,
      Faible = c("Primaire ou Brevet"),
      Moyen  = "Lycée",
      Élevé  = c("Bac + 2","Licence ou Master","Grandes Écoles"))
  )

decomp2 <- oaxaca(
  formula = A06X_SCMOYMATH ~ mère_educ_5ans + père_educ_5ans + revenu_part_dec_5y +
    A04X_SCmoymath + A04X_SCmoylect | SEXE_ENF,
  data = oaxaca_data, R = 1000)

decomp2$y
decomp2$twofold$overall
plot(decomp2, components = c("endowments","coefficients"))
