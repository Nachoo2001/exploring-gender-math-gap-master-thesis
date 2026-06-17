library(haven)
library(dplyr)
library(tidyr)
library(summarytools)
library(ggplot2)
library(stargazer)
library(tidyverse)
library(gmodels)
library(stargazer)
library(skimr)
library(patchwork)
library(purrr)
library(quantreg)
library(broom)
library(knitr)
library(lmtest)
library(sandwich)
library(margins)
library(scales)







# Importer la base toutes les variables
data <- read.csv("/Users/nacho/Documents/DATA_DEM_1055_RG.csv", fileEncoding = "latin1")
variables_construites <- read.csv("/Users/nacho/Documents/EQR12_VARIABLESOCIODEMO.csv", fileEncoding = "latin1")
data2 <- read.csv("/Users/nacho/Documents/EQR39_SCOREIDE.csv", fileEncoding = "latin1")


# Statistiques descriptives ==========================================================
## Pratiques éducatives --------------------------------------------------------------------------------

#Define parent types and activity variable suffixes
parent_types <- c("A05R", "A05R2", "A05C", "A05N")
activity_vars <- paste0("ACT", 1:14)

#Build long-format dataset
activity_data <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")
  
  # Skip if QUIREP column doesn't exist
  if (!quirep_var %in% names(data)) return(NULL)
  
  map_dfr(activity_vars, function(act) {
    act_var <- paste0(prefix, "_", act)
    
    # Skip if ACT variable doesn't exist
    if (!act_var %in% names(data)) return(NULL)
    
    # Only select rows where QUIREP is not NA
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[act_var]])
    
    # Create clean tibble for valid rows only
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],
      sexe_parent = data[[quirep_var]][valid_rows],
      activité = act,
      valeur = data[[act_var]][valid_rows]
    )
  })
})

#Clean and recode
clean_activity <- activity_data %>%
  filter(sexe_parent %in% c(1, 2),
         sexe_enfant %in% c(1, 2),
         valeur %in% c(1, 2)) %>%
  mutate(
    sexe_parent = recode(sexe_parent, `1` = "Father", `2` = "Mother"),
    sexe_enfant = recode(sexe_enfant, `1` = "Boy", `2` = "Girl"),
    activité = factor(activité, levels = paste0("ACT", 1:14))
  )

## Calculate mean values per group
mean_table <- clean_activity %>%
  group_by(activité, sexe_parent, sexe_enfant) %>%
  summarise(mean_value = mean(valeur, na.rm = TRUE), .groups = "drop") %>%
  arrange(activité, sexe_parent, sexe_enfant)
print(mean_table, n=56)

#Plot
ggplot(mean_table, aes(x = activité, y = mean_value, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ sexe_parent) +
  labs(
    title = "Mean participation in educational activities",
    x = "Activity", y = "Mean response",
    fill = "Child sex"
  ) +
  coord_cartesian(ylim = c(1, 2)) +
  scale_fill_manual(
    values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")  # You can tweak these hex codes
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Recite numbers  --------------------------------------------------------------------------------
tapply(
  clean_activity$valeur[clean_activity$activité == "ACT7"],
  list(
    Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT7"],
    Child = clean_activity$sexe_enfant[clean_activity$activité == "ACT7"]
  ),
  function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x)))
)
#Comment -> There is no difference between gender


### Copy letter or words  -----------------------------------------------------------------------
tapply(
  clean_activity$valeur[clean_activity$activité == "ACT8"],
  list(
    Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT8"],
    Child = clean_activity$sexe_enfant[clean_activity$activité == "ACT8"]
  ),
  function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x)))
)
#Comment -> Parents tend to do more copying letters with the girls


### Memory games  --------------------------------------------------------------------------------
tapply(
  clean_activity$valeur[clean_activity$activité == "ACT10"],
  list(
    Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT10"],
    Child = clean_activity$sexe_enfant[clean_activity$activité == "ACT10"]
  ),
  function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x)))
)
#Comment -> Parents tend to actually play more often memory games with girls 


### Do puzzles ----------------------------------------------------------------------------------------
tapply(
  clean_activity$valeur[clean_activity$activité == "ACT9"],
  list(
    Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT9"],
    Child = clean_activity$sexe_enfant[clean_activity$activité == "ACT9"]
  ),
  function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x)))
)
#Comment-> Parents tend to do more puzzle with girls                                                           play more often puzzle with boys 


### Play with a ball -------------------------------------------
tapply(
  clean_activity$valeur[clean_activity$activité == "ACT12"],
  list(
    Parent = clean_activity$sexe_parent[clean_activity$activité == "ACT12"],
    Child = clean_activity$sexe_enfant[clean_activity$activité == "ACT12"]
  ),
  function(x) sprintf("%.3f (n=%d)", mean(x, na.rm = TRUE), sum(!is.na(x)))
)
#Comment -> Fathers play ball much more often with boys than girls. Also mothers


## Toys  ----------------------------------------------------------------------------------------
#Comment -> ("Je vais vous citer des jouets, pouvez-vous me dire si [enfant Elfe] joue avec:")
game_vars <- c("JPOUP", "JPELUCH", "JVOIT", "JBAL", "JCONS",
               "JMUZ", "JSOCART", "JEDUC", "JDEGUIS", "JDINET", "JFIGUR")

#Build long-format dataset for games
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
      jeu = game,
      valeur = data[[game_var]][valid_rows]
    )
  })
})

#Clean and recode
clean_games <- long_games %>%
  filter(sexe_parent %in% c(1, 2),
         sexe_enfant %in% c(1, 2),
         valeur %in% c(1, 2)) %>%
  mutate(
    sexe_parent = recode(sexe_parent, `1` = "Père", `2` = "Mère"),
    sexe_enfant = recode(sexe_enfant, `1` = "Garçon", `2` = "Fille"),
    jeu = factor(jeu, levels = game_vars),
    
    # ✅ Recode: 1 = Yes → 1, 2 = No → 0
    valeur = recode(valeur, `1` = 1, `2` = 0)
  )


#Calculate mean values
mean_games <- clean_games %>%
  group_by(jeu, sexe_parent, sexe_enfant) %>%
  summarise(
    mean_value = mean(valeur, na.rm = TRUE),
    n = n(),  # 👈 total number of children in that group
    .groups = "drop"
  ) %>%
  arrange(jeu, sexe_parent, sexe_enfant)
print(mean_games, n = 50)


#Mean differences 
game_diffs <- mean_games %>%
  select(-n) %>%  # 👈 drop the count column for now
  pivot_wider(names_from = sexe_enfant, values_from = mean_value) %>%
  mutate(
    diff_boy_minus_girl = Boy - Girl
  )
print(game_diffs, n = 30)



# Plot toy differences
ggplot(game_diffs, aes(x = reorder(jeu, diff_boy_minus_girl), y = diff_boy_minus_girl, fill = sexe_parent)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Gender difference in toy play (Boy - Girl)",
    x = "Toy",
    y = "Mean difference (Boy - Girl)",
    fill = "Parent"
  ) +
  scale_x_discrete(labels = c(
    JPOUP   = "Dolls",
    JPELUCH = "Stuffed animals",
    JVOIT   = "Toy cars",
    JBAL    = "Balls",
    JCONS   = "Building toys",
    JMUZ    = "Musical instruments",
    JSOCART = "Board/card games",
    JEDUC   = "Educational games (CD/DVD)",
    JDEGUIS = "Costumes",
    JDINET  = "Toy kitchen",
    JFIGUR  = "Action figures"
  )) +
  scale_fill_manual(values = c("Father" = "#1f78b4", "Mother" = "#e377c2")) +  # blue and pink
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal()
#Comment -> Dolls: Gendered toy play. Girls more likely to be playing with dolls. Both fathers and mothers report this, but the gap is slightly bigger for fathers.
#.       -> Kitchen toys: Again, girls are reported to play much more with toy kitchens.
#.       -> Building toys: Boys seem to play a little bit more with building toys. 0,05 mean difference for fathers ans 0,09 mean difference for nmothers.


### Computing proportions ----------------------------------------------------------------------

prop_table <- clean_games %>%
  group_by(jeu, sexe_parent, sexe_enfant) %>%
  summarise(
    n = n(),  # Total observations
    proportion_yes = mean(valeur, na.rm = TRUE),
    .groups = "drop"
  )

print(prop_table, n = 50)


#Plot 
ggplot(prop_table, aes(x = reorder(jeu, proportion_yes), y = proportion_yes, fill = sexe_enfant)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ sexe_parent, labeller = label_value) +
  coord_flip() +
  labs(
    title = "Proportion of Children Playing with Each Toy",
    subtitle = "Split by Parent and Child Gender",
    x = "Toy Type",
    y = "Proportion (Yes)",
    fill = "Child Gender"
  ) +
  scale_x_discrete(labels = c(
    JPOUP   = "Dolls",
    JPELUCH = "Stuffed Animals",
    JVOIT   = "Toy Cars",
    JBAL    = "Balls",
    JCONS   = "Building Toys",
    JMUZ    = "Musical Instruments",
    JSOCART = "Board/Card Games",
    JEDUC   = "Educational Games",
    JDEGUIS = "Costumes",
    JDINET  = "Toy Kitchen",
    JFIGUR  = "Action Figures"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Garçon" = "#1f78b4", "Fille" = "#e377c2")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank(),  # removes horizontal lines
    panel.grid.minor = element_blank()
  )
#Comment -> "Only 23% of boys (vs. 95% of girls) are reported to play with dolls by fathers"
#           Construction toys:
#           according to fathers -> 92% of girls vs while 97% of boys 
#           according to mothers -> 86% girls vs 95% of boys


## CDI ----------------------------------------------------------------------------------------

# Define CDI variable codes (CDI1 to CDI52)
cdi_vars <- paste0("CDI", 1:52)

# Build long-format dataset for CDI responses
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
      item = cdi,
      valeur = data[[cdi_var]][valid_rows]
    )
  })
})

# Clean and recode
clean_cdi <- long_cdi %>%
  filter(
    sexe_enfant %in% c(1, 2),
    sexe_parent %in% c(1, 2),
    valeur %in% c(1, 2)  # keep only "yes" or "no"
  ) %>%
  mutate(
    sexe_enfant = recode(sexe_enfant, `1` = "Boy", `2` = "Girl"),
    sexe_parent = recode(sexe_parent, `1` = "Father", `2` = "Mother"),
    valeur_binary = 2 - valeur  # 1 → 1 (yes), 2 → 0 (no)
  )

prop_cdi <- clean_cdi %>%
  group_by(item, sexe_parent, sexe_enfant) %>%
  summarise(
    proportion_yes = mean(valeur_binary, na.rm = TRUE),
    n = n(),  # Count the number of observations
    .groups = "drop"
  )

print(prop_cdi, n=500)

### Maths related variables  ----------------------------------------------------------------------
math_cognitive_cdi <- c("CDI16", "CDI17", "CDI18", "CDI20", "CDI21",
                        "CDI23", "CDI24", "CDI36", "CDI41", "CDI42",
                        "CDI43", "CDI44", "CDI46", "CDI50", "CDI51", "CDI52")

# Filter proportions table to just those
math_prop <- prop_cdi %>%
  filter(item %in% math_cognitive_cdi)
# Add readable labels
math_prop <- math_prop %>%
  mutate(label = recode(item,
                        CDI16 = "4+ word sentences",
                        CDI17 = "Uses 'because'",
                        CDI18 = "Clear speech",
                        CDI20 = "Asks 'why', 'how'",
                        CDI21 = "20+ words",
                        CDI23 = "Says 'big/small'",
                        CDI24 = "Knows object use",
                        CDI36 = "Tells structured story",
                        CDI41 = "Writes letters/numbers",
                        CDI42 = "Writes name",
                        CDI43 = "Counts 10+ objects",
                        CDI44 = "Compares size",
                        CDI46 = "Draws square",
                        CDI50 = "Knows alphabet",
                        CDI51 = "Counts 1–30",
                        CDI52 = "Simple arithmetic"
  ))
print(math_prop, n=70)

# Plot
ggplot(math_prop, aes(x = reorder(label, proportion_yes), y = proportion_yes, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ sexe_parent) +
  coord_flip() +
  labs(
    title = "Cognitive/Math Skills by Child and Parent Sex",
    x = "Skill",
    y = "Proportion of children achieving skill",
    fill = "Child sex"
  ) +
  scale_fill_manual(values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) +
  theme_minimal()
#Comment -> Simple arithmetic
#           For fathers: 62% boys vs 56% girls
#           For mothers: 62% boys vs 57% girls
#           Counts +10 objects
#           Equal for both genders
#           


## SDQ   -------------------------------------------------------------------------------

# Define the list of SDQ variables
sqi_vars <- c("AGITE", "PLAINT", "CRISES", "SOLITA", "OBEIS", "INQUIE", "NTIENTP",
              "AAMI", "BAGAR", "PLEURE", "ESTAIME", "DISTRA", "ANXIEU", "MENTTRI",
              "HARCEL", "REFLECH", "VOLEMA", "PREADUL", "PEURFREQ", "ATTENTI", 
              "ESTIMDIF", "ESTIMDIFD", "DERANDIF", "VIEMDIF", "AMITDIF", "APPRDIF")

# Create long-format dataset
clean_sqi <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")  # Column that identifies the parent
  
  # Skip if QUIREP column does not exist
  if (!quirep_var %in% names(data)) return(NULL)
  
  map_dfr(sqi_vars, function(var) {
    sqi_var <- paste0(prefix, "_", var)  # Build the full variable name
    
    # Skip if the column does not exist
    if (!sqi_var %in% names(data)) return(NULL)
    
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[sqi_var]])
    
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],  # Child's gender
      sexe_parent = data[[quirep_var]][valid_rows],  # Parent's gender
      item = var,  # Stores which SQI variable this row belongs to
      valeur = data[[sqi_var]][valid_rows]  # The actual value (1 or 2)
    )
  })
})

clean_sqi <- clean_sqi %>%
  filter(valeur %in% c(1, 2, 3, 4), sexe_enfant %in% c(1, 2), sexe_parent %in% c(1, 2)) %>%
  mutate(
    sexe_enfant = recode(sexe_enfant, `1` = "Boy", `2` = "Girl"),
    sexe_parent = recode(sexe_parent, `1` = "Father", `2` = "Mother")
  )


## Child's vs parent prefered activity  -------------------------------------------
pref_activity_vars <- c("IMPERACTIV", "PREFACTIV", "PREFACTIVP", "AVACTIVEC", 
                   "FAITPA", "FAITPAP", "PRQPAS")

# Create long-format dataset for activities
clean_prefered_activity <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")  # Column that identifies the parent
  
  # Skip if QUIREP column does not exist
  if (!quirep_var %in% names(data)) return(NULL)
  
  map_dfr(pref_activity_vars, function(var) {
    pref_activity_var <- paste0(prefix, "_", var)  # Build the full variable name
    
    # Skip if the column does not exist
    if (!pref_activity_var %in% names(data)) return(NULL)
    
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[pref_activity_var]])
    
    tibble(
      id = data[["id_DEM_1055_RG"]][valid_rows],           # ✅ Add ID here
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],
      sexe_parent = data[[quirep_var]][valid_rows],
      item = var,
      valeur = data[[pref_activity_var]][valid_rows]
    )
  })
})

# Recode gender variables
clean_prefered_activity <- clean_prefered_activity %>%
  filter(sexe_enfant %in% c(1, 2), sexe_parent %in% c(1, 2)) %>%
  mutate(
    sexe_enfant = recode(sexe_enfant, `1` = "Boy", `2` = "Girl"),
    sexe_parent = recode(sexe_parent, `1` = "Father", `2` = "Mother")
  )


### Most important thing for parents regarding the activities  -------------------------------------------

impr_activity_summary <- clean_prefered_activity %>%
  filter(item == "IMPERACTIV", valeur %in% 1:7) %>%
  mutate(valeur = unlist(valeur)) %>%  # 👈 Flatten list column
  group_by(sexe_parent, sexe_enfant, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe_parent, sexe_enfant) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    reason = factor(valeur, levels = 1:7, labels = c(
      "Be independent", "Learn things", "Exercise", 
      "Be social", "Pass the time", "Be creative", "Other"
    ))
  ) %>%
  arrange(sexe_parent, sexe_enfant, desc(percentage)) %>%
  select(sexe_parent, sexe_enfant, reason, n, percentage)

print(impr_activity_summary, n = Inf)

#Plot
ggplot(impr_activity_summary, aes(x = reason, y = percentage, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ sexe_parent) +
  labs(
    title = "What Parents Value in Children's Extracurricular Activities",
    subtitle = "By Parent and Child Gender",
    x = "Most Important Aspect",
    y = "Percentage of Responses",
    fill = "Child's Gender"
  ) +
  scale_fill_manual(values = c("Boy" = "#4F81BD", "Girl" = "#C0504D")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold")
  )
#Comment -> Parents value more creativity development for girls


### Reasons why the child does not do some activity that he likes  -------------------------------------------
prqpas_distribution <- clean_prefered_activity %>%
  filter(item == "PRQPAS") %>%
  filter(valeur %in% 1:8) %>%  # <-- Exclude values 9 and 10
  group_by(sexe_parent, sexe_enfant, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe_parent, sexe_enfant) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Plot
ggplot(prqpas_distribution, aes(x = sexe_enfant, y = percentage, fill = factor(valeur))) +
  geom_col(position = "stack") + 
  facet_wrap(~ sexe_parent) +  
  labs(
    title = "Why Doesn't the Child Participate in the Activity?",
    x = "Child Gender",
    y = "Percentage",
    fill = "Main Reason"
  ) +
  scale_fill_manual(values = c(
    "1" = "#1f78b4", "2" = "#33C3FF", "3" = "#33A02C", 
    "4" = "#F0E442", "5" = "#AA78A6", "6" = "#FF9F1C", 
    "7" = "#50C878", "8" = "#D95F02"
  ),
  labels = c(
    "1" = "Not available nearby",
    "2" = "Too expensive",
    "3" = "No time",
    "4" = "Parents don't allow it",
    "5" = "Too complicated to organize",
    "6" = "Too young",
    "7" = "Unrealistic",
    "8" = "Health issues"
  )) +
  theme_minimal()
#Comment -> "Parent's don't allow it" is not more present among girls


### Crossing if preferred child's activity = prefered parent activity for the child  -------------------------------------------

pref_cross <- clean_prefered_activity %>%
  filter(item %in% c("PREFACTIV", "PREFACTIVP")) %>%
  pivot_wider(
    names_from = item,
    values_from = valeur,
    values_fn = list
  ) %>%
  unnest(c(PREFACTIV, PREFACTIVP)) %>%
  filter(PREFACTIV %in% 1:8)  # Keep only valid activity preferences

alignment_summary <- pref_cross %>%
  group_by(sexe_enfant, PREFACTIV, PREFACTIVP) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe_enfant, PREFACTIV) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(
    activity = factor(PREFACTIV, levels = 1:8, labels = c(
      "Indoor Play", "Outdoor Play", "Reading", "TV/Video", 
      "Music", "Video Games", "Sports", "Art/Manual/Culture"
    )),
    agreement = factor(PREFACTIVP, levels = 1:3, labels = c(
      "Parent Agrees", "Parent Disagrees", "No Preference"
    ))
  ) %>%
  select(sexe_enfant, activity, agreement, n, percentage)
print(alignment_summary, n = Inf)

# Plot
ggplot(alignment_summary, aes(x = activity, y = percentage, fill = agreement)) +
  geom_col(position = "stack") +
  facet_wrap(~ sexe_enfant) +
  labs(
    title = "Parent Agreement with Child's Favorite Activity by Child Gender",
    x = "Child's Favorite Activity",
    y = "Percentage of Responses",
    fill = "Parental Response"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_manual(values = c(
    "Parent Agrees" = "#4CAF50",
    "Parent Disagrees" = "#F44336",
    "No Preference" = "#9E9E9E"
  ))
#Comment -> there is no major difference across gender. It is not that parents disagree more with preferred child's activity depending on child's gender


data <- data %>%
  mutate(across(
    c(SOUTEC, SOUT1, SOUT2, SOUTECP1, SOUTECP2),
    ~ na_if(., 9)
  )) %>%
  mutate(across(
    c(SOUTEC, SOUT1, SOUT2, SOUTECP1, SOUTECP2),
    ~ if_else(. %in% c(1, 2), ., NA_real_)
  ))

## Soutien ------------------------------------------------------------------------------------

soutien_variables <- c("SOUTEC", "SOUT1", "SOUT2", "SOUTECP1", "SOUTECP2", "SOUTECP3")

for (var in soutien_variables) {
  
  # Clean for valid values (1 = yes, 2 = no)
  A05R_var   <- paste0("A05R_", var)
  A05R2_var  <- paste0("A05R2_", var)
  A05R_rep   <- "A05R_QUIREP"
  A05R2_rep  <- "A05R2_QUIREP"
  
  # Create father's version
  data[[paste0(var, "_father")]] <- case_when(
    data[[A05R_rep]] == 1 & data[[A05R_var]] %in% c(1, 2) ~ data[[A05R_var]],
    data[[A05R2_rep]] == 1 & data[[A05R2_var]] %in% c(1, 2) ~ data[[A05R2_var]],
    TRUE ~ NA_real_
  )
  
  # Create mother's version
  data[[paste0(var, "_mother")]] <- case_when(
    data[[A05R_rep]] == 2 & data[[A05R_var]] %in% c(1, 2) ~ data[[A05R_var]],
    data[[A05R2_rep]] == 2 & data[[A05R2_var]] %in% c(1, 2) ~ data[[A05R2_var]],
    TRUE ~ NA_real_
  )
}

data <- data %>%
  mutate(across(ends_with("_mother"), ~ factor(.x, levels = c(1, 2), labels = c("Oui", "Non")))) %>%
  mutate(across(ends_with("_father"), ~ factor(.x, levels = c(1, 2), labels = c("Oui", "Non"))))


soutien_plot_data <- data %>%
  select(SEXE_ENF,
         SOUTECP1_mother, SOUTECP2_mother, SOUTECP3_mother,
         SOUTECP1_father, SOUTECP2_father, SOUTECP3_father) %>%
  pivot_longer(
    cols = -SEXE_ENF,
    names_to = c("variable", "parent"),
    names_pattern = "(SOUTECP[123])_(mother|father)",
    values_to = "support"
  ) %>%
  filter(!is.na(support))

soutien_plot_data <- soutien_plot_data %>%
  mutate(
    variable = recode(variable,
                      "SOUTECP1" = "Graphisme / Écriture",
                      "SOUTECP2" = "Numération",
                      "SOUTECP3" = "Autre"),
    parent = recode(parent,
                    "mother" = "Mère",
                    "father" = "Père"),
    SEXE_ENF = recode_factor(SEXE_ENF,
                             `1` = "Garçon",
                             `2` = "Fille")
  )


#Plot
ggplot(soutien_plot_data, aes(x = variable, fill = support)) +
  geom_bar(position = "fill") +
  facet_grid(SEXE_ENF ~ parent) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Oui" = "#1f77b4", "Non" = "#ff7f0e")) +
  labs(
    title = "Soutien scolaire par domaine, parent et sexe de l'enfant",
    x = "Type de soutien scolaire",
    y = "Proportion",
    fill = "Réponse"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Proportion table
soutien_proportion_table <- soutien_plot_data %>%
  group_by(SEXE_ENF, parent, variable, support) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SEXE_ENF, parent, variable) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(SEXE_ENF, parent, variable, support)
print(soutien_proportion_table, n = 30)
#Comment -> 
tapply(data$SOUTECP2_mother, data$SEXE_ENF, summary)
table(data$A05R_SOUTECP2)


## A HACER: FIJARME CORRELACION ELEGIR JUEGO DE CONSTRUCCION EN EL JUEGO, Y MATH SCORE


## Temperament  -------------------------------------------

#Define the new variables to analyze
temperament_vars <- c("ENERGIQ", "CALM", "ATTENT", "ADROIT", "ENDUR", "SPORTIF")

#Transform data into long format
clean_temperament <- map_dfr(parent_types, function(prefix) {
  quirep_var <- paste0(prefix, "_QUIREP")  # Identify parent gender
  
  if (!quirep_var %in% names(data)) return(NULL)  # Skip if QUIREP column missing
  
  map_dfr(temperament_vars, function(var) {
    temperament_vars <- paste0(prefix, "_", var)  # Build full variable name
    
    if (!temperament_vars %in% names(data)) return(NULL)  # Skip if variable missing
    
    valid_rows <- !is.na(data[[quirep_var]]) & !is.na(data[[temperament_vars]]) & data[[temperament_vars]] %in% c(1,2) # Exclude NSP (9)
    
    tibble(
      sexe_enfant = data[["SEXE_ENF"]][valid_rows],  # Child gender
      sexe_parent = data[[quirep_var]][valid_rows],  # Parent gender
      item = var,  # The personality variable name
      valeur = data[[temperament_vars]][valid_rows]  # Actual value (1 = Yes, 2 = No)
    )
  })
})

#Convert values into meaningful categories
clean_temperament <- clean_temperament %>%
  filter(sexe_enfant %in% c(1, 2), sexe_parent %in% c(1, 2)) %>%
  mutate(
    sexe_enfant = recode(sexe_enfant, `1` = "Boy", `2` = "Girl"),
    sexe_parent = recode(sexe_parent, `1` = "Father", `2` = "Mother"),
    valeur_binary = ifelse(valeur == 1, 1, 0)  # Convert Yes/No into binary for mean calculation
  )

#Calculate proportion of "Yes" responses by parent and gender
personality_prop <- clean_temperament %>%
  group_by(item, sexe_parent, sexe_enfant) %>%
  summarise(
    proportion_yes = mean(valeur_binary, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(item, sexe_parent, sexe_enfant)
print(personality_prop, n=50)

#Plot
ggplot(personality_prop, aes(x = item, y = proportion_yes, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ sexe_parent) +
  labs(
    title = "Personality and Physical Traits by Gender & Parent",
    x = "Trait",
    y = "Proportion of Parents Saying 'Yes'",
    fill = "Child Gender"
  ) +
  scale_fill_manual(values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) +
  scale_x_discrete(labels = c(
    "ADROIT" = "Skillful",
    "ATTENT" = "Attentive",
    "CALM" = "Calm",
    "ENDUR" = "Enduring",
    "ENERGIQ" = "Energetic",
    "SPORTIF" = "Sportive"
  )) +
  theme_minimal() +
  coord_flip()
#Comment -> Boys more described as "sportifs" and "enduring". Girls more described as "calm" and "attentives"


## Maternelle: Questionnaire Enseignant  -------------------------------------------

#Define teacher-reported variables with correct prefix
competency_vars <- c("A04X_LANGAGE", "A04X_ACTIPHYS", "A04X_ACTIARTIS", 
                     "A04X_NOMBFORM", "A04X_TEMPESPA")

behavior_vars <- c("A04X_ATTENTA", "A04X_PLAINT", "A04X_PARTAG", "A04X_OBEIS", 
                   "A04X_INQUIE", "A04X_NTIENTP", "A04X_AAMI", "A04X_DISTRA", 
                   "A04X_ANXIEU", "A04X_AIDE", "A04X_REFLECHI", "A04X_ATTENTI")

# Combine all teacher-reported variables
teacher_vars <- c(competency_vars, behavior_vars)

# Convert data into long format
clean_maternelle <- map_dfr(teacher_vars, function(var) {
  if (!var %in% names(data)) return(NULL)  # Skip if variable is missing
  
  valid_rows <- !is.na(data[[var]])  # Exclude missing values
  
  tibble(
    sexe_enfant = data[["SEXE_ENF"]][valid_rows],  # Child gender
    item = var,  # Variable name
    valeur = as.numeric(data[[var]][valid_rows])  # Convert to numeric
  )
})

#Recode gender
clean_maternelle <- clean_maternelle %>%
  filter(sexe_enfant %in% c(1, 2)) %>%
  mutate(sexe_enfant = recode(sexe_enfant, `1` = "Boy", `2` = "Girl"))


### Perceived skill level  -------------------------------------------

competency_distribution <- clean_maternelle %>%
  filter(item %in% competency_vars) %>%
  group_by(item, valeur, sexe_enfant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item, sexe_enfant) %>%
  mutate(
    percentage = (n / sum(n)) * 100
  ) %>%
  ungroup()
print(competency_distribution, n = 50)

#Plot
ggplot(competency_distribution, aes(x = factor(valeur), y = percentage, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ item, ncol = 2, labeller = as_labeller(c(
    A04X_ACTIARTIS = "Artistic Activity",
    A04X_ACTIPHYS  = "Physical Activity",
    A04X_LANGAGE   = "Language",
    A04X_NOMBFORM  = "Numbers and Forms",
    A04X_TEMPESPA  = "Time and Space"
  ))) +
  labs(
    title = "Perceived Competency by Gender in Different Kindergarten Domains",
    x = "Competency Level (1 = Very above average, 5 = Very below average)",
    y = "Percentage",
    fill = "Child Gender"
  ) +
  scale_fill_manual(values = c("Boy" = "#1f77b4", "Girl" = "#e377c2")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 16, face = "bold")
  )
#Comment -> Boys are more present among students with below and very below average math skills


### Perceived behavior  -------------------------------------------

behavior_prop <- clean_maternelle %>%
  filter(item %in% behavior_vars) %>%
  group_by(item, sexe_enfant, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percentage = (n / sum(n)) * 100, .by = c(item, sexe_enfant)) %>%
  arrange(item, sexe_enfant, valeur)
print(behavior_prop, n=75)

#Plotting social behaviour
clean_maternelle %>%
  filter(item %in% c("A04X_AAMI", "A04X_AIDE", "A04X_ATTENTA",
                     "A04X_PARTAG")) %>%
  group_by(item, sexe_enfant, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percentage = (n / sum(n)) * 100, .by = c(item, sexe_enfant)) %>%  # Calculate percentage
  ggplot(aes(x = factor(valeur), y = percentage, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ item, scales = "free_x", ncol = 3, 
             labeller = as_labeller(c(
               "A04X_AAMI" = "Has a Friend",
               "A04X_AIDE" = "Helps Others",
               "A04X_ATTENTA" = "Attentive to Others",
               "A04X_PARTAG" = "Shares Easily"
             ))) +
  labs(
    title = "Behavioral Traits by Gender (Teacher Ratings)",
    x = "Behavior Rating (1 = Strongly Applies, 3 = Doesn't Apply)",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) + # Custom Colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels
    strip.text = element_text(size = 12, face = "bold")  # Make facet titles readable
  )
#Comment -> Teachers tend to rate girls higher on prosocial traits (helpfulness, sharing, obedience).

#Plotting emotional behaviour
clean_maternelle %>%
  filter(item %in% c("A04X_OBEIS", "A04X_ANXIEU", "A04X_INQUIE", 
                     "A04X_PLAINT", "A04X_NTIENTP", "A04X_DISTRA", 
                     "A04X_REFLECHI", "A04X_ATTENTI")) %>%
  group_by(item, sexe_enfant, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percentage = (n / sum(n)) * 100, .by = c(item, sexe_enfant)) %>%  # Calculate percentage
  ggplot(aes(x = factor(valeur), y = percentage, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ item, scales = "free_x", ncol = 3, 
             labeller = as_labeller(c(
               "A04X_OBEIS" = "Obedient",
               "A04X_ANXIEU" = "Anxious",
               "A04X_INQUIE" = "Worried",
               "A04X_PLAINT" = "Complains Often",
               "A04X_NTIENTP" = "Restless",
               "A04X_DISTRA" = "Easily Distracted",
               "A04X_REFLECHI" = "Thinks Before Acting",
               "A04X_ATTENTI" = "Stays Focused"
             ))) +
  labs(
    title = "Behavioral Traits by Gender (Teacher Ratings)",
    x = "Behavior Rating (1 = Strongly Applies, 3 = Doesn't Apply)",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) + # Custom Colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels
    strip.text = element_text(size = 12, face = "bold")  # Make facet titles readable
  )
#Comment -> Girls described as more obedient and less impulsive.
#           Slightly more anxious, and complain more.


### Rôle des grands parents


## Jeu 6 ans  -------------------------------------------

jeu_6ans <- c("A06E_AIMECOLE1", "A06E_AIMECOLE2", "A06E_AIMECOLE3", "A06E_AIMECOLE4", "A06E_AIMECOLE5", "A06E_AIMECOLE6",
  "A06E_ACTIMATIN1", "A06E_ACTIMATIN2", "A06E_ACTIMATIN3", "A06E_ACTIMATIN4", "A06E_ACTIMATIN5", "A06E_ACTIMATIN6",
  "A06E_ACTIMATIN7", "A06E_ACTIMATIN8",
  "A06E_ACTICLASSE1", "A06E_ACTICLASSE2", "A06E_ACTICLASSE3", "A06E_ACTICLASSE4", "A06E_ACTICLASSE5",
  "A06E_ACTICLASSE7", "A06E_ACTICLASSE8",
  "A06E_BONBONS", "A06E_JOUETS1", "A06E_JOUETS2", "A06E_JOUETS3", "A06E_JOUETS4", "A06E_JOUETS5", "A06E_JOUETS6",
  "A06E_JOUETS7", "A06E_JOUETS8", "A06E_JOUETS9", "A06E_JOUETS10", "A06E_JOUETSTV", "A06E_MANGER",
  "A06E_ACTCHA", "A06E_ACTCHI", "A06E_ACTLIO", "A06E_ACTOIS", "A06E_ACTOUR",
  "A06E_BEAU", "A06E_BETE", "A06E_FORT", "A06E_FRAGILE", "A06E_GENTIL", "A06E_MALIN", "A06E_MECHANT", "A06E_MOCHE",
  "A06E_PASETREANIMAL", "A06E_ETREANIMAL", "A06E_OBJET",
  "A06E_BEAUPLACE", "A06E_BETEPLACE", "A06E_FORTPLACE", "A06E_FRAGILEPLACE", "A06E_GENTILPLACE",
  "A06E_MALINPLACE", "A06E_MECHANTPLACE", "A06E_MOCHEPLACE", "A06E_PASETREANIMALPLACE", "A06E_ETREANIMALPLACE",
  "A06E_SANCTION",
  "A06E_MET1", "A06E_MET2", "A06E_MET3", "A06E_MET4", "A06E_MET5", "A06E_MET6")

clean_jeu_6ans <- data %>%
  select(SEXE_ENF, all_of(jeu_6ans)) %>%
  pivot_longer(cols = -SEXE_ENF, names_to = "item", values_to = "valeur") %>%
  filter(!is.na(valeur)) %>%
  mutate(sexe_enfant = recode(SEXE_ENF, `1` = "Boy", `2` = "Girl"))
print(clean_jeu_6ans, n= 570)


### Toy preferences  -------------------------------------------

jeu_6ans_prop <- clean_jeu_6ans %>%
  group_by(item, valeur, sexe_enfant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item, valeur) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  arrange(item, valeur, sexe_enfant)
print(jeu_6ans_prop, n = 410)


# List of toy variables
toy_vars <- c("A06E_JOUETS1", "A06E_JOUETS2", "A06E_JOUETS3", "A06E_JOUETS4", "A06E_JOUETS5",
              "A06E_JOUETS6", "A06E_JOUETS7", "A06E_JOUETS8", "A06E_JOUETS9", "A06E_JOUETS10")

# Step 1: Identify who played the toy-selection game
# Create a logical vector: TRUE if at least one toy variable is not NA
played_game <- apply(!is.na(data[toy_vars]), 1, any)

# Step 2: Recode missing values (NAs) to 0 *only for those who played the game*
data[played_game, toy_vars] <- lapply(data[played_game, toy_vars], function(x) ifelse(is.na(x), 0, x))

# Optional: ensure all toy variables are numeric (0 or 1)
data[toy_vars] <- lapply(data[toy_vars], as.integer)

# Filter only participants who played the toy game
participants_data <- data[played_game, ]

# Add the math score variable to the toy variables
vars_for_corr <- c(toy_vars, "A06X_SCMOYMATH")

# Compute the correlation matrix using complete cases
corr_matrix <- cor(participants_data[vars_for_corr], use = "complete.obs")

#
corr_with_math <- corr_matrix["A06X_SCMOYMATH", toy_vars]
print(corr_with_math)


# Gender recoding
participants_data$SEXE_ENF <- factor(participants_data$SEXE_ENF, labels = c("Boy", "Girl"))

# Step 1: Gather toy variables into long format
toy_long <- participants_data %>%
  dplyr::select(SEXE_ENF, all_of(toy_vars)) %>%
  pivot_longer(cols = all_of(toy_vars), names_to = "Toy", values_to = "Chosen")

# Step 2: Count number of children per gender
gender_totals <- participants_data %>%
  count(SEXE_ENF, name = "total")

# Filter to only those who chose the toy (Chosen == 1)
toy_chosen_props <- toy_long %>%
  filter(Chosen == 1) %>%
  group_by(SEXE_ENF, Toy) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(gender_totals, by = "SEXE_ENF") %>%
  mutate(prop = n / total * 100)
print(toy_chosen_props)


#Plot
ggplot(toy_chosen_props, aes(x = reorder(Toy, -prop), y = prop, fill = SEXE_ENF)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c(
    "A06E_JOUETS1" = "Book",
    "A06E_JOUETS2" = "Doll",
    "A06E_JOUETS3" = "Toy Cars",
    "A06E_JOUETS4" = "Building Blocks",
    "A06E_JOUETS5" = "Video Games",
    "A06E_JOUETS6" = "Princess Costume",
    "A06E_JOUETS7" = "Pirate Costume",
    "A06E_JOUETS8" = "Play Kitchen",
    "A06E_JOUETS9" = "Action Figures",
    "A06E_JOUETS10" = "Board Games"
  )) +
  scale_fill_manual(
    values = c("Boy" = "#1f78b4", "Girl" = "#e377c2"),
    labels = c("Boy" = "Boys", "Girl" = "Girls")
  ) +
  labs(
    title = "Toy Preferences at Age 6",
    subtitle = "Percentage of boys and girls who chose each toy",
    x = "Toy",
    y = "Percent of All in Gender Group",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 12)
  )
# Comment -> Nearly 40% of boys chose building blocks, compared to just 10,7% for girls
          






### Animals game  ------------------------------------------------------------

animal_vars <- c(
  "A06E_BEAU", "A06E_BETE", "A06E_FORT", "A06E_FRAGILE", "A06E_GENTIL",
  "A06E_MALIN", "A06E_MECHANT", "A06E_MOCHE",
  "A06E_ETREANIMAL", "A06E_PASETREANIMAL"
)

animal_data <- data %>%
  select(id_DEM_1055_RG, SEXE_ENF, all_of(animal_vars)) %>%
  mutate(sexe_enfant = recode(SEXE_ENF, `1` = "Boy", `2` = "Girl"))


animal_data %>%
  pivot_longer(
    cols = starts_with("A06E_") & !c("A06E_ETREANIMAL", "A06E_PASETREANIMAL", "SEXE_ENF"),
    names_to = "trait_type", 
    values_to = "animal_for_trait"
  ) %>%
  mutate(
    matches_etre = animal_for_trait == A06E_ETREANIMAL,
    matches_paset = animal_for_trait == A06E_PASETREANIMAL
  ) %>%
  filter(matches_etre | matches_paset) %>%
  mutate(
    reference = case_when(
      matches_etre ~ "ETREANIMAL",
      matches_paset ~ "PASETREANIMAL"
    )
  ) -> animal_trait_matches

animal_trait_summary <- animal_trait_matches %>%
  group_by(reference, sexe_enfant, trait_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percentage = round(n / sum(n) * 100, 1), .by = c(reference, sexe_enfant)) %>%
  arrange(reference, sexe_enfant, desc(percentage))
print(animal_trait_summary, n = 50)

#Plot 
animal_trait_matches %>%
  count(reference, sexe_enfant, trait_type) %>%
  group_by(reference, sexe_enfant) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = trait_type, y = percentage, fill = sexe_enfant)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(
    ~ reference,
    ncol = 1,
    labeller = as_labeller(c(
      ETREANIMAL = "Would Like to Be",
      PASETREANIMAL = "Would NOT Like to Be"
    ))
  ) +
  scale_x_discrete(labels = c(
    A06E_BEAU    = "Beautiful",
    A06E_BETE    = "Stupid",
    A06E_FORT    = "Strong",
    A06E_FRAGILE = "Fragile",
    A06E_GENTIL  = "Kind",
    A06E_MALIN   = "Smart",
    A06E_MECHANT = "Mean",
    A06E_MOCHE   = "Ugly"
  )) +
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
  scale_fill_manual(
    values = c("Boy" = "#1f78b4", "Girl" = "#e377c2"),
    labels = c("Boy" = "Boys", "Girl" = "Girls")
  ) +
  labs(
    title = "Traits Associated with Preferred vs. Avoided Animals by Gender",
    subtitle = "Children's perceptions of animal traits by gender and preference",
    x = "Trait Assigned to the Animal",
    y = "Percentage of Responses",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
#Comment -> Preferred traits 
#.          Beautiful: 25% girls vs 20% boys. 
#.          Kind: 27,7% girls vs 17% boys. 
#           Fragile: 8,3% boys vs 14,3% girls.
#           Avoided Traits
#           Mean: 25% girls vs 15% boys
#           Strong: 20% girls vs 15% boys
#           Fragile: 4,9% girls vs 11,7% boys


### Compare preferred activity at school  -------------------------------------------

clean_jeu_6ans %>%
  filter(item %in% c(
    "A06E_ACTICLASSE1", "A06E_ACTICLASSE2", "A06E_ACTICLASSE3",
    "A06E_ACTICLASSE4", "A06E_ACTICLASSE5", "A06E_ACTICLASSE7",
    "A06E_ACTICLASSE8"
  )) %>%
  filter(valeur == 1) %>%  # Keep only "Yes" responses
  group_by(item, sexe_enfant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe_enfant) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(sexe_enfant, desc(percentage))
#Comment -> Physical activity (faire des parcours) is the highest one for both genders, even though boys tend to prefer it a little bit more (28% vs 21.8%)
#.          Drawing second prefered activites for both, but higher for girls (21% vs 18%)


### Compare preferred activity at school 2  -------------------------------------------

clean_jeu_6ans %>%
filter(item %in% c(
  "A06E_AIMECOLE1", "A06E_AIMECOLE2", "A06E_AIMECOLE3",
  "A06E_AIMECOLE4", "A06E_AIMECOLE5", "A06E_AIMECOLE6"
)) %>%
  filter(valeur == 1) %>%  # Keep only "Yes" responses
  group_by(item, sexe_enfant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe_enfant) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(sexe_enfant, desc(percentage))
#Comment -> Equal proportion of boys and girls like doing work in class
#           Common Ground – Social Play is #1: Both boys (28.2%) and girls (28%) most frequently reported “playing with friends” (AIMECOLE4) as a favorite part of school.
#           Boys (4.1%) are more than twice as likely as girls (1.9%) to say they don’t like school
#           Teacher Attachment – Higher Among Girls: Girls (17.6%) are more likely than boys (11.9%) to say they like their teacher


### Compare preferred jobs  -------------------------------------------

# List of profession variables
metier_vars <- c("A06E_MET1", "A06E_MET2", "A06E_MET3", 
                 "A06E_MET4", "A06E_MET5", "A06E_MET6")
# Profession labels
job_labels <- c(
  "1" = "Construction Worker",
  "2" = "Teacher",
  "3" = "Singer",
  "4" = "Baker",
  "5" = "Cleaner",
  "6" = "Doctor"
)

clean_jeu_6ans %>%
  filter(item %in% c("A06E_MET1", "A06E_MET2", "A06E_MET3", "A06E_MET4", "A06E_MET5", "A06E_MET6")) %>%
  filter(!is.na(valeur)) %>%
  group_by(item, valeur, sexe_enfant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item, sexe_enfant) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(item, sexe_enfant, desc(percentage)) %>%
  print(n = 100)

#Plot
data %>%
  dplyr::select(SEXE_ENF, all_of(metier_vars)) %>%
  pivot_longer(cols = all_of(metier_vars), names_to = "item", values_to = "valeur") %>%
  filter(!is.na(valeur)) %>%
  mutate(sexe_enfant = recode(SEXE_ENF, `1` = "Boy", `2` = "Girl")) %>%
  group_by(item, sexe_enfant, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item, sexe_enfant) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  mutate(valeur = factor(valeur, levels = names(job_labels), labels = job_labels)) %>%
  ggplot(aes(x = valeur, y = percentage, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ item, ncol = 2, labeller = as_labeller(c(
    A06E_MET1 = "1st Choice",
    A06E_MET2 = "2nd Choice",
    A06E_MET3 = "3rd Choice",
    A06E_MET4 = "4th Choice",
    A06E_MET5 = "5th Choice",
    A06E_MET6 = "Last Choice"
  ))) +
  scale_fill_manual(values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) +
  labs(
    title = "Children's Preferred Professions by Gender",
    x = "Profession",
    y = "Percentage (by Gender)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
#Comment -> Construction workers: 36% boys vs 1,3%
#.          Teacher: 36,7% girls vs 9,4% boys
#           By the last choice (MET6):
#           Girls overwhelmingly choose "Construction Worker" last (58.9%), suggesting strong rejection.
#           Boys place "Cleaner" as their last choice most often (41.3%).



## CP: Questionnaire Enseignant  -------------------------------------------

regression_data <- data %>%
  left_join(variables_construites, by = "id_DEM_1055_RG")

regression_data <- regression_data %>%
  mutate(sexe_dummy = case_when(
    SEXE_ENF == 1 ~ 0,
    SEXE_ENF == 2 ~ 1,
    TRUE ~ NA_real_
  ))

regression_data <- regression_data %>%
  mutate(dummy_construction = case_when(
    A05C_JCONS == "No" ~ 0,
    A05C_JCONS == "Yes" ~ 1,
    TRUE ~ NA_real_
  ))

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
    niveau_classe = factor(niveau_classe)  # Convert to categorical
  )

regression_data <- regression_data %>%
  mutate(
    type_classe_cp = case_when(
      A06X_NIVECLASS2 == 1 ~ "Classe simple : CP",
      A06X_NIVECLASS4 == 1 ~ "Double niveau : GS-CP",
      A06X_NIVECLASS5 == 1 ~ "Double niveau : CP-CE1",
      TRUE ~ NA_character_
    ),
    type_classe_cp = factor(type_classe_cp, levels = c(
      "Classe simple : CP",
      "Double niveau : GS-CP",
      "Double niveau : CP-CE1"
    ))
  )


## Ecole privé/public en numérique
regression_data <- regression_data %>%
  mutate(A06X_PUBLPRIVc_num = case_when(
    A06X_PUBLPRIVc == "PR" ~ 1,
    A06X_PUBLPRIVc == "PU" ~ 0,
    TRUE ~ NA_real_
  ))

# Cross-tabulation of school type by gender
table(regression_data$SEXE_ENF, regression_data$A04X_PUBLPRIV)


## teacher's sex
regression_data <- regression_data %>%
  mutate(A06X_ENSSEXE = na_if(A06X_ENSSEXE, 0))


## Domestic tasks distribution
recode_relative_to_mother <- function(x, qui_rep) {
  dplyr::case_when(
    x %in% c(1, 2) & qui_rep == 2 ~ -1,  # Mother says she does it
    x == 3 ~ 0,
    x %in% c(4, 5) & qui_rep == 2 ~ 1,   # Mother says father does it
    
    x %in% c(1, 2) & qui_rep == 1 ~ 1,   # Father says he does it
    x %in% c(4, 5) & qui_rep == 1 ~ -1,  # Father says mother does it
    
    x %in% c(6, 7) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

tasks <- c("VAISS", "COURSES", "REPAS", "LINGE", "MENAGE")

for (task in tasks) {
  var_A05R  <- paste0("A05R_", task)
  var_A05R2 <- paste0("A05R2_", task)
  
  new_var <- paste0(task, "_recoded")
  
  regression_data <- regression_data %>%
    mutate(
      !!sym(new_var) := coalesce(
        recode_relative_to_mother(.data[[var_A05R]],  .data[["A05R_QUIREP"]]),
        recode_relative_to_mother(.data[[var_A05R2]], .data[["A05R2_QUIREP"]])
      )
    )
}

recoded_vars <- paste0(tasks, "_recoded")

# Composite variable
regression_data <- regression_data %>%
  rowwise() %>%
  mutate(task_balance_score = mean(c_across(all_of(recoded_vars)), na.rm = TRUE)) %>%
  ungroup()

#Typology
regression_data <- regression_data %>%
  mutate(task_balance_typology = case_when(
    task_balance_score <= -0.5 ~ "Mostly mother",
    task_balance_score >= 0.5 ~ "Mostly father",
    abs(task_balance_score) < 0.5 ~ "Shared"
  ))


## Situation professionnelle parents en facteur

regression_data$mere_emploi_3ans <- factor(regression_data$mother_occup_status_3y,
                                           levels = c(1, 2, 3, 4),
                                           labels = c("En_activité", "Chômage", "Autre", "Étudiante"))

regression_data$pere_emploi_3ans <- factor(regression_data$father_occup_status_3y,
                                           levels = c(1, 2, 3, 4),
                                           labels = c("En_activité", "Chômage", "Autre", "Étudiante"))


regression_data$mere_emploi <- factor(regression_data$mother_occup_status_5y,
                                      levels = c(1, 2, 3, 4),
                                      labels = c("En_activité", "Chômage", "Autre", "Étudiante"))

regression_data$pere_emploi <- factor(regression_data$father_occup_status_5y,
                                      levels = c(1, 2, 3, 4),
                                      labels = c("En_activité", "Chômage", "Autre", "Étudiante"))


## Education parents en facteur

regression_data$mère_educ_3ans <- factor(regression_data$meduc_3y,
                                         levels = c(0, 1, 2, 3, 4, 5, 6),
                                         labels = c("Aucun", "Enseignement primaire", "Enseignement primaire", "Enseignements secondaire niveau lycée","Enseignement supérieur 1er cycle", "Enseignement supérieur diplôme de 2ème cycle", "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))

regression_data$père_educ_3ans <- factor(regression_data$feduc_3y,
                                         levels = c(0, 1, 2, 3, 4, 5, 6),
                                         labels = c("Aucun", "Enseignement primaire", "Enseignement primaire", "Enseignements secondaire niveau lycée","Enseignement supérieur 1er cycle", "Enseignement supérieur diplôme de 2ème cycle", "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))

regression_data$mère_educ_5ans <- factor(regression_data$meduc_5y,
                                         levels = c(0, 1, 2, 3, 4, 5, 6),
                                         labels = c("Aucun", "Enseignement primaire", "Enseignement secondaire niveau collège(Brevet)", "Enseignements secondaire niveau lycée","Enseignement supérieur 1er cycle", "Enseignement supérieur diplôme de 2ème cycle", "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))

regression_data$père_educ_5ans <- factor(regression_data$feduc_5y,
                                         levels = c(0, 1, 2, 3, 4, 5, 6),
                                         labels = c("Aucun", "Enseignement primaire", "Enseignement secondaire niveau collège(Brevet)", "Enseignements secondaire niveau lycée","Enseignement supérieur 1er cycle", "Enseignement supérieur diplôme de 2ème cycle", "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"))


# Fusionner Primaire et Collège
regression_data$mère_educ_5ans <- fct_collapse(
  regression_data$mère_educ_5ans,
  "Primaire ou Brevet" = c("Enseignement primaire", "Enseignement secondaire niveau collège(Brevet)"),
  "Lycée" = "Enseignements secondaire niveau lycée",
  "Bac + 2" = "Enseignement supérieur 1er cycle",
  "Licence ou Master" = "Enseignement supérieur diplôme de 2ème cycle",
  "Grandes Écoles" = "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"
)

regression_data$père_educ_5ans <- fct_collapse(
  regression_data$père_educ_5ans,
  "Primaire ou Brevet" = c("Enseignement primaire", "Enseignement secondaire niveau collège(Brevet)"),
  "Lycée" = "Enseignements secondaire niveau lycée",
  "Bac + 2" = "Enseignement supérieur 1er cycle",
  "Licence ou Master" = "Enseignement supérieur diplôme de 2ème cycle",
  "Grandes Écoles" = "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles"
)




#Chez qui vit l'enfant à 3 ans
regression_data <- regression_data %>%
  mutate(chez_qui_vit_3ans = case_when(
    child_hhld_3y == 1 ~ "Avec ses deux parents",
    child_hhld_3y %in% c(2, 3) ~ "Famille monoparentale",
    child_hhld_3y == 4 ~ "Garde alternée",
    child_hhld_3y == 5 ~ "Enfant placé",
    TRUE ~ NA_character_
  )) %>%
  mutate(chez_qui_vit_3ans = factor(chez_qui_vit_3ans,
                                    levels = c("Avec ses deux parents", "Famille monoparentale", "Garde alternée", "Enfant placé")))

# Chez qui vit l'enfant à 5 ans
regression_data <- regression_data %>%
  mutate(chez_qui_vit_5ans = case_when(
    child_hhld_5y == 1 ~ "Avec ses deux parents",
    child_hhld_5y %in% c(2, 3) ~ "Famille monoparentale",
    child_hhld_5y == 4 ~ "Garde alternée",
    child_hhld_5y == 5 ~ "Enfant placé",
    TRUE ~ NA_character_
  )) %>%
  mutate(chez_qui_vit_5ans = factor(chez_qui_vit_5ans,
                                    levels = c("Avec ses deux parents", "Famille monoparentale", "Garde alternée", "Enfant placé")))

# Origine Parents
regression_data$migration_mère <- factor(regression_data$mimm,
                                         levels = c(1, 2, 3, 4),
                                         labels = c("Mère qui n’est pas de nationalité française", "Mère de nationalité Française descendante de deux parents immigrés", "Mère de nationalité Française descendante d’un parent immigré", "Autre mère Française"))


regression_data$migration_père <- factor(regression_data$fimm,
                                         levels = c(1, 2, 3, 4),
                                         labels = c("Père qui n’est pas de nationalité française", "Père de nationalité Française descendante de deux parents immigrés", "Père de nationalité Française descendante d’un parent immigré", "Autre Père Française"))


# Change reference level for migration_mère
regression_data$migration_mère <- relevel(regression_data$migration_mère, ref = "Autre mère Française")

# Change reference level for migration_père
regression_data$migration_père <- relevel(regression_data$migration_père, ref = "Autre Père Française")


#Toy preferences

regression_data <- regression_data %>%
  mutate(
    A05C_JCONS    = factor(A05C_JCONS, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JVOIT    = factor(A05C_JVOIT, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JDINET   = factor(A05C_JDINET, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JBAL     = factor(A05C_JBAL, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JDEGUIS  = factor(A05C_JDEGUIS, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JPELUCH  = factor(A05C_JPELUCH, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JMUZ     = factor(A05C_JMUZ, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JSOCART  = factor(A05C_JSOCART, levels = c(1, 2), labels = c("Yes", "No")),
    A05C_JEDUC    = factor(A05C_JEDUC, levels = c(1, 2), labels = c("Yes", "No"))
  )



### Children class level  -------------------------------------------

table(recode(as.character(data$A06X_SCOL), 
             "1" = "GS", 
             "2" = "CP", 
             "3" = "CE1", 
             "4" = "CLIS", 
             "5" = "Autre"))
#Comment -> la grande majorité des enfants sont bien en CP

### Maths and lecture scores   -------------------------------------------
#### Mean scores   -------------------------------------------

data %>%
  filter(!is.na(A06X_SCMOYMATH), !is.na(SEXE_ENF)) %>%
  mutate(
    sexe_enfant = recode(SEXE_ENF, `1` = "Boy", `2` = "Girl")
  ) %>%
  group_by(sexe_enfant) %>%
  summarise(
    n = n(),
    mean_math = mean(A06X_SCMOYMATH, na.rm = TRUE),
    .groups = "drop"
  )
#Comment -> results are standardized. Score = 0 = average.

#t test
t.test(
  A06X_SCMOYMATH ~ factor(SEXE_ENF, levels = c(1, 2), labels = c("Boy", "Girl")),
  data = data
)
#Comment -> mean difference girls vs boys is statistically very significant.


#1 Box Plot -----------------------------
ggplot(
  data %>%
    filter(!is.na(SEXE_ENF)) %>%
    dplyr::select(SEXE_ENF,
                  A04X_SCmoymath, A04X_SCmoylect,
                  A06X_SCMOYMATH, A06X_SCmoylect) %>%
    pivot_longer(
      cols = -SEXE_ENF,
      names_to = "var",
      values_to = "score"
    ) %>%
    mutate(
      subject = case_when(
        var %in% c("A04X_SCmoymath", "A06X_SCMOYMATH") ~ "Mathématiques",
        var %in% c("A04X_SCmoylect", "A06X_SCmoylect") ~ "Lecture"
      ),
      age_group = case_when(
        var %in% c("A04X_SCmoymath", "A04X_SCmoylect") ~ "4 ans",
        var %in% c("A06X_SCMOYMATH", "A06X_SCmoylect") ~ "6 ans"
      ),
      sexe_enfant = recode_factor(SEXE_ENF, `1` = "Garçon", `2` = "Fille")
    ),
  aes(x = sexe_enfant, y = score, fill = sexe_enfant)
) +
  geom_boxplot() +
  facet_grid(age_group ~ subject) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  labs(
    title = "Scores standardisés en lecture et mathématiques à 4 et 6 ans",
    x = "Sexe de l'enfant",
    y = "Score standardisé",
    fill = "Sexe"
  ) +
  scale_fill_manual(values = c("Garçon" = "#56B4E9", "Fille" = "#F0E442")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )


#### Distribution on score between gender   -------------------------------------------
describe_maternelle_scores <- function(data, variable, gender_var = "SEXE_ENF") {
  data %>%
    group_by(!!sym(gender_var)) %>%
    summarise(
      Mean = mean(.data[[variable]], na.rm = TRUE),
      Min = min(.data[[variable]], na.rm = TRUE),
      Max = max(.data[[variable]], na.rm = TRUE),
      SD = sd(.data[[variable]], na.rm = TRUE),
      Q10 = quantile(.data[[variable]], probs = 0.10, na.rm = TRUE),
      Q25 = quantile(.data[[variable]], probs = 0.25, na.rm = TRUE),
      Q75 = quantile(.data[[variable]], probs = 0.75, na.rm = TRUE),
      Q90 = quantile(.data[[variable]], probs = 0.90, na.rm = TRUE),
      N = sum(!is.na(.data[[variable]])),
      Prop_NA = round(mean(is.na(.data[[variable]])), 3)
    ) %>%
    mutate(Score = variable) %>%
    select(Score, everything())
}

# Math score summary
math_stats_maternelle <- describe_maternelle_scores(data, "A06X_SCMOYMATH")

# Lecture (reading) score summary
lecture_stats_maternelle <- describe_maternelle_scores(data, "A06X_SCmoylect")

# Combine both into one table
descriptive_table <- bind_rows(math_stats_maternelle, lecture_stats_maternelle) %>%
  mutate(SEXE_ENF = recode_factor(SEXE_ENF, `1` = "Boy", `2` = "Girl"))

descriptive_table <- descriptive_table %>%
  filter(!is.na(SEXE_ENF))

print(descriptive_table, n = Inf)
#Comment -> Girls do worse in maths at any level of the distribution


#### Cross section ---------------------------------------------------
gender_gap_long <- data %>%
  filter(SEXE_ENF %in% c(1, 2)) %>%
  pivot_longer(cols = c(A04X_SCmoymath, A04X_SCmoylect, A06X_SCMOYMATH, A06X_SCmoylect),
               names_to = "variable", values_to = "score") %>%
  mutate(
    subject = case_when(
      variable %in% c("A04X_SCmoymath", "A06X_SCMOYMATH") ~ "Mathématiques",
      TRUE ~ "Français"
    ),
    time = case_when(
      variable %in% c("A04X_SCmoymath", "A04X_SCmoylect") ~ "Âge 4",
      TRUE ~ "Âge 6"
    )
  ) %>%
  group_by(subject, time, SEXE_ENF) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SEXE_ENF, values_from = mean_score, names_prefix = "sex_") %>%
  mutate(
    gender_gap = sex_1 - sex_2  # Boy - Girl
  )


##### Mean gap at 4 and 6 ---------------------------------------------

# 1 "Ecart dans les scores standardisés à 4 et 6 ans" =====================
ggplot(gender_gap_long, aes(x = time, y = gender_gap, fill = subject)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(
    values = c("Mathématiques" = "#4daf4a", "Français" = "#984ea3")
  ) +
  labs(
    title = "Écart de genre dans les scores standardisés",
    subtitle = "Garçons – Filles (positif = garçons meilleurs)",
    x = "Âge de passation",
    y = "Différence moyenne de score",
    fill = "Matière"
  ) +
  coord_cartesian(ylim = c(-0.07, 0.15)) +  # Adjust limits to reveal more below 0
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

#Comment -> Le gap en lecture reste similaire en faveur des filles, alors que celui en maths explose pour les garçons


##### Parents education and gender gap  ----------------------

# 1. Compute parental education index
regression_data <- regression_data %>%
  mutate(
    educ_index = rowMeans(cbind(meduc_5y, feduc_5y), na.rm = TRUE)
  )

# 2. Group into SES bins
regression_data <- regression_data %>%
  mutate(
    educ_group = cut(
      educ_index,
      breaks = quantile(educ_index, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("Low Education", "Medium Education", "High Education", "Very High Education"),
      include.lowest = TRUE
    )
  )

# 3. Create percentile rank variables
regression_data <- regression_data %>%
  mutate(
    math_percentile_4 = percent_rank(A04X_SCmoymath),
    math_percentile_6 = percent_rank(A06X_SCMOYMATH)
  )

gap_table <- regression_data %>%
  filter(!is.na(SEXE_ENF), !is.na(educ_group)) %>%
  mutate(
    sexe_enfant = recode(SEXE_ENF, `1` = "Boy", `2` = "Girl")
  ) %>%
  pivot_longer(
    cols = c(math_percentile_4, math_percentile_6),
    names_to = "time_point",
    values_to = "percentile"
  ) %>%
  group_by(educ_group, time_point, sexe_enfant) %>%
  summarise(
    mean_percentile = mean(percentile, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sexe_enfant,
    values_from = mean_percentile
  ) %>%
  mutate(
    gender_gap = Boy - Girl,
    time_label = recode(time_point,
                        "math_percentile_4" = "Age of 4",
                        "math_percentile_6" = "Age of 6")
  )


#Plot 
ggplot(gap_table, aes(x = educ_group, y = gender_gap, fill = time_label)) +
  geom_col(position = "dodge") +
  labs(
    title = "Gender gap in math by parental education index",
    y = "Percentile rank difference (Boys − Girls)",
    x = "Parental Education Level",
    fill = "Time Point"
  ) +
  scale_fill_manual(values = c(
    "Age of 4" = "#49006a",
    "Age of 6" = "#78c679"
  )) +
  theme_minimal()
#Comment -> Gender math gap appears to be slightly lower for higher levels of education



#2 Scores distribution ------------------------------------------

library(ggplot2)
library(patchwork)

common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "none"
  )

# Math at age 4
p1 <- ggplot(regression_data, aes(x = A04X_SCmoymath, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Mathématiques à 4 ans",
    x = "Score standardisé",
    y = "Densité"
  ) +
  common_theme

# Math at age 6
p2 <- ggplot(regression_data, aes(x = A06X_SCMOYMATH, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Mathématiques à 6 ans",
    x = "Score standardisé",
    y = "Densité"
  ) +
  common_theme

# Reading at age 4
p3 <- ggplot(regression_data, aes(x = A04X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Lecture à 4 ans",
    x = "Score standardisé",
    y = "Densité"
  ) +
  common_theme

# Reading at age 6 (with legend)
p4 <- ggplot(regression_data, aes(x = A06X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Lecture à 6 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Combine all plots
(p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")




##### Girls proportion across each percentile ---------------

# Function to compute % of girls by percentile for a given score
get_percentile_gender_dist <- function(data, score_var, sexe_var = "SEXE_ENF", label = "Timepoint") {
  data %>%
    filter(!is.na(.data[[score_var]]), .data[[sexe_var]] %in% c(1, 2)) %>%
    mutate(
      Percentile = ntile(.data[[score_var]], 100),
      IsGirl = ifelse(.data[[sexe_var]] == 2, 1, 0)
    ) %>%
    group_by(Percentile) %>%
    summarise(
      ProportionGirls = mean(IsGirl) * 100,
      .groups = "drop"
    ) %>%
    mutate(Time = label)
}

girls_maternelle <- get_percentile_gender_dist(
  data = data,
  score_var = "A04X_SCmoymath",
  label = "Maternelle"
)

girls_cp <- get_percentile_gender_dist(
  data = data,
  score_var = "A06X_SCMOYMATH",
  label = "CP (début)"
)

gender_curve <- bind_rows(girls_maternelle, girls_cp)
print(gender_curve, n = 300)


## TABLEAU 1 ---------------------------------------------
gender_curve_selected <- gender_curve %>%
  filter(Percentile %in% c(3, 10, 20, 50, 65, 90, 99,100)) %>%
  pivot_wider(
    names_from = Time,
    values_from = ProportionGirls
  ) %>%
  mutate(
    Evolution = `CP (début)` - Maternelle
  ) %>%
  rename(
    `Position dans la distribution` = Percentile,
    `4 ans (Maternelle)` = Maternelle,
    `6 ans (CP début)` = `CP (début)`,
    `Évolution (points de %)` = Evolution
  ) %>%
  mutate(across(c(`4 ans (Maternelle)`, `6 ans (CP début)`, `Évolution (points de %)`), round, 1))

# Affichage avec kable
kable(gender_curve_selected, align = "c", format = "simple")


# 3 Prop girls across maths distribution =====================
ggplot(gender_curve, aes(x = Percentile, y = ProportionGirls, color = Time)) +
  geom_line(linewidth = 0.5, alpha = 0.9) +  # Thicker lines for clarity
  geom_point(size = 1.5, shape = 21, fill = "white", stroke = 1) +  # Better visible points
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +  # % scale
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  # Clean X axis
  labs(
    title = "Proportion of Girls Across Math Score Percentiles",
    subtitle = "Distribution of girls by math score percentiles at different ages",
    x = "Percentile in Mathematics",
    y = "Proportion of Girls (%)",
    color = "Timepoint"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),       # Centered title
    plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0.5), # Centered subtitle
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title.position = "plot"
  )

#Comment -> Proportion of girls gets higher for lower percentile and lower for higher percentiles


#### Longitudinal -------------------------------------------------

# Data setting
# Create longitudinal_data and then merge controls
transition_data <- data %>%
  dplyr::select(
    id_DEM_1055_RG,
    SEXE_ENF,
    A04X_SCmoymath, A06X_SCMOYMATH,
    A04X_SCmoylect, A06X_SCmoylect
  ) %>%
  filter(
    !is.na(A04X_SCmoymath),
    !is.na(A06X_SCMOYMATH),
    !is.na(A04X_SCmoylect),
    !is.na(A06X_SCmoylect),
    !is.na(SEXE_ENF)
  ) %>%
  left_join(
    regression_data %>% dplyr::select(
      id_DEM_1055_RG, A06X_AGEM, A06X_PUBLPRIVc_num,
      mère_educ_5ans, père_educ_5ans,
      mere_emploi, pere_emploi,
      chez_qui_vit_5ans, migration_père, migration_mère, revenu_part_dec_5y, A06X_ENSSEXE,
      A06X_ENSDUREE, 
      A06X_ENSDISCI3, 
      A06X_ENSAGE, 
      type_classe_cp,
      A06X_REPPLUSc,
      A06X_NBELEVES,
    ),
    by = "id_DEM_1055_RG"
  )

transition_data <- transition_data %>%
  mutate(sexe_dummy = case_when(
    SEXE_ENF == 1 ~ 0,
    SEXE_ENF == 2 ~ 1,
    TRUE ~ NA_real_
  ))

transition_data <- transition_data %>%
  mutate(sexe_enfant = case_when(
    SEXE_ENF == 1 ~ "Boy",
    SEXE_ENF == 2 ~ "Girl",
    TRUE ~ NA_character_
  ))


transition_data <- transition_data %>%
  mutate(
    # Percentiles (0–100 scale)
    percentile_4y = percent_rank(A04X_SCmoymath) * 100,
    percentile_6y = percent_rank(A06X_SCMOYMATH) * 100,
    percentile_change = percentile_6y - percentile_4y,
    
    percentile_4y_read = percent_rank(A04X_SCmoylect) * 100,
    percentile_6y_read = percent_rank(A06X_SCmoylect) * 100,
    percentile_change_read = percentile_6y_read - percentile_4y_read,
    
    # Advantage metrics
    math_advantage_4y = percentile_4y - percentile_4y_read,
    reading_advantage_4y = percentile_4y_read - percentile_4y,
    
    # Quartiles
    math_quartile_4y = ntile(A04X_SCmoymath, 4),
    math_initial_quartile_f = factor(math_quartile_4y,
                                     labels = c("Q1", "Q2", "Q3", "Q4")),
    
    reading_quartile_4y = ntile(A04X_SCmoylect, 4),
    reading_initial_quartile_f = factor(reading_quartile_4y,
                                        labels = c("Q1", "Q2", "Q3", "Q4")),
    
    # Deciles (new)
    math_decile_4y = ntile(A04X_SCmoymath, 10),
    math_decile_6y = ntile(A06X_SCMOYMATH, 10)
  )


regression_data <- regression_data %>%
  mutate(A06X_MATHEMATIQUES_f = factor(
    A06X_MATHEMATIQUES,
    levels = 1:5,
    labels = c("très en dessous", "en dessous", "moyen", "au dessus", "très en dessus")
  ))


##### Score progress by quartiles -----------------------------------

progress_summary <- transition_data %>%
  group_by(math_quartile_4y, sexe_enfant) %>%
  summarise(
    mean_progress = mean(percentile_change, na.rm = TRUE),
    sd_progress = sd(percentile_change, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Compute standard error
progress_summary <- progress_summary %>%
  mutate(se_progress = sd_progress / sqrt(n))

# Plot
ggplot(progress_summary, aes(x = math_quartile_4y, y = mean_progress, fill = sexe_enfant)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_progress - se_progress, ymax = mean_progress + se_progress),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  labs(
    title = "Average Math Progress by Initial Math Quartile and Gender",
    x = "Initial Math Quartile (age 4)",
    y = "Average Math Percentile Change",
    fill = "Gender"
  ) +
  theme_minimal()



#CONCLUSION: Comparative advantage can be discarded. Both maths and reading advantage
# 


#### Decile mobility -------------------------------------------------------

transition_counts <- transition_data %>%
  group_by(sexe_enfant, math_decile_4y, math_decile_6y) %>%
  summarise(n = n(), .groups = "drop")


transition_props <- transition_counts %>%
  group_by(sexe_enfant, math_decile_4y) %>%
  mutate(perc = n / sum(n))

transition_matrix <- transition_counts %>%
  group_by(sexe_enfant, math_decile_4y) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

matrix_girls <- transition_matrix %>%
  filter(sexe_enfant == "Girl") %>%
  dplyr::select(-n, -sexe_enfant) %>%
  pivot_wider(names_from = math_decile_6y, values_from = percentage, values_fill = 0) %>%
  arrange(math_decile_4y)

matrix_boys <- transition_matrix %>%
  filter(sexe_enfant == "Boy") %>%
  dplyr::select(-n, -sexe_enfant) %>%
  pivot_wider(names_from = math_decile_6y, values_from = percentage, values_fill = 0) %>%
  arrange(math_decile_4y)

library(knitr)

kable(matrix_girls, digits = 1, caption = "Transition Matrix for Girls (Age 4 → Age 6)")
kable(matrix_boys, digits = 1, caption = "Transition Matrix for Boys (Age 4 → Age 6)")
#Comment -> 
#Starting Decile	   Girls in D8–10 (%)	 Boys in D8–10 (%)	  Gap (Boys − Girls)
#D10	                    51.5.               62.2	                 +10.7
#D9	                      31.4	              46.4	                 +15.0
#D8	                      31.9	              48.2	                 +16.3



## Maths Average percentile change

percentile_mobility <- transition_data %>%
  mutate(
    percentile_4y_bucket = floor(percentile_4y) + 1  # to go from 0-99 to 1-100
  ) %>%
  group_by(sexe_enfant, percentile_4y_bucket) %>%
  summarise(
    avg_percentile_change = mean(percentile_change, na.rm = TRUE),
    .groups = "drop"
  )



#!!! Maths average gained or lost percentile ranks -----------------------
ggplot(percentile_mobility, aes(x = percentile_4y_bucket, y = avg_percentile_change, color = sexe_enfant)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c(
      "Girl" = "#FF69B4",  # Hot Pink
      "Boy" = "#1f78b4"  # Nice strong blue
    )
  ) +
  labs(
    title = "Average Change in Math Percentile (Age 4 → Age 6)",
    subtitle = "Boys gain more and lose less at every level of the distribution",
    x = "Initial Percentile Rank at Age 4",
    y = "Average Percentile Change",
    color = "Sexe"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "top"
  )



## Stickiness Table

mobility_data <- transition_data %>%
  mutate(
    sticky_low = ifelse(math_decile_4y <= 3 & math_decile_6y <= 3, 1,
                        ifelse(math_decile_4y <= 3, 0, NA)),
    
    sticky_high = ifelse(math_decile_4y >= 8 & math_decile_6y >= 8, 1,
                         ifelse(math_decile_4y >= 8, 0, NA))
  )


stickiness_table <- mobility_data %>%
  group_by(sexe_enfant) %>%
  summarise(
    `Stickiness in Bottom Deciles (1–3)` = round(mean(sticky_low, na.rm = TRUE) * 100, 1),
    `Stickiness in Top Deciles (8–10)` = round(mean(sticky_high, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(sexe_enfant = ifelse(sexe_enfant == "Girl", "Girls", "Boys"))

# Display table
stickiness_table %>%
  kable(
    caption = "Stickiness in Math Performance by Gender: Probability of Staying in Top or Bottom Deciles (Age 4 → Age 6)",
    col.names = c("Genre", "Bas de la distribution (1–3)", "Haut de la distribution (8–10)"),
  )
#Comment -> Lower part of the distribution: Among Girls that started at the first three deciles, 57% of them stayed there. It's 43.8% for boys
#           Higher part of the distribution: Among girls who were at the very top of the distribution, 38% stayed there, vs 52% of the boys.


## Reading Average percentile change

reading_percentile_mobility <- transition_data %>%
  mutate(
    percentile_4y_bucket = floor(percentile_4y_read) + 1  # Converts 0–99 into 1–100
  ) %>%
  group_by(sexe_enfant, percentile_4y_bucket) %>%
  summarise(
    avg_percentile_change = mean(percentile_change_read, na.rm = TRUE),
    .groups = "drop"
  )


#!!! French average gained or lost percentile ranks -----------------------

ggplot(reading_percentile_mobility, aes(x = percentile_4y_bucket, y = avg_percentile_change, color = sexe_enfant)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Girl" = "#e41a1c", "Boy" = "#377eb8")) +
  labs(
    title = "Gendered Change in Reading Percentile (Age 4 → Age 6)",
    subtitle = "Are girls and boys equally stable in reading scores?",
    x = "Initial Reading Percentile at Age 4",
    y = "Average Percentile Change",
    color = "Sexe"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"
  )



## Upward or Downward Mobility
mobility_data <- transition_data %>%
  mutate(
    decile_change = math_decile_6y - math_decile_4y,
    mobility_type = case_when(
      decile_change > 0 ~ "Upward",
      decile_change < 0 ~ "Downward",
      TRUE ~ "Same"
    )
  )

mobility_summary <- mobility_data %>%
  group_by(sexe_enfant, math_decile_4y, mobility_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe_enfant, math_decile_4y) %>%
  mutate(share = round(100 * n / sum(n), 1)) %>%
  select(-n)
print(mobility_summary, n = 60)


#!!! Plot Decile mobility ======
ggplot(mobility_summary, aes(x = factor(math_decile_4y), y = share, fill = mobility_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  facet_wrap(~ sexe_enfant, labeller = labeller(sexe_enfant = c("Girl" = "Girls", "Boy" = "Boys"))) +
  scale_fill_manual(
    values = c(
      "Upward" = "#1b9e77",
      "Same" = "#bdbdbd",
      "Downward" = "#d95f02"
    ),
    labels = c(
      "Upward" = "Mobilité ascendante",
      "Same" = "Stable",
      "Downward" = "Mobilité descendante"
    )
  ) +
  scale_y_continuous(expand = c(0, 0)) +  # ✅ fixed here
  labs(
    title = "Mobilité entre 4 et 6 ans selon le genre et le décile initial en mathématiques",
    subtitle = "Type de mobilité observée entre les déciles de score en mathématiques (âge 4 → âge 6)",
    x = "Décile initial en mathématiques (âge 4)",
    y = "Pourcentage des enfants",
    fill = "Type de mobilité"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )



### Decomposing maths test  -------------------------------------------
CPtest_math_vars <- c("A06X_SCCALCD", "A06X_SCPROB", "A06X_SCCALCM", "A06X_SCSUITE", "A06X_SCCOMPA", "A06X_SCMOYMATH")

maths_quantile_table <- data %>%
  group_by(SEXE_ENF) %>%
  summarise(across(all_of(CPtest_math_vars), 
                   list(p10 = ~quantile(.x, 0.10, na.rm = TRUE),
                        p50 = ~quantile(.x, 0.50, na.rm = TRUE),
                        p90 = ~quantile(.x, 0.90, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))
print(maths_quantile_table, width = Inf)


## Distribution for each of the variables  
long_math_data <- data %>%
  select(SEXE_ENF, all_of(CPtest_math_vars)) %>%
  pivot_longer(cols = all_of(CPtest_math_vars), names_to = "Variable", values_to = "Score") %>%
  drop_na(SEXE_ENF, Score)

# Optional: clean variable names for display
long_math_data$Variable <- gsub("A06X_", "", long_math_data$Variable)



# Box Plot
ggplot(long_math_data, aes(x = as.factor(SEXE_ENF), y = Score, fill = as.factor(SEXE_ENF))) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.6, alpha = 0.7) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.25) +
  facet_wrap(~ Variable, scales = "free_y", 
             labeller = as_labeller(c(
               "SCCALCD" = "Calcul mental dicté",
               "SCPROB" = "Résolution de problèmes",
               "SCCALCM" = "Calcul mental pratique",
               "SCSUITE" = "Suites numériques",
               "SCCOMPA" = "Comparaison de nombres",
               "SCMOYMATH" = "Score moyen de maths"
             ))) +
  scale_fill_manual(values = c("skyblue", "salmon"), labels = c("Garçons", "Filles")) +
  labs(title = "Distribution des scores de mathématiques par sexe",
       x = "Sexe (1 = Garçon, 2 = Fille)", y = "Score", fill = "Sexe") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
#Comment -> Boys do better on average on each one o 


### Perceived skill level  -------------------------------------------

perceived_skills_teacher_6ans <- c("A06X_FRANCAIS", "A06X_MATHEMATIQUES", "A06X_ACTIPHYS", 
                          "A06X_LANGVIV", "A06X_ACTIARTIS", "A06X_QUESTMONDE", "A06X_MORALCIVIC")

distribution_perceived_skills_teacher_6ans <- data %>%
  select(SEXE_ENF, all_of(perceived_skills_teacher_6ans)) %>%
  pivot_longer(-SEXE_ENF, names_to = "Subject", values_to = "Score") %>%
  filter(!is.na(Score), Score %in% 1:5) %>%  # Filter to keep scores from 1 to 5 only
  group_by(Subject, SEXE_ENF, Score) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Subject, SEXE_ENF) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  ungroup()
print(distribution_perceived_skills_teacher_6ans, n = 100)


# Update gender labels in the data
distribution_perceived_skills_teacher_6ans <- distribution_perceived_skills_teacher_6ans %>%
  mutate(Gender = case_when(
    SEXE_ENF == 1 ~ "Boy",
    SEXE_ENF == 2 ~ "Girl",
    TRUE ~ NA_character_
  ))


distribution_perceived_skills_teacher_6ans %>%
  filter(Subject %in% c("A06X_FRANCAIS", "A06X_MATHEMATIQUES", "A06X_LANGVIV")) %>%
  ggplot(aes(x = factor(Score), y = Percent, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(
    ~ Subject,
    scales = "free_y",
    ncol = 3,
    labeller = as_labeller(c(
      A06X_FRANCAIS      = "Français",
      A06X_MATHEMATIQUES = "Mathématiques",
      A06X_LANGVIV       = "Langues vivantes"
    ))
  ) +
  labs(
    title = "Répartition des compétences perçues par les enseignants (6 ans)",
    x = "Score attribué (1 = Très au-dessus de la moyenne, 5 = Très en dessous de la moyenne)",
    y = "Pourcentage (%)",
    fill = "Sexe de l’enfant"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )
#Comment -> Girls are perceived to have worst perceived maths skills: "Average" -> 31,8% girls vs 26,2% boys
#                                   "Above average" -> 6,7% boys vs 4,6 boys
#                                   "Very above average" -> 13,3% boys vs 8,4% girls




# Teacher bias -----------------------------

# Inversing dependant variable 

regression_data$A06X_MATHEMATIQUES[regression_data$A06X_MATHEMATIQUES == 0] <- NA

regression_data$perceived_math_skill <- 6 - regression_data$A06X_MATHEMATIQUES

regression_data$perceived_math_skill_4ans <- 6 - regression_data$A04X_NOMBFORM

regression_data$A06X_FRANCAIS[regression_data$A06X_FRANCAIS == 0] <- NA

regression_data$perceived_french_skill <- 6 - regression_data$A06X_FRANCAIS

regression_data$perceived_french_skill_4ans <- 6 - regression_data$A04X_LANGAGE


regression_data$perceived_math_skill_f <- factor(
  regression_data$perceived_math_skill,
  levels = 1:5,
  labels = c("très en dessous", "en dessous", "moyen", "au dessus", "très en dessus"),
  ordered = FALSE
)


table(regression_data$perceived_math_skill_f)

jujux <- lm(
  perceived_math_skill ~ 
    sexe_dummy +
    A06X_SCMOYMATH +
    A06X_SCmoylect +
    père_educ_5ans,
  data = regression_data
)
summary(jujux)

jujux <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    mère_educ_5ans +
    perceived_math_skill_f +
    père_educ_5ans,
  data = regression_data
)
summary(jujux)



jujux <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    perceived_math_skill +
    mère_educ_5ans +
    père_educ_5ans,
  data = regression_data
)
summary(jujux)

regression_data$perceived_math_skill_f <- factor(
  regression_data$perceived_math_skill,
  levels = 1:5,
  labels = c("VBA", "BA", "A", "AA", "VAB"),
  ordered = TRUE
)


regression_data$perceived_math_skill_4ans_f <- factor(
  regression_data$perceived_math_skill_4ans,
  levels = 1:5,
  labels = c("Very Below Average", "Below Average", "Average", "Above Average", "Very Above Average"),
  ordered = TRUE
)

regression_data$perceived_french_skill_f <- factor(
  regression_data$perceived_french_skill,
  levels = 1:5,
  labels = c("Very Below Average", "Below Average", "Average", "Above Average", "Very Above Average"),
  ordered = TRUE
)

regression_data$perceived_french_skill_4ans_f <- factor(
  regression_data$perceived_french_skill_4ans,
  levels = 1:5,
  labels = c("Very Below Average", "Below Average", "Average", "Above Average", "Very Above Average"),
  ordered = TRUE
)



# Ordinal regression
library(MASS)
oprobit_1 <- polr(
  perceived_math_skill_f ~ sexe_dummy + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_1, type = "text")

#Comment -> Interpretaion: A positive coefficient → pushes the latenthigher, so more likely to be classified into a higher category (higher perceived skill).
#                          A negative coefficient → pushes lower, so more likely to be classified into a lower category (lower perceived skill).
#                          The size of the coefficient tells you about the strength of the push, but it’s not directly interpretable as a percentage or points like in OLS.
# Girls have a lower underlying perceived math ability, controlling for real skill, age, reading score, etc.

# building dummy variable -> computting a dummy for being above and below. standardize the teacher rating. and see if there is distance or not, 

# look at the correlation between teacher perceived skill and actual math ability. So correlation between both standardized variables (teacher rating and math score).

# in the ideal world, there should be no difference in teacher rating and standardized test, or at least be equally good or bad across gender.

# explicitly say now I am speculation. Not getting in the room, 

table(regression_data$perceived_math_skill_f)
# Including teacher effects
oprobit_2 <- polr(
  perceived_math_skill_f ~ sexe_dummy + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + sexe_enseignant + experience_std_1,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_2, type = "text")


jeje <- polr(
  perceived_math_skill_f ~ sexe_dummy + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + sexe_enseignant + experience_std_1 + A06X_SCORE_SDQ_PRO_6ANS + A06X_SCORE_SDQ_EMO_6ANS + A06X_SCORE_SDQ_HYPER_6ANS,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(jeje, type = "text")

lm_test <- lm(as.numeric(perceived_math_skill_f) ~ sexe_dummy + A06X_AGEM + A06X_SCMOYMATH + A06X_SCmoylect + sexe_enseignant + experience_std_1,  data = regression_data)
summary(lm_test)


tuti <- polr(
  perceived_math_skill_f ~ sexe_dummy + A06X_SCMOYMATH + A06X_AGEM + sexe_enseignant + experience_std_1,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(tuti, type = "text")


## Effets marginaux

library(erer)
ME <- ocME(oprobit_2)
ME$out

# Créer un sous-tableau sans les variables indésirables
me_table_clean <- ME$out$ME.all[
  !rownames(ME$out$ME.all) %in% c(
    "sexe_enseignant",
    "experience_std_1"
  ),
]

# Renommer les lignes restantes
rownames(me_table_clean) <- c(
  "Fille",
  "Score en maths",
  "Âge",
  "Score en français"
)

# Rename column names (outcome categories)
colnames(me_table_clean) <- c(
  "Très en dessous",
  "En dessous",
  "Moyen",
  "En dessus",
  "Très en desssus"
)

# Affichage avec stargazer
stargazer(
  me_table_clean,
  type = "text",
  summary = FALSE,
  digits = 3
)


#Effets marginaux: Niveau en maths de l'élève perçu par l'enseignant

# Rename column names (outcome categories)
colnames(ME$out$ME.all) <- c(
  "Très en dessous",
  "En dessous",
  "Moyen",
  "En dessus",
  "Très en desssus"
)

# Print with stargazer
stargazer(
  ME$out$ME.all,
  type = "text",
  summary = FALSE,
  title = "Marginal Effects: Teacher's Perceived Math Ability by Category",
  digits = 3
)



# Por sexe

oprobit_12 <- polr(
  perceived_math_skill_f ~ sexe_enseignant * A06X_ENSDISCI3 + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data %>% filter(sexe_dummy == 0),
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_12, type = "text")


oprobit_13 <- polr(
  perceived_math_skill_f ~ sexe_enseignant * A06X_ENSDISCI3 + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data %>% filter(sexe_dummy == 1),
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_13, type = "text")



# Interaction terms
oprobit_3 <- polr(
  perceived_math_skill_f ~ sexe_dummy * sexe_enseignant * A06X_ENSDISCI3  + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_3, type = "text")



table(regression_data$A04X_ENSDUREE)
regression_data$experience_std <- scale(regression_data$A04X_ENSDUREE, center = TRUE, scale = TRUE)
regression_data$experience_std_1 <- scale(regression_data$A06X_ENSDUREE, center = TRUE, scale = TRUE)

table(regression_data$experience_std)
## Robustness check: 

#Teacher Bias at 4 years old: MATHS
library(MASS)

oprobit_4ans_maths <- polr(
  perceived_math_skill_4ans_f ~ sexe_dummy + A04X_SCmoymath + A04X_AGE4A + A04X_SCmoylect + sexe_enseignant_4ans + experience_std,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_4ans_maths, type = "text")


# Teacher Bias at 6 years old: FRENCH
oprobit_6ans_french <- polr(
  perceived_french_skill_f ~ sexe_dummy + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + sexe_enseignant + experience_std_1,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_6ans_french, type = "text")


# Teacher Bias at 4 years old: FRENCH
oprobit_4ans_french <- polr(
  perceived_french_skill_4ans_f ~ sexe_dummy + A04X_SCmoymath + A04X_AGE4A + A04X_SCmoylect + sexe_enseignant_4ans + experience_std,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_4ans_french, type = "text")

summary(regression_data$A04X_SCmoylect)
summary(regression_data$A04X_SCmoymath)
summary(regression_data$A06X_SCMOYMATH)
summary(regression_data$A06X_SCmoylect)




# Teacher's background --------------------------------------

transition_data <- transition_data %>%
  left_join(
    data %>%
      dplyr::select(id_DEM_1055_RG, A06X_ENSDISCI3),
    by = "id_DEM_1055_RG"
  )


#Raw math score
lm_juju <- lm(A06X_SCMOYMATH_rescaled ~ sexe_dummy * A06X_ENSDISCI3 + A06X_AGEM + A04X_SCmoymath + A04X_SCmoylect, data = regression_data)
summary(lm_juju)

lm_juju_2 <- lm(A04X_SCmoymath ~ sexe_dummy * A04X_ENSDISCI3 + A04X_AGEM, data = regression_data)
summary(lm_juju_2)


lm_00 <- lm(A06X_SCMOYMATH_rescaled ~ 
              sexe_dummy * A06X_ENSDISCI3 * sexe_enseignant + 
              revenu_part_dec_5y +
              mère_educ_5ans + 
              A06X_AGEM +
              père_educ_5ans,
            data = regression_data)
summary(lm_00)

library(sandwich)
library(lmtest)

# Robust standard errors (HC1 is the common option for robust SE)
coeftest(lm_00, vcov. = vcovHC(lm_00, type = "HC1"))


lm_001 <- lm(A06X_SCMOYMATH_rescaled ~ 
              sexe_dummy * A06X_ENSDISCI3 * A06X_ENSSEXE + 
              revenu_part_dec_5y +
              mère_educ_5ans + 
              A06X_AGEM +
              père_educ_5ans,
            data = regression_data)
summary(lm_001)


# Male teacher
lm_00 <- lm(A06X_SCMOYMATH ~ 
              sexe_dummy * A06X_ENSDISCI3 +
              A06X_ENSDUREE +
              mère_educ_5ans + 
              père_educ_5ans,
            data = regression_data %>% filter(sexe_enseignant == 1))
summary(lm_00)


# Female teacher
lm_00_ <- lm(A06X_SCMOYMATH ~ 
              sexe_dummy * A06X_ENSDISCI3 +
              A06X_ENSDUREE +
              mère_educ_5ans + 
              père_educ_5ans,
            data = regression_data %>% filter(A06X_ENSSEXE == 1))
summary(lm_00_)

table(regression_data$A06X_SCORE_SDQ_PRO_6ANS)

Math_Achievement ~ Teacher_Gender*Child_Sex +
  Teacher_Disciplinary_Background*Child_Sex + Teacher_Educational_Attainment*Child_Sex +
  Teacher_Experience*Child_Sex + Controls.


table(regression_data$A06X_ENSSEXE)



## PLOT
lilas <- regression_data %>%
  filter(!is.na(SEXE_ENF), !is.na(A06X_ENSSEXE), !is.na(A06X_ENSDISCI3), !is.na(A06X_SCMOYMATH)) %>%
  mutate(
    sexe_enfant = recode_factor(SEXE_ENF, `1` = "Boys", `2` = "Girls"),
    teacher_gender = recode_factor(A06X_ENSSEXE, `1` = "Female Teachers", `2` = "Male Teachers"),
    teacher_discipline = ifelse(A06X_ENSDISCI3 == 1, "Scientific", "Other")
  ) %>%
  group_by(teacher_gender, teacher_discipline, sexe_enfant) %>%
  summarise(
    mean_score = mean(A06X_SCMOYMATH),
    se = sd(A06X_SCMOYMATH) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Step 2: Plot
ggplot(lilas, aes(x = teacher_discipline, y = mean_score, fill = sexe_enfant)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_score - se, ymax = mean_score + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.8),
    vjust = -1.2,
    size = 3
  ) +
  geom_text(
    aes(label = n, y = -0.55),
    position = position_dodge(width = 0.8),
    vjust = 1.2,
    size = 3
  ) +
  facet_wrap(~teacher_gender) +
  scale_fill_manual(values = c("Boys" = "#80B1D3", "Girls" = "#FDB462")) +
  labs(
    title = "Averaged mathematics scores by child sex, teacher sex, and scientific background",
    x = "Disciplinary background",
    y = "Scaled math score",
    fill = "Child's Sex"
  ) +
  coord_cartesian(ylim = c(-0.6, 0.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(size = 12)
  )



# ANXIETY --------------------------------------------------------------


## Cross Section: Are girls on average more anxious at age of 6? -----------------------------------------


traits <- c("ATTENTA", "PLAINT", "PARTAG", "OBEIS", "INQUIE",
            "NTIENTP", "AAMI", "DISTRA", "ANXIEU", "AIDE",
            "REFLECHI", "ATTENTI")

vars_4 <- paste0("A04X_", traits)
vars_6 <- paste0("A06X_", traits)

traits_data <- data %>%
  dplyr::select(id_DEM_1055_RG, all_of(vars_4), all_of(vars_6))

transition_data <- transition_data %>%
  left_join(traits_data, by = "id_DEM_1055_RG")

data[vars_6] <- lapply(data[vars_6], function(x) ifelse(x == 0, NA, x))

vars_4 <- paste0("A04X_", traits)

# Age 4
long_4 <- data %>%
  dplyr::select(id_DEM_1055_RG, SEXE_ENF, all_of(vars_4)) %>%
  pivot_longer(cols = all_of(vars_4), names_to = "Trait", values_to = "Value") %>%
  mutate(
    Trait = gsub("A04X_", "", Trait),
    Age = 4
  )

# Age 6
long_6 <- data %>%
  dplyr::select(id_DEM_1055_RG, SEXE_ENF, all_of(vars_6)) %>%
  pivot_longer(cols = all_of(vars_6), names_to = "Trait", values_to = "Value") %>%
  mutate(
    Trait = gsub("A06X_", "", Trait),
    Age = 6
  )

# Combine
long_traits <- bind_rows(long_4, long_6) %>%
  mutate(
    Value = factor(Value, levels = c(1, 2, 3), labels = c("Très vrai", "Un peu vrai", "Pas vrai"))
  )

trait_props <- long_traits %>%
  group_by(Trait, Age, SEXE_ENF, Value) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(!is.na(Value), !is.na(SEXE_ENF)) %>%
  mutate(SEXE_ENF = recode(SEXE_ENF, `1` = "Boy", `2` = "Girl")) %>%  # Recode gender
  group_by(Trait, Age, SEXE_ENF) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
print(trait_props, n = 200)


# Define the traits you're focusing on
focus_traits <- c("ANXIEU", "REFLECHI", "ATTENTI", "INQUIE", "DISTRA")


# Plot only selected traits
trait_props %>%
  filter(Trait %in% focus_traits) %>%
  ggplot(aes(x = factor(Age), y = prop, fill = Value)) +
  geom_col(position = "fill") +
  facet_grid(SEXE_ENF ~ Trait) +
  scale_fill_manual(values = c("Très vrai" = "#1b9e77", "Un peu vrai" = "#7570b3", "Pas vrai" = "#d95f02")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Teacher-Reported Traits at Ages 4 and 6",
    subtitle = "Gender differences over time",
    x = "Age",
    y = "Proportion of Responses",
    fill = "Response"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


#ANXIEU
trait_props %>%
  filter(Trait == "ANXIEU") %>%
  dplyr::select(Age, SEXE_ENF, Value, prop) %>%
  pivot_wider(names_from = Value, values_from = prop) %>%
  arrange(Age, SEXE_ENF) %>%
  kable(format = "pipe")
#Comment -> Biggest change for girls: more “Un peu vrai”
#           The "Un peu vrai" category rises more sharply for girls than for boys:
#           Boys: 36.1% → 37.2% (+1.1 pts)
#           Girls: 34.7% → 41.3% (+6.6 pts)
#           Fewer children rated “Pas vrai” (i.e., not anxious) at age 6
#           "Pas Vrai  Boys: 50.5% → 47.4% (−3.1 pts)
#           Girls: 54.3% → 44.9% (−9.4 pts)
#           Again, the change is much more pronounced for girls, showing that fewer girls are seen as free of anxiety-related behaviors by age 6.



## Longitudinal: Did girls get more anxious than boys? -----------------------------


for (trait in focus_traits) {
  t4 <- paste0("A04X_", trait)
  t6 <- paste0("A06X_", trait)
  change_col <- paste0("change_", trait)
  
  transition_data[[change_col]] <- dplyr::case_when(
    is.na(transition_data[[t4]]) | is.na(transition_data[[t6]]) ~ NA_character_,
    transition_data[[t6]] < transition_data[[t4]] ~ "Increase",
    transition_data[[t6]] > transition_data[[t4]] ~ "Decrease",
    transition_data[[t6]] == transition_data[[t4]] ~ "No change"
  )
}

change_vars <- paste0("change_", focus_traits)

long_changes <- transition_data %>%
  dplyr::select(sexe_dummy, all_of(change_vars)) %>%
  tidyr::pivot_longer(
    cols = all_of(change_vars),  # Only pivot the change_ columns
    names_to = "Trait",
    values_to = "Change"
  ) %>%
  dplyr::mutate(
    Trait = gsub("change_", "", Trait),
    Change = factor(Change, levels = c("Decrease", "No change", "Increase")),
    sexe_enfant = factor(sexe_dummy, levels = c(0, 1), labels = c("Boy", "Girl"))
  )

## Longitudinal
long_changes %>%
  filter(!is.na(Change)) %>%
  group_by(Trait, sexe_enfant, Change) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Trait, sexe_enfant) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Change, y = prop, fill = sexe_enfant)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Trait, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) +
  labs(
    title = "Change in Teacher Ratings from Age 4 to 6",
    subtitle = "By Trait and Gender",
    x = "Direction of Change",
    y = "Proportion of Children",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"), legend.position = "bottom")
#Comment -> Girls get more anxious in time.


regression_data$A06X_ANXIEU <- ifelse(regression_data$A06X_ANXIEU == 0, NA, regression_data$A06X_ANXIEU)
regression_data$A06X_INQUIE <- ifelse(regression_data$A06X_INQUIE == 0, NA, regression_data$A06X_INQUIE)
regression_data$A06X_ATTENTI <- ifelse(regression_data$A06X_ATTENTI == 0, NA, regression_data$A06X_ATTENTI)
regression_data$A06X_MATHEMATIQUES <- ifelse(regression_data$A06X_MATHEMATIQUES == 0, NA, regression_data$A06X_MATHEMATIQUES)

# Anxieu en facteur
regression_data$A06X_ANXIEU_f<- factor(
  regression_data$A06X_ANXIEU,
  levels = c(1, 2, 3),
  labels = c("very true", "a little bit true", "not true")
)

regression_data$A06X_INQUIE_f<- factor(
  regression_data$A06X_INQUIE,
  levels = c(1, 2, 3),
  labels = c("very true", "a little bit true", "not true")
)

regression_data$A04X_ANXIEU_f<- factor(
  regression_data$A04X_ANXIEU,
  levels = c(1, 2, 3),
  labels = c("very true", "a little bit true", "not true")
)

regression_data <- regression_data %>%
  left_join(data %>% dplyr::select(id_DEM_1055_RG, A05R_SCORE_CAT_SDQ_RELA_5ANS), by = "id_DEM_1055_RG")



# Recoder la variable en facteur
regression_data$pro_social_factor_5ans <- factor(regression_data$A05R_SCORE_CAT_SDQ_RELA_5ANS,
                                                       levels = c(1, 2, 3),
                                                       labels = c("normal", "limite", "anormal"))

regression_data$pro_social_rating <- factor(regression_data$A06X_SCORE_CAT_SDQ_PRO_6ANS,
                                                       levels = c(1, 2, 3),
                                                       labels = c("normal", "limite", "anormal"))

regression_data$hyperactivity_rating <- factor(regression_data$A06X_SCORE_CAT_SDQ_HYPER_6ANS,
                                            levels = c(1, 2, 3),
                                            labels = c("normal", "limite", "anormal"))

regression_data$emotional_regulation_rating <- factor(regression_data$A06X_SCORE_CAT_SDQ_EMO_6ANS,
                                               levels = c(1, 2, 3),
                                               labels = c("normal", "limite", "anormal"))

# Assuming your child ID variable is 'id_DEM_1055_RG' and these variables are in a separate dataframe, e.g., 'participants_data'

transition_data <- transition_data %>%
  left_join(
    regression_data %>%
      dplyr::select(id_DEM_1055_RG, pro_social_rating, 
                    A06X_SCORE_CAT_SDQ_HYPER_6ANS, 
                    A06X_SCORE_CAT_SDQ_EMO_6ANS),
    by = "id_DEM_1055_RG"
  )

table(regression_data$emotional_regulation_rating)

# Pro social skills effect =====================

regression_data$pro_social_rating <- relevel(regression_data$pro_social_rating, ref = "limite")

regression_data$emotional_regulation_rating <- relevel(regression_data$emotional_regulation_rating, ref = "limite")

regression_data$hyperactivity_rating <- relevel(regression_data$hyperactivity_rating, ref = "limite")

mere_emploi +
  pere_emploi +
  chez_qui_vit_5ans +
  migration_père +
  migration_mère +
  revenu_part_dec_5y,

## Testing for impact of prosocial skills

#Sur score général en maths
hihi <- lm(
  A06X_SCMOYMATH ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
                 sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
                 sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(hihi)

pro_social_generalmaths <- lm(
  A06X_SCMOYMATH ~ sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, 
  data = regression_data)
summary(pro_social_generalmaths)


# Sur comparaison de nombres
hihi_1 <- lm(
  A06X_SCCOMPA ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans, 
  data = regression_data)
summary(hihi_1)


# Gender model
c1 <- lm(
  A06X_SCCOMPA ~ 
     A06X_SCORE_SDQ_PRO_6ANS * sexe_enseignant * A06X_ENSDISCI3 +
     A06X_SCORE_SDQ_EMO_6ANS * sexe_enseignant * A06X_ENSDISCI3 +
     A06X_SCORE_SDQ_HYPER_6ANS * sexe_enseignant * A06X_ENSDISCI3 +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans, 
  data = regression_data %>% filter(sexe_dummy == 1))
  summary(c1)

   
# Avec past scores
pro_social_numbcomp <- lm(
  A06X_SCCOMPA ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, 
  data = regression_data)
summary(pro_social_numbcomp)

table(regression_data$A06X_SCORE_SDQ_PRO_6ANS)


# Sur calcul mental dicté
hihi_2 <- lm(
  A06X_SCCALCD ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans, 
  data = regression_data)
summary(hihi_2)
  

# Model genre
c2 <- lm(
  A06X_SCCALCD ~ 
    A06X_SCORE_SDQ_PRO_6ANS * sexe_enseignant * A06X_ENSDISCI3 +
    A06X_SCORE_SDQ_EMO_6ANS * sexe_enseignant * A06X_ENSDISCI3 +
    A06X_SCORE_SDQ_HYPER_6ANS * sexe_enseignant * A06X_ENSDISCI3 +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans, 
  data = regression_data %>% filter(sexe_dummy == 1))
summary(c2)

# Avec past scores
pro_social_mentalcalcul <- lm(
  A06X_SCCALCD ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, 
  data = regression_data)
summary(pro_social_mentalcalcul)

table(regression_data$A06X_SCORE_CAT_SDQ_PRO_6ANS)
table(regression_data$A06X_SCORE_SDQ_PRO_6ANS)


pro_social_mentalcalcul_1 <- lm(
  A06X_SCCALCD ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath + A06X_SCmoylect, 
  data = regression_data)
summary(pro_social_mentalcalcul_1)

# Sur calcul mental pratique
hihi_3 <- lm(
  A06X_SCCALCM ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans, 
  data = regression_data)
summary(hihi_3)

pro_social_mentalpract <- lm(
  A06X_SCCALCM ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, 
  data = regression_data)
summary(pro_social_mentalpract)


pro_social_mentalpract_1 <- lm(
  A06X_SCCALCM ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, 
  data = regression_data)
summary(pro_social_mentalpract_1)


# Résolution problèmes
hihi_4 <- lm(
  A06X_SCPROB ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans, 
  data = regression_data)
summary(hihi_4)

pro_social_probresolu <- lm(
  A06X_SCPROB ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, 
  data = regression_data)
summary(pro_social_probresolu)


# Suites numériques
hihi_5 <- lm(
  A06X_SCSUITE ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(hihi_5)


table(regression_data$A06X_ENSDISCI3)
pro_social_suitnum <- lm(
  A06X_SCSUITE ~ 
    sexe_dummy * A06X_SCORE_SDQ_PRO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_EMO_6ANS + 
    sexe_dummy * A06X_SCORE_SDQ_HYPER_6ANS + 
    A06X_AGEM + père_educ_5ans + mère_educ_5ans + A04X_SCmoymath, 
  data = regression_data)
summary(pro_social_suitnum)




"SCCALCD" = "Calcul mental dicté",
"SCPROB" = "Résolution de problèmes",
"SCCALCM" = "Calcul mental pratique",
"SCSUITE" = "Suites numériques",
"SCCOMPA" = "Comparaison de nombres",
"SCMOYMATH" = "Score moyen de maths"


# En maternelle
hihi_maternelle <- lm(
  A04X_SCmoymath ~ 
    sexe_dummy * A04X_Score_sdq_pro_4ans +
    sexe_dummy * A04X_Score_cat_sdq_emo_4ans +
    sexe_dummy * A04X_Score_sdq_hyper_4ans +
    mère_educ_3ans + père_educ_3ans + A04X_AGE4A, 
  data = regression_data)
summary(hihi_maternelle)

# Tester ESTIME DE SOI

furak <- lm(
  A06X_SCMOYMATH ~ 
    sexe_dummy * A06X_IMESSTDSCOL +
    sexe_dummy * A06X_IMESSTDSOC +
    sexe_dummy * A06X_IMESSTDCOMP +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(furak)


furak <- lm(
  A06X_SCPROB ~ 
    sexe_dummy * A06X_IMESSTDSCOL +
    sexe_dummy * A06X_IMESSTDSOC +
    sexe_dummy * A06X_IMESSTDCOMP +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(furak)


furak <- lm(
  A06X_SCSUITE ~ 
    sexe_dummy * A06X_IMESSTDSCOL +
    sexe_dummy * A06X_IMESSTDSOC +
    sexe_dummy * A06X_IMESSTDCOMP +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(furak)


furak <- lm(
  A06X_SCCALCD ~ 
    sexe_dummy * A06X_IMESSTDSCOL +
    sexe_dummy * A06X_IMESSTDSOC +
    sexe_dummy * A06X_IMESSTDCOMP +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(furak)


furak <- lm(
  A06X_SCCALCM ~ 
    sexe_dummy * A06X_IMESSTDSCOL +
    sexe_dummy * A06X_IMESSTDSOC +
    sexe_dummy * A06X_IMESSTDCOMP +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(furak)


furak <- lm(
  A06X_SCCALCM ~ 
    sexe_dummy * A06X_IMESSTDSCOL +
    sexe_dummy * A06X_IMESSTDSOC +
    sexe_dummy * A06X_IMESSTDCOMP +
    A06X_AGEM + père_educ_5ans + mère_educ_5ans,
  data = regression_data)
summary(furak)



table(regression_data$A06X_IMESSTDSCOL)


table(regression_data$A06X_SCORE_SDQ_PRO_6ANS) 
table(regression_data$A06X_SCORE_SDQ_EMO_6ANS) 
table(regression_data$A06X_SCORE_SDQ_HYPER_6ANS)
  



summary(regression_data$A06X_IMESFACTSCOL)

  # Perceived behavior  -------------------------------------------

perceived_behaviour_6ans <- c("A06X_ATTENTA", "A06X_PLAINT", "A06X_PARTAG", "A06X_OBEIS", "A06X_INQUIE", "A06X_NTIENTP", "A06X_AAMI", "A06X_DISTRA", "A06X_ANXIEU", "A06X_AIDE", "A06X_REFLECHI", "A06X_ATTENTI")

distribution_perceived_behaviour_teacher_6ans <- data %>%
  select(SEXE_ENF, all_of(perceived_behaviour_6ans)) %>%
  pivot_longer(-SEXE_ENF, names_to = "Subject", values_to = "Score") %>%
  filter(!is.na(Score), Score %in% 1:3) %>%
  group_by(Subject, SEXE_ENF, Score) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Subject, SEXE_ENF) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  ungroup()
print(distribution_perceived_behaviour_teacher_6ans, n = 100)

# Update gender labels in the data
distribution_perceived_behaviour_teacher_6ans <- distribution_perceived_behaviour_teacher_6ans %>%
  mutate(Gender = case_when(
    SEXE_ENF == 1 ~ "Boy",
    SEXE_ENF == 2 ~ "Girl",
    TRUE ~ NA_character_
  ))

#Plot
ggplot(distribution_perceived_behaviour_teacher_6ans, aes(x = factor(Score), y = Percent, fill = Gender)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Subject, scales = "free_y", labeller = labeller(Subject = function(x) {
    recode(x,
           "A06X_ATTENTA" = "Attentif aux autres",
           "A06X_PLAINT" = "Se plaint maux de tete",
           "A06X_PARTAG" = "Partage facilement",
           "A06X_OBEIS" = "Obéit facilement",
           "A06X_INQUIE" = "S'inquiète souvent",
           "A06X_NTIENTP" = "Ne se tient pas",
           "A06X_AAMI" = "A un ami",
           "A06X_DISTRA" = "Du mal à se concentrer",
           "A06X_ANXIEU" = "Anxieuse face à situation nouvelles",
           "A06X_AIDE" = "Pret à aider les autres",
           "A06X_REFLECHI" = "Réflechi avant d'agir",
           "A06X_ATTENTI" = "Va au bout des ses taches"
    )
  })) +
  scale_fill_manual(
    values = c("Boy" = "#1f78b4", "Girl" = "#e377c2")) +
  labs(
    x = "Évaluation de l’enseignant (1 = Un peu vrai, 3 = Pas vrai)",
    y = "Pourcentage",
    title = "Behaviour perception by subject and gender",
    fill = "Genre"
  ) +
  theme_minimal()
#Comment -> Anxieux: "Very True" -> 41,3% girls vs 37,2% boys
#                    "Not true" -> 44,9% girls vs 47,4% boys
#           S'inquiètes souvent:


# Results =========================================================================

## Table 1: Effect of gender and controls  =====================================================================

### On direct math score ------------------------------------------------


## Dependant Variable Rescaling

summary(regression_data$A06X_SCMOYMATH)


# Define min and max
min_val <- min(regression_data$A06X_SCMOYMATH, na.rm = TRUE)
max_val <- max(regression_data$A06X_SCMOYMATH, na.rm = TRUE)

# Rescale to [0,1]
regression_data$A06X_SCMOYMATH_rescaled <- (regression_data$A06X_SCMOYMATH - min_val) / (max_val - min_val)

#Check
summary(regression_data$A06X_SCMOYMATH_rescaled)


### Naive regression

regression_naive <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy,
  data = regression_data
)
summary(regression_naive)
#Comment -> 

table(regression_data$sexe_dummy)

### Adding age

regression_age <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num,
  data = regression_data
)
summary(regression_age)
#Comment -> 


### Adding parental education

regression_parent_educ <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num +
    mère_educ_5ans +
    père_educ_5ans,
  data = regression_data
)
summary(regression_parent_educ)
#Comment -> Gender: coefficient stable, remains very significant
#           Age: remains the same
#           Public / Priveé: Being on a private school is associated on average with 6,7 points lower math score, everything else fixed
#           Mother's education: Very significant for higher levels of education. Having a mom that did "grandes écoles" is associated on average with an increase of 30 SD score, everything being equal.
#           Father's education: Father's education seems a little bit more linear, and coefficient also gets larger the higher the education


### Adding parental occupation

regression_parent_job <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi,
  data = regression_data
)
summary(regression_parent_job)
#Comment -> All coefficients commented before remain pretty much unchanged
#           Mother's occupation: Unemployed and "at home" coefficients are significant and negative. If the mother is unemployed it is associated with -11 SD math score on average, everything else being fixed.
#           Father's occupation: Not significant


### Adding family structure

regression_family_structure <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans,
  data = regression_data
)
summary(regression_family_structure)
#Comment -> All coefficients commented before remain stable and significant
#.          Family structure: Not significant

### Adding migration status

regression_migration <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    migration_père +
    migration_mère,
  data = regression_data
)
summary(regression_migration)
#Comment -> Coefficients commented before remained stable
#.          Migration status of both parents: not significant


### Adding house income

regression_house_income <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = regression_data
)
summary(regression_house_income)
coeftest(regression_house_income, vcov. = vcovHC(regression_house_income, type = "HC1"))


regression_house_income_9 <- lm(
  perceived_math_skill ~ 
    sexe_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = regression_data
)
summary(regression_house_income_9)



regression_house_income_10 <- lm(
  A06X_SCMOYMATH ~ 
    sexe_dummy +
    perceived_math_skill +
    A06X_AGEM +
    A06X_SCmoylect,
  data = regression_data
)
summary(regression_house_income_10)

regression_house_income_11 <- lm(
  perceived_math_skill ~ 
    sexe_dummy +
    A06X_SCMOYMATH +
    A06X_AGEM +
    A06X_SCmoylect,
  data = regression_data
)
summary(regression_house_income_11)



# Create a cleaned version of the data used for regression
regression_clean <- regression_data %>%
  dplyr::select(perceived_math_skill, A06X_SCMOYMATH, A06X_SCmoylect, A06X_AGEM, sexe_dummy) %>%
  na.omit()

# Re-run the benchmark model on cleaned data
benchmark1 <- lm(
  perceived_math_skill ~ A06X_SCMOYMATH + A06X_SCmoylect,
  data = regression_clean
)
summary(benchmark1)

# Add residuals to cleaned dataset
regression_clean$residuals_perception <- residuals(benchmark1)

# Now regress residuals on gender
bias_model <- lm(residuals_perception ~ sexe_dummy, data = regression_clean)
summary(bias_model)

bias_model_1 <- lm(residuals_probit ~ sexe_dummy, data = regression_clean)
summary(bias_model_1)

# ==============================

regression_clean$perceived_math_skill_f <- factor(
  regression_clean$perceived_math_skill,
  levels = 1:5,
  labels = c("très en dessous", "en dessous", "moyen", "au dessus", "très en dessus"),
  ordered = TRUE
)

table(regression_data$perceived_math_skill_f)



regression_house_income_10 <- lm(
  perceived_math_skill ~ 
    sexe_dummy +
    A06X_SCMOYMATH +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans,
    data = regression_data
)
summary(regression_house_income_10)








#Comment -> Being a girl is associated with a 16,7698 SD lower math score than being a boy, holding everything else constant (e.g., parental education, school type, age).

#Conclusion: "The gender gap in standardized math scores at age 6 is robust to the inclusion of a wide set of controls, including age, school type, parental education, occupational status, family structure, migration background, and household income. 
#            Across all specifications, girls consistently score around 0.17 standard deviations lower than boys, a difference that remains statistically significant and substantively meaningful. 
#            These findings strongly suggest that the gap is not simply due to socioeconomic or demographic confounding, and may instead reflect deeper psychological, pedagogical, or sociocultural mechanisms — including parental attitudes, expectations, early socialization, or classroom dynamics — that warrant further investigation."
table(regression_data$chez_qui_vit_5ans)

### Adding past scores

# Maths score
regression_past_maths_score <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    migration_père +
    migration_mère +
    A04X_SCmoymath +
    revenu_part_dec_5y,
  data = regression_data
)
summary(regression_past_maths_score)
#Comment -> Gender coefficient: increases slightly in absolute value, from -0.168 to -0.186, and remains highly significant.
#           Past math score: Strongly positive, with a large effect size (β = 0.325, p < 0.001).
#           The R² increases substantially from 0.128 to 0.272, meaning you now explain more than double the variance in math scores at age 6.
#           Interpretation: Even when accounting for where children started in math at age 4, the gender gap remains large and significant, and actually slightly grows in magnitude.
#           This means that the gap is not explained by prior math ability: girls and boys may start at similar math levels, but girls make less progress between 4 and 6.
#           The strong positive coefficient on past math score indicates that early math ability is highly predictive of later math performance (as expected).



# Maths and literature scores
regression_past_lecture_score <- lm( 
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A06X_AGEM +
    A06X_PUBLPRIVc_num +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    migration_père +
    migration_mère +
    revenu_part_dec_5y +
    A04X_SCmoymath +
    A04X_SCmoylect,
  data = regression_data
)
summary(regression_past_lecture_score)
#Comment -> past literature and maths score very significant, and gender coefficient gets larger

coeftest(regression_past_lecture_score, vcov. = vcovHC(regression_past_lecture_score, type = "HC1"))
## Conclusion: Including both early math and reading scores shows that prior ability in both domains helps predict math outcomes — math, more strongly than reading, but both matter.
#              The fact that the gender gap grows further after controlling for both abilities suggests something structural is happening:
#              Girls do not underperform at age 4, but between 4 and 6, boys pull ahead — despite having similar or even lower initial skill levels.
#.             You’re now adjusting for both inputs into “comparative advantage”. So this result strengthens your broader comparative advantage framework.
#.             IMPORTANT: The loss of significance for many education or employment variables (e.g. mother’s education) suggests that much of their effect was mediated through early scores — i.e., more educated parents help boost early math/reading, and once that's controlled for, their effect on later math is diminished.




### On relative progress (Percentile change) -------------------------------------------------------------

## Naive Regression

lm_percentile_change <- lm(
  percentile_change ~ sexe_dummy,
  data = transition_data
)
summary(lm_percentile_change)


## Adding age and private school
lm_percentile_change_1 <- lm(
  percentile_change ~ sexe_dummy + A06X_AGEM+ A06X_PUBLPRIVc_num,
  data = transition_data
)
summary(lm_percentile_change_1)


## Adding parents education
lm_percentile_change_2 <- lm(
  percentile_change ~ sexe_dummy + A06X_AGEM+ A06X_PUBLPRIVc_num + mère_educ_5ans + 
    père_educ_5ans,
  data = transition_data
)
summary(lm_percentile_change_2)


## Adding parents professional status
lm_percentile_change_3 <- lm(
  percentile_change  ~ sexe_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi,
  data = transition_data
)
summary(lm_percentile_change_3)


## Adding family structure
lm_percentile_change_4 <- lm(
  percentile_change  ~ sexe_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans,
  data = transition_data
)
summary(lm_percentile_change_4)


## Adding migration status
lm_percentile_change_5 <- lm(
  percentile_change  ~ sexe_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère,
  data = transition_data
)
summary(lm_percentile_change_5)


## Adding household income
lm_percentile_change_6 <- lm(
  percentile_change  ~ sexe_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = transition_data
)
summary(lm_percentile_change_6)


## Adding past scores
lm_percentile_change_7 <- lm(
  percentile_change  ~ sexe_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    revenu_part_dec_5y +
    A04X_SCmoymath +
    A04X_SCmoylect,
  data = transition_data
)
summary(lm_percentile_change_7)




## Table 2: Comparative Advantage and girls’ mathematics progress =====================================

# Gender gap in reading and math
lm_math_gap <- lm(percentile_change ~ sexe_enfant, data = transition_data)
summary(lm_math_gap)
#Comment -> On average, girls lose about 5.3 percentile points in math between age 4 and 6, while boys gain ~5.6.


lm_reading_gap <- lm(percentile_change_read ~ sexe_dummy, data = transition_data)
summary(lm_reading_gap)
#Comment -> In reading, boys lose about 0.6 percentile points, while girls gain about 0.6 points between age 4 and 6.
#           The difference is very small (about 1.25 points) and statistically not significant, meaning that there’s no meaningful gender gap in reading progress — unlike in math, where the gap is large and significant

# “Our longitudinal results show a growing gender gap in math, but a stable gap in reading. 
# This pattern does not support a comparative advantage explanation — if children were specializing based on early strengths, we would expect girls to advance in reading and boys in math. 
# Instead, the data suggest that other mechanisms, such as socialization, confidence, or institutional bias, may better explain the divergence.”


## Math Comparative Advantage

#Making Q2 reference level
transition_data$math_initial_quartile_f <- relevel(transition_data$math_initial_quartile_f, ref = "Q2")
transition_data$reading_initial_quartile_f <- relevel(transition_data$reading_initial_quartile_f, ref = "Q2")



# Gender gap in reading and math
lm_math_gap <- lm(percentile_change ~ sexe_dummy, data = transition_data)
summary(lm_math_gap)
#Comment -> On average, girls lose about 5.3 percentile points in math between age 4 and 6, while boys gain ~5.6.


# Adding math comparative advantage
lm_math_adv_1 <- lm(
  percentile_change ~ 
    sexe_dummy * math_advantage_4y,
  data = transition_data
)
summary(lm_math_adv_1)
#Comment -> For boys with zero math advantage at age 4, the expected math progress is +5.91 percentile point
#           Significant gender gap in math progress: Girls progress ~11.86 percentile points less than boys on average, even after accounting for early advantage.
#.          Regression to the mean effect: Children with early math strength (in either gender) tend to progress a bit less, possibly because they already started high


# Adding initial math level
lm_math_adv_2 <- lm(
  percentile_change ~ 
    sexe_dummy * math_advantage_4y +
    sexe_dummy * math_initial_quartile_f +
    math_advantage_4y * math_initial_quartile_f,
  data = transition_data
)
summary(lm_math_adv_2)
# Comment -> Intercept: Baseline math progress for boys in Q1 (lowest quartile), with zero math advantage. They improve ~37 percentile points. That’s huge progress
#            Gender: Girls in lowest math quartile with zero advantage progress ~12 percentile points less than boys. Still a significant gender gap.
#            Math advantage: Higher math advantage is still negatively associated with progress, but the effect is weaker than before. This shows that part of the earlier negative effect was due to initial level.            
#            Initial math quartile: Moving up a quartile (from Q1 to Q2, etc.) means about 12.5 percentile points less progress. Again, reflects strong regression to the mean — the higher you start, the less you "gain".
#            Interaction terms: So the effects of advantage and quartile are roughly the same for boys and girls — the structure of progress seems parallel across gender, even though girls have a lower intercept (baseline)


# Adding French progress
lm_math_adv_3 <- lm(
  percentile_change ~ 
    sexe_dummy * math_advantage_4y +
    sexe_dummy * math_initial_quartile_f +
    sexe_dummy * percentile_change_read,
  data = transition_data
)
summary(lm_math_adv_3)
# Comment -> French progress positively impacts math progress


# Adding individual controls
lm_math_adv_4 <- lm(
  percentile_change ~ 
    sexe_dummy * math_advantage_4y +
    sexe_dummy * math_initial_quartile_f +
    sexe_dummy * percentile_change_read +
    A06X_AGEM +
    père_educ_5ans +
    mère_educ_5ans,
  data = transition_data
)
summary(lm_math_adv_4)



## Absolute Progress (Math score as a dependant variable)

lm_maths_progress <- lm(
  A06X_SCMOYMATH ~ 
    sexe_dummy * math_initial_quartile_f,
  data = transition_data
)
summary(lm_maths_progress)

#Comment -> In Quartile 4 (high initial math performance), the gender gap (girls doing worse than boys) is reduced by about 0.10 standard deviations compared to Quartile 2.
#           Among children who were initially very strong at math (Q4), girls catch up slightly relative to boys by age 6 compared to those in the middle of the initial distribution (Q2).
#           Visual way to think of it: If the "standard" gender gap (girls worse) is -0.21 SD in Q2,
#           then in Q4, the gender gap becomes roughly: -0.21 + 0.10 = − 0.11 (still a gap, but smaller
#           Girls with strong initial math skills lose less ground than girls with lower initial skills.
#           Girls who were top math performers at age 4 do less poorly compared to boys than girls in the middle of the distribution (Q2). The gender gap is narrower among top performers.
#           Among children who were in the top math quartile at age 4, girls score 0.10 standard deviations higher at age 6 relative to boys than girls in the second quartile do relative to boys in that same quartile.


## CONCLUSION ABOUT DIFFERENCES BTW ABSOLUTE AND RELATIVE PROGRESS:
#. In the first model (math score at 6):
#. Girls in Q4 (highest initial quartile) seem to catch up slightly (sexe_dummy:math_initial_quartile_fQ4 is significant and positive).
#. In the second model (percentile change):
#. The same interaction (sexe_dummy:math_initial_quartile_fQ4) is positive, but not statistically significant anymore.
#. Why?
#  → Because even if girls improve their math score at the top, it doesn't necessarily mean they gain percentile rank relative to boys — maybe everyone at the top improves at a similar rate.
#. Percentile change is harder to achieve at the extremes of a distribution!


# Reading gap
lm_reading_gap <- lm(percentile_change_read ~ sexe_dummy, data = transition_data)
summary(lm_reading_gap)

## Reading comparative advantage

lm_read_adv_1 <- lm(
  percentile_change_read ~ 
    sexe_dummy * reading_advantage_4y,
  data = transition_data
)
summary(lm_read_adv_1)
#Comment -> Boys with no reading advantage at age 4 have on average no meaningful gain or loss (very close to 0, and not significant
#           Children with a reading advantage at age 4 tend to make less progress in reading

# Adding initial reading level
lm_read_adv_2 <- lm(
  percentile_change_read ~ 
    sexe_dummy * reading_advantage_4y +
    sexe_dummy * reading_initial_quartile_f,
  data = transition_data
)
summary(lm_read_adv_2)
#Comment -> Intercept: Boys with 0 reading percentile and no advantage gain ~26 percentile points
#           Gender: No significant difference in reading progress between girls and boys at baseline
#           Reading advantage: Still negative, but smaller than before: once you control for actual reading level.
#           Interaction term: "sexe_dummy:reading_advantage_4y". Not significant. Girls do not specialize more in reading than boys when they start with a reading advantage.
#           This contradicts the comparative advantage theory that says girls shift more toward reading if they’re relatively better at it.
#           Initial reading level: Higher initial quartile in reading scores at age of 4 is highly significant and negative: ceiling effect


# # CONCLUSION: Comparative advantage does not lead to specialisation or benefit.
#             Across both domains, a comparative advantage predicts worse outcomes, especially among lower-ability children.
#             No support for the “girls specialise in reading, boys in math” hypothesis.
#             Girls with a reading advantage do not gain more than boys.
#.            Boys with a math advantage also do not thrive.



## Table 3: School Characteristics -------------------------------------

# Caractéristiques de l'établissement

regression_school_2 <- lm(
  percentile_change ~ 
    sexe_dummy * A06X_PUBLPRIVc_num +
    sexe_dummy * A06X_NBELEVES + 
    sexe_dummy * A06X_REPPLUSc +
    sexe_dummy * type_classe_cp +
    A06X_AGEM +
    A04X_SCmoymath +
    A04X_SCmoylect +
    mère_educ_5ans +
    père_educ_5ans,
  data = transition_data
)
summary(regression_school_2)

regression_school_2 <- lm(
  percentile_change ~ 
    sexe_dummy * A06X_PUBLPRIVc_num +
    sexe_dummy * A06X_NBELEVES + 
    sexe_dummy * type_classe_cp +
    A06X_AGEM +
    mère_educ_5ans +
    A04X_SCmoymath +
    A04X_SCmoylect +
    père_educ_5ans,
  data = transition_data
)
summary(regression_school_2)


regression_school_01 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * A06X_PUBLPRIVc_num +
    sexe_dummy * A06X_NBELEVES + 
    sexe_dummy * type_classe_cp +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans,
  data = regression_data
)
summary(regression_school__01)

regression_school__01 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * A06X_PUBLPRIVc_num +
    sexe_dummy * A06X_NBELEVES + 
    sexe_dummy * type_classe_cp +
    A06X_AGEM +
    A04X_SCmoymath +
    A04X_SCmoylect +
    mère_educ_5ans +
    père_educ_5ans,
  data = regression_data
)
summary(regression_school__01)


# Caractéristiques de l'enseignant

regression_data$sexe_enseignant <- ifelse(regression_data$A06X_ENSSEXE == 2, 1, 0)
regression_data$sexe_enseignant_4ans <- ifelse(regression_data$A04X_ENSSEXE == 2, 1, 0)

table(regression_data$sexe_enseignant_4ans)

table(regression_data$A06X_PUBLPRIVc_num)


regression_teacher_21 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * sexe_enseignant +
    sexe_dummy * A06X_ENSDUREE + 
    sexe_dummy * A06X_ENSDISCI3 +
    A06X_AGEM +
    A04X_SCmoymath +
    A04X_SCmoylect +
    mère_educ_5ans +
    père_educ_5ans,
  data = regression_data
)
summary(regression_teacher_21)


regression_teacher_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * sexe_enseignant +
    sexe_dummy * A06X_ENSDUREE + 
    sexe_dummy * A06X_ENSDISCI3 +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans,
  data = regression_data
)
summary(regression_teacher_2)



lm_00 <- lm(A06X_SCMOYMATH ~ 
              sexe_dummy * A06X_ENSDISCI3 * sexe_enseignant + 
              sexe_dummy * A06X_ENSDISCI3 +
              sexe_dummy * sexe_enseignant +
              revenu_part_dec_5y +
              mère_educ_5ans + 
              A06X_AGEM +
              père_educ_5ans,
            data = regression_data)
summary(lm_00)


regression_teacher_3 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * A06X_ENSSEXE +
    sexe_dummy * A06X_ENSDISCI3 +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    revenu_part_dec_5y,
  data = regression_data
)
summary(regression_teacher_3)



regression_teacher_2 <- lm(
  percentile_change ~ 
    A06X_ENSSEXE +
    A06X_ENSDUREE + 
    A06X_ENSDISCI3 +
    A06X_ENSAGE +
    A06X_AGEM +
    A04X_SCmoymath +
    A04X_SCmoylect +
    mère_educ_5ans +
    père_educ_5ans,
  data = transition_data %>% filter(sexe_dummy == 0)
)
summary(regression_teacher_2)


#Comment -> sexe_dummy:type_classe_cpDouble niveau : CP-CE1 is marginally significant and negative.


regression_teacher_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * A06X_ENSSEXE +
    sexe_dummy * A06X_ENSDUREE + 
    sexe_dummy * A06X_ENSDISCI3 +
    sexe_dummy * A06X_ENSAGE +
    A06X_AGEM +
    A04X_SCmoymath +
    A04X_SCmoylect +
    mère_educ_5ans +
    père_educ_5ans,
  data = regression_data
)
summary(regression_teacher_2)


## Table 4: ---------------------------------------------------

# Parents education
lm_ses_index <- lm(
  percentile_change ~ 
    sexe_dummy * mère_educ_5ans +
    sexe_dummy * père_educ_5ans +
    A06X_AGEM +
    A04X_SCmoymath +
    A04X_SCmoylect,
  data = transition_data
)
summary(lm_ses_index)

# Family structure
lm_family_structure <- lm(
  percentile_change ~ 
    sexe_dummy * chez_qui_vit_5ans +
    A04X_SCmoymath +
    A04X_SCmoylect +
    A06X_AGEM +
    père_educ_5ans +
    mère_educ_5ans,
    data = transition_data
)
summary(lm_family_structure)

#Migration status
regression_interac_migration <- lm(
  percentile_change ~ 
    sexe_dummy * migration_père +
    sexe_dummy * migration_mère +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    A04X_SCmoymath + 
    A04X_SCmoylect,
  data = transition_data
)
summary(regression_interac_migration)

#Job status

regression_interac_job <- lm(
  percentile_change ~ 
    sexe_dummy * mere_emploi +
    sexe_dummy * pere_emploi +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    A04X_SCmoymath +
    A04X_SCmoylect,
  data = transition_data
)
summary(regression_interac_job)





## Quantile regression ----------------------------

taus <- seq(0.1, 0.9, by = 0.1)

# Regress for each maths score quantile
qr_models <- lapply(taus, function(tau) {
  rq(
    A06X_SCMOYMATH_rescaled ~ sexe_dummy +
      A06X_PUBLPRIVc_num +
      A06X_AGEM +
      mère_educ_5ans +
      père_educ_5ans +
      mere_emploi +
      pere_emploi +
      chez_qui_vit_5ans +
      migration_père +
      migration_mère +
      A04X_SCmoymath +
      A04X_SCmoylect +
      revenu_part_dec_5y,
    tau = tau,
    data = regression_data
  )
})

gender_gap_by_quantile <- data.frame(
  tau = taus,
  estimate = sapply(qr_models, function(model) coef(model)["sexe_dummy"]),
  se = sapply(qr_models, function(model) summary(model, se = "boot")$coefficients["sexe_dummy", "Std. Error"])
)

#Plot
ggplot(gender_gap_by_quantile, aes(x = tau, y = estimate)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
              fill = "#0072B2", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Gender Gap in Math Scores Across Quantiles",
    x = "Quantile (τ)",
    y = "Coefficient on Gender (Female vs. Male)"
  ) +
  theme_minimal()
#Comment -> Effect of gender on the math gap shrinks going up on the distribution.
#           The quantile regression results reveal that the gender gap in math is not uniform across the distribution of achievement. 
#           Girls significantly underperform boys at the lower quantiles of the math score distribution (e.g., around -0.2 at the 20th percentile), while the gap narrows and even approaches zero as we move toward the upper quantiles. 
#           This pattern suggests that gender disparities in math are particularly concentrated among lower-performing students.
#           This is consistent with the OLS result restricted to the top 50% of students, which estimates a much smaller average gap (around -0.04). This implies that the gender gap is driven primarily by differences in the bottom half of the distribution, and interventions might be most needed there


library(knitr)

# Add confidence intervals
gender_gap_by_quantile <- gender_gap_by_quantile %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )

# Print table
kable(
  gender_gap_by_quantile,
  digits = 4,
  col.names = c("Quantile (τ)", "Estimate", "Std. Error", "Lower 95% CI", "Upper 95% CI"),
  caption = "Gender Coefficient (Female vs. Male) Across Math Score Quantiles"
)
#Comment -> Quantile regression estimates how predictors affect the conditional distribution of outcomes. In this case, the negative gender coefficients at lower quantiles indicate that, among children with the same background, girls are more likely than boys to fall into the lower tail of the math score distribution. 
#           It does not imply that girls and boys who actually scored in the bottom decile differ significantly in their performance once controls are applied — rather, it shows that gender is a stronger negative predictor of performance at the lower end of the predicted outcome distribution.
#           Girls are more likely than boys to be placed in the lower tail of the conditional outcome distribution, holding all else equal.
#           the correct interpretation is that quantile regression estimates a distributional shift — not score differences within observed groups.



## Model by gender   -------------------------------------------

#Boys
regression_boys <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    migration_père +
    A06X_ANXIEU_f +
    migration_mère +
    A04X_SCmoymath +
    revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 0)
)
summary(regression_boys)

#Girls
regression_girls <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    migration_père +
    A06X_ANXIEU_f +
    migration_mère +
    A04X_SCmoymath +
    revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1)
)
summary(regression_girls)
#Comment -> Mother's education is significant for girls and not for boys.


lm_prosocial <- lm(
  A06X_SCMOYMATH_rescaled ~ A06X_IMESFACTSCOL + A06X_IMESFACTSOC + A06X_IMESFACTCOMP + A04X_SCmoymath,
  data = regression_data %>% filter(sexe_dummy == 0)
)
summary(lm_prosocial)


lm_prosocial_1 <- lm(
  A06X_SCMOYMATH_rescaled ~ A06X_IMESFACTSCOL + A06X_IMESFACTSOC + A06X_IMESFACTCOMP + A06X_ENSAGE + A06X_ENSSEXE + A06X_ENSDUREE + A04X_SCmoymath,
  data = regression_data %>% filter(sexe_dummy == 1)
)
summary(lm_prosocial_1)


lm_prosocial_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06X_IMESFACTSOC + A06X_ENSSEXE + A06X_ENSDUREE + A04X_SCmoymath,
  data = regression_data
)
summary(lm_prosocial_2)


lm_teach_back_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ A06X_ENSDISCI3 * perceived_math_skill_f + A06X_ENSAGE + A06X_ENSDUREE + A04X_SCmoymath + A04X_SCmoylect,
  data = regression_data %>% filter(sexe_dummy == 1)
)
summary(lm_teach_back_2)

table(regression_data$perceived_math_skill_f)


lm_teach_back_3 <- lm(
  A06X_SCMOYMATH_rescaled ~ A06X_ENSDISCI3 * perceived_math_skill_f + A06X_ENSAGE + A06X_ENSDUREE + A04X_SCmoymath + A04X_SCmoylect,
  data = regression_data %>% filter(sexe_dummy == 0)
)
summary(lm_teach_back_3)


table(regression_data$perceived_math_skill_f)    

## Interaction terms ==============================


## Interacting parents education with gender

interaction_mother_educ_1 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy *
    mère_educ_5ans +
    sexe_dummy *
    père_educ_5ans +
    A06X_AGEM +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans,
  data = regression_data
)
summary(interaction_mother_educ)
#Comment -> Interaction term not significant, and sexe variable looses significance

# Adding past scores
interaction_mother_educ_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy *
    mère_educ_5ans +
    sexe_dummy *
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    chez_qui_vit_5ans +
    A04X_SCmoymath +
    A04X_SCmoylect,
  data = regression_data
)
summary(interaction_mother_educ_2)

table(regression_data$)



## Interacting sex with migration

# All interactions
regression_interac_migration <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * migration_père +
    sexe_dummy * migration_mère +
    sexe_dummy * mère_educ_5ans +
    sexe_dummy * père_educ_5ans +
    sexe_dummy * chez_qui_vit_5ans +
    A06X_AGEM +
    revenu_part_dec_5y,
  data = regression_data
)
summary(regression_interac_migration)
coeftest(regression_interac_migration, vcov. = vcovHC(regression_interac_migration, type = "HC1"))

table(regression_data$chez_qui_vit_5ans)
## + past scores
regression_interac_migration_1 <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy * migration_père +
    sexe_dummy * migration_mère +
    sexe_dummy * mère_educ_5ans +
    sexe_dummy * père_educ_5ans +
    sexe_dummy * chez_qui_vit_5ans +
    A06X_AGEM +
    revenu_part_dec_5y +
    A04X_SCmoymath +
    A04X_SCmoylect,
  data = regression_data
)
summary(regression_interac_migration_1)


tapply(regression_data$chez_qui_vit_5ans, regression_data$sexe_dummy, summary)

## El laboratorio -------------

# Toys

# Merge recoded toy variables into regression_data
regression_data <- regression_data %>%
  left_join(
    participants_data %>%
      dplyr::select(id_DEM_1055_RG, all_of(toy_vars)),
    by = "id_DEM_1055_RG"
  )

transition_data <- transition_data %>%
  left_join(
    participants_data %>%
      dplyr::select(id_DEM_1055_RG, all_of(toy_vars)),  # Ensure `id_DEM_1055_RG` is included along with the toy variables
    by = "id_DEM_1055_RG"
  )


## On Raw Math Score

table(regression_data$A06E_JOUETS4)

lm_toys_raw <- lm(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06E_JOUETS4 + A06E_JOUETS10,
  data = regression_data
)
summary(lm_toys_raw)


lm_toys_raw_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06E_JOUETS4 + A06E_JOUETS10 + A06E_JOUETS3 + A06E_JOUETS1 + A06E_JOUETS5 + A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS2 + A06X_AGEM + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data
)
summary(lm_toys_raw_2)

lm_toys_raw_2_boys <- lm(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06E_JOUETS4 + A06E_JOUETS10 + A06E_JOUETS3 + A06E_JOUETS1 + A06E_JOUETS5 + A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS2 + A06X_AGEM + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 0)
)
summary(lm_toys_raw_2_boys) 

lm_toys_raw_2_girls <- lm(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06E_JOUETS4 + A06E_JOUETS10 + A06E_JOUETS3 + A06E_JOUETS1 + A06E_JOUETS5 + A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS2 + A06X_AGEM + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1)
)
summary(lm_toys_raw_2_girls)

tapply(regression_data$A06E_JOUETS8, regression_data$sexe_dummy, summary)

# Adding past math scores 
lm_toys_raw_3 <- lm(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy + A06E_JOUETS4 + A06E_JOUETS10 + A06E_JOUETS3 + A06E_JOUETS1 + A06E_JOUETS5 + A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS2 + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y + A04X_SCmoymath + A04X_SCmoylect,
  data = regression_data
)
summary(lm_toys_raw_3)


lm_toys_raw_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ A06E_JOUETS4 + A06E_JOUETS10 + A06E_JOUETS3 + A06E_JOUETS1 + A06E_JOUETS5 + A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS2 + A06X_AGEM + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1)
)
summary(lm_toys_raw_2)


lm_toys_raw_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ A06E_JOUETS4 + A06E_JOUETS10 + A06E_JOUETS3 + A06E_JOUETS1 + A06E_JOUETS5 + A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS8 + A06E_JOUETS9 + A06E_JOUETS2 + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1)
)
summary(lm_toys_raw_2)



lm_toys_raw_2 <- lm(
  A06X_SCMOYMATH_rescaled ~ sexe_dummy * A06E_JOUETS8 + A06E_JOUETS10 + A06E_JOUETS3 + A06E_JOUETS1 + A06E_JOUETS5 + A06E_JOUETS6 + A06E_JOUETS7 + A06E_JOUETS4 + A06E_JOUETS9 + A06E_JOUETS2 + père_educ_5ans + mère_educ_5ans + revenu_part_dec_5y,
  data = regression_data
)
summary(lm_toys_raw_2)


# Sélectionner uniquement les variables de jouets
toy_vars <- regression_data[, c("A06E_JOUETS1", "A06E_JOUETS2", "A06E_JOUETS3",
                                "A06E_JOUETS4", "A06E_JOUETS5", "A06E_JOUETS6",
                                "A06E_JOUETS7", "A06E_JOUETS8", "A06E_JOUETS9",
                                "A06E_JOUETS10")]

# Calculer la matrice de corrélation
cor_matrix <- cor(toy_vars, use = "pairwise.complete.obs")

# Afficher les corrélations supérieures à 0.4 par exemple
cor_matrix[abs(cor_matrix) > 0.4 & cor_matrix != 1]



## Score en lecture  -------------------------------------------------------------------------

regression_lecture <- lm(
  log(A06X_SCmoylect + 4) ~ 
    sexe_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = regression_data
)
summary(regression_lecture)


## Scores maternelle -------------------------

regression_maths_maternelle <- lm(
  A04X_SCmoymath ~ 
    sexe_dummy +
    A04X_AGE4A +
    mère_educ_3ans +
    père_educ_3ans +
    mere_emploi_3ans +
    pere_emploi_3ans +
    chez_qui_vit_3ans +
    migration_père +
    migration_mère +
    revenu_part_dec_3y,
  data = regression_data
)
summary(regression_maths_maternelle)


regression_lect_maternelle <- lm(
  A04X_SCmoylect ~ 
    sexe_dummy +
    A04X_AGE4A +
    mère_educ_3ans +
    père_educ_3ans +
    mere_emploi_3ans +
    pere_emploi_3ans +
    chez_qui_vit_3ans +
    migration_père +
    migration_mère +
    revenu_part_dec_3y,
  data = regression_data
)
summary(regression_lect_maternelle)


regression_lect_maternelle_2 <- lm(
  A04X_SCmoylect ~ 
    sexe_dummy *
    mère_educ_3ans +
    sexe_dummy *
    père_educ_3ans +
    A04X_AGE4A +
    mere_emploi_3ans +
    pere_emploi_3ans +
    chez_qui_vit_3ans +
    migration_père +
    migration_mère +
    revenu_part_dec_3y,
  data = regression_data
)
summary(regression_lect_maternelle_2)




jueguetes_vars <- c("A05C_JFIGUR", "A05C_JVOIT", "A05C_JPOUP", "A05C_JBAL", 
              "A05C_JCONS", "A05C_JDINET", "A05C_JMUZ", 
              "A05C_JSOCART", "A05C_JPELUCH", "A05C_JDEGUIS", "A05C_JEDUC")

# Create new dummy variables with suffix "_dummy"
regression_data <- regression_data %>%
  mutate(across(all_of(jueguetes_vars), 
                .fns = ~ case_when(
                  . == 1 ~ 1,
                  . == 2 ~ 0,
                  . == 9 ~ NA_real_,
                  TRUE ~ NA_real_),
                .names = "{.col}_dummy"))

prudencio <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A05C_JCONS_dummy +
    A05C_JDINET_dummy +
    A05C_JPELUCH_dummy +
    A05C_JMUZ_dummy +
    A05C_JBAL_dummy +
    A05C_JSOCART_dummy +
    A05C_JPOUP_dummy +
    A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = regression_data
)
summary(prudencio)
stargazer(prudencio, type = "text")

prudencio_score <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    sexe_dummy +
    A05C_JCONS_dummy +
    A05C_JDINET_dummy +
    A05C_JPELUCH_dummy +
    A05C_JMUZ_dummy +
    A05C_JBAL_dummy +
    A05C_JSOCART_dummy +
    A05C_JPOUP_dummy +
    A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy +
    A06X_PUBLPRIVc_num +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    A04X_SCmoymath +
    A04X_SCmoylect +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = regression_data
)
summary(prudencio)
stargazer(prudencio_score, type = "text")



# Filles
prudencio_filles <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    A05C_JCONS_dummy +
    A05C_JDINET_dummy +
    A05C_JPELUCH_dummy +
    A05C_JMUZ_dummy +
    A05C_JBAL_dummy +
    A05C_JSOCART_dummy +
    A05C_JPOUP_dummy +
    A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy +
    A06X_PUBLPRIVc_num +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1))
summary(prudencio_filles)
stargazer(prudencio_filles, type = "text")

#Garçons
prudencio_boys <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    A05C_JCONS_dummy +
    A05C_JDINET_dummy +
    A05C_JPELUCH_dummy +
    A05C_JMUZ_dummy +
    A05C_JBAL_dummy +
    A05C_JSOCART_dummy +
    A05C_JPOUP_dummy +
    A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 0))
summary(prudencio_filles)


#Adding past scores
prudencio_boys_score <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    A05C_JCONS_dummy +
    A05C_JDINET_dummy +
    A05C_JPELUCH_dummy +
    A05C_JMUZ_dummy +
    A05C_JBAL_dummy +
    A05C_JSOCART_dummy +
    A05C_JPOUP_dummy +
    A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy +
    A06X_PUBLPRIVc_num +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    A04X_SCmoymath +
    A04X_SCmoylect +
    revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 0))
summary(prudencio_filles)
stargazer(prudencio_boys_score, type = "text")

prudencio_filles_score <- lm(
  A06X_SCMOYMATH_rescaled ~ 
    A05C_JCONS_dummy +
    A05C_JDINET_dummy +
    A05C_JPELUCH_dummy +
    A05C_JMUZ_dummy +
    A05C_JBAL_dummy +
    A05C_JSOCART_dummy +
    A05C_JPOUP_dummy +
    A05C_JEDUC_dummy +
    A05C_JDEGUIS_dummy +
    A06X_PUBLPRIVc_num +
    A06X_PUBLPRIVc_num +
    A06X_AGEM +
    mère_educ_5ans +
    père_educ_5ans +
    mere_emploi +
    pere_emploi +
    migration_père +
    migration_mère +
    A04X_SCmoymath +
    A04X_SCmoylect +
    revenu_part_dec_5y,
  data = regression_data %>% filter(sexe_dummy == 1))
summary(prudencio_filles_score)
stargazer(prudencio_filles_score, type = "text")


# Fixed effects model =================================================================

# Prendre revenu absolu
regression_data$log_revenu_cp <- log(regression_data$revenu_part_5y + 1)
regression_data$log_revenu_maternelle <- log(regression_data$revenu_part_3y + 1)


# Reshape wide to long format
long_panel <- regression_data %>%
  rename(
    id = id_DEM_1055_RG,
    math_maternelle = A04X_SCmoymath,
    french_maternelle = A04X_SCmoylect,
    age_maternelle = A04X_AGE4A,
    math_cp = A06X_SCMOYMATH,
    french_cp = A06X_SCmoylect,
    age_cp = A06X_AGEM,
    nbeleves_maternelle = A04X_NBELEVES,
    nbeleves_cp = A06X_NBELEVES,
    
    chez_qui_vit_maternelle = chez_qui_vit_3ans,
    chez_qui_vit_cp = chez_qui_vit_5ans,
    
    père_emploi_maternelle = pere_emploi_3ans,
    père_emploi_cp = pere_emploi,
    
    mère_emploi_maternelle = mere_emploi_3ans,
    mère_emploi_cp = mere_emploi,
    # Teacher-rated traits (child characteristics)
    ATTENTA_maternelle = A04X_ATTENTA,
    PLAINT_maternelle = A04X_PLAINT,
    PARTAG_maternelle = A04X_PARTAG,
    OBEIS_maternelle = A04X_OBEIS,
    INQUIE_maternelle = A04X_INQUIE,
    NTIENTP_maternelle = A04X_NTIENTP,
    AAMI_maternelle = A04X_AAMI,
    DISTRA_maternelle = A04X_DISTRA,
    ANXIEU_maternelle = A04X_ANXIEU,
    AIDE_maternelle = A04X_AIDE,
    REFLECHI_maternelle = A04X_REFLECHI,
    ATTENTI_maternelle = A04X_ATTENTI,
    
    ATTENTA_cp = A06X_ATTENTA,
    PLAINT_cp = A06X_PLAINT,
    PARTAG_cp = A06X_PARTAG,
    OBEIS_cp = A06X_OBEIS,
    INQUIE_cp = A06X_INQUIE,
    NTIENTP_cp = A06X_NTIENTP,
    AAMI_cp = A06X_AAMI,
    DISTRA_cp = A06X_DISTRA,
    ANXIEU_cp = A06X_ANXIEU,
    AIDE_cp = A06X_AIDE,
    REFLECHI_cp = A06X_REFLECHI,
    ATTENTI_cp = A06X_ATTENTI,
    
    # Teacher characteristics
    ensex_maternelle = A04X_ENSSEXE,
    ensduree_maternelle = A04X_ENSDUREE,
    
    ensex_cp = A06X_ENSSEXE,
    ensduree_cp = A06X_ENSDUREE,
    
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
    
    ATTENTA_maternelle, ATTENTA_cp,
    PLAINT_maternelle, PLAINT_cp,
    PARTAG_maternelle, PARTAG_cp,
    OBEIS_maternelle, OBEIS_cp,
    INQUIE_maternelle, INQUIE_cp,
    NTIENTP_maternelle, NTIENTP_cp,
    AAMI_maternelle, AAMI_cp,
    DISTRA_maternelle, DISTRA_cp,
    ANXIEU_maternelle, ANXIEU_cp,
    AIDE_maternelle, AIDE_cp,
    REFLECHI_maternelle, REFLECHI_cp,
    ATTENTI_maternelle, ATTENTI_cp,
    
    ensex_maternelle, ensex_cp,
    ensduree_maternelle, ensduree_cp  # ← no comma here!
  ) %>%
  pivot_longer(
    cols = c(
      math_maternelle, french_maternelle, age_maternelle,
      math_cp, french_cp, age_cp,
      nbeleves_maternelle, nbeleves_cp,
      
      ATTENTA_maternelle, ATTENTA_cp,
      PLAINT_maternelle, PLAINT_cp,
      PARTAG_maternelle, PARTAG_cp,
      OBEIS_maternelle, OBEIS_cp,
      INQUIE_maternelle, INQUIE_cp,
      NTIENTP_maternelle, NTIENTP_cp,
      AAMI_maternelle, AAMI_cp,
      DISTRA_maternelle, DISTRA_cp,
      ANXIEU_maternelle, ANXIEU_cp,
      AIDE_maternelle, AIDE_cp,
      REFLECHI_maternelle, REFLECHI_cp,
      ATTENTI_maternelle, ATTENTI_cp,
      
      ensex_maternelle, ensex_cp,
      ensduree_maternelle, ensduree_cp
    ),
    names_to = c(".value", "time"),
    names_pattern = "(.*)_(maternelle|cp)"
  ) %>%
  mutate(
    time = factor(time, levels = c("maternelle", "cp")),
    sexe_enfant = factor(sexe_enfant, levels = c(1, 2), labels = c("Garçon", "Fille")),
    
    revenu_part = ifelse(time == "maternelle", log_revenu_maternelle, log_revenu_cp),
    
    chez_qui_vit = case_when(
      time == "maternelle" ~ as.character(chez_qui_vit_maternelle),
      time == "cp"         ~ as.character(chez_qui_vit_cp)
    ),
    père_emploi = case_when(
      time == "maternelle" ~ as.character(père_emploi_maternelle),
      time == "cp"         ~ as.character(père_emploi_cp)
    ),
    mère_emploi = case_when(
      time == "maternelle" ~ as.character(mère_emploi_maternelle),
      time == "cp"         ~ as.character(mère_emploi_cp)
    ),
    
    # Convert categorical variables to factors
    chez_qui_vit = factor(chez_qui_vit),
    père_emploi = factor(père_emploi),
    mère_emploi = factor(mère_emploi)
  )

#Numeric time
long_panel <- long_panel %>%
  mutate(
    time_num = ifelse(time == "maternelle", 0, 1)  # or use age directly
  )


## Growth model (Linear mixed effects)
library(lme4)

growth_model <- lmer(
  math ~ time_num * sexe_enfant + (1 | id),
  data = long_panel
)
summary(growth_model)

growth_model_controls <- lmer(
  math ~ time_num * sexe_enfant +
    french + revenu_part +
    mère_emploi + père_emploi +
    chez_qui_vit +
    (1 | id),
  data = long_panel
)
summary(growth_model_controls)


# Convert to panel structure
library(plm)

panel_data <- pdata.frame(long_panel, index = c("id", "time"))

panel_data$ensex <- ifelse(panel_data$ensex == "Homme", 1, 0)
panel_data$ANXIEU <- as.numeric(panel_data$ANXIEU)
panel_data$ATTENTI <- as.numeric(panel_data$ATTENTI)
panel_data$REFLECHI <- as.numeric(panel_data$REFLECHI)
panel_data$DISTRA <- as.numeric(panel_data$DISTRA)
panel_data <- panel_data %>%
  mutate(ensduree_z = as.numeric(scale(ensduree)))



# The model
install.packages("fixest")
library(fixest)

fe_model <- feols(
  math ~ sexe_enfant * (ensex + ensduree_z + ANXIEU + ATTENTI) | id,
  data = panel_data
)

summary(fe_model)


construire una variable qui est la différence de score
faire une diff and diff

table(regression_data$A06X_NIVECLASS2)


# Oaxaca Decomposition ================================================================
library(oaxaca)
library(forcats)

# Recode and clean your Oaxaca dataset

regression_data <- regression_data %>%
  mutate(
    math_quantile = ntile(A06X_SCMOYMATH, 10),  # deciles (or 5 for quintiles)
    math_quantile = factor(math_quantile)
  )

oaxaca_data <- regression_data %>%
  select(A06X_SCMOYMATH, A06X_AGEM, mère_educ_5ans, père_educ_5ans,
         revenu_part_dec_5y, SEXE_ENF, A04X_SCmoymath, A04X_SCmoylect) %>%
  filter(
    !is.na(A06X_SCMOYMATH),
    !is.na(A06X_AGEM),
    !is.na(mère_educ_5ans),
    !is.na(père_educ_5ans),
    !is.na(revenu_part_dec_5y),
    !is.na(SEXE_ENF),
    !is.na(A04X_SCmoymath),
    !is.na(A04X_SCmoylect)
  ) %>%
  mutate(
    # Convert SEXE_ENF to numeric (0 = boy, 1 = girl)
    SEXE_ENF = ifelse(SEXE_ENF == 1, 0, ifelse(SEXE_ENF == 2, 1, NA)),
    
    # Combine mother's education into 3 categories
    mère_educ_5ans = fct_collapse(
      mère_educ_5ans,
      Faible = c("Aucun", "Enseignement primaire", "Enseignement secondaire niveau collège(Brevet)"),
      Moyen = "Enseignements secondaire niveau lycée",
      Élevé = c("Enseignement supérieur 1er cycle",
                "Enseignement supérieur diplôme de 2ème cycle",
                "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles")
    ,
    
    # Combine father's education into 3 categories
    père_educ_5ans = fct_collapse(
      père_educ_5ans,
      Faible = c("Aucun", "Enseignement primaire", "Enseignement secondaire niveau collège(Brevet)"),
      Moyen = "Enseignements secondaire niveau lycée",
      Élevé = c("Enseignement supérieur 1er cycle",
                "Enseignement supérieur diplôme de 2ème cycle",
                "Enseignement supérieur diplôme de 3ème cycle universitaire et grandes écoles")
    )
  )


# Descriptive about distribution
decomp1 <- oaxaca(
  formula = A06X_SCMOYMATH ~ mère_educ_5ans + père_educ_5ans + revenu_part_dec_5y + A04X_SCmoymath + A04X_SCmoylect + math_quantile | SEXE_ENF,
  data = oaxaca_data,
  R = 1000
)


#Plot
plot(decomp1, components = c("endowments","coefficients"))
#Comment -> Math quantiles (especially math_quantile10, 9, 8, etc.) contribute strongly and positively → this means boys are more concentrated in the top of the math distribution at age 4, and this explains a large part of the math gap at 6 years.
#.          Other predictors like parental education or reading scores contribute very little to the explained gap.
#.          So: Most of the explained part of the gender gap is due to the fact that girls were less present in high quantiles of math at 4 years. This is the “distributional composition” effect you were aiming to test — and it’s confirmed here.
#           The gender math gap is strongly driven by the distribution of boys and girls across the outcome distribution — not just a uniform gap across the board



# Decomp
decomp2 <- oaxaca(
  formula = A06X_SCMOYMATH ~ mère_educ_5ans + père_educ_5ans + revenu_part_dec_5y + A04X_SCmoymath + A04X_SCmoylect | SEXE_ENF,
  data = oaxaca_data,
  R = 1000
)


#The gender maths score gap
decomp2$y
#Comment -> On average, boys score 0.16 points higher than girls on the math outcome, after adjusting for centering or standardization (since your scores are likely standardized).
#           The raw gender gap is positive, meaning girls underperform relative to boys.

plot(decomp2, components = c("endowments","coefficients"))

#Two fold decomposition
decomp2$twofold$overall
#Comment -> “The Oaxaca-Blinder decomposition shows that approximately 89% of the observed gender difference in math scores cannot be attributed to observable characteristics such as parental education, income, or prior academic achievement. Only 11% is explained by these factors. 
#.           This suggests that unobserved factors or differential returns to the same characteristics — possibly related to classroom dynamics, teacher expectations, or intrinsic motivation — are driving most of the gender gap in math scores.”

# Decomposing the unexplained part following Neumark
plot(decomp2, 
     decomposition = "twofold", 
     group.weight = -1,  
     unexplained.split = TRUE, 
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "In Favor of Boys", 
                          "unexplained B" = "Against Girls"),
     variables = c("mère_educ_5ans", "père_educ_5ans"), 
     variable.labels = c("mère_educ_5ans" = "Mother's Education", 
                         "père_educ_5ans" = "Father's Education"
                         ))


install.packages(c("VGAM", "marginaleffects"))

install.packages("marginaleffects")

library(VGAM)

oprobit_vgam <- vglm(
  perceived_math_skill_f ~ sexe_dummy * sexe_enseignant * A06X_ENSDISCI3 +
    A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data,
  family = cumulative(link = "probit", parallel = TRUE)
)


library(marginaleffects)

avg_slopes(
  oprobit_3,
  variables = c("sexe_dummy", "sexe_enseignant", "A06X_ENSDISCI3"),
  newdata = regression_data
)



library(MASS)
oprobit_3 <- polr(
  perceived_math_skill_f ~ sexe_dummy * sexe_enseignant * A06X_ENSDISCI3  + A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data,
  method = "probit",
  Hess = TRUE
)
stargazer(oprobit_3, type = "text")

regression_data_fixed <- regression_data
regression_data_fixed$experience_std_1 <- as.numeric(regression_data_fixed$experience_std_1)
str(regression_data_fixed$experience_std_1)


oprobit_3_fixed <- polr(
  perceived_math_skill_f ~ sexe_dummy * sexe_enseignant * A06X_ENSDISCI3 +
    A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data_fixed,
  method = "probit",
  Hess = TRUE
)

avg_slopes(
  oprobit_3_fixed,
  variables = c("sexe_dummy", "sexe_enseignant", "A06X_ENSDISCI3"),
  newdata = regression_data_fixed
)

regression_data_fixed$triple_interaction <- with(
  regression_data_fixed,
  sexe_dummy * sexe_enseignant * A06X_ENSDISCI3
)

oprobit_3_final <- polr(
  perceived_math_skill_f ~ sexe_dummy + sexe_enseignant + A06X_ENSDISCI3 +
    sexe_dummy:sexe_enseignant + sexe_dummy:A06X_ENSDISCI3 +
    sexe_enseignant:A06X_ENSDISCI3 + triple_interaction +
    A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data_fixed,
  method = "probit",
  Hess = TRUE
)

avg_slopes(
  oprobit_3_final,
  variables = "triple_interaction",
  newdata = regression_data_fixed
)

regression_data_fixed$inter_gender_teacher <- with(
  regression_data_fixed,
  sexe_dummy * sexe_enseignant
)

regression_data_fixed$inter_gender_background <- with(
  regression_data_fixed,
  sexe_dummy * A06X_ENSDISCI3
)

oprobit_full <- polr(
  perceived_math_skill_f ~ sexe_dummy + sexe_enseignant + A06X_ENSDISCI3 +
    inter_gender_teacher + inter_gender_background + triple_interaction +
    A06X_SCMOYMATH + A06X_AGEM + A06X_SCmoylect + experience_std_1,
  data = regression_data_fixed,
  method = "probit",
  Hess = TRUE
)

avg_slopes(
  oprobit_full,
  variables = "inter_gender_teacher",
  newdata = regression_data_fixed
)

avg_slopes(
  oprobit_full,
  variables = "inter_gender_background",
  newdata = regression_data_fixed
)







