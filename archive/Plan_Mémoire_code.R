

# 1. Box Plot: "Mean scores per gender" ==========================
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
        var %in% c("A04X_SCmoylect", "A06X_SCmoylect") ~ "Français"
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
  scale_fill_manual(values = c("Garçon" = "#A6CEE3", "Fille" = "#B2DF8A")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

ggsave("boxplot_scores_sexe_age.pdf", width = 8, height = 6, plot =
         
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
                 var %in% c("A04X_SCmoylect", "A06X_SCmoylect") ~ "Français"
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
           x = "Sexe de l'enfant",
           y = "Score standardisé",
           fill = "Sexe"
         ) +
         scale_fill_manual(values = c("Garçon" = "#A6CEE3", "Fille" = "#B2DF8A")) +
         theme_minimal() +
         theme(
           strip.text = element_text(size = 12),
           plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
         )
)



# 1.1 Decomposing Math Test =============================================
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

summary(regression_data$A06X_SCCOMPA)

# 2. Bar Plot: "Math and Reading Gap at 4 and 6"  =========================================================

ggplot(gender_gap_long, aes(x = time, y = gender_gap, fill = subject)) +
  geom_col(
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.7,
    color = "gray95"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(
    values = c(
      "Mathématiques" = "#A6D854",  # pastel green
      "Français" = "#BC80BD"         # pastel purple
    )
  ) +
  labs(
    x = "Âge de passation",
    y = "Différence moyenne de score",
    fill = "Matière"
  ) +
  coord_cartesian(ylim = c(-0.07, 0.15)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_text(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("gender_gap_plot.pdf", width = 6.5, height = 4.5, units = "in", dpi = 300)


# 3. Density Plot: "Math and Reading Scores Distribution" =======

common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10), 
    legend.direction = "vertical",  # Make the legend items vertical
    legend.key.size = unit(0.6, "cm"),  # Adjust the size of legend items
    # smaller title
  )

# Math at age 4
p1 <- ggplot(regression_data, aes(x = A04X_SCmoymath, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0", "#55A868"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Mathématiques à 4 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  scale_x_continuous(
    breaks = seq(-2, 1, 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  common_theme +
  theme(axis.title.x = element_blank())


# Math at age 6
p2 <- ggplot(regression_data, aes(x = A06X_SCMOYMATH, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0", "#55A868"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Mathématiques à 6 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  scale_x_continuous(
    breaks = seq(-2, 1, 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  common_theme +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )



# Reading at age 4
p3 <- ggplot(regression_data, aes(x = A04X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0", "#55A868"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Français à 4 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  common_theme


# Reading at age 6 (with legend)
p4 <- ggplot(regression_data, aes(x = A06X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("#4C72B0", "#55A868"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Français à 6 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  common_theme +
  theme(axis.title.y = element_blank())


# Combine all plots
(p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")

# Combine all plots
combined_plot <- (p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")

# Save the plot to a PDF file
ggsave("combined_plot.pdf", plot = combined_plot, width = 12, height = 8, units = "in", dpi = 300)






common_theme_1 <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10), 
    legend.direction = "vertical",  # Make the legend items vertical
    legend.key.size = unit(0.6, "cm"),  # Adjust the size of legend items
    # smaller title
  )


# Math at age 6
ggplot(regression_data, aes(x = A06X_SCMOYMATH_rescaled, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("lightblue", "#FDB462"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Mathématiques à 6 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  scale_x_continuous(
    breaks = seq(-2, 1, 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  common_theme +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )



# Reading at age 4
p3 <- ggplot(regression_data, aes(x = A04X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("lightblue", "#FDB462"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Mathématiques à 4 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  common_theme


# Reading at age 6 (with legend)
p4 <- ggplot(regression_data, aes(x = A06X_SCmoylect, fill = as.factor(sexe_dummy))) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  scale_fill_manual(values = c("lightblue", "#FDB462"), labels = c("Garçon", "Fille")) +
  labs(
    title = "Français à 6 ans",
    x = "Score standardisé",
    y = "Densité",
    fill = "Sexe"
  ) +
  common_theme +
  theme(axis.title.y = element_blank())


# Combine all plots
(p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")

# 4. Girls Proportion across Math Score Percentiles ================

ggplot(gender_curve, aes(x = Percentile, y = ProportionGirls, color = Time)) +
  geom_line(linewidth = 0.5, alpha = 0.9) +
  geom_point(size = 1.5, shape = 21, fill = "white", stroke = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    x = "Percentile in Mathematics",
    y = "Proportion of Girls (%)",
    color = "Timepoint"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(t = 20)),  # <- added space here
    plot.title.position = "plot"
  )

# En PDF — recommandé
ggsave("proportion_girls_percentiles.pdf", width = 9, height = 7, plot =
         ggplot(gender_curve, aes(x = Percentile, y = ProportionGirls, color = Time)) +
         geom_line(linewidth = 0.5, alpha = 0.9) +
         geom_point(size = 1.5, shape = 21, fill = "white", stroke = 1) +
         scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
         scale_x_continuous(breaks = seq(0, 100, by = 10)) +
         labs(
           x = "Percentile in Mathematics",
           y = "Proportion of Girls (%)",
           color = "Timepoint"
         ) +
         theme_minimal(base_size = 13) +
         theme(
           legend.position = "bottom",
           plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
           plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0.5),
           axis.title = element_text(face = "bold", size = 10),
           axis.text = element_text(color = "black"),
           axis.title.x = element_text(margin = margin(t = 20)),  # space above x-axis title
           plot.title.position = "plot"
         )
)


## Table

stargazer(gender_curve_selected,
          type = "text",
          summary = FALSE,
          rownames = FALSE,
          title = "Proportion de filles selon le rang de performance en mathématiques",
          label = "tab:gender_distribution",
          digits = 1)



#5. Average Change in Math Percentile ================================================

ggplot(percentile_mobility, aes(x = percentile_4y_bucket, y = avg_percentile_change, color = sexe_enfant)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c(
      "Girl" = "#55A868",  # Hot Pink
      "Boy" = "#4C72B0"  # Nice strong blue
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
    legend.position = "right"
  )

ggsave("percentile_change_math.pdf", width = 10, height = 5, plot = 
         ggplot(percentile_mobility, aes(x = percentile_4y_bucket, y = avg_percentile_change, color = sexe_enfant)) +
         geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.4) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         scale_color_manual(
           values = c(
             "Girl" = "#55A868",
             "Boy" = "#4C72B0"
           )
         ) +
         labs(
           x = "Initial Percentile Rank at Age 4",
           y = "Average Percentile Change",
           color = "Sexe"
         ) +
         theme_minimal(base_size = 13) +
         theme(
           legend.position = "right",
           legend.text = element_text(size = 9),
           legend.title = element_text(size = 9),
           legend.key.size = unit(0.4, "cm"),
           axis.title.x = element_text(size = 10, margin = margin(t = 10)),
           axis.title.y = element_text(size = 10, margin = margin(r = 10))
         )
)



#6. Decile Mobility =======================================

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



ggsave("mobility_by_decile.pdf", width = 8, height = 6, plot = 
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
         scale_y_continuous(expand = c(0, 0)) +
         labs(
           x = "Décile initial en mathématiques (âge 4)",
           y = "Pourcentage des enfants",
           fill = "Type de mobilité"
         ) +
         theme_minimal(base_size = 13) +
         theme(
           legend.position = "bottom",
           legend.title = element_text(face = "bold", size = 10),
           axis.title.x = element_text(size = 10, margin = margin(t = 10)),
           axis.title.y = element_text(size = 10, margin = margin(r = 10)),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           strip.text = element_text(face = "bold")  # Make facet labels bold
         )
)



#7. Table: Stickiness Table ============================================

library(stargazer)

# Recrée le tableau si ce n’est pas déjà fait
stickiness_table <- mobility_data %>%
  group_by(sexe_enfant) %>%
  summarise(
    `Stickiness in Bottom Deciles (1–3)` = round(mean(sticky_low, na.rm = TRUE) * 100, 1),
    `Stickiness in Top Deciles (8–10)` = round(mean(sticky_high, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(Genre = ifelse(sexe_enfant == "Girl", "Filles", "Garçons")) %>%
  dplyr::select(Genre, everything(), -sexe_enfant)

# Tableau avec stargazer
stargazer(stickiness_table, 
          type = "latex",
          summary = FALSE,
          rownames = FALSE,
          title = "Stickiness des performances en mathématiques selon le genre",
          label = "tab:stickiness_gender",
          digits = 1)




# Regression Tables ========================================================

## Table 1: ===============

stargazer(regression_naive, regression_parent_educ, regression_house_income,
          type = "latex", 
          covariate.labels = c(
            "Fille",
            "Âge",
            "École Privée",
            "Educ Mère: Primaraire / Brevet",
            "Educ Mère: Bac",
            "Educ Mère: Bac + 2",
            "Educ Mère: Licence ou Master",
            "Educ Mère: Grandes écoles",
            "Educ Père: Primaraire / Brevet",
            "Educ Père: Bac",
            "Educ Père: Bac + 2",
            "Educ Père: Licence ou Master",
            "Educ Père: Grandes écoles",
            "Mère: Chômage",
            "Mère: Autre",
            "Mère: Étudiante",
            "Père: Chômage",
            "Père: Autre",
            "Père: Étudiante",
            "Famille monoparentale",
            "Garde alternée",
            "Père non français",
            "Père français: 2 parents immigrés",
            "Père français: 1 parent immigré",
            "Mère non française",
            "Mère française: 2 parents immigrés",
            "Mère française: 1 parent immigré",
            "Revenu du ménage"
          )
)
          dep.var.labels = "Score en maths (6ans)",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))


stargazer(regression_past_lecture_score,
          type = "latex", 
          covariate.labels = c(
            "Fille",
            "Âge",
            "École Privée",
            "Educ Mère: Primaire / Brevet",
            "Educ Mère: Bac",
            "Educ Mère: Bac + 2",
            "Educ Mère: Licence ou Master",
            "Educ Mère: Grandes écoles",
            "Educ Père: Primaraire / Brevet",
            "Educ Père: Bac",
            "Educ Père: Bac + 2",
            "Educ Père: Licence ou Master",
            "Educ Père: Grandes écoles",
            "Mère: Chômage",
            "Mère: Autre",
            "Mère: Étudiante",
            "Père: Chômage",
            "Père: Autre",
            "Père: Étudiante",
            "Famille monoparentale",
            "Garde alternée",
            "Père non français",
            "Père français: 2 parents immigrés",
            "Père français: 1 parent immigré",
            "Mère non française",
            "Mère française: 2 parents immigrés",
            "Mère française: 1 parent immigré",
            "Revenu du ménage",
            "Score en maths (4 ans)",
            "Score en français (4 ans)"
          )
)
          dep.var.labels = "Score en maths (6 ans)",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))




## Interaction terms

stargazer(
  regression_interac_migration,
  regression_interac_migration_1,
  type = "latex",
  covariate.labels = c(
    "Fille",
    "Père non français",
    "Père français: 2 parents immigrés",
    "Père français: 1 parent immigré",
    "Mère non française",
    "Mère française: 2 parents immigrés",
    "Mère française: 1 parent immigré",
    "Educ Mère: Primaire / Brevet",
    "Educ Mère: Bac",
    "Educ Mère: Bac + 2",
    "Educ Mère: Licence ou Master",
    "Educ Mère: Grandes écoles",
    "Educ Père: Primaire / Brevet",
    "Educ Père: Bac",
    "Educ Père: Bac + 2",
    "Educ Père: Licence ou Master",
    "Educ Père: Grandes écoles",
    "Famille Monoparentale",
    "Garde Alternée",
    "Âge",
    "Revenu du ménage",
    "Score en mathématiques (4 ans)",
    "Score en français (4 ans)",
    "Famille monoparentale",
    "Garde alternée",
    "Mère: Autre",
    "Mère: Étudiante",
    "Père: Chômage",
    "Père: Autre",
    "Père: Étudiante",
    "Famille monoparentale",
    "Garde alternée",
    "Père non français",
    "Père français: 2 parents immigrés",
    "Père français: 1 parent immigré",
    "Mère non française",
    "Mère française: 2 parents immigrés",
    "Mère française: 1 parent immigré",
    "Revenu du ménage",
    "Score en maths (4 ans)",
    "Score en français (4 ans)"
  )
)
  title = "Tableau 7 – Interactions entre genre et caractéristiques familiales",
  float.env = "table",
  float = TRUE
)

stargazer(
  regression_interac_migration,
  regression_interac_migration_1,
  type = "text",
  title = "Tableau 7 bis – Interactions entre genre et caractéristiques familiales",
  keep = c("sexe_dummy:"),
  covariate.labels = c(
    "Fille x Père non français",
    "Fille x Père français: 2 parents immigrés",
    "Fille x Père français: 1 parent immigré",
    "Fille x Mère non française",
    "Fille x Mère française: 2 parents immigrés",
    "Fille x Mère française: 1 parent immigré",
    "Fille x Educ Mère: Primaire / Brevet",
    "Fille x Educ Mère: Bac",
    "Fille x Educ Mère: Bac + 2",
    "Fille x Educ Mère: Licence ou Master",
    "Fille x Educ Mère: Grandes écoles",
    "Fille x Educ Père: Primaire / Brevet",
    "Fille x Educ Père: Bac",
    "Fille x Educ Père: Bac + 2",
    "Fille x Educ Père: Licence ou Master",
    "Fille x Educ Père: Grandes écoles",
    "Fille x Famille Monoparentale",
    "Fille x Garde Alternée",
    "Âge",
    "Revenu du ménage",
    "Score en mathématiques (4 ans)",
    "Score en français (4 ans)",
    "Famille monoparentale",
    "Garde alternée",
    "Mère: Autre",
    "Mère: Étudiante",
    "Père: Chômage",
    "Père: Autre",
    "Père: Étudiante",
    "Famille monoparentale",
    "Garde alternée",
    "Père non français",
    "Père français: 2 parents immigrés",
    "Père français: 1 parent immigré",
    "Mère non française",
    "Mère française: 2 parents immigrés",
    "Mère française: 1 parent immigré",
    "Revenu du ménage",
    "Score en maths (4 ans)",
    "Score en français (4 ans)"
  )
)
  label = "tab:interactions_genre",
  float.env = "table",
  float = TRUE
)








## Table 2 =============

# Assuming the models lm_math_adv_1, lm_math_adv_2, lm_math_adv_3, and lm_math_adv_4 are already created.

stargazer(lm_math_adv_1, lm_math_adv_2, lm_math_adv_3, lm_math_adv_4,
          type = "latex", 
          omit = c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect", 
                   "mère_educ_5ans", "père_educ_5ans"),
          covariate.labels = c("Fille", "Avantage comparatif en maths", 
                               "Math Initial Quartile: Q1", "Math Initial Quartile: Q3", 
                               "Math Initial Quartile: Q4", "Progrès en français", "Fille x Avantage", "Fille x Q1 ", "Fille x Q3", "Fille x Q4", "Avantage x Q1", "Avantage x Q3", "Avantage x Q4", "Fille x Progrès en français"),
          add.lines = list(c("Contrôles individuels", "Non", "Non", "Non", "Oui")),
          dep.var.labels = "Progrès en mathématiques",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))


stargazer(lm_math_adv_1, lm_math_adv_2, lm_math_adv_3, lm_math_adv_4,
          type = "text", 
          omit = c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect", 
                   "mère_educ_5ans", "père_educ_5ans"),
          add.lines = list(c("Contrôles individuels", "Oui", "Oui", "Oui", "Oui")),
          dep.var.labels = "Progrès en mathématiques",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))



## Reading advantage 

stargazer(lm_reading_gap, lm_read_adv_1, lm_read_adv_2,
          type = "text",
          covariate.labels = c("Fille", "Avantage comparatif en français", 
                               "Quartile en français à 4 ans: Q1", "Quartile en français à 4 ans: Q3", 
                               "Quartile en français à 4 ans: Q4", "Fille x Avantage en français", "Fille x Q1", "Fille x Q2 ", "Fille x Q4"),
          dep.var.labels = "Progrès en français",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))

stargazer(regression_school__01,
          type = "text",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))




stargazer(regression_school_2,
          type = "text",  # ou "latex" si tu veux un tableau pour un article
          omit = c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect", 
                   "mère_educ_5ans", "père_educ_5ans"),
          covariate.labels = c(
            "Fille", 
            "École privée", 
            "Taille de la classe", 
            "École REP+",
            "Classe double niveau : GS-CP",
            "Classe double niveau : CP-CE1",
            "Fille × Privé",
            "Fille x taille de la classe",
            "Fille x REP +",
            "Fille x Double Niveau GS - CP",
            "Fille x Double Niveau CP - CE1"
          ),
          add.lines = list(c("Contrôles individuels", "Oui")),
          dep.var.labels = "Progrès en mathématiques",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))

library(stargazer)

# Assuming the models lm_math_gap, lm_math_adv_1, lm_math_adv_2, lm_math_adv_3, and lm_math_adv_4 are already created.

stargazer(lm_math_gap, lm_math_adv_2, lm_math_adv_3, lm_math_adv_4, 
          type = "text", 
          covariate.labels = c("Girl", "Math Advantage at Age 4", 
                               "Math Initial Quartile Q1", "Math Initial Quartile Q3", 
                               "Math Initial Quartile Q4", "French Progress", 
                               "Girl x Math Advantage", 
                               "Girl x Math Initial Quartile Q1", 
                               "Girl x Math Initial Quartile Q3", 
                               "Girl x Math Initial Quartile Q4", 
                               "Girl x French Progress"), 
          omit = c("A06X_AGEM", "père_educ_5ans", "mère_educ_5ans", "A04X_SCmoylect"),  # Omit specific control variables
          add.lines = list(
            c("Individual Controls", "No", "No", "No", "Yes", "Yes")
          )
)
          
stargazer(
  lm_math_adv_1, lm_math_adv_2, lm_math_gap, lm_math_adv_4,
  type = "text", 
  covariate.labels = c("Constant", "Girl", "Math Advantage at Age 4"))


stargazer(
  lm_math_gap, lm_math_adv_1, lm_math_adv_2, lm_math_adv_3,
  type = "text", 
  covariate.labels = c("Constant", "Girl", "Math Advantage at Age 4")
)

## Teacher Bias Regression Table ========================

stargazer(
  oprobit_2,
  type = "text", 
  dep.var.labels = "Perception niveau en français (6 ans) ",
  keep.stat = c("n", "rsq", "adj.rsq", "f"))

)


# Sortie code LateX
stargazer(oprobit_2, oprobit_4ans_maths, oprobit_6ans_french, oprobit_4ans_french,
          type = "text",
          notes = "*p<0.1; **p<0.05; ***p<0.01", 
          omit.table.layout = "n")  # "n" removes the column for the coefficients and standard errors



# Teacher Bias with interaction terms

stargazer(oprobit_3, oprobit_12, oprobit_13,
          type = "latex",  # or "latex"
          dep.var.labels = "Perception de l'enseignant",
          covariate.labels = c(
            "Fille", 
            "Enseignant Homme", 
            "Enseignant à formation scientifique", 
            "Score en maths",
            "Âge de l'enfant",
            "Score en français",
            "Expérience de l'enseignant",
            "Fille × Enseignant Homme",
            "Fille × Formation scientifique",
            "Enseignant Homme × Formation scientifique",
            "Fille × Enseignant Homme x Formation scientifique"
          ),
          column.labels = c("Tous", "Garçons", "Filles"),
          keep.stat = c("n", "rsq", "adj.rsq", "f"))



## French bias 4 and 6 years old

stargazer(oprobit_6ans_french, oprobit_4ans_french,
          type = "latex",
          dep.var.labels = "Perception niveau en français (6 ans) ",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))




## Table: School caracteristics ===========

library(stargazer)

stargazer(regression_school_01,regression_school__01,
          type = "text",  # ou "latex" si tu veux un tableau pour un article
          omit = c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect", 
                   "mère_educ_5ans", "père_educ_5ans"),
          covariate.labels = c(
            "Fille", 
            "École privée", 
            "Taille de la classe", 
            "Classe double niveau : GS-CP",
            "Classe double niveau : CP-CE1",
            "Fille × Privé",
            "Fille x taille de la classe",
            "Fille x Double Niveau GS - CP",
            "Fille x Double Niveau CP - CE1"
          ),
          add.lines = list(c("Contrôles individuels", "Oui")),
          dep.var.labels = "Score en maths (6 ans)",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))


stargazer(regression_teacher_2,regression_teacher_21,
          type = "text",  # ou "latex" si tu veux un tableau pour un article
          omit = c("A06X_AGEM", "A04X_SCmoymath", "A04X_SCmoylect", 
                   "mère_educ_5ans", "père_educ_5ans"),
          covariate.labels = c(
            "Fille", 
            "Enseignant Homme", 
            "Expérience de l'enseignant", 
            "Formation scientifique",
            "Fille × Enseignant Homme",
            "Fille x Expérience de l'enseignant",
            "Fille x Formation scientifique"
          ),
          add.lines = list(c("Contrôles individuels", "Oui", "Oui")),
          dep.var.labels = "Score en mathématiques (6ans)",
          keep.stat = c("n", "rsq", "adj.rsq", "f"))





## Pro social skills 

stargazer(
  pro_social_generalmaths, 
  pro_social_numbcomp, 
  pro_social_mentalcalcul,
  type = "text",
  column.labels = c("Général", "Comparaison de nombres", "Calcul mental dicté"),
  omit = c("A06X_AGEM", "A04X_SCmoymath", "mère_educ_5ans", "père_educ_5ans"),
  covariate.labels = c(
    "Fille", 
    "Score Pro Social",
    "Score Trouble Émotionnels",
    "Score Hyperactivité",
    "Fille x Pro Social",
    "Fille × Trouble Émotionnel",
    "Fille × Hyperactivité"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f")
)
library(stargazer)
stargazer(
  pro_social_mentalpract, 
  pro_social_probresolu, 
  pro_social_suitnum,
  type = "text",
  column.labels = c("Calcul mental pratique", "Résolution de problèmes", "Suite numérique"),
  omit = c("A06X_AGEM", "A04X_SCmoymath", "mère_educ_5ans", "père_educ_5ans"),
  covariate.labels = c(
    "Fille", 
    "Score Pro Social",
    "Score Trouble Émotionnels",
    "Score Hyperactivité",
    "Fille x Pro Social",
    "Fille × Trouble Émotionnel",
    "Fille × Hyperactivité"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f")
)


stargazer(
  hihi, 
  hihi_1, 
  hihi_2,
  type = "latex",
  covariate.labels = c(
    "Fille", 
    "Score Pro Social",
    "Score Trouble Émotionnels",
    "Score Hyperactivité",
    "Fille x Pro Social",
    "Fille × Trouble Émotionnel",
    "Fille × Hyperactivité"
  ),
  column.labels = c("Général", "Comparaison de nombres", "Calcul mental dicté"),
  omit = c("A06X_AGEM", "mère_educ_5ans", "père_educ_5ans"),
  keep.stat = c("n", "rsq", "adj.rsq", "f")
)

stargazer(
  hihi_3, 
  hihi_4, 
  hihi_5,
  type = "latex",
  column.labels = c("Calcul mental pratique", "Résolution de problèmes", "Suite numérique"),
  omit = c("A06X_AGEM", "mère_educ_5ans", "père_educ_5ans"),
  covariate.labels = c(
    "Fille", 
    "Score Pro Social",
    "Score Trouble Émotionnels",
    "Score Hyperactivité",
    "Fille x Pro Social",
    "Fille × Trouble Émotionnel",
    "Fille × Hyperactivité"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f")
)

covariate.labels = c(
  "Fille", 
  "Score Pro Social",
  "Score Trouble Émotionnels",
  "Score Hyperactivité",
  "Fille x Pro Social",
  "Fille × Trouble Émotionnel",
  "Fille × Hyperactivité"
),

stargazer(
  hihi_maternelle, 
  type = "latex",
  omit = c("A04X_AGE4A", "mère_educ_3ans", "père_educ_3ans"),
  covariate.labels = c(
    "Fille", 
    "Score Pro Social",
    "Score Trouble Émotionnels",
    "Score Hyperactivité",
    "Fille x Pro Social",
    "Fille × Trouble Émotionnel",
    "Fille × Hyperactivité"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f")
)


## Toy use and math score ===================================================================


stargazer(
  lm_toys_raw_2, lm_toys_raw_2_boys,lm_toys_raw_2_girls,
  type = "latex", 
  omit = c("A06X_AGEM", 
           "mère_educ_5ans", "père_educ_5ans", "revenu_part_dec_5y"),
  covariate.labels = c(
    "Fille", 
    "Jeu de construction",
    "Jeu de l'oie",
    "Petites voitures",
    "Livre",
    "Jeux vidéos",
    "Déguisement princesse",
    "Déguisement pirate",
    "Dinette",
    "Petits personnages",
    "Poupée"
  ),
  add.lines = list(c("Contrôles individuels", "Oui", "Oui", "Oui")),
  column.labels = c("Tous", "Garçons", "Filles"),
  
  dep.var.labels = "Score en mathématiques (6ans) ",
  keep.stat = c("n", "rsq", "adj.rsq", "f"))

## Toy use parents

stargazer(
  prudencio, prudencio_filles, prudencio_boys, prudencio_boys_score,
  type = "latex", 
  omit = c("A06X_AGEM", 
           "mère_educ_5ans", "A04X_SCmoylect", "A04X_SCmoymath", "père_educ_5ans", "revenu_part_dec_5y", "mere_emploi", "pere_emploi", "migration_père", "migration_mère", "A06X_PUBLPRIVc_num"),
  add.lines = list(c("Contrôles individuels", "Oui", "Oui", "Oui")),
  covariate.labels = c(
    "Fille", 
    "Jeu de construction",
    "Dinette",
    "Peluche",
    "Instrument de musique",
    "Ballon",
    "Jeu de société",
    "Poupée",
    "Jeu éducatif",
    "Déguisements"
  ),
  column.labels = c("Tous", "Filles", "Garçons", "Garçons + Score"),
  dep.var.labels = "Score en mathématiques (6ans) ",
  keep.stat = c("n", "rsq", "adj.rsq", "f"))


stargazer(
  prudencio_filles_score,
  type = "latex", 
  omit = c("A06X_AGEM", 
           "mère_educ_5ans", "père_educ_5ans", "revenu_part_dec_5y", "mere_emploi", "pere_emploi", "migration_père", "migration_mère", "A06X_PUBLPRIVc_num", "A04X_SCmoylect", "A04X_SCmoymath"),
  covariate.labels = c(
    "Jeu de construction",
    "Dinette",
    "Peluche",
    "Instrument de musique",
    "Balle",
    "Jeu de société",
    "Poupée",
    "Jeu éducatif",
    "Déguisements"
    ),  
  add.lines = list(c("Contrôles individuels", "Oui", "Oui", "Oui")),
  column.labels = c("Filles + Score"),
  dep.var.labels = "Score en mathématiques (6ans) ",
  keep.stat = c("n", "rsq", "adj.rsq", "f"))




#8. Bar Plot: Teacher's skill rating ========

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




#10. Bar Plot: Child's Toy Preferences according to parents =================================

ggplot(prop_table, aes(x = reorder(jeu, proportion_yes), y = proportion_yes, fill = sexe_enfant)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ sexe_parent, labeller = label_value) +
  coord_flip() +
  labs(
    x = "Type de jouet",
    y = "Proportion (Oui)",
    fill = "Sexe de l'enfant"
  ) +
  scale_x_discrete(labels = c(
    JPOUP   = "Poupée",
    JPELUCH = "Peluche",
    JVOIT   = "Voitures",
    JBAL    = "Ballon",
    JCONS   = "Jeux de construction",
    JMUZ    = "Instrument musical",
    JSOCART = "Jeux de société",
    JEDUC   = "Jeu éducatif",
    JDEGUIS = "Costume",
    JDINET  = "Dinette",
    JFIGUR  = "Figurines d'action"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Garçon" = "#80B1D3", "Fille" = "#FDB462")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 12, margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")  # ← ceci augmente l'espacement entre panels
  )

ggsave("jouets_enfants.pdf", width = 9, height = 6,, plot = 
         ggplot(prop_table, aes(x = reorder(jeu, proportion_yes), y = proportion_yes, fill = sexe_enfant)) +
         geom_col(position = "dodge", width = 0.7) +
         facet_wrap(~ sexe_parent, labeller = label_value) +
         coord_flip() +
         labs(
           x = "Type de jouet",
           y = "Proportion (Oui)",
           fill = "Sexe de l'enfant"
         ) +
         scale_x_discrete(labels = c(
           JPOUP   = "Poupée",
           JPELUCH = "Peluche",
           JVOIT   = "Voitures",
           JBAL    = "Ballon",
           JCONS   = "Jeux de construction",
           JMUZ    = "Instrument musical",
           JSOCART = "Jeux de société",
           JEDUC   = "Jeu éducatif",
           JDEGUIS = "Costume",
           JDINET  = "Dinette",
           JFIGUR  = "Figurines d'action"
         )) +
         scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
         scale_fill_manual(values = c("Garçon" = "#A6CEE3", "Fille" = "#B2DF8A")) +
         theme_minimal(base_size = 14) +
         theme(
           axis.text.y = element_text(size = 12),
           axis.text.x = element_text(size = 11),
           axis.title.x = element_text(size = 12, margin = margin(t = 10)),
           axis.title.y = element_text(size = 12, margin = margin(r = 10)),
           legend.title = element_text(size = 13),
           legend.text = element_text(size = 12),
           strip.text = element_text(size = 11, face = "plain"),  # this now works
           panel.grid.major.y = element_blank(),
           panel.grid.minor = element_blank(),
           panel.spacing = unit(2, "lines")
         ))
scale_fill_manual(values = c("Garçon" = "#A6CEE3", "Fille" = "#B2DF8A")) +
  #11. Bar Plot: Child's Toy Preferences according to himself ========================================

ggplot(toy_chosen_props, aes(x = reorder(Toy, -prop), y = prop, fill = SEXE_ENF)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c(
    "A06E_JOUETS1" = "Livre",
    "A06E_JOUETS2" = "Poupée",
    "A06E_JOUETS3" = "Voitures",
    "A06E_JOUETS4" = "Jeux de construction",
    "A06E_JOUETS5" = "Jeux vidéo",
    "A06E_JOUETS6" = "Costume de princesse",
    "A06E_JOUETS7" = "Costume de pirate",
    "A06E_JOUETS8" = "Dinette",
    "A06E_JOUETS9" = "FIgurines d'action",
    "A06E_JOUETS10" = "Jeux de société"
  )) +
  scale_fill_manual(
    values = c("Boy" = "#80B1D3", "Girl" = "#FDB462"),
    labels = c("Boy" = "Boys", "Girl" = "Girls")
  ) +
  labs(
    x = "Jouet",
    y = "Pourcentage",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),  # overall spacing around plot
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),  # space above x-axis title
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),  # space to the right of y-axis title (if not flipped)
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12)
  )


ggsave("jouet_prefere_6ans.pdf", width = 8, height = 5, plot = 
         ggplot(toy_chosen_props, aes(x = reorder(Toy, -prop), y = prop, fill = SEXE_ENF)) +
         geom_col(position = "dodge") +
         scale_x_discrete(labels = c(
           "A06E_JOUETS1" = "Livre",
           "A06E_JOUETS2" = "Poupée",
           "A06E_JOUETS3" = "Voitures",
           "A06E_JOUETS4" = "Jeux de construction",
           "A06E_JOUETS5" = "Jeux vidéo",
           "A06E_JOUETS6" = "Costume de princesse",
           "A06E_JOUETS7" = "Costume de pirate",
           "A06E_JOUETS8" = "Dinette",
           "A06E_JOUETS9" = "Figurines d'action",
           "A06E_JOUETS10" = "Jeux de société"
         )) +
         scale_fill_manual(
           values = c("Boy" = "#A6CEE3", "Girl" = "#B2DF8A"),
           labels = c("Boy" = "Boys", "Girl" = "Girls")
         ) +
         labs(
           x = "Jouet",
           y = "Proportion (par sexe)",
           fill = "Genre"
         ) +
         theme_minimal(base_size = 13) +
         theme(
           plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(b = 10)),
           plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 12)),
           axis.text.x = element_text(angle = 45, hjust = 1),
           axis.title.x = element_text(size = 12, margin = margin(t = 10)),
           axis.title.y = element_text(size = 12, margin = margin(r = 10)),
           legend.title = element_text(size = 12),
           legend.position = "bottom"
         )
)





