library(tidyverse)
library(ggh4x)


skills <-
bind_rows(
  tibble(class = "Expertise", skill = "Marine Biogeochemistry", rating = 8, target = 9),
  tibble(class = "Expertise", skill = "Marine CO2 system fundamentals", rating = 9, target = 9),
  tibble(class = "Expertise", skill = "Global scale processes", rating = 7, target = 8),
  tibble(class = "Expertise", skill = "Regional scale processes", rating = 7, target = 8),
  tibble(class = "Data acquisition", skill = "Chem. lab experiments", rating = 6, target = 6),
  tibble(class = "Data acquisition", skill = "Biol. lab experiments", rating = 4, target = 4),
  tibble(class = "Data acquisition", skill = "Field work", rating = 7, target = 7),
  tibble(class = "Data acquisition", skill = "Autonomous measurements", rating = 5, target = 5),
  tibble(class = "Data acquisition", skill = "Numerical modelling", rating = 3, target = 4),
  tibble(class = "Data interpretation", skill = "BGC observations", rating = 8, target = 10),
  tibble(class = "Data interpretation", skill = "BGC models", rating = 7, target = 9),
  tibble(class = "Data interpretation", skill = "Multi-platform observations", rating = 7, target = 9),
  tibble(class = "Data interpretation", skill = "Model-observation fusion", rating = 7, target = 9),
  tibble(class = "Data interpretation", skill = "Advanced statistics", rating = 6, target = 9),
  tibble(class = "General", skill = "Project coordination", rating = 9, target = 10),
  tibble(class = "General", skill = "International network", rating = 8, target = 9),
  tibble(class = "General", skill = "Communication / outreach", rating = 8, target = 9),
  tibble(class = "Academic", skill = "Teaching", rating = 7, target = 9),
  tibble(class = "Academic", skill = "Supervision", rating = 6, target = 9),
) %>% 
  mutate(class = fct_inorder(class),
         skill = fct_inorder(skill))



skills %>%
  ggplot() +
  geom_col(aes(target, skill, col = "target"), fill = "grey90", width = 0.5) +
  geom_col(aes(rating, skill, fill = "current"), width = 0.7) +
  geom_text(aes(0.2, skill, label = skill),
            hjust = 0, col = "white") +
  scale_x_continuous(
    breaks = seq(0, 10, 2),
    limits = c(0, 10.2),
    expand = c(0, 0),
    name = "Self assessment"
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = "#43a2ca") +
  scale_fill_manual(values = "#43a2ca") +
  guides(fill = guide_legend("Skill level", order = 1),
         colour = guide_legend(title = NULL, order = 2)) +
  facet_grid2(class ~ .,
              scales = "free_y",
              space = "free_y",
              switch = "y") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top")

ggsave("skill_self_assessment.png",
       height = 8,
       width = 6)
