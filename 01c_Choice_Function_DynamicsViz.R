reentry <- expand_grid(x = seq(10000,750000,25000),
                  y = seq(1,200,10)) %>% 
  mutate(p_reenter = 
    (x - 10000) / (740000 - 10000) * (.8 / (1 + exp(-0.1 * (.5*y-10)))))

nvessels_start <- 200
cost_multiplier <- tibble(x = 1:199,
                          cost_multiplier = 3/(nvessels_start^2)*(nvessels_start-x)^2-3/(nvessels_start^2)+1)
  
skill_fx <- tibble(Skill_N = 1:500,
                   Skill_FX = (1.75 * Skill_N + 52) / (Skill_N + 52))

ggplot(reentry, aes(x = y, y = x/1000, z = p_reenter)) +
  geom_contour_filled() +
  ggthemes::theme_tufte() + 
  scale_fill_manual(values = c(
    "#AAFAC8", "#92D9B9", "#7AC7A9", "#62B699", 
    "#4A9E91", "#35757B", "#205466", "#2D728B"
  )) +
  labs(x = "Vessels in the Region",
       y = "Revenue / Vessel ($1k)",
       title = "Re-entry Function",
       subtitle = "Portion of non-fishing vessels that can re-enter",
       fill = "Portion")

ggplot(cost_multiplier, aes(x = x, y = cost_multiplier)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Vessels in the Region",
       y = "Cost Multiplier",
       title = "Cost Multiplier for a Vessel / Fishery",
       subtitle = "Scales Fishing Costs")

ggplot(skill_fx, aes(x = Skill_N, y = Skill_FX)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Weeks Fished",
       y = "Skill Multiplier",
       title = "Skill Multiplier for a Vessel / Fishery",
       subtitle = "Scales Expected Revenue")
