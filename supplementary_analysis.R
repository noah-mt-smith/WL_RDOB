library(tidyverse)
library(ggplot2);theme_set(theme_linedraw())
library(DHARMa)

# ----------------------- importing and cleaning mood data ----------------------- #

# load data 

WL_mood_df <- read_csv("WL_mood_df.csv")

# removing unimportant columns 

WL_mood_df <- (WL_mood_df 
               %>% select(-"pref.spell",
                          -"motivation",
                          -"exp.level")
)

# changing relevant variables to factors 

WL_mood_df <- (WL_mood_df 
               %>% mutate(across(where(is.character), as.factor))
               %>% mutate(participant.ID = as.factor(participant.ID))
)

# renaming mood columns for pivoting

WL_mood_df <- (WL_mood_df 
               %>% rename(positive_affect_baseline = pos.baseline,
                          negative_affect_baseline = neg.baseline, 
                          positive_affect_postgame = pos.post,
                          negative_affect_postgame = neg.post
               )
)

# pivoting data to long format

WL_mood_long_df <- (WL_mood_df %>% 
                      pivot_longer(
                        cols = -c(participant.ID, gender, experiment, pref,
                                  age, score, treatment),
                        names_to = c("emotion", "timepoint"),
                        names_pattern = "(.*)_affect_(.*)")
)
# renaming factors for clarity

WL_mood_long_df <- (WL_mood_long_df  
                    %>% rename(affect.score = value,
                               affect.type = emotion) 
                    %>% mutate(across(where(is.character), as.factor)) 
)

WL_mood_long_df$treatment <- factor(WL_mood_long_df$treatment, 
                                    levels = rev(levels(WL_mood_long_df$treatment)))

WL_mood_long_df$affect.type <- factor(WL_mood_long_df$affect.type, 
                                      levels = rev(levels(WL_mood_long_df$affect.type)))

summary(WL_mood_long_df)

# Converting values of positive and neg affect at baseline and postgame to percent change
# from baseline.

WL_mood_percent_df <- (WL_mood_df
                       %>% mutate(positive_percent_change = 
                                    ((positive_affect_postgame - positive_affect_baseline)/
                                       positive_affect_baseline)*100) 
                       %>% mutate(negative_percent_change = 
                                    ((negative_affect_postgame - negative_affect_baseline)
                                     /negative_affect_baseline)*100)
)

WL_mood_percent_df1 <- (WL_mood_percent_df
                       %>% select(-positive_affect_baseline, 
                                  -positive_affect_postgame, 
                                  -negative_affect_baseline, 
                                  -negative_affect_postgame)
                       %>% pivot_longer(cols = ends_with("change"),
                                        values_to = ("affect.change"),
                                        names_to = ("affect.type"))
                       %>% mutate(across(where(is.character), as.factor))
                       %>% mutate(affect.type = fct_recode(affect.type,
                                                           "negative" = "negative_percent_change",
                                                           "positive" = "positive_percent_change"))
                       %>% mutate(affect.type = fct_rev(affect.type))
                       %>% mutate(treatment = fct_rev(treatment))
                       %>% rename(affect.percent.change = affect.change)
)

summary(WL_mood_percent_df1)

# ------------Clean data so can combine switching and waiting into one model----------------- #

# Goal is to make switching DF binomial so can combine with waiting DF

# need to first get rid of the "3" responses in the switching data, since these were 
# "indifferent" to switching or not switching. These responses are incompatible
# with a binomial model so just removing them.

filtered_mood_percent_df1 <- WL_mood_percent_df1 %>%
  group_by(participant.ID) %>%
  filter(!any(pref == 3)) %>%
  ungroup()

summary(filtered_mood_percent_df1)

# now need to change any responses that appear in the switching 
# experiment that are 1 (strong preference to not switch) and 2 
# (moderate preference to not switch) in the ordinal data to 0 (don't switch)
# in the binomial data.

# Also need to change 4 (moderate preference to switch) and 5 (strong preference
# to switch) in the ordinal data to 1 (switch) for the binomial data. 

# below command ensures it's only the participants who took the switching experiment
# (not the waiting experiment) that have their numbers changed.

filtered_mood_percent_df1 <- filtered_mood_percent_df1 %>%
  mutate(pref = case_when(
    experiment == "switching" & pref == 1 ~ 0,
    experiment == "switching" & pref == 2 ~ 0,
    experiment == "switching" & pref == 4 ~ 1,
    experiment == "switching" & pref == 5 ~ 1,
    TRUE ~ pref
  ))

summary(filtered_mood_percent_df1)

# great, data are clean. Now to make binomial GLM for these data.

# ----------BINOMIAL GLM FOR CHANGE IN AFFECT ON SWITCHING/WAITING--------------- #

# make model

pref_mood_binomial_glm <- glm(pref ~ affect.percent.change:affect.type + age,
                             family = "binomial", data = filtered_mood_percent_df1)

# check fit

testResiduals(pref_mood_binomial_glm)
testQuantiles(pref_mood_binomial_glm)

# fit looks good.

# now summary() command for coefficients

summary(pref_mood_binomial_glm)

# interactions sig for pos affect and change in affect (p < 0.005) and 
# neg affect and change in affect (p = 0.038)

# Can construct ggplot fig to visualize data and binomial GLM

pref_mood_plot <- ggplot(data = filtered_mood_percent_df1, 
                         aes(x = affect.percent.change, y = pref, color = affect.type)) +
                  geom_point() + (scale_color_manual(values = c("positive" = "gold",
                                                                "negative" = "blue"))) + 
                  geom_smooth(method = "glm", method.args = list(family = binomial), level = 0.95) +
                  labs(x = "Change in affect from baseline (%)", y = "Probability of choosing to switch or wait", 
                     color = "Affect type") +
                  theme(axis.title.x = element_text(size = 20, 
                                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.title.y = element_text(size = 20),
                        axis.text.x = element_text(size = 14),   
                        axis.text.y = element_text(size = 14),
                        legend.text = element_text(size = 14),
                        legend.title = element_text(size = 18)) +
                  annotate("text", x = 145, y = 0.79, label = "(z = 2.1, p = 0.04)",
                           size = 5, colour = "black") +
                  annotate("text", x = 137, y = 0.15, label = "(z = -3.5, p < 0.005)",
                           size = 5, colour = "black")

print(pref_mood_plot)

# ggsave(filename = "/Users/noahsmith/WL_RDOB/supp_figures/pref_mood_plot.png",
#        plot = pref_mood_plot, 
#        width = 12,
#        height = 8, 
#        dpi = 500)