library(tidyverse)

# ---------------------------------------- Cleaning switching data -----------------------------------------#

WL_switching_df <- read_csv("WL_switching_df.csv")

WL_switching_df <- (WL_switching_df 
                    %>% mutate(across(where(is.character), as.factor)) # converting characters to factors
) 

WL_switching_df <- (WL_switching_df
                    %>% mutate(treatment = fct_recode(treatment,
                                                      "winner" = "win",
                                                      "loser" = "loss"))
)

WL_switching_df <- (WL_switching_df 
                    %>% mutate(participant.ID = as.factor(participant.ID)) # transforming participant.ID to factor (for mood model)
) 

WL_switching_df <- (WL_switching_df
                    %>% mutate(switch.pref = factor(switch.pref, ordered = TRUE))
)

if(is.ordered(WL_switching_df$switch.pref)) { ## function that checks whether "switch.pref" is an ordered factor
  print("Ordered")
} else {
  print("Not ordered")}

# removing 6 mood columns, since they're not important to the switching model 

WL_switching_df <- (WL_switching_df 
                    %>% select(-c("neg.baseline", "neg.post",
                                  "pos.baseline", "pos.post", 
                                  "pos.change", "neg.change"))
)

WL_switching_df <- (WL_switching_df
                    %>% mutate(treatment = fct_recode(treatment,
                                                      "winner" = "win",
                                                      "loser" = "loss"))
)

summary(WL_switching_df)

write_rds(WL_switching_df, "WL_switching_df.rds")

# ---------------------------------------- Cleaning waiting data -----------------------------------------#

WL_waiting_df <- read_csv("WL_waiting_df.csv")

WL_waiting_df <- (WL_waiting_df 
                  %>% mutate(across(where(is.character), as.factor))
)

WL_waiting_df <- (WL_waiting_df
                  %>% mutate(treatment = fct_recode(treatment,
                                                    "winner" = "win",
                                                    "loser" = "loss"))
)

WL_waiting_df <- (WL_waiting_df 
                  %>% mutate(participant.ID = as.factor(participant.ID),
                             exp.level = as.factor(exp.level))
)

# only have 8 responses who played more than 5 hours per week. 
# should we just create two levels for this? -- "experienced" and "inexperienced"? 


WL_waiting_df <- (WL_waiting_df
                  %>% mutate(exp.level = fct_collapse(exp.level, "2" = c("2","3")))
)

WL_waiting_df <- (WL_waiting_df
                  %>% mutate(exp.level = fct_recode(exp.level,
                                                    "inexperienced" = "1",
                                                    "experienced" = "2"))
)

#  make sure wait.pref is ordered

WL_waiting_df <- (WL_waiting_df
                  %>% mutate(wait.pref = factor(wait.pref, ordered = TRUE))
)

if(is.ordered(WL_waiting_df$wait.pref)) { ## function that checks whether "wait.pref" is an ordered factor
  print("Ordered")
} else {
  print("Not ordered")}

WL_waiting_df <- (WL_waiting_df 
                  %>% select(-c("neg.baseline", "neg.post",
                                "pos.baseline", "pos.post",
                                "pos.change", "neg.change"))
)

summary(WL_waiting_df) # check that everything looks good, which it does :^)

WL_waiting_df <- (WL_waiting_df
                  %>% mutate(treatment = fct_recode(treatment,
                                                    "winner" = "win",
                                                    "loser" = "loss"))
)

write_rds(WL_waiting_df, "WL_waiting_df.rds")

# ---------------------------------------- Cleaning mood data ----------------------------------------- #

WL_mood_df <- read_csv("WL_mood_df.csv")

WL_mood_df <- (WL_mood_df 
               %>% select(-"pos.change")
               %>% select(-"neg.change")
)

WL_mood_df <- (WL_mood_df %>%
                 rename(positive_affect_baseline = pos.baseline,
                        negative_affect_baseline = neg.baseline, 
                        positive_affect_postgame = pos.post,
                        negative_affect_postgame = neg.post
                 )
)

WL_mood_df <- (WL_mood_df 
               %>% mutate(across(where(is.character), as.factor))
               %>% mutate(participant.ID = as.factor(participant.ID),
                          pref = as.factor(pref),
                          exp.level = as.factor(exp.level))
)

WL_mood_df <- (WL_mood_df
               %>% mutate(pref = factor(pref, ordered = TRUE))
)

WL_mood_df <- (WL_mood_df
               %>% mutate(exp.level = fct_collapse(exp.level, "2" = c("2","3")))
)

WL_mood_df <- (WL_mood_df
               %>% mutate(exp.level = fct_recode(exp.level,
                                                 "inexperienced" = "1",
                                                 "experienced" = "2"))
)

WL_mood_df <- (WL_mood_df
               %>% mutate(treatment = fct_recode(treatment,
                                                 "winner" = "win",
                                                 "loser" = "loss"))
)



WL_mood_long_df <- (WL_mood_df %>% 
                      pivot_longer(
                        cols = -c(participant.ID, gender, experiment, exp.level, age, score, pref, treatment),
                        names_to = c("emotion", "timepoint"),
                        names_pattern = "(.*)_affect_(.*)")
)

WL_mood_long_df <- (WL_mood_long_df %>% 
                      rename(affect.score = value,
                             affect.type = emotion) %>%
                      mutate(across(where(is.character), as.factor)) 
)

summary(WL_mood_long_df)

write_rds(WL_mood_long_df, "WL_mood_long_df.rds")
