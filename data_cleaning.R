library(tidyverse)

# ---------------------------------------- Cleaning switching data -----------------------------------------#

WL_switching_df <- read_csv("WL_switching_df.csv")

WL_switching_df <- (WL_switching_df 
                    %>% mutate(across(where(is.character), as.factor)) # converting characters to factors
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
                  %>% mutate(participant.ID = as.factor(participant.ID),
                             exp.level = as.factor(exp.level))
)

WL_waiting_df <- (WL_waiting_df
                  %>% mutate(motivation = factor(motivation, ordered = TRUE))
)

if(is.ordered(WL_waiting_df$motivation)) { ## function that checks whether "motivation" is an ordered factor
  print("Ordered")
} else {
  print("Not ordered")}

# removing mood columns since not important to this dataframe.

WL_waiting_df <- (WL_waiting_df 
                  %>% select(-c("neg.baseline", "neg.post",
                                "pos.baseline", "pos.post"))
)

summary(WL_waiting_df) # check that everything looks good, which it does :^)

write_rds(WL_waiting_df, "WL_waiting_df.rds")

# ---------------------------------------- Cleaning mood data ----------------------------------------- #

WL_mood_df <- read_csv("WL_mood_df.csv")

# removing unimportant columns 

WL_mood_df <- (WL_mood_df 
               %>% select(-"pref",
                          -"pref.spell",
                          -"motivation")
)

# changing characters to factors 

WL_mood_df <- (WL_mood_df 
               %>% mutate(across(where(is.character), as.factor))
               %>% mutate(participant.ID = as.factor(participant.ID),
                          exp.level = as.factor(exp.level))
)

# renaming mood columns for pivoting later. 

WL_mood_df <- (WL_mood_df 
               %>% rename(positive_affect_baseline = pos.baseline,
                          negative_affect_baseline = neg.baseline, 
                          positive_affect_postgame = pos.post,
                          negative_affect_postgame = neg.post
               )
)

# pivoting mood dataframe into long format

WL_mood_long_df <- (WL_mood_df %>% 
                      pivot_longer(
                        cols = -c(participant.ID, gender, experiment, exp.level, age, score, treatment),
                        names_to = c("emotion", "timepoint"),
                        names_pattern = "(.*)_affect_(.*)")
)

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

write_rds(WL_mood_long_df, "WL_mood_long_df.rds")

# Making percent change mood dataframe

WL_mood_percent_df <- (WL_mood_df
                       %>% mutate(positive_percent_change = ((positive_affect_postgame - positive_affect_baseline)/positive_affect_baseline)*100) 
                       %>% mutate(negative_percent_change = ((negative_affect_postgame - negative_affect_baseline)/negative_affect_baseline)*100)
)

WL_mood_percent_df <- (WL_mood_percent_df
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

summary(WL_mood_percent_df)

write_rds(WL_mood_percent_df, "WL_mood_percent_df.rds")