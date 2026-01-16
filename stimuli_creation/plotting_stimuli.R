## getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

# stimuli (target words)---------------
df <- read_csv('stimuli_all_words_ratings.csv')

df <- df %>% filter(str_detect(Type, 'Target [123]')) %>% 
  rename(Length = length,
         Frequency = Lg10CD) %>% 
  pivot_longer(
    cols = c(Concreteness, Specificity, Frequency, Length, Valence, Arousal, Dominance),
    names_to = "rating",
    values_to = "score"
  ) %>% 
  print()

df$rating <- factor(df$rating, 
                    levels=c("Concreteness", "Specificity", "Frequency", 
                             "Length", "Valence", "Arousal", "Dominance"))

agr_df <- df %>% 
  group_by(Domain, rating) %>% 
  summarise(
    mean = mean(score),
    sd= sd(score)
  )

plot_stimuli <- ggplot(agr_df, 
       aes(x=rating, y=mean, fill=Domain)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.8),
                width = 0.25) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_brewer(palette = "Set1")

plot_stimuli

# superordinates (target concepts)---------------
df_super <- read_csv('superordinates_ratings.csv')

df_super <- df_super %>% 
  rename(Length = length,
         Frequency = Lg10CD) %>% 
  pivot_longer(
    cols = c(Concreteness, Specificity, Frequency, Length, Valence, Arousal, Dominance),
    names_to = "rating",
    values_to = "score"
  ) %>% 
  print()

df_super$rating <- factor(df_super$rating, 
                    levels=c("Concreteness", "Specificity", "Frequency", 
                             "Length", "Valence", "Arousal", "Dominance"))

agr_df_super <- df_super %>% 
  group_by(Domain, rating) %>% 
  summarise(
    mean = mean(score),
    sd= sd(score)
  )

plot_super <- ggplot(agr_df_super, 
                       aes(x=rating, y=mean, fill=Domain)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.8),
                width = 0.25) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_brewer(palette = "Set1")

plot_super

