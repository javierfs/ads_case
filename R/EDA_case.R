library(tidyverse)
library(here)
library(dplyr)
library(patchwork)

data <- read_csv(here('data','cleaned_merged_data_v1.csv'))


str(data)

summary(data$PUBLISH_DTT)

selected_data <- data %>%
                  select(AD_ID, Ad_minutes_alive, Ad_hours_alive)

summary_data <- data %>%
                group_by(CATEGORY_SECTION) %>%
                summarise(nmr_users = n_distinct(USER_ID)) %>%
                mutate(share_users = (nmr_users/sum(nmr_users))*100)
                
summary_data %>%
  ggplot(aes(y = share_users, x = reorder(CATEGORY_SECTION, -share_users))) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(
    aes(label = paste('n =', nmr_users), y = share_users + 0.35),
    position = position_dodge(0.9),
    vjust = 0,
    size=3
  ) +
  labs(x='',
    y = "% of Share of users by category section",
       title = "How big is the share of users who inserted an ad in each category section? ",
       subtitle = "Home&Garden is the section where more distinct users post")+
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(labels = function(share_users) paste0(share_users, '%'))

  
  
                
                
                