# data cleaning script
# prepared by Roland 
# dated: 05-29-2023
# Nursing home quality rating

# prep (libraries) ------

library(tidyverse)
library(ggthemes)
library(viridis)
library(gt)
library(showtext)
library(GGally)

showtext_auto()

# determine the % of states with the highest nursing homes in the US

plt2 <- df4 |>
  group_by(provider_state) |>
  tally() |>
  mutate(
    n_sum = sum(n)
  ) |>
  ungroup() |>
  mutate(
    pct = round((n/n_sum)*100,2)
  ) |>
  filter(pct >= 3.00) |>
  ggplot(aes(x=pct, y = fct_reorder(provider_state, pct),
             fill = (pct == 7.74))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#9DB2BF", "#27374D"))+
  geom_text(aes(label = pct), color = "white", size = 8, hjust = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = c(0.0, 2.5, 5.0, 7.5),
                     labels = c("0.0%", "2.5%", "5.0%", "7.5%"))+
  labs(
    title = "Top 10  states with the highest percentage of nursing homes",
    x = "Percentage",
    y = "States"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = .5
    ),
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  )


# bed utilization 
plt3 <- df4 |>
  group_by(provider_name, new_ownership) |>
  filter(number_of_certified_beds >= 500) |>
  mutate(
    pct_beds = round((number_of_residents_in_certified_beds/number_of_certified_beds)*100,2)
  ) |>
  mutate(
    provider_name = str_to_title(provider_name, locale = "en")
  ) |>
  select(provider_name, pct_beds, new_ownership) |>
  ggplot(aes(x = pct_beds, y = fct_reorder(provider_name, pct_beds), 
             fill = new_ownership)) +
  geom_bar(stat = "identity") +
  facet_wrap(~new_ownership) +
  scale_fill_tableau() +
  geom_text(aes(label = pct_beds), color = "white", size = 4, hjust = 1.2) +
  theme_classic() +
  labs(
    title = "Nursing homes with >=500 beds capacity and their percentage utilization",
    subtitle = "Distribution of hospital beds utilization by hospital ownership type",
    caption = "Kaggle: US Nursing home data",
    x = "Percentage",
    y = "Nursing homes",
    fill = "Ownership type"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = .5
    ),
    plot.subtitle = element_text(
      face = "italic",
      size = 10,
      hjust = .5
    ),
    legend.position = "bottom",
    axis.text.y = element_text(size = 10)
  )

#Service type availability for residents and families
plt4 <- df4 |>
  group_by(provider_type, with_a_resident_and_family_council) |>
  tally() |>
  mutate(
    n_sum = sum(n))|>
  ungroup() |>
  mutate(
    pct = round((n/n_sum)*100,1)
  ) |>
  ggplot(aes(x = provider_type, y = pct,
             fill = fct_reorder(with_a_resident_and_family_council,pct))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = pct), size = 5, color = "white", vjust = 1.5,
            position = position_dodge(width = .9, preserve = "total")) +
  scale_fill_tableau() +
  theme_bw() +
  labs(
    title = "Service type and availability for residents and families",
    y = "Percentage (%)",
    x = "Provider type",
    fill = NULL,
    caption = "Kaggle: US Nursing home data"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = .5
    ),
    legend.position = "bottom",
    panel.grid = element_blank()
  ) 


# Ratings for nursing homes with more than 700 beds space.
plt5 <- df4 |>
  group_by(provider_name, overall_rating, provider_state, number_of_certified_beds) |>
  summarise(
    n = sum(number_of_certified_beds)
  ) |>
  filter(n >= 700) |>
  ggplot() +
  geom_hline(aes(
    yintercept = overall_rating),
    color = "lightgrey") +
  geom_col(
    aes(
      x = reorder(str_wrap(provider_name, 6), overall_rating),
      y = overall_rating,
      fill = overall_rating
    ),
    position = "dodge",
    show.legend = TRUE,
    alpha = .9
  ) +
  geom_segment(
    aes(
      x = reorder(str_wrap(provider_name, 6), overall_rating),
      y = 0,
      xend = reorder(str_wrap(provider_name, 6), overall_rating),
      yend = 5
    ),
    linetype = "dashed",
    color = "gray12"
  ) +
  coord_polar() +
  scale_fill_gradientn(
    "Overall rating",
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195","#FFE569")
  ) +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 10, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme_bw() +
  theme(
    # Remove axis ticks and text
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = .5
    ),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 8),
    # Move the legend to the bottom
    legend.position = "bottom",
  ) 

plt6 <- plt5 + facet_wrap(~provider_state) +
  labs(
    title = "Overall rating of nursing homes with over 700 bed space",
    caption = "Kaggle: US Nursing home data"
  )

#summaries 
tbl1 <- df4 |>
  select(provider_name, staffing_rating,
         number_of_residents_in_certified_beds, number_of_facility_reported_incidents,
         number_of_fines) |>
  rename(
    "Provider name" = provider_name,
    "Staff rating" = staffing_rating,
    "Residents certified beds" = number_of_residents_in_certified_beds,
    "Reported incidents" = number_of_facility_reported_incidents,
    "Fines" = number_of_fines
  ) |>
  filter(`Reported incidents` >= 1 & `Fines` >= 1
         & `Residents certified beds` >= 200) |>
  arrange(desc(`Fines`)) |>
  slice(1:10) |>
  gt()

# average hours
tbl2 <- df4 |>
  select(provider_state, 
         reported_cna_staffing_hours_per_resident_per_day:reported_physical_therapist_staffing_hours_per_resident_per_day)|>
  group_by(provider_state) |>
  summarise(
    "CNA staff" = round(mean(reported_cna_staffing_hours_per_resident_per_day, na.rm = T),1),
    "LPN staff" = round(mean(reported_lpn_staffing_hours_per_resident_per_day, na.rm = T),1),
    "RN staff" = round(mean(reported_rn_staffing_hours_per_resident_per_day, na.rm = T),1),
    "Physical therapist" = round(mean(reported_physical_therapist_staffing_hours_per_resident_per_day, na.rm = T),1),
    ) |>
  arrange(desc(`CNA staff`)) |>
  head(10) |>
  gt()


plt7 <- df4 |>
  filter(number_of_certified_beds >= 700) |>
  ggplot() +
  geom_segment(aes(
    x = str_to_title(provider_name), xend = str_to_title(provider_name), 
    y = cycle_1_total_health_score,
    yend = cycle_2_total_health_score
  ), lwd = 2, color = "grey") +
  geom_point(aes(x = str_to_title(provider_name), y = cycle_1_total_health_score), size = 8,
             color = "#C38154") +
  geom_point(aes(x = str_to_title(provider_name), y = cycle_2_total_health_score), size = 8,
             color = "#FFC26F") +
  geom_text(aes(x = str_to_title(provider_name), y = cycle_1_total_health_score,
                label = cycle_1_total_health_score), color = "white", size = 6) +
  geom_text(aes(x = str_to_title(provider_name), y = cycle_2_total_health_score,
                label = cycle_2_total_health_score), color = "white", size = 6) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Total health score between cycle 1 and cycle 2",
    y = "Total health score",
    x = "Provider name",
    caption = "Kaggle: US Nursing home data" 
  )  +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = .5
    ),
    axis.text.y = element_text(size = 20))

df6 <- df4 |>
  select(cycle_1_health_revisit_score, cycle_2_health_revisit_score,
         cycle_3_health_revisit_score, provider_type, provider_state, provider_name) |>
  group_by(provider_type, provider_state, provider_name) |>
  summarise(
    "cycle 1" = mean(cycle_1_health_revisit_score, na.rm = T),
    "cycle 2" = mean(cycle_2_health_revisit_score, na.rm = T),
    "cycle 3" = mean(cycle_3_health_revisit_score, na.rm = T)
  ) |>
  filter(`cycle 1` >=1 & `cycle 2` >=1 & `cycle 3` >= 1)


plt8 <- ggparcoord(df6,
           columns = c(4,5,6), groupColumn = 1,
           showPoints = TRUE,
           title = "Health revisit score across states",
           alphaLines = 1
           ) +
  scale_color_colorblind() +
  theme_bw() +
  theme(
    plot.title = element_text(size=10)
  ) +
  facet_wrap(vars(df6$provider_name, df6$provider_state)) +
  ylab("") +
  xlab("Health revisit cycle")+
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = .5
    ),
    legend.position = "top",
    legend.title = element_blank())
  
