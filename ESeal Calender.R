# Load libraries
library(ggplot2)
library(dplyr)
library(tibble)

# Define the data
seal_presence <- tribble(
  ~Class,            ~Month, ~Activity,
  
  # Weanlings
  "Weanlings",       "Jan",  "X",
  "Weanlings",       "May",  "X",
  "Weanlings",       "Jun",  "X",
  "Weanlings",       "Jul",  "X",
  "Weanlings",       "Aug",  "X",
  "Weanlings",       "Sep",  "X",
  "Weanlings",       "Oct",  "X",
  "Weanlings",       "Nov",  "X",
  "Weanlings",       "Dec",  "X",
  
  # Yearlings
  "Yearlings",       "Apr",  "M",
  "Yearlings",       "May",  "M",
  "Yearlings",       "Jun",  "M",
  "Yearlings",       "Jul",  "X",
  "Yearlings",       "Aug",  "X",
  "Yearlings",       "Sep",  "X",
  
  # Juveniles (Molt: Apr–Jun)
  "Juveniles",       "Apr",  "M",
  "Juveniles",       "May",  "M",
  "Juveniles",       "Jun",  "M",
  "Juveniles",       "Sep",  "X",
  "Juveniles",       "Oct",  "X",
  "Juveniles",       "Nov",  "X",
  
  # Sub-adult males (Breed: Dec–Mar)
  "Sub-adult males", "Dec",  "B",
  "Sub-adult males", "Jan",  "B",
  "Sub-adult males", "Feb",  "B",
  "Sub-adult males", "Mar",  "B",
  
  # Adult males (Molt: Jun–Aug)
  "Adult males",     "Dec",  "B",
  "Adult males",     "Jan",  "B",
  "Adult males",     "Feb",  "B",
  "Adult males",     "Mar",  "B",
  "Adult males",     "Jun",  "M",
  "Adult males",     "Jul",  "M",
  "Adult males",     "Aug",  "M",
  
  # Adult females (Molt: Apr–Jun)
  "Adult females",   "Dec",  "B",
  "Adult females",   "Jan",  "B",
  "Adult females",   "Feb",  "B",
  "Adult females",   "Mar",  "B",
  "Adult females",   "Apr",  "M",
  "Adult females",   "May",  "M",
  "Adult females",   "Jun",  "M"
)

# Ensure months are in order
seal_presence$Month <- factor(seal_presence$Month, levels = month.abb)

# Define activity colors
activity_colors <- c("B" = "#F4A6A6", "M" = "#88C0D0", "X" = "#A3D9A5")

# Plot
ggplot(seal_presence, aes(x = Month, y = Class, fill = Activity)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = activity_colors,
                    labels = c("B" = "Breeding", "M" = "Molting", "X" = "General presence")) +
  labs(title = "Elephant Seal Shore Presence by Class",
       subtitle = "Observed monthly presence by life stage",
       fill = "Activity",
       caption = "B = Breeding; M = Molting; X = General presence") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )