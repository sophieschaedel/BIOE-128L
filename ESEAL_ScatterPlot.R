library(ggplot2)
library(tidyr)
library(readr)

# Load the data
df <- read_csv("/Users/sophieschaedel/Desktop/ESEALAMWEIGHTS - Sheet1.csv")

# Reshape data to long format
df_long <- df %>%
  pivot_longer(cols = c(`Estimated Mass Dorsal`, `Estimated Mass Lateral`),
               names_to = "Estimate_Type",
               values_to = "Estimated_Mass") 

# Plot
ggplot(df_long, aes(x = Estimated_Mass, y = ProcedureWeight, color = Estimate_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Estimated Mass (Dorsal & Lateral) vs. Procedure Weight",
    subtitle = "Dashed line = perfect match; Lines = linear regressions",
    x = "Estimated Mass (kg)",
    y = "Procedure Weight (kg)",
    color = "Estimate Type"
  ) +
  theme_minimal()



