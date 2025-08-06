# fst_descriptive_and_latency_two_way.R

# 0) Install required packages if needed:
# install.packages(c("readxl","dplyr","tidyr","ggplot2","emmeans"))

# 1) Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(broman)

burntorange <- brocolors("crayons")["Burnt Orange"]
sunglow <- brocolors("crayons")["Sunglow"]
lemonyellow <- brocolors("crayons")["Lemon Yellow"]

# 2) Read data
df <- read_excel("Time_spent_immobile_FST_Data_Erin_2.xlsx", sheet = 1)

# 3) Tidy & factor
df <- df %>%
  rename(
    AnimalID      = `Animal ID`,
    CORT_OIL      = `CORT/OIL`,
    FLX_DXT       = `FLX/DXT`,
    KIN_SAL       = `KIN/SAL`,
    Time_Immobile = `Time Immobile`,
    Time_Swim     = `Time Swim`,
    Time_Climb    = `Time Climb`,
    Time_mobile   = `Time mobile`,
    Latency       = `Latency to immobility`,
    Treatment     = Treatment
  ) %>%
  mutate(
    Treatment = factor(Treatment, levels=c("DXT-SAL","FLX-SAL","FLX-KIN")),
    CORT_OIL  = factor(CORT_OIL,  levels=c("OIL","CORT"))
  ) 

# reshape longer
df_long <- df %>%
  pivot_longer(
    cols = c(Time_Immobile, Time_Swim, Time_Climb, Time_mobile, Latency),
    names_to  = "Measure",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))

# common palette
pal_treat <- c("DXT-SAL"="#fff44f","FLX-SAL"="#ffcf48","FLX-KIN"="#ff7f49")
pal_cort  <- c("OIL"="#E69F00","CORT"="#56B4E9")

# 4) Plot 1: boxplot with individual data points and sample size labels

df_long$Measure <- recode(df_long$Measure,
                          "Time_Immobile" = "Time Immobile (s)",
                          "Time_Swim"     = "Time Swim (s)",
                          "Time_Climb"    = "Time Climb (s)",
                          "Time_mobile"   = "Time Mobile (s)",
                          "Latency"       = "Latency (s)"
)

# Compute counts and dynamic Y position for each Measure × Treatment
df_counts <- df_long %>%
  group_by(Measure, Treatment) %>%
  summarise(
    n     = n(),
    y_pos = max(Value, na.rm = TRUE) * 1.05,  # place label 5% above max
    .groups = "drop"
  )

# Create the plot
p1 <- ggplot(df_long, aes(x=Treatment, y=Value, fill=Treatment)) +
  geom_boxplot(outlier.shape=NA, width=0.5, color="black", alpha=0.7) +
  geom_jitter(width=0.2, shape=21, size=2, stroke=0.3, aes(fill=Treatment), alpha=0.8) +
  geom_text(
    data = df_counts,
    aes(x = Treatment, y = y_pos, label = paste0("n=", n)),
    vjust = 0,
    size = 3.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Measure, scales = "fixed", nrow = 1) +
  scale_fill_manual(values = pal_treat) +
  labs(
    title = "Behavioral Measures by Treatment",
    x     = "Treatment",
    y     = "Time (s)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "none"
  )

# Save and print the plot
ggsave("descriptive_stats_boxplot_counts_cleaned.png", p1, units = "in", width = 14, height = 5, dpi = 300)
print(p1)

# 5) Two-way ANOVA on Time_Immobile
aov_lat <- aov(Time_Immobile ~ Treatment * CORT_OIL, data=df)
tab      <- summary(aov_lat)[[1]]
p_cort   <- round(tab["CORT_OIL","Pr(>F)"], 5)

# count per Treatment x CORT_OIL
df_counts <- df %>%
  group_by(CORT_OIL, Treatment) %>%
  summarise(n=n(), maxVal=max(Time_Immobile, na.rm=TRUE), .groups="drop")

# Plot 2: Time_Immobile boxplots by Treatment & CORT/OIL with N labels and ANOVA p
p2 <- ggplot(df, aes(x=Treatment, y=Time_Immobile, fill=Treatment)) +
  geom_boxplot(outlier.shape=NA, width=0.6, color="black") +
  geom_jitter(aes(color=Treatment),
              position=position_jitter(width=0.15),
              size=2, alpha=0.7) +
  facet_wrap(~ CORT_OIL) +
  geom_text(
    data=df_counts,
    aes(x=Treatment, y=maxVal + 0.05*maxVal, label=paste0("n=",n)),
    inherit.aes=FALSE, size=3
  ) +
  scale_fill_manual(values=pal_treat) +
  scale_color_manual(values=pal_treat) +
  labs(
    title    = "Time Immobility by Treatment and CORT/OIL",
    subtitle = paste0("Main effect CORT/OIL: p = ", p_cort),
    x        = "Treatment",
    y        = "Time Immobility (s)"
  ) +
  theme_classic(base_size=14) +
  theme(
    strip.background = element_rect(fill="grey90", color=NA),
    strip.text       = element_text(face="bold"),
    axis.text.x      = element_text(angle=45, hjust=1),
    legend.position  = "none"
  )
  ggsave("main_effect_cort_oil.png", units="in", width=10, height=8, dpi=300)

print(p2)

