# Compare diabetes, CHD, obesity

library(dplyr)
library(fingertipsR)
library(ggplot2)
library(stringr)
library(lubridate)

AreaCodes <- c(
  "E92000001", # England
  "E08000025", # Birmingham
  "E08000027", # Dudley
  "E08000028", # Sandwell
  "E08000029", # Solihull
  "E08000030", # Walsall
  "E08000031"  # Wolverhampton
)

data <- fingertips_data(
  IndicatorID = c(
    241, # diabetes
    273, # CHD
    94136 # obesity
  ),
  AreaTypeID = 502,
  AreaCode = AreaCodes
) %>%
  filter(
    TimeperiodSortable == max(TimeperiodSortable)
  ) %>%
  mutate(
    IndicatorTitle = paste0(
      IndicatorName, " (", Timeperiod, ")"
    )
  )

plotdata <- data %>%
  filter(
    AreaName != "England"
  )

EngData <- data %>%
  filter(
    AreaName == "England"
  )

plt <- ggplot(plotdata, aes(x = AreaName, y = Value/100, fill = AreaName)) +
  geom_col() +
  geom_errorbar(aes(ymin = LowerCI95.0limit/100, ymax = UpperCI95.0limit/100), 
                width = 0.3) + 
  geom_hline(data = EngData, aes(yintercept = Value/100, color = "England Average"),
             linetype='dotted', lwd = 1.1) +
  theme_bw() +
  facet_wrap(~IndicatorTitle, ncol = 1, scale = "free_y") +
  scale_fill_manual(
    values = c(
      c("Birmingham" = "#b08ade", 
        "Dudley" = "#f88509",
        "Sandwell"  = "#031d44",
        "Solihull" = "#58c1f0",
        "Walsall" = "darkgray",
        "Wolverhampton" = "#90e7ae"
      )
    ),
    guide = NULL
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c("England Average" = "black")
  ) +
  labs(
    y = "",
    color = "",
    caption = 
      paste(
        "Error bars show 95% confidence intervals.", 
        "Office for Health Improvement and Disparities. Public health profiles. ",
        "2026 https://fingertips.phe.org.uk/ © Crown copyright 2026",
        sep = "\n"
      ),
    x = ""
  ) +
  theme(
    legend.position = "top",
    plot.caption = element_text(color = "gray40", margin = margin(t = -5)),
    strip.background = element_rect(fill="white"),
    legend.margin = margin(0, 0, 0, 0)
  ) 

plt

ggsave("output/related-conditions.png", plt, height = 5, width = 6)