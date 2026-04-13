library(dplyr)
library(fingertipsR)
library(ggplot2)
library(stringr)
library(lubridate)

convert_qtr <- function(x) {
  tibble(x) %>%
    mutate(
      year_start = as.integer(str_extract(x, "^\\d{4}")),
      quarter = as.integer(str_extract(x, "(?<=Q)\\d")),
      month = case_when(
        quarter == 1 ~ 4,   # April
        quarter == 2 ~ 7,   # July
        quarter == 3 ~ 10,  # October
        quarter == 4 ~ 1    # January (next calendar year)
      ),
      year = if_else(quarter == 4, year_start + 1, year_start),
      date = make_date(year, month, 1)
    ) %>%
    pull(date)
}
AreaCodes <- c(
  "E92000001", # England
  "E08000025", # Birmingham
  "E08000027", # Dudley
  "E08000028", # Sandwell
  "E08000029", # Solihull
  "E08000030", # Walsall
  "E08000031"  # Wolverhampton
)
# Percentage of NHS Health Checks received by the total eligible population 
# in the quarter
data <- fingertips_data(
  IndicatorID = 91041,
  AreaTypeID = 502,
  AreaCode = AreaCodes
  ) %>%
  mutate(
    Date = convert_qtr(Timeperiod),
    Area = case_when(
      AreaName == "Birmingham" ~ "Birmingham",
      AreaName == "England" ~ "England",
      TRUE ~ "BBCSol ICB Cluster"
    ),
    Area = case_when(
      AreaName == "Birmingham" ~ "Birmingham",
      AreaName == "England" ~ "England",
      TRUE ~ "BBCSol ICB Cluster"
    ),
  ) %>%
  filter(
    Date > as.Date("2022-04-01")
  )

# Birmingham-focused plot

plt <- ggplot(data, aes(x = Date, y = Value/100, color = Area, 
                        group = AreaCode, order = Area)) +
  
  geom_line(data = subset(data, Area == "BBCSol ICB Cluster"), lwd = 0.8) +
  geom_line(data = subset(data, Area != "BBCSol ICB Cluster"), lwd = 1.1) +
  geom_ribbon(data = subset(data, AreaName == "Birmingham") ,
              aes(ymin = LowerCI95.0limit/100, ymax = UpperCI95.0limit/100,
                  fill = Area),
              lwd = 0, alpha = 0.5, show.legend = FALSE) +
  geom_hline(yintercept = 2.5/100, color = "red", linetype='dotted', lwd = 1.1) + 
  geom_label(
    data = data.frame(
      Date = as.Date("2022-10-15"),
      Value = 2.7,
      label = "Quarterly 2.5% target",
      Area = NA,
      AreaCode = NA
    ),
    aes(label = label),
    fill = "white",
    alpha = 0.5,
    label.size = 0,  # removes border,
    hjust = 0,
    col = "red"
  ) +
  theme_bw() + 
  scale_color_manual(
    values = c(
      c("Birmingham" = "#b08ade", 
        "England" = "black", 
        "BBCSol ICB Cluster" = "gray")
    )
  ) +
  scale_fill_manual(
    values = c(
      c("Birmingham" = "#b08ade")
    )
  ) +
  labs(
    x = "",
    y = "",
    title = str_wrap(unique(data$IndicatorName), 100),
    color = "",
    caption = 
      paste(
      "Purple shaded region shows 95% confidence interval for Birmingham.", 
      "Office for Health Improvement and Disparities. Public health profiles. ",
      "2026 https://fingertips.phe.org.uk/ © Crown copyright 2026",
      sep = "\n"
    ),
  ) +
  theme(
    legend.position.inside = c(0.15, 0.9),
    legend.text = element_text(size = 12),
    legend.position = "inside",
    legend.background = element_rect(fill='transparent'),
    plot.caption = element_text(color = "gray40", margin = margin(t = -5))
  ) +
  scale_y_continuous(labels = scales::percent) 

plt

ggsave("output/hc-over-time.png", plt, height = 5, width = 8)

# All shown

plt2 <- ggplot(data, aes(x = Date, y = Value/100, color = AreaName, 
                        group = AreaCode)) +
  
  geom_line(lwd = 1.1) +
  geom_ribbon(data = subset(data, AreaName == "Birmingham") ,
              aes(ymin = LowerCI95.0limit/100, ymax = UpperCI95.0limit/100,
                  fill = Area),
              lwd = 0, alpha = 0.5, show.legend = FALSE) +
  geom_hline(yintercept = 2.5/100, color = "red", linetype='dotted', lwd = 1.1) + 
  geom_label(
    data = data.frame(
      Date = as.Date("2022-10-15"),
      Value = 2.7,
      label = "Quarterly 2.5% target",
      Area = NA,
      AreaCode = NA
    ),
    aes(label = label),
    fill = "white",
    alpha = 0.5,
    label.size = 0,  # removes border,
    hjust = 0,
    col = "red"
  ) +
  theme_bw() + 
  scale_color_manual(
    values = c(
      c("Birmingham" = "#b08ade", 
        "England" = "black", 
        "Dudley" = "#f88509",
        "Sandwell"  = "#031d44",
        "Solihull" = "#58c1f0",
        "Walsall" = "darkgray",
        "Wolverhampton" = "#90e7ae"
        )
    ),
    guide = guide_legend(
      ncol = 2
    )
  ) +
  scale_fill_manual(
    values = c(
      c("Birmingham" = "#b08ade")
    )
  ) +
  labs(
    x = "",
    y = "",
    title = str_wrap(unique(data$IndicatorName), 100),
    color = "",
    caption = 
      paste(
        "Purple shaded region shows 95% confidence interval for Birmingham.", 
        "Office for Health Improvement and Disparities. Public health profiles. ",
        "2026 https://fingertips.phe.org.uk/ © Crown copyright 2026",
        sep = "\n"
      ),
  ) +
  theme(
    legend.position.inside = c(0.15, 0.9),
    legend.text = element_text(size = 10),
    legend.position = "inside",
    legend.background = element_rect(fill='transparent'),
    plot.caption = element_text(color = "gray40", margin = margin(t = -5))
  ) +
  scale_y_continuous(labels = scales::percent) 

plt2

ggsave("output/hc-over-time-all.png", plt2, height = 5, width = 8)