# Load libraries

library(ggplot2)
library(readxl)
library(here)
library(dplyr)
library(scales)
library(ggtext)


# Load data

load_data <- function(file_path, sheet_name) {
  df <- read_xlsx(file_path, sheet = sheet_name) %>%
    mutate(year = as.factor(year))
  return(df)
}


# Plots

# Box office

uk_box_office <- function() {
  ggplot(df, aes(x = year, y = uk_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +  
    geom_text(aes(label = scales::comma(uk_box_office_m)), vjust = 1.5, color = 'white') + 
    labs(
      title = 'UK box office, £ million', 
      subtitle = 'All titles on release, including event titles', 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal()
}

uk_roi_box_office <- function() {
  ggplot(df, aes(x = year, y = uk_roi_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#783df6') +  
    geom_text(aes(label = scales::comma(uk_roi_box_office_m)), vjust = 1.5, color = 'white') + 
    labs(
      title = 'UK and Republic of Ireland box office, £ million', 
      subtitle = "<span style='color:#783df6'>All films</span> released in calendar year, excluding event titles",
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}

uk_market_share <- function() {
  df$year <- as.numeric(as.character(df$year))

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years)

  ggplot(df_filtered, aes(x = factor(year), y = box_office_m)) +
    # First layer: all films
    geom_bar(aes(fill = ifelse(film_type == "all_titles", "all_titles", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Second layer: all UK qualifying films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "all_uk_qualifying", "all_uk_qualifying", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Text labels for relevant bars 
    geom_text(data = df_filtered %>% filter(film_type == "all_uk_qualifying"), 
              aes(label = scales::comma(round(box_office_m, 0)), vjust = 1.5),
              color = 'white') +
    labs(
      title = 'UK and Republic of Ireland box office, £ million',
      subtitle = "For <span style='color:#1197FF'>**all UK qualifying films**</span> 
                  released in calendar year, excluding event titles",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'all_uk_qualifying' = '#1197FF'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}

uk_market_share_indie <- function() {
  df$year <- as.numeric(as.character(df$year))

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years)

  ggplot(df_filtered, aes(x = factor(year), y = box_office_m)) +
    # First layer: all films
    geom_bar(aes(fill = ifelse(film_type == "all_titles", "all_titles", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Second layer: all UK qualifying films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "all_uk_qualifying", "all_uk_qualifying", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Third layer: uk independent films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "uk_independent", "uk_independent", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Text labels for relevant bars
    geom_text(data = df_filtered %>% filter(film_type == "uk_independent"), 
              aes(label = scales::comma(round(box_office_m, 0)), vjust = -0.5),
              color = '#e50076') +
    labs(
      title = 'UK and Republic of Ireland box office, £ million',
      subtitle = "For <span style='color:#e50076'>**all UK independent films**</span> 
                  released in calendar year, excluding event titles",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'all_uk_qualifying' = 'lightgrey',
                                 'uk_independent' = '#e50076'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}

uk_market_share_percent <- function() {
  df$year <- as.numeric(as.character(df$year))

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years)

  ggplot(df_filtered, aes(x = factor(year), y = market_share_percent)) +
    # First layer: all films
    geom_bar(aes(fill = ifelse(film_type == "all_titles", "all_titles", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Second layer: all UK qualifying films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "other_uk_qualifying", "other_uk_qualifying", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Third layer: uk independent films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "uk_independent", "uk_independent", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Text labels for relevant bars
    geom_text(data = df_filtered %>% filter(film_type %in% c("other_uk_qualifying", "uk_independent")), 
              aes(label = scales::comma(round(market_share_percent, 0)), vjust = 1.5),
              color = 'white') +
    labs(
      title = 'Share of UK and Republic of Ireland box office, %',
      subtitle = "For <span style='color:#e50076'>**all UK independent films**</span> 
                  and <span style='color:#1197FF'>**other UK qualifying films**</span>",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'other_uk_qualifying' = '#1197FF',
                                 'uk_independent' = '#e50076'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}

# Admissions

uk_admissions <- function() {
  df <- df %>%
    group_by(year) %>%
    summarise(admissions_m = sum(admissions_m, na.rm = TRUE))

  ggplot(df, aes(x = year, y = admissions_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +  
    geom_text(aes(label = scales::comma(round(admissions_m, 0))), vjust = 1.5, color = 'white') + 
    labs(
      title = 'UK admissions, million',
      subtitle = 'All titles on release, including event titles',
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal()
}

uk_admissions_month <- function() {
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      month = factor(month, levels = month.name),
      latest_year = max(year, na.rm = TRUE),
      color = ifelse(year == latest_year, "#e50076", "#D3D3D3")
    )

  ggplot(df, aes(x = month, y = admissions_m, group = year, color = factor(year))) +
    geom_line(aes(color = color), size = 1) +
    geom_point(aes(color = color), size = 2) +
    geom_text(
      data = df %>% filter(month == "December"),
      aes(label = year, x = month, y = admissions_m, color = color),
      hjust = 0, vjust = 0.5, size = 5, fontface = "bold",
      position = position_nudge(x = 0.1)
    ) +
    labs(
      title = 'UK admissions, million',
      subtitle = 'All titles on release, including event titles',
      x = 'Month',
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    scale_x_discrete(expand = expansion(mult = 0.1)) +  
    scale_color_identity() + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}
