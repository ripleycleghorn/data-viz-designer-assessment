
R version 4.5.2 (2025-10-31) -- "[Not] Part in a Rumble"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin20

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.82 (8556) x86_64-apple-darwin20]

[Workspace restored from /Users/ripleycleghorn/.RData]
[History restored from /Users/ripleycleghorn/.Rapp.history]

> # Load libraries
> library(tidyverse)
── Attaching core tidyverse packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.6
✔ forcats   1.0.1     ✔ stringr   1.6.0
✔ ggplot2   4.0.1     ✔ tibble    3.3.0
✔ lubridate 1.9.4     ✔ tidyr     1.3.1
✔ purrr     1.2.0     
── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
> library(scales)

Attaching package: ‘scales’

The following object is masked from ‘package:purrr’:

    discard

The following object is masked from ‘package:readr’:

    col_factor

> 
> # Define colors and font
> sector_colors <- c(
+   "Local services"   = "#D0CECE",
+   "Tradable goods"   = "#65ACA5",
+   "Tradable services"= "#BA578C"
+ )
> 
> # Load data for figure 7
> df <- read.csv("https://raw.githubusercontent.com/ripleycleghorn/data-viz-designer-assessment/refs/heads/main/data/Assessment%20Data%20-%20Figure%207.csv",
+                header = TRUE, stringsAsFactors = FALSE)
> 
> # Prepare 2022 data
> df_2022 <- df %>%
+   filter(year == 2022) %>%
+   filter(!is.na(share_sector)) %>%
+   mutate(
+     sector = factor(sector,
+                     levels = c("Tradable services", "Tradable goods", "Local services")),
+     is_rural = factor(is_rural, levels = c("Nonrural", "Rural" ))  # Nonrural on left
+   )
> 
> # Create stacked bar chart
> bar_chart <- ggplot(df_2022, aes(x = is_rural, y = share_sector, fill = sector)) +
+   geom_bar(stat = "identity", width = 0.6) +
+   geom_text(
+     aes(label = scales::percent(share_sector, accuracy = 1)),
+     position = position_stack(vjust = 0.5),
+     size = 4,
+     family = "Lato",
+     color = "white"
+   ) +
+   scale_fill_manual(values = sector_colors) +
+   scale_y_continuous(labels = NULL) +  
+   labs(
+     x = NULL,
+     y = NULL,  
+     fill = "Sector",
+     title = "Employment Share by Sector in 2022"
+   ) +
+   theme_minimal(base_family = "Lato") +
+   theme(
+     plot.title = element_text(size = 12, face = "bold"),
+     legend.position = "none",
+     axis.line = element_blank(),
+     axis.ticks = element_blank(),
+     axis.text.y = element_blank(),      
+     panel.grid.major = element_blank(),
+     panel.grid.minor = element_blank()
+   )
> 
> 
> # Load libraries
> library(patchwork)
> 
> # Define colors and font
> sector_colors <- c(
+   "Local services"   = "#D0CECE",
+   "Tradable goods"   = "#65ACA5",
+   "Tradable services"= "#BA578C"
+ )
> 
> df$sector <- factor(df$sector,
+                     levels = c("Local services", "Tradable goods", "Tradable services"))
> 
> df_rural <- df %>% filter(is_rural == "Rural")
> df_nonrural <- df %>% filter(is_rural == "Nonrural")
> 
> # Create slope chart
> make_slope_chart_cori <- function(data, subtitle) {
+   
+   data$year <- as.numeric(as.character(data$year))
+   
+   ggplot(data, aes(
+     x = factor(year),
+     y = share_sector,
+     group = sector,
+     color = sector
+   )) +
+     geom_line(linewidth = 1.5) +
+     geom_point(size = 3) +
+     
+     # Colors
+     scale_color_manual(values = sector_colors) +
+     
+     # Y-axis: 0–50%
+     scale_y_continuous(
+       limits = c(0, 0.50),
+       labels = scales::percent_format(accuracy = 1)
+     ) +
+     
+     # Only show 2001 and 2022 on x-axis
+     scale_x_discrete(limits = c("2001", "2022")) +
+     
+     labs(
+       title = NULL,
+       subtitle = subtitle,
+       x = NULL,
+       y = "Share of employment",
+       color = "Sector",
+     ) +
+     
+     theme_minimal(base_family = "Lato") +
+     theme(
+       plot.subtitle = element_text(size = 12, margin = margin(b = 10))
+     )
+ }
> 
> # Create slope charts
> p_rural <- make_slope_chart_cori(df_rural, subtitle = "Rural areas") +
+   theme(legend.position = "none")
> p_nonrural <- make_slope_chart_cori(df_nonrural, subtitle = "Nonrural areas") +
+   theme(legend.position = "none")
> 
> # Combine slope charts side-by-side with shared legend
> slope_combined <- (p_nonrural | p_rural) +
+   plot_layout(guides = "collect") +
+   plot_annotation(
+     title = "Tradable services represent a low share of rural employment",
+     theme = theme(
+       plot.title = element_text(
+         size  = 16,
+         face  = "bold",
+         family = "Lato",
+         hjust = 0,
+         margin = margin(b = 15)
+       )
+     )
+   ) &
+   theme(legend.position = "top")
> 
> ggsave(
+   filename = "figure7_slope.png",
+   plot = slope_combined,
+   width = 7.5,      
+   height = 5,       
+   dpi = 300         
+ )
Warning messages:
1: Removed 60 rows containing missing values or values outside the scale range (`geom_line()`). 
2: Removed 60 rows containing missing values or values outside the scale range (`geom_point()`). 
3: Removed 60 rows containing missing values or values outside the scale range (`geom_line()`). 
4: Removed 60 rows containing missing values or values outside the scale range (`geom_point()`). 
> 
> ggsave(
+   filename = "figure7_bars.png",
+   plot = bar_chart,
+   width = 3.75,      
+   height = 2.5,       
+   dpi = 300         
+ )
> # Load data for figure 2
> df_figure2 <- read.csv("https://raw.githubusercontent.com/ripleycleghorn/data-viz-designer-assessment/refs/heads/main/data/Assessment%20Data%20-%20Figure%202.csv",
+                header = TRUE, stringsAsFactors = FALSE)
> 
> # Load libraries
> library(ggplot2)
> 
> # Define percent labels function
> percent_labels <- function(x) sprintf("%d%%", x * 100)
> 
> # Build the line chart
> p <- ggplot(df_figure2, aes(x = year, y = indexed_value)) +   
+   geom_line(aes(color = is_rural), linewidth = 1.1) +    
+   scale_color_manual(                                   
+     values = c("Nonrural" = "#211448", "Rural" = "#00825B")
+   ) +
+   scale_y_continuous(                                   
+     limits = c(1, 2.5),
+     breaks = c(1, 1.5, 2, 2.5),                          
+     labels = percent_labels
+   ) +
+   facet_wrap(                                           
+     ~is_rural,
+     nrow = 1,
+     labeller = labeller(
+       is_rural = c(Nonrural = "Nonrural Areas",
+                    Rural = "Rural Areas")
+     )
+   ) +
+   labs(                                                  
+     title = "Rural employment growth has not kept up with nonrural areas",
+     subtitle = "Employment relative to 1969 levels for nonrural and rural areas, respectively",
+     x = NULL,
+     y = NULL,
+     caption = "Source: Bureau of Economic Analysis\nNote: \"Rural\" refers to the nonmetro definition which includes all nonmetro counties"
+   ) +
+   theme_minimal(base_family = "lato") +                  
+   theme(
+     text = element_text(family = "lato"),
+     plot.title = element_text(size = 16, face = "bold"),
+     plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
+     axis.title.y = element_text(size = 10),
+     strip.text = element_text(size = 12, face = "bold"),
+     
+     # Grid lines
+     panel.grid.major.x = element_blank(),                   
+     panel.grid.minor = element_blank(),                    
+     panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5),  
+     
+     legend.position = "none",
+     plot.caption = element_text(
+       size = 10,
+       color = "gray20",
+       margin = margin(t = 15),
+       hjust = 0                       
+     ),
+     plot.margin = margin(20, 20, 20, 20)
+   )
> 
> ggsave(
+   filename = "figure2_rural_nonrural.png",
+   plot = p,
+   width = 7.5,     
+   height = 5,      
+   dpi = 300         
+ )
> 
> 
> 
> 
2025-11-20 14:54:47.806 R[18014:11922823] The class 'NSSavePanel' overrides the method identifier.  This method is implemented by class 'NSWindow'
> 