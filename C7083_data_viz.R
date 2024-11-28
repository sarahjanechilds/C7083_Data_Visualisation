## C7083 Data Visualisation ####
## student 24370500
## S-J Childs
## Data Visualisation essay R script, rough and inelegant coding
## Due 2024-11-28
## Source data from Github's Tidy Tuesday 2019-01-29
## https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-01-29/readme.md

## CONTENTS ####
## 00 Setup
## 01 Graph 01 Line chart: Milk produced vs Milk consumed
## 02 Graph 02 Faceted line: Type of Milk Sales
## 03 Graph 03 Scatter plot in base R, yogurt to milk
## 04 Graph 04 Area chart of Ice cream Consumption
## 05 Graph 05 Interactive Line Chart of Milk Products
## 06 Graph 06 Bar chart comparison of Cheeses

## 00 Setup ####

# Install packages and load libraries
if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'tidyverse',
               'ggplot2',  'ggalt', 'reshape2', 'plotly',
               'forcats', 'R.utils', 'png', 'wesanderson', 
               'grid', 'ggpubr', 'scales', 'htmlwidgets',
               'devtools', 'readr', 'bbplot') # inspiration from https://bbc.github.io/rcookbook/
devtools::install_github('bbc/bbplot') # bbplot is not CRAN


# Load the data
# Define the URLs
url1 <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/master/data/2019/2019-01-29/fluid_milk_sales.csv"
url2 <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/master/data/2019/2019-01-29/milk_products_facts.csv"
url3 <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/master/data/2019/2019-01-29/milkcow_facts.csv"
url4 <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/master/data/2019/2019-01-29/clean_cheese.csv"
  
# Read the data into R
milk_sales <- read.csv(url1)
products <- read.csv(url2)
milkcows <- read.csv(url3)
cheese <- read.csv(url4)


## Graph 01: Line chart comparing milk produced and milk consumed ####

# Comparing trends, but the comparison is between billions and hundreds of lbs
production <- milkcows %>%
  mutate(milk_production_billion_lbs = round(milk_production_lbs / 1e9)) %>%
  select(-milk_production_lbs) # rounding the billions, helps view comparison

# Merge on the 'year' column
comparison <- merge(production, products, by = "year")

# Plotting

ggplot(comparison, aes(x = year)) +
  geom_line(aes(y = milk_production_billion_lbs, 
                color = "Milk Production (Billion lbs)"), linewidth = 1) +
  geom_line(aes(y = fluid_milk, 
                color = "Fluid Milk (Hundreds lbs)"), linewidth = 1) +
  geom_hline(yintercept = 100, size = 0.7, colour="#333333") +
  scale_y_continuous(
    name = "Milk Production (Billion lbs)",
    limits = c(100, 250),
    sec.axis = sec_axis(~ ., name = "Fluid Milk (Hundreds lbs)")
  ) + 
  scale_colour_manual(values = c("sienna2", "lightseagreen")) + # colours via https://sape.inf.usi.ch/quick-reference/ggplot2/colour
  bbc_style() +
  labs(
    x = "Year",
    y = "Milk Production (Billion lbs)",
    color = "Legend",
    title = "Comparison of Milk Produced vs Milk Consumed"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) 

# have to now finalise and save 
finalise_plot(
  plot_name = graph_01,
  source = "github.com",
  save_filepath = "C:\\Users\\sarah\\OneDrive\\Documents\\graph_01.png",
  width_pixels = 640,
  height_pixels = 550 
  ) # have to assign my plot to a variable to get finalise function to work!
 
print(graph_01) # doesn't show up in viewer once assigned, so have to print to see it


## Graph 02: Types of Milk Sold ####

# I'm going to filter milk_sales data as 'total production' isn't needed
# Also taking out eggnog and buttermilk as they're so niche, I want to focus on milk

sales <- milk_sales %>%
  mutate(pounds = round(pounds / 1e8)) # rounding the billions, helps view comparison

milk_sales_filter <- sales %>%
  filter(!milk_type %in% c("Buttermilk", "Eggnog", "Total Production"))

# Plotting

graph_02 <-ggplot(milk_sales_filter, aes(x = year, y = pounds, color = milk_type)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour="#333333") +
  facet_wrap(~ milk_type, scales = "free_y") +
  labs(title = "Type of Milk Produced in the USA",
       x = "Year",
       y = "Billion (lbs)",
       color = "Milk Types") +
  bbc_style() + # very much like this style
  theme_minimal() +
    theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.text = element_text(size = 10)
  ) 

# Saving the graph
finalise_plot(
  plot_name = graph_02,
  source = "github.com", # can't workout how to make this appear smaller
  save_filepath = "C:\\Users\\sarah\\OneDrive\\Documents\\graph_02.png",
  width_pixels = 800,
  height_pixels = 450  
)
print(graph_02)



## Graph 03: Base R scatter plot of yogurt and milk relationship ####


png("C:\\Users\\sarah\\OneDrive\\Documents\\yogurt_milk_relationship.png", 
    width = 650, height = 550)
# Set the axis text size
par(cex.axis = 1) 

plot(products$fluid_milk, products$fluid_yogurt,
     xlab = "Milk (lbs)",
     ylab = "Yogurt (lbs)",
     main = "",
     pch = 19, col = "lightseagreen",
     cex = 1.2)

# Add the main title with adjusted position
title(main = "Yogurt to Milk Relationship", line = 2, adj = 0)

dev.off()



## Graph 04: Area chart depicting Ice Cream consumption ####

frozen <- melt(products, id.vars = "year",
          measure.vars = c("frozen_ice_cream_regular", "frozen_ice_cream_reduced_fat", 
                           "frozen_sherbet", "frozen_other"))
frozen$variable <- factor(frozen$variable, 
          levels = c("frozen_ice_cream_regular", "frozen_ice_cream_reduced_fat", 
                     "frozen_sherbet", "frozen_other"))

# Plotting

graph_04 <-ggplot(frozen, aes(x = year, y = value, fill = variable)) +
  geom_area(alpha = 0.6, linewidth = 0.5, colour = "grey", position = "identity") +
  geom_hline(yintercept = 0, size = 0.7, colour = "#333333") +
  scale_x_continuous(breaks = seq(min(frozen$year), max(frozen$year), by = 5)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5), 
                     limits = c(0, 20)) +
  scale_fill_manual(values = wes_palette("GrandBudapest1"), # Thank you Karthik Ram
      labels = c("Regular Ice Cream", "Low Fat Ice Cream", "Sherbet", "Other")) +
  bbc_style() +
  labs(title = "Frozen Dairy Products Consumed Per Person",
       x = "Year",
       y = "Lbs per person",
       fill = "Frozen Categories") +
  theme(
    legend.position = "right",  # trying to be consistent across all graphs
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12), 
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

# Saving graph

finalise_plot(
  plot_name = graph_04,
  source = "github.com",
  save_filepath = "C:\\Users\\sarah\\OneDrive\\Documents\\graph_04.png",
  width_pixels = 650,
  height_pixels = 550  
)
print(graph_04)




## Graph 05: Interactive Line Chart of Milk Products ####

products$frozen_combined <- products$frozen_ice_cream_regular + 
  products$frozen_ice_cream_reduced_fat + 
  products$frozen_sherbet +
  products$frozen_other

products$cheese_combined <- products$cheese_american +
  products$cheese_other + 
  products$cheese_cottage


# Plotting
# Once ggplot is made, I then assign it so I can use ggplotly function
p <- ggplot(products, aes(x = year)) +
  geom_line(aes(y = fluid_yogurt, color = "Fluid Yogurt"), linewidth = 1.1) +
  geom_line(aes(y = frozen_combined, color = "Ice Cream"), linewidth = 1.1) +
  geom_line(aes(y = cheese_combined, color = "Cheese"), linewidth = 1.1) +
  geom_line(aes(y = butter, colour = "Butter"), linewidth = 1.1) +
  geom_hline(yintercept = 0, size = 0.7, colour="#333333") +
  scale_color_manual(values = c("Fluid Yogurt" = "orange", "Butter" = "steelblue3",
                              "Ice Cream" = "lightseagreen", "Cheese" = "sienna2")) +
  scale_x_continuous(breaks = seq(min(products$year), max(products$year), by = 5)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40)) +
  bbc_style() +
  labs(title = "Changes to Consumed Dairy Products Over Time",
       x = "Year",
       y = "Lbs per person",
       color = "Product") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10)
  ) 

# Then make it interactive

graph_05 <- ggplotly(p, tooltip = c("x", "y", "color")) %>%
  layout(
    title = list(text = "Changes to Consumed Dairy Products Over Time", 
                font = list(size = 14, face = "bold")),
    xaxis = list(title = list(text = "Year", font = list(size = 14)), 
                tickmode = "linear", tick0 = min(products$year), dtick = 5),
    yaxis = list(title = list(text = "Lbs per person", font = list(size = 14)),
                tickmode = "linear", tick0 = 0, dtick = 5, range = c(0, 40)),
    legend = list(title = list(text = "Product", font = list(size = 14)), 
                font = list(size = 12)),
    hovermode = "x unified"
  )
print(graph_05) # It's a bit rough, but I've run out of time!


# Saving graph, but have to use plotly's 
saveWidget(graph_05, "C:\\Users\\sarah\\OneDrive\\Documents\\graph_05.html")

# Graph 05 is a nightmare to render, just can't seem to affect the title size



## Graph 06: Bar chart comparison of Cheeses ####

colnames(cheese) # spelling mistake will confuse me

cheese <- cheese %>%
  rename("Total.American.Cheese" = "Total.American.Chese")

# Create a new column for decades
cheese$Decade <- floor(cheese$Year / 10) * 10

# Filter the data to include only the 1970s and 2010s
cheese_decades <- cheese %>%
  filter(Decade == 1970 | Decade == 2010)

# Group by Decade and calculate the mean for each column
cheese_compare <- cheese_decades %>%
  group_by(Decade) %>%
  summarise(
    Total_American = mean(`Total.American.Cheese`, na.rm = TRUE),
    Total_Italian = mean(`Total.Italian.Cheese`, na.rm = TRUE),
    Total_Processed = mean(`Total.Processed.Cheese.Products`, na.rm = TRUE),
    Total_Natural = mean(`Total.Natural.Cheese`, na.rm = TRUE)
  )

# Reshape the data for plotting
chevy_cheese <- cheese_compare %>%
  pivot_longer(
    cols = starts_with("Total"), 
    names_to = "Cheese_Type",
    values_to = "Amount",
    names_prefix = "Total_"
  )

# Define the order of cheese types
cheese_order <- c("Natural", "Italian", "American", "Processed")

# Convert Cheese_Type to a factor with the specified order
chevy_cheese$Cheese_Type <- factor(chevy_cheese$Cheese_Type, levels = cheese_order)

# Plotting

ggplot(chevy_cheese, aes(x = Cheese_Type, 
           y = Amount, 
           fill = as.factor(Decade))) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept = 0, size = 0.7, colour="#333333") +
  scale_y_continuous(breaks = seq(0, 35, by = 5), 
                     limits = c(0, 35)) +
  bbc_style() +
  scale_fill_manual(values = c("sienna2", "lightseagreen")) +
  labs(title = "Cheese Consumed: 1970s vs 2010s",
       x = "Cheese Type",
       y = "Lbs per person",
       fill = "Decade") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0, size = 14, face = "bold"), 
    axis.title = element_text(size = 12),  
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 12)
  )

# Saving

finalise_plot(
  plot_name = graph_06,
  source = "github.com",
  save_filepath = "C:\\Users\\sarah\\OneDrive\\Documents\\graph_06.png",
  width_pixels = 650,
  height_pixels = 550  
)
print(graph_06)

# Sorry my code isn't tidier! I go with whatever works, which means it can look a bit rough!