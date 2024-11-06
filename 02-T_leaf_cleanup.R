#################################
# Thermal Sensitivity Curves - TSC
# Project - Unterlüß 2024 Lift Trees
# Time Series of direct Leaf Temperatures
#
# Code for cleaning up the data and making it useful
#
# Currently in progress - assigning REMOVE, AIR, NEWLEAF and OLDLEAF comments to the dataset
# so that unusable temps can be removed.
###############################

# 1. Load packages ############-----------------
# install.packages('pacman')
pacman::p_load(tidyverse, janitor, here, ggsci, viridis, lubridate, dplyr)

# 2. Organize your files #####
temp_data <- read_csv("RawData/Tl.csv")

#check out the names of your variables
temp_data %>% names()
str(temp_data)

#check out your unique leaf sensor numbers
unique(temp_data$Tno)

#this dataset is huge so be careful with this next part
# 3. Clean up the data to get what you want #####
tleaf_clean <- temp_data %>% 
  dplyr::select(TS, battery, PTemp_C_Avg, species, Parameter, Value, Tno, speciesTno) %>% 
  # filter(species == 'FS') %>% 
  mutate(Tno = as.character(Tno),
         species = case_when(species == 'FS' ~ 'Beech', 
                             species == 'PM' ~ 'Douglas fir', 
                             species == 'PA' ~ 'Spruce', 
                             TRUE ~ 'nana'),
         # variable for conifers and broadleaves
         tree_type = case_when(species == 'Spruce' ~ 'conifer',
                               species == 'Douglas fir' ~ 'conifer',
                               TRUE ~ 'broadleaf'),
  ) %>% 
  clean_names()
str(tleaf_clean)

#make a function to easily get the date from the posixct since make.Date didnt work
extractdate <- function(date) {
  day <- format(date, format="%d")
  month <- format(date, format="%m")
  year <- format(date, format="%Y")
  cbind(day, month, year)
}

leaf_temps <- cbind(tleaf_clean, extractdate(tleaf_clean$ts))
leaf_temps <- leaf_temps %>% 
  mutate(newdate = make_date(year = year, month = month, day = day))


leaf_temps %>% 
  count(value, sort = TRUE)

# baby graphs -------------------------------------------------------------
#prepare start and end for plots:
endTime <- now()
startTime = now() - ddays(40)

# create a start and end time R object
start.end <- c(startTime,endTime)

g <- leaf_temps %>% 
  filter(
    parameter == "Avg",
    value > 0
  ) %>% 
  drop_na(value) %>% 
  ggplot() +
  geom_line(aes(x=ts,y=value,color = tno)) +
  labs(title = "Average (10min) leaf temperature",
       subtitle = "") +
  scale_x_datetime(#limits=start.end,
    date_breaks = "1 week",
    date_labels = "%d.%m")+
  ylab("")+
  xlab("") +
  facet_wrap(~species)


# g_p <- 
  leaf_temps %>% 
  filter(parameter == "Avg", value > 0) %>% 
  drop_na(value) %>% 
  ggplot() +
  geom_point(aes(x=ts,y=value,color = tno), alpha = 0.1) +
  labs(title = "Average (10min) leaf temperature",
       subtitle = "") +
  scale_x_datetime(#limits=start.end,
    date_breaks = "1 week",
    date_labels = "%d.%m")+
  ylab("")+
  xlab("") +
  facet_wrap(~species)

# 4. tag and remove unusable data -----------------------------------------
real_temps <- leaf_temps %>% 
  #very quick assignment of unuseable values
  #maybe just tag everything that is detached as REMOVE
  #everything that is air as AIR
  #everything else as NEWLEAF or OLDLEAF?
  mutate(prev_touching = case_when( #ugh still not working despite making the
    # species == 'Beech' & newdate < '2024-07-11' & tno == '9' ~ 'detached',
    # species == 'Beech' & newdate < '2024-07-11' & tno == '21' ~ 'detached',
    # species == 'Beech' & newdate < '2024-07-11' & tno == '10' ~ 'air',
    # species == 'Beech' & newdate > '2024-07-11' & tno == '10' ~ 'leaf',
    # species == 'Beech' & newdate > '2024-07-11' & tno == '7' ~ 'air', 
    # species == 'Beech' & newdate > '2024-07-22' & newdate <= '2024-08-05' & tno == '19' ~ 'detached',
    # species == 'Spruce' & newdate < '2024-07-17' & tno != '16' ~ 'unknown',
    # species == 'Spruce' & newdate < '2024-07-17' & tno == '16' ~ 'air',
    # species == 'Spruce' & newdate > '2024-07-22' & tno == '10' ~ 'air',
    # species == 'Spruce' & newdate > '2024-07-17' & newdate < '2024-07-22' & tno == '8' ~ 'detached',
    # species == 'Spruce' & newdate > '2024-07-17' & newdate < '2024-07-22' & tno == '11' ~ 'detached',
    # species == 'Spruce' & newdate > '2024-07-17' & newdate < '2024-07-22' & tno == '12' ~ 'detached',
    # species == 'Spruce' & newdate > '2024-07-22' & newdate < '2024-08-12' & tno == '12' ~ 'detached',
    # species == 'Spruce' & newdate > '2024-07-17' & newdate < '2024-07-22' & tno == '17' ~ 'detached',
    # species == 'Spruce' & newdate > '2024-07-22' & newdate < '2024-08-12' & tno == '18' ~ 'detached',
    # species == 'Spruce' & newdate > '2024-07-22' & newdate < '2024-08-12' & tno == '28' ~ 'detached',
    # species == 'Spruce' & newdate > '2024-07-22' & newdate < '2024-08-12' & tno == '31' ~ 'detached',
    # species == 'Douglas fir' & newdate < '2024-06-24' & tno != '12' ~ 'unknown',
    species == 'Douglas fir' & newdate == '2024-06-25' & tno != '12' ~ 'unknown',
    # species == 'Douglas fir' & tno == '12' ~ 'air',
    # species == 'Douglas fir' & newdate > '2024-06-25' & newdate < '2024-07-22' & tno != '6' ~ 'detached',
    # species == 'Douglas fir' & newdate > '2024-06-25' & newdate < '2024-07-22' & tno != '10' ~ 'detached',
    # species == 'Douglas fir' & newdate > '2024-06-25' & newdate < '2024-07-22' & tno != '16' ~ 'detached',
    # species == 'Douglas fir' & newdate == '2024-06-25' ~ 'retaped',
    # species == 'Douglas fir' & newdate == '2024-06-24' ~ 'retaped',
    # species == 'Douglas fir' & newdate > '2024-07-22' & newdate < '2024-08-20' & tno == '17' ~ 'detached',
    # species == 'Douglas fir' & newdate > '2024-07-22' & newdate < '2024-08-20' & tno == '9' ~ 'detached',
    # species == 'Douglas fir' & newdate > '2024-07-22' & newdate < '2024-08-20' & tno == '11' ~ 'detached',
    # species == 'Douglas fir' & newdate > '2024-07-22' & newdate < '2024-08-20' & tno == '6' ~ 'detached',
    TRUE ~ 'leaf'))

tidy_temps <- l_temps %>% pivot_wider(names_from = Parameter, values_from = c(Value))
str(tidy_temps)
tidy_leaf_temps <- data.frame(leaf_temps)
str(tidy_leaf_temps)
write_csv(tidy_temps, "outputs/leaftemps/tidy_leaf_temps.csv")

tidy_temps <- read.csv('outputs/leaftemps/tidy_leaf_temps.csv')
# basic report ------------------------------------------------------------

lt_report <- tidy_temps %>% 
  # mutate(month = fct_relevel(month, "June", "July", "August", "September")) %>% 
  group_by(species, month, day, newdate, tno, prev_touching) %>% 
  summarise(
    maximum = max(Max, na.rm = TRUE),
    meanhigh = mean(Max, na.rm = TRUE),
    meanmean = mean(Avg, na.rm = TRUE),
    minmean = min(Avg, na.rm = TRUE)) %>% 
  filter(!maximum > 100)

lt_report$date <- as.Date(lt_report$newdate)
plot(minmean ~ month, data = lt_report)

lt_monthly <- tidy_temps %>% 
  # mutate(month = fct_relevel(month, "June", "July", "August", "September")) %>% 
  group_by(species, month) %>% 
  summarise(
    maximum = max(Max, na.rm = TRUE),
    meanhigh = mean(Max, na.rm = TRUE),
    meanmean = mean(Avg, na.rm = TRUE),
    minmean = min(Avg, na.rm = TRUE)) %>% 
  filter(!maximum > 100)
plot(meanmean ~ month, data = lt_monthly)
# visualize ---------------------------------------------------------------

#visualize

t <- lt_report %>% 
  # filter(newdate >= '2024-08-10' & newdate <='2024-08-11') %>% 
  ggplot(aes(x = month, y = maximum, group = species))
t + 
  # geom_point(aes(color = species), alpha = 0.5, size = 3) +
  geom_line(aes(newdate, maximum, color = species), alpha = 0.8) +
  scale_color_viridis(discrete=TRUE, name = 'Species', option = 'mako', begin = 0.1, end = 0.9) +
  theme_bw()

lt_report %>% 
  filter(species == 'Douglas fir') %>% 
  ggplot(aes(x = newdate, y = meanmean)) +
  geom_line(alpha = 1.5, color = "black") +
  geom_ribbon(aes(ymin = minmean, ymax = maximum), fill = '#b73779', alpha = 0.5) +
  theme_bw()

lt_report %>% 
  ggplot(aes(x = month, y = maximum, group = month, fill = month)) +
  scale_fill_viridis(name = 'Month', option = 'magma', begin = 0.4, end = 0.9) +
  geom_boxplot() +
  geom_point(alpha = 0.1, 
             # aes(color = day),
             position=position_jitterdodge()) +
  # scale_color_viridis(name = 'Day', option = 'turbo', begin = 0, end = 1) +
  facet_wrap(~species)

ggsave("figures/leaf_temps/monthly_raws.png", width = 400, height = 250, units = "mm", dpi = 600)



lt_report %>% 
  ggplot(aes(x = newdate, y = maximum, group = species, fill = species)) +
  geom_point(aes(color = species), alpha = 0.2) #+
facet_wrap(~month)
