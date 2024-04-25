# Load packages
library(tidyverse)
library(ggplot2)

# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory
setwd("/Users/joannecarminucci/OneDrive - brandeis.edu/Grad School")
schooldemo=read_csv("DACSS 690.csv")

# see data ----------------------------------------------------------

head(schooldemo)

schooldemo$urban <- factor(case_when(grepl("City", schooldemo$Urbanicity) ~ "City", 
                           grepl("Rural", schooldemo$Urbanicity) ~ "Rural",
                           grepl("Suburb", schooldemo$Urbanicity) ~ "Suburb",
                           grepl("Town", schooldemo$Urbanicity) ~ "Town"), 
                           levels = c("Suburb", "Rural", "City", "Town"))

localepct <- data.frame(table(schooldemo$urban))
localepct$pct <- round(localepct$Freq / sum(localepct$Freq) * 100, 1)
localepct$labels <- paste0(localepct$pct, "%")
localepct

# DRAFT DELIVERABLE 1
base = ggplot(data=localepct) 
draft1 = base + geom_bar(aes(x=Var1, y=pct), stat="identity", 
                         fill=c("Rural" = "darkgreen", "Suburb" = 'darkgreen', "Town" = "grey28", "City" = 'grey28'), 
                         width = 0.5) + 
  labs(title = "Urbanicity of U.S. Public Schools",
       subtitle = "The percentage of U.S. public schools located in each type of locale",
       caption = "Source: NCES Common Core of Data, 2019") +
  xlab("Locale Type") +
  ylab("Percent of Schools") + 
  geom_text(aes(x = Var1, y=pct, label = labels), 
            position = position_stack(vjust = .88),
            color="white") +
  annotate("text", label = "Nearly 65% of schools are in\nsuburban or rural areas.",
           x = 3.15, y = 28, color = "darkgreen", size = 4, fontface = "bold") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, family = "Verdana"),
    plot.subtitle = element_text(size = 11, hjust = 0.7, family = "Verdana", face="italic"),
    axis.title.x = element_text(family = "Verdana", size = 12),
    axis.title.y = element_text(family = "Verdana", size = 12),
    axis.text = element_text(family = "Verdana", color = "black", size = 10),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white")
  ) 
draft1

saveRDS(draft1, file = "barchart.rds")


# DRAFT DELIVERABLE 2
schooldemo$total[schooldemo$total==0] <- NA
schooldemo$total <- as.numeric(gsub("[^0-9.-]", "", schooldemo$total))

average <- mean(schooldemo$total, na.rm = TRUE)
average

base = ggplot(data=schooldemo, aes(x=total)) 
draft2 = base + 
  geom_histogram(width = 20, stat="count", fill="navy") +
  xlim(0,2000) +
  xlab("Size of Schools") +
  ylab("Number of Schools") +
  labs(title = "Sizes of U.S. Public Schools",
      subtitle = "The numbers of U.S. public schools according to their size",
      caption = "Source: NCES Common Core of Data, 2019") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, family = "Verdana"),
    plot.subtitle = element_text(size = 11, hjust = 0.7, family = "Verdana", face="italic"),
    axis.title.x = element_text(family = "Verdana", size = 12),
    axis.title.y = element_text(family = "Verdana", size = 12),
    axis.text = element_text(family = "Verdana", color = "black", size = 10),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white")
  ) + 
  geom_vline(xintercept = average, color = "deepskyblue", linetype = "solid", size = 1) +
  annotate("text", label = "The average school size in the U.S.\nis about 484.63 students.",
           x = 950, y = 100, color = "black", size = 3, fontface = "bold")

draft2
saveRDS(draft2, file = "histogram.rds")

# Draft deliverable 3, num/cat

schooldemo$urban <- factor(schooldemo$urban, levels = c("Suburb", "City", "Town", "Rural"))

summary(schooldemo$cs_mn_avg_ol[schooldemo$urban == "City"])
summary(schooldemo$cs_mn_avg_ol[schooldemo$urban == "Suburb"])

schooldemo$urbanbelow = case_when(schooldemo$urban == "City" & schooldemo$cs_mn_avg_ol < -1 ~ 1)
schooldemo$urbanabove = case_when(schooldemo$urban == "City" & schooldemo$cs_mn_avg_ol > 1 ~ 1)
schooldemo$suburbbelow = case_when(schooldemo$urban == "Suburb" & schooldemo$cs_mn_avg_ol < -1 ~ 1)
schooldemo$suburbabove = case_when(schooldemo$urban == "Suburb" & schooldemo$cs_mn_avg_ol > 1 ~ 1)

table(schooldemo$urbanbelow)
table(schooldemo$urbanabove)
table(schooldemo$suburbbelow)
table(schooldemo$suburbabove)


# School achievement based on locale type

draft3 <- ggplot(data=schooldemo, aes(y=cs_mn_avg_ol, fill=urban)) + 
  geom_hline(yintercept = 0, color = "grey28", linetype = "dotted", size = 1) + 
  geom_boxplot(width = 0.6, position = position_dodge(width = 1)) + 
  xlim(-.5, .5) + ylim(-4, 2) +
  ylab("Average School Achievement") +
  xlab("") +
  labs(title = "School Achievement vs. National Average",
       subtitle = "Reading and math achievement against the\nnational average for schools in each type of locale",
       fill = "Locale",
       caption = "Sources: NCES Common Core of Data, 2023;\nStanford Education Data Archive (Version 5.0), 2024") +
    theme(
    plot.title = element_text(size = 14, hjust = 0.5, family = "Verdana"),
    plot.subtitle = element_text(size = 11, hjust = 0.7, family = "Verdana", face="italic"),
    axis.title.y = element_text(family = "Verdana", size = 12),
    axis.text = element_text(family = "Verdana", color = "black", size = 10),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  annotate("text", label = "Cities tend to have more schools below one standard deviation (SD) from
   the national average (NATAVG) compared to suburban schools (323 vs. 58), 
           and fewer schools above one SD from the NATAVG (116 vs. 246).",
           x = 0, y = -3.5, color = "black", size = 3)
  
draft3
saveRDS(draft3, "boxplot.rds")


# Map! Draft 4

library(sf)
library(classInt)

usa <- st_read("cb_2018_us_state_20m")
head(usa)

achievement <- schooldemo %>%
  group_by(stateabb) %>%
  summarise(average = mean(cs_mn_avg_ol, na.rm = T))

names(achievement)[names(achievement) == "stateabb"] <- "STUSPS"
map <- merge(usa, achievement, by="STUSPS", all.x = T)
map$ach <- map$achievement_value <- ifelse(map$STUSPS %in% achievement$STUSPS, achievement$average, NA)

num_cat <- 5
dist_avg <- classInt::classIntervals(map$ach, num_cat,
                                          style = "jenks",
                                          dataPrecision=2)
map$ach=classInt::findCols(dist_avg,factor = T)
newNames=c("Far Below","Slightly Below","On Average","Slightly Above","Far Above")
levels(map$ach)=newNames

base=ggplot(data = map)
draft4 <- base + geom_sf(aes(fill=ach)) + 
  coord_sf(crs = st_crs(map), xlim = c(-125, -68), ylim=c(24,50)) + 
  scale_fill_brewer(palette = "YlGnBu", na.translate = F) + 
  labs(title = "Average Reading and Math Achievement",
       subtitle = "School-Level Achievement in Each U.S. State",
       fill = "Distance from\nNational Average",
       caption = "Sources: NCES Common Core of Data, 2023;\nStanford Education Data Archive (Version 5.0), 2024\nNote: States without color are not included in this analysis.") +
  theme(
    plot.title = element_text(size = 14, hjust=-0.1, family = "Verdana"),
    plot.subtitle = element_text(size = 11, hjust = 0, family = "Verdana", face="italic"),
    axis.text = element_text(family = "Verdana", color = "black", size = 10),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) + guides(fill = guide_legend(reverse = TRUE)) +
  annotate("text", label = "Minnesota, Iowa, and Missouri schools perform far above average.",
           x = -98, y = 23.5, color = "black", size = 3)

draft4
saveRDS(draft4, "map.rds")
