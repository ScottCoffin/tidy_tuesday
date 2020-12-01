# Week 16
# ggplot Hacky Hour

library(tidyverse) 
library(palmerpenguins) #contains the penguin data!
library(RColorBrewer) #contains color palettes we can can call on
library(ggsci) #another color package, one of my favorites!
library(scales) #will use to view color scales and hex codes for ggsci palettes
library(ggpubr) #will use to add some statistical outputs to our plots
library(ggbeeswarm) #cool data viz to reduce overplotting
library(wesanderson) # cool colors
library(gganimate) # animation!
library(plotly)
library(calecopal)
library(devtools) #to update gganimate from github
 devtools::install_github('thomasp85/gganimate') #necessary to update gganimate



penguins <- palmerpenguins::penguins %>% 
  rename(flipper = flipper_length_mm, bill_length = bill_length_mm, bill_depth = bill_depth_mm) %>% 
  mutate(mass_kg = body_mass_g/1000, bill_ratio = bill_length/bill_depth) %>% 
  na.omit() 

ggplot(data = penguins, aes(x = species, y = flipper, color = sex))+
  geom_beeswarm()+
  gganimate()
