install.packages("palmerpenguins")

library(palmerpenguins)
View(penguins)

# ctrl+shift+m = %>% 
 # ctrl+ shift+ c = comment out
# ctrl+ alt+ i = insert code chunk

#load packages
library(tidyverse)
library(plotly)
library(htmlwidgets)

#load data
penguins <- read_csv("penguins.csv")


###data exploration
view(penguins)
str(penguins)
head(penguins)
tail(penguins)

### create scatterplot with ggplot

#-x axis: body_mass_g
#-y axis: bill_length_mm
#point-color: "species"


col_scale <- c("lightskyblue", "slateblue4", "darkslategray4")
names(col_scale) <- c("Adelie", "Chinstrap", "Gentoo") #can assign particular levels if we name them

theme_set(theme_classic())
p <- penguins %>% 
  na.omit() %>% #need to omit NAs preliminarily for uniform cleanup
ggplot(aes(x = body_mass_g, y = bill_length_mm,
       color = species, shape = sex))+
  geom_point() +
  labs(x = "Body Mass (g)",
       y = "Bill Length (mm)",
       title = "IncrediBILL Penguins",
       caption = "Data from Palmer LTER",
       tag = "a)") + #useful if you have multiple figures in a panel
  scale_color_manual(values = col_scale,
                     name = "Species",
                     labels = c("Adelie", "Chinstrap", "Gentoo"))+
  scale_shape_manual(values = c(19, 20))
  
  theme(
    #text = element_text(color = "grey70"),
    title = element_text(face = "bold"), #makes all titles bold
      plot.title = element_text(color = "darkblue",
                                size = 16,
                                hjust = 0.5, vjust = 5),
      plot.margin = margin(25, 15, 10, 10), #Remember TRBL
    axis.text = element_text(color = "grey70")
      )
#### Saving a theme
  
  #check the current default theme
  
  ggplot(cars, aes(x = speed, y = dist)) + geom_point()

  #set the detault theme as one of the included thems
theme_set(theme_gray())

#check it out  
ggplot(cars, aes(x = speed, y = dist)) + geom_point()

#make your own theme
theme_awful <- theme_gray()

#copy an existing theme and save it with a new name
theme_set(theme_awful)
#Set your new theme as the default

#adjust the theme
theme_update(panel.grid.minor = element_line(color = "red", size = 3),
             plot.background = element_rect(fill = "yellow"),
             title = element_text(size = 60, face = "italic", color = "tan"))

#see how it looks
ggplot(cars, aes(x = speed, y = dist)) + geom_point()

###save the plot

#check the filepath
getwd() #usually run in console

ggsave("ugly_plot.png", #filepath
  p, #item saving
  height = 4, width = 5, units = "in" #size
      )


###make it interactive!
ggplotly(p)
#loses legend formatting and shape/sizes/weights

theme_get()

p2 <- 
  penguins %>% 
  na.omit() %>% 
  ggplot(aes(x = flipper_length_mm/10, y =body_mass_g/1000, color = species,
             text = sex,
             place = island,
             frame = year))+ # Time series makes it MOVE!!
  geom_point() +
  scale_color_manual(values = col_scale) +
  labs(x = "Flipper Length (cm)", y = "Mass (kg)")

ggplotly(p2,
         tooltip = c("text", "place"))

#save as standalone HTML
saveWidget(p2, "ts_graph.html")
