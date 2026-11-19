# let's load the penguin dataset dataset
data('penguins')

# let's assign the data set to df
df<- penguins
# let's observe the data 
#--------------------------------------------------------
# SCATTER PLOT
#--------------------------------------------------------
ggplot(
  data =  df,
  mapping = aes(x = flipper_length_mm, y =body_mass_g)
) +
  geom_point(
    mapping = aes(color = species, shape = species)
  ) +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Relationship between flipper length and body mass by species",
    x = "Flipper length in mm",
    y = 'Body Mass in g',
    shape = "Species",
    color = "Species"
  ) +
  scale_color_colorblind()

#-------------------------------------------------
# BAR CHART 
#-------------------------------------------------

ggplot(
  data = penguins,
  mapping = aes(x= species)
) +
  geom_bar()

