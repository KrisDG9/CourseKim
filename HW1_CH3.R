#Required package is tidyverse.

#Q: Do cars with big engines use more fuel than cars with small engines?

mpg #show the dataframe (displ = engine size; hwy = fuel efficiency) werkt niet dus alternatief:

library(readxl)
mpg <- read_excel("mpg.xlsx")

?mpg #learn more about this package

ggplot(data = mpg) #this is an empty graph; you can add layers to that plot

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) #geom_point adds a layer of points

#TEMPLATE: ggplot(data = <DATA>) + 
  #<GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

#EXERCISES.3.2.4

#Ex1Q: What does ggplot(data = mpg) do?

ggplot(data = mpg)

#Ex1A: Empty graph (needs layers and mapping).

#Ex2Q: How many rows are in mpg? How many columns?

View(.GlobalEnv)

summary(mpg)

#Ex2A: 234 x 12.

#Ex3Q: What does the drv variable describe?

?mpg

#Ex3A: f = front-wheel drive, r = rear-wheel drive, 4 = 4-wheel drive.

#Ex4Q: Make a scatterplot of hwy vs cyl.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

#Ex5Q: What happens if you make a scatterplot of class vs drv? Why is the plot not useful?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

#Ex5A: Because both variables are categorical.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class)) #mapping aesthetics (ggplot does the scaling: assigning colours)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) #this leads to warning because unwise (using size for discrete variable is unwise)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) #alpha mapping is about transparency, again warning for using alpha on a discrete variable

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)) #and shape also unwise because we have too many so R leaves out the 7th

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue") #for setting aes manually, put it outside of aes (as an argument of your geom function)

#R has 25 built in shapes: 1-14 are hollow (colour = border); 15-18 are solid (colour = fill); 21-24 are filled with fill (colour = border)

#EXERCISES.3.3.1

#Ex1Q: What's gone wrong with this code? Why are the points not blue?
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = "blue"))

#Ex1A: For setting aes manually it should be outside of aes (as an argument of your geom function).

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")

#Ex2Q: Which variables in mpg are categorical? Which variables are continuous? How can you see this information when you run mpg?

mpg

glimpse(mpg)

#Ex2A: Categorical variables have <chr> in the data frame: manufacturer, model, trans, drv, fl, and class.

#Ex3Q: Map a continuous variable to colour, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = cty)) #continuous mapped to colour

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty)) #continuous mapped to size

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty)) #continuous mapped to shape

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class)) #categorical mapped to colour

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) #categorical mapped to size

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)) #categorical mapped to shape

#Ex3A: A continuous variable cannot be mapped to shape; colour is okay (gradient); size is very unclear here. For categorical colour and shape are nice (but too many levels for shape here). Size still unclear.

#Ex4Q: What happens if you map the same variable to multiple aesthetics?

#Ex4A: As we just saw some 'crash' because they e.g. can't handle that many levels, and/or it becomes unclear. Using multiple aes is also often redundant (so the graphs becomes less clear).

#Ex5Q: What does the stroke aesthetic do? What shapes does it work with?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) #basis

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 22, colour = 5, fill = 7, size = 4) #met aes

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 22, colour = 5, fill = 7, size = 4, stroke = 2) #met aes en dikkere rand

#Ex5A: Stroke impacts the border size. It only works with shapes 21-24 because they have a separate border and fill.

#Ex6Q: What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = cty < 15))

#Ex6A: It splits up the data (separate colour for each site of the border that you set for that extra variable).

#Facets are subplots that each display one subset of the data.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

#Facet_wrap was separate for 1 variable; facet_grid is voor een combinatie (van 2).

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

#EXERCISES.3.5.1

#Ex1Q: What happens if you facet on a continuous variable?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty, nrow = 2)

#Ex1A: You get a separate plot for each level, which can get really messy for a continuous variable.

#Ex2Q: What do the empty cells in plot with facet_grid(drv ~ cyl) mean? How do they relate to this plot?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#Ex2A: It means that those combinations of drv and cyl do not exist (in your data).

#Ex3Q: What plots does the following code make? What does . do?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

#Ex3A: It ignores that dimension for faceting, so it uses the full range of scores for that dimension.

#Ex4Q: Take the first faceted plot in this section. What are the advantages to using faceting instead of the colour aesthetic? What are the disadvantages? How might the balance change if you had a larger dataset?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

#Ex4A: Facets are clearer when there are more groups, e.g. the shape aes errors with more than 6 groups (and colour) is also not that clear anymore. The advantage of aes above facets though is that aes is an overlay so in 1 Figure.

#Ex5Q: What does nrow do? What does ncol do? What other options control the layout of the individual panels? Why doesn't facet_grid() have nrow and ncol arguments?

?facet_wrap

#Ex5A: Nrow and ncol are the number of rows and columns you request. There are many other options for the wrap function, like setting the direction. For grid nrow and ncol are useless because this is determined by the data (how many classes both variables have).

#Ex6Q: When using facet_grid() you should usually put the variable with more unique levels in the columns. Why?

#Ex6A: Otherwise it will probably not fit on an A4 size and be visible. Also, it is easier to compare relative levels of y by scanning horizontally, so it may be easier to visually compare these levels.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

#Note that not every aesthetic works with every geom, e.g. you could set the shape of a point, but not of a line.

#You can get different lines (and linetypes) for constructs by mapping it, e.g. for all 3 drv types.

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

#ggplot2 provides over 30 geoms and combinations (and extensions have even more)

#You can set a group aesthetic to a categorical variable to draw multiple objects.

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, colour = drv), show.legend = FALSE)

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, colour = drv), show.legend = TRUE)

#To display multiple geoms in the same plot, just add multiple geom functions.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

#This, however, introduces some duplication in our code (if you want to change a variable you now have to do that twice). You can avoid this type of repetition by passing a set of mappings to ggplot, which will treat them as global.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

#You can then still place mappings in a geom function: ggplot will treat them as local mappings and they will extend or overwrite the global ones. This way you can display different aesthetics in different layers.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth()

#You can use the same idea to specify different data for each layer. Example in which the smooth line displays just a subset of the data (subcompact cars) because the local data argument in geom_smooth() overrides the global data argument in ggplot() for that layer only.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth(data = filter(mpg, class == "subcompact"), se = TRUE)

#EXERCISES.3.6.1

#Ex1Q: What geom would you use to draw a line chart? A boxplot? A histogram? An area chart?

#Ex1A: Respectively geom_line, geom_boxplot, geom_histogram, and geom_ribbon.

#Ex2Q: Run this code in your head and predict what the output will look like. Then, run the code in R and check your predictions.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

#Ex2A: Scatter with 3 different colours (corresponding to drv) and 3 separate lines with 3 colours.

#Ex3Q: What does show.legend = FALSE do? What happens if you remove it? Why do you think I used it earlier in the chapter?

#Ex3A: Legend is not shown; I would opt for plotting legends and did not like the example where the book did not.

#Ex4Q: What does the se argument to geom_smooth() do?

?geom_smooth

#Ex4A: It determines whether or not the CI is plotted around the line.

#Ex5Q: Will these two graphs look different? Why/why not?

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

#Ex5A: Both are the same graph but in case of 1 it's global and 2 it's local. 1 is less error-prone when you change the variables; 2 is more versatile because you can adapt things (local runs over global).

#Ex6Q: Recreate the R code necessary to generate the following [6] graphs.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se = FALSE) #1

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE) #2

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group = drv, colour = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE) #3

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy), se = FALSE) #4

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, linetype = drv), se = FALSE) #5

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, fill = drv), shape = 21, colour = "white", size = 4, stroke = 3) #6

#For geom_bar you can just input the x; the y (here the count) is calculated by an algorithm, a so-called 'stat' (statistical transformation). The default stat for a boxplot is stat_count.

library(readxl)
diamonds <- read_excel("diamonds.xlsx")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

#You can generally use geoms and stats interchangeably. For example, using stat_count() instead of geom_bar().

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# This works because every geom has a default stat; and every stat has a default geom. But when do you use the stat explicity? 
  
  # 1. To overwrite the default stat.

  demo <- tribble(
    ~cut,         ~freq,
    "Fair",       1610,
    "Good",       4906,
    "Very Good",  12082,
    "Premium",    13791,
    "Ideal",      21551)

  ggplot(data = demo) + 
    geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

  # 2. To overwrite the default mapping.
  
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
  
  # 3. To draw attention to the stat, to the summary of your data.
  
  ggplot(data = diamonds) + 
    stat_summary(
      mapping = aes(x = cut, y = depth),
      fun.ymin = min,
      fun.ymax = max,
      fun.y = median)
  
#EXERCISES.3.7.1
  
#Ex1Q: What is the default geom associated with stat_summary()? How could you rewrite the previous plot to use that geom function instead of the stat function?
  
ggplot(data = diamonds) +
  geom_pointrange(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max, 
    fun.y = median, 
    stat = "summary")

#Ex1A: The default geom associated with stat_summary is geom_pointrange.

#Ex2Q: What does geom_col() do? How is it different to geom_bar()?

ggplot(data = diamonds) + 
  geom_col(mapping = aes(x = cut, y = price))  

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

?geom_col

#Ex2A: There are 2 types of bar chart: geom_col and geom_bar. For col, the heights of the bars represent values in the data (hence input a y). For bar, the height is proportional to the number of cases (default is stat_count and that cannot be used with a y aes).

#Ex3Q: Most geoms and stats come in pairs that are almost always used in concert. Read through the documentation and make a list of all the pairs. What do they have in common?

#Ex3A: (1) stats_summary & geom_pointrange, (2) stat_bin & geom_bar, (3) stat_count & geom_histogram. Their common denominator is that the are all plotted vertically, indicating height or size or range etc.

#Ex4Q: What variables does stat_smooth() compute? What parameters control its behaviour?

?stat_smooth

#Ex4A: Stat_smooth computes the predicted value of y for each x (and by default the se (CI) of the predicted value). It's behaviour is controlled by the method argument.

#Ex5Q: In our proportion bar chart, we need to set group = 1. Why? In other words what is the problem with these two graphs?

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..,))

#Ex5A: If you don't specify the group (miss the argument) you get all filled up bars because all bars have proportion == 1 (because without the group argument they calculate the proportion within the group and that is then always 1 (total)).

#You can also give your bar charts some colour. For most bar charts the fill will be more useful than the colour.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

#If you map the fill aesthetic to another variable (e.g. clarity) the bars are automatically stacked. Each bar then represents a combination of cut and clarity.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

#The stacking is performed automatically by the position adjustment specified by the position argument. If you don't want a stacked bar chart, you can use 1 of 3 other options.

  # 1. Identify, places each object exactly where it falls in the context of the graph (not useful for bars because it overlaps them, see example with lower alpha or no fill). It's useful for 2D geoms like points (where it is the default).

  ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
    geom_bar(alpha = 1/5, position = "identity")
  
  ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
    geom_bar(fill = NA, position = "identity")

  # 2. Fill, stacking but then with each set of stacked bars the same height (easier to compare proportions across groups but a bit misleading sometimes).
  
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

  # 3. Dodge, places overlapping objects directly beside one another (easier to compare individual values).
  
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

#An adjustment that is useful for scatterplots helps against overplotting: the values of hwy and displ are rounded so the points appear on a grid and many points overlap. Solution is jitter.
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

#Jitter adds a small amount of random noise to each point. This spreads the points out because no two points are likely to receive the same amount of random noise.

#Jitter makes your graph less accurate at small scales, but makes it more revealing at large scales. 

#So useful that there is a shortcut: geom_point(position = "jitter") == geom_jitter().

#EXERCISES.3.8.1

#Ex1Q: What is the problem with this plot? How could you improve it?

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

#EX1A: There is overplotting because certain combination of cty and hwy occur more than once. Adding jitter will fix this.

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position = "jitter")

#Ex2Q: What parameters to geom_jitter() control the amount of jittering?

?geom_jitter

#Ex2A: Width and hight (controlling the amount of vertical and horizontal displacement). You can set them high or e.g. at 0 so that there is no jitter in a certain direction.

#Ex3Q: Compare and contrast geom_jitter() with geom_count().

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()

#Ex3A: Geom_jitter reduces overlapping issues by adding a bit of jitter (con = changed coordinates). Geom_count resizes the points relative to the number of observations at each location so that points with more observations get bigger (con = can itself introduce overlapping).

#Ex4Q: What's the default position adjustment for geom_boxplot()? Create a visualisation of the mpg dataset that demonstrates it.

ggplot(data = mpg, aes(x = drv, y = hwy, colour = class)) + 
  geom_boxplot() #dodged so next to each other

ggplot(data = mpg, aes(x = drv, y = hwy, colour = class)) + 
  geom_boxplot(position = "identity") #identity so overlapping

#Ex4A: Position_dodge. 

#Ggplot2 coordinate system: default system is Cartesian system where the x and y positions act independently to determine the location of each point.

#What others systems can be useful?

#coord_flip() switches x and y axis (e.g. useful if you want horizontal boxplots)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() + 
  coord_flip()

#coord_quickmap() sets the aspect ratio correctly for maps (necessary when you use special data)

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  coord_quickmap()

#coord_polar() uses polar coordinates (e.g. nice to reveal possibly interesting connections between a bar chart and a Coxcomb chart)

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()

bar + coord_polar()

#EXERCISES.3.9.1

#Ex1Q: Turn a stacked bar chart into a pie chart using coord_polar().

ggplot(diamonds, aes(x = factor(1), fill = color)) + 
  geom_bar() #stacked bar

ggplot(diamonds, aes(x = factor(1), fill = color)) + 
  geom_bar(width = 1) + 
  coord_polar() #no pie yet, bulls eye

ggplot(diamonds, aes(x = factor(1), fill = color)) + 
  geom_bar(width = 1) + 
  coord_polar(theta = "y") #pie!

#Ex1A: So use coord_polar AND specify the theta as y.

#Ex2Q: What does labs() do? Read the documentation.

?labs()

#Ex2A: Helps you make proper axes, legends, and plot labels.

#Ex3Q: What's the difference between coord_quickmap() and coord_map()?

?coord_quickmap()

#Ex3A: Coord_map projects a portion of the earth onto a flat 2D plane and uses the Mercator projection for this (by default). This projection must be applied to ALL geoms in the plot, and coord_quickmap() does a faster (but approximate) map projection that ignores the curvature of the earth and adjusts the map for the latitude/longitude ratio.

#Ex4Q: What does the plot below tell you about the relationship between city and highway mpg? Why is coord_fixed() important? What does geom_abline() do?

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() + 
  geom_abline() + 
  coord_fixed()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() + 
  geom_abline()

#Ex4A: This is information that can be compared (points vs. straight line). Humans are best able to perceive differences in angles of 45 degrees, and coord_fixed makes sure that the line that geom_abline produces is at 45 degrees (so that we can easily compare the mileagses to a situation in which they are equal).

#UPDATED TEMPLATE: ggplot(data = <DATA>) + 
  #<GEOM_FUNCTION>(mapping = aes(<MAPPINGS>), stat = <STAT>, position = <POSITION>) + 
  #<COORDINATE_FUNCTION> + 
  #<FACET_FUNCTION>