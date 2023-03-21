library(tidyverse)

## **3.2** Notes - Creating a ggplot {-}

head(mpg)

# Plot mileage (hwy) against engine displacement (displ):
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  ggtitle("Engine displacement (x-axis) vs Mileage (y-axis)") +
  theme(plot.title = element_text(hjust = 0.5))

# Adding a trend line to the plot:
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  ggtitle("Engine displacement (x-axis) vs Mileage (y-axis)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = 'lm', se = F)


## **3.2.4** Exercises {-}

# 1. Run ggplot(data = mpg). What do you see? {-}
ggplot(data = mpg)

# 2. How many rows are in mpg? How many columns? {-}
dim(mpg)

# 3. What does the drv variable describe? Read the help for ?mpg to find out. {-}
# ?mpg

# 4. Make a scatterplot of hwy vs cyl. {-}
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy)) +
  ggtitle("Number of Cylinders (x-axis) vs Mileage (y-axis)") +
  theme(plot.title = element_text(hjust = 0.5))

# 5. What happens if you make a scatterplot of class vs drv? Why is the plot not useful? {-}
ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv)) +
  ggtitle("Number of Class (x-axis) vs Type of Drive (y-axis)") +
  theme(plot.title = element_text(hjust = 0.5))

# Color-code the points in the scatterplot by another variable in the data set
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Another example, this time color coding based on the "drv" variable
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))

# Using size variable to further categorize in the graph:
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cyl, color = class, alpha = drv))


## **3.3.1** Exercises {-}

# 1. What’s gone wrong with this code? Why are the points not blue? {-}
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
# points are not blue because the "color =" parameter lies within aes()
# remove the "aes" and use "color = 'blue'" instead

## **3.3.1** Exercises {-}

### 1. What’s gone wrong with this code? Why are the points not blue? {-}

# ggplot(data = mpg) +
#   geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
# The points are not blue because the "color =" parameter lies within aes(). This means the function will be looking for a column within the mpg dataset called "blue", which does not exist. So to fix this, place the "color =" parameter outside aes(), but within geom_point().

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

### 2. Which variables in mpg are categorical? Which variables are continuous? (Hint: type ?mpg to read the documentation for the dataset). How can you see this information when you run mpg? {-}

# One way you can figure out which are categorical vs continuous is by using the summary() function. The continuous variables will have the quartiles specified, whereas the categorical variables will not. You might have to be wary about categorical variables in numerical form, in which you would have to read the documentation. Runing just mpg would show the type of varable under the column name (char vs int vs dbl, etc.) which would also let you know this information.

summary(mpg)

### 3. Map a continuous variable to color, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables? {-}

# I mapped the continuous variable, "cty", city miles per gallon, using color and size. By color, the points are now on a gradient. By size, the larger points have higher city miles per gallon. I couldn't map the continuous variable to shape, since there are a set number of shapes available. This was also an issue when running it for the variable "class", since there was one more class than there were number of shapes as well (the SUV category has no points as a result).

# install gridExtra package to plot multiple graphs side by side, could also use cowplots package
# install.packages("gridExtra")
library(gridExtra)
byColor <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cty)) +
  ggtitle("City miles mapped by color")
bySize <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cty)) +
  ggtitle("City miles mapped by size")

grid.arrange(byColor, bySize, ncol=2)
byShape <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))+
  ggtitle("Number of Class (x-axis) vs Type of Drive (y-axis)")
byShape

### 4. What happens if you map the same variable to multiple aesthetics? {-}

# The points all lie on the same area of the spectrum for each aesthetic.

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cyl, color = cyl, alpha = cyl))

### 5. What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point) {-}

# The stroke modified the width of the border for geom_points that have a border. Below I increase the size of the points after categorizing by the drv variable.

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape
# 6. What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)? {-}

#The plot will be mapped to the output of the argument. Displ < 5 will return TRUE for all points less than 5, and these points will be mapped to a separate color. Below is an example of displ < 5 and cyl < 5.

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cyl < 5))

