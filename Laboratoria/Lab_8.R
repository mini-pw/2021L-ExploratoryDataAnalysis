#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM  8           ###
#########################################

# Wykresy interaktywne w ploty 

install.packages("plotly")

library(plotly)
library(tidyr)
library(dplyr)

install.packages("DALEX")
library(DALEX)
# Dane o smokach
data("dragons")
head(dragons)

?dragons

# https://plotly.com/r/

# 1) wykres punktowy

plot_ly()

plot_ly(data = dragons, x = ~height, y = ~weight, type = "scatter", mode = "markers")

plot_ly(data = dragons, x = ~height, y = ~weight, color =~colour, type = "scatter", mode = "markers")

pal <- levels(dragons$colour)

plot_ly(data = dragons, x = ~height, y = ~weight, color =~colour, 
        type = "scatter", mode = "markers", colors = pal)

x_ranged <- list(range = c(30, 50))

plot_ly(data = dragons, x = ~height, y = ~weight, color =~colour, 
        type = "scatter", mode = "markers", colors = pal) %>% 
  layout(xaxis = x_ranged)

yaxis <- list(
  title = "WEIGHT",
  dtick = 1,
  tickwidth = 4,
  tickcolor = "#371ea3"
)

plot_ly(data = dragons, x = ~height, y = ~weight, color =~colour, 
        type = "scatter", mode = "markers", colors = pal) %>% 
  layout(xaxis = x_ranged, yaxis = yaxis, title = "Tytuł")

ax <- list(
  title = "",
  showticklabels = FALSE,
  showgrid = FALSE
)

plot_ly(data = dragons, x = ~height, y = ~weight, color =~colour, 
        type = "scatter", mode = "markers", colors = pal) %>% 
  layout(xaxis = ax, yaxis = ax, title = "Tytuł")



# 2) wykres liniowy

set.seed(123)
dragons_small <- dragons[sample(1:nrow(dragons), 10),]

plot_ly(dragons_small, x = ~height, y = ~weight, type = "scatter", mode = "lines")

dragons_small <- dragons_small %>% arrange(height)

plot_ly(dragons_small, x = ~height, y = ~weight, type = "scatter", mode = "markers+lines")

# 3) wykres słupkowy

plot_ly(dragons, x = ~colour)

dragons_count <- dragons %>% 
  count(colour) %>% 
  arrange(n)

plot_ly(dragons_count, x = ~colour, y = ~n, type = "bar")

dragons_count$colour <- factor(dragons_count$colour, levels = dragons_count$colour)

plot_ly(dragons_count, x = ~n, y = ~colour, type = "bar", orientation = "h")

plot_ly(dragons_count, x = ~n, y = ~colour, type = "bar", orientation = "h",
        text = substr(dragons_count$colour, 1, 3))


summary(dragons$scars)

dragons$scars_cut <- cut(dragons$scars, breaks = c(0, 7, 76), include.lowest = TRUE) 

plot_ly(dragons, x = ~colour, y = ~height, color = ~scars_cut, typ = "bar") %>% 
  layout(legend = list(title = list(text = 'scars')))

plot_ly(dragons, x = ~colour, y = ~height, color = ~scars_cut, typ = "bar") %>% 
  layout(legend = list(title = list(text = 'scars')), barmode = 'stack')


ggplot(dragons, aes(x = scars)) + 
  geom_bar()

plot_ly(dragons, x = ~scars)


# 4) boxplot

plot_ly(dragons, x = ~height, type = "box", name = "")

plot_ly(dragons, x = ~colour, y = ~height, type = "box")

plot_ly(dragons, x = ~colour, y = ~height, color = ~scars_cut, type = "box") %>% 
  layout(boxmode = "group")

plot_ly(dragons, x = ~colour, y = ~height, type = "violin", 
        box = list(visible = TRUE), points = "all")

# 5) histogram

plot_ly(dragons, x = ~height, type = "histogram")

# 6) add_trace()
x <- c(1:100)
trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)

data_points <- data.frame(x, trace_0, trace_1, trace_2)

plot_ly(data_points, x = ~x, y = ~trace_0, type = "scatter", mode = "lines")

plot_ly(data_points, x = ~x) %>% 
  add_trace(y = ~trace_0, name = "trace 0", type = "scatter", mode = "lines") %>% 
  add_trace(y = ~trace_1, name = "trace 1", type = "scatter", mode = "lines+markers") %>% 
  add_trace(y = ~trace_2, name = "trace 2", type = "scatter", mode = "markers") %>% 
  layout(title = "Traces example", yaxis=list(title="y"))


# 7) ggplotly

p <- ggplot(dragons, aes(x = colour, y = height, fill = colour)) + 
  geom_boxplot()
p

ggplotly(p)

ggplot(dragons, aes(x = colour, y = height, fill = colour)) + 
  geom_violin() +
  geom_boxplot() 

plot_ly(dragons, x = ~colour, y = ~height, color = ~colour, type = "violin",
        box = list(visible = TRUE))
