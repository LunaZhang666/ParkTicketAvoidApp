# DESCRIPTION
Parking suggestion web application is an application for giving drivers in LA suggestions on potential risks in parking at certain areas. 
The application is written in R Shiny. 

# INSTALLATION
Install R studio and R compiler from the official website:
rstudio: https://www.rstudio.com/
r compiler: https://www.r-project.org/

To install packages needed, simply run the following from an R console:

```bash
install.packages("shiny")
install.packages("tidyverse")
install.packages("plotly")
install.packages("janitor")
install.packages("leaflet")
install.packages("colorRamps")
install.packages("proj4")
install.packages("validate")
install.packages("htmlwidgets")
install.packages("mapview")
install.packages("shinyBS")
```

# EXECUTION
Open "app.r" file in rstudio, then click the "Run App" button. A Shiny webapp window will pop up, and the application is ready to use. 
To reload the map, click "Reload App" button. To view it in a web browser, click the "Open in Browser" button in the pop up window. 