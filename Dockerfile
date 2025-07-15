# Use an official R runtime as a parent image
FROM rocker/r-ver:4.3.2

# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# Install R
RUN apt-get update && apt-get install -y r-base

RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"

# System libraries
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libnetcdf-dev \
    libcurl4-gnutls-dev \
    libhts-dev \ 
    libbz2-dev \ 
    liblzma-dev \ 
    libxml2-dev \
    libglpk40 \
    default-jdk \
    libpq-dev \
    libssh2-1-dev
    


# Install other CRAN packages
RUN R -e 'install.packages(c("shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinycssloaders","shinyalert","readxl","DT","data.table","dplyr","tidyr","leaflet","igraph","ggraph","plotly"))'

RUN mkdir -p /app


COPY shiny-app/ /app/R/


# Set up entry point script
COPY /docker/entrypoint.sh /usr/bin/entrypoint.sh

RUN chmod +x /usr/bin/entrypoint.sh


# Expose the Shiny app port
EXPOSE 3839


ENTRYPOINT ["/usr/bin/entrypoint.sh"]
# Set the entry point to the script
#ENTRYPOINT ["/usr/bin/entrypoint.sh"]

