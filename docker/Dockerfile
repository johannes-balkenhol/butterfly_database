# Use rocker/shiny as base image
FROM rocker/shiny:4.3.2

# Install required system libraries
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libnetcdf-dev \
    libcurl4-gnutls-dev \
    libbz2-dev \
    liblzma-dev \
    libxml2-dev \
    libglpk40 \
    libpq-dev \
    libssh2-1-dev \
    default-jdk \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny','shinydashboard','shinyWidgets','shinyjs','shinycssloaders','shinyalert','readxl','DT','data.table','dplyr','tidyr','leaflet','igraph','ggraph','plotly'), repos='https://cran.rstudio.com/')"

# Copy app files
WORKDIR /app
COPY shiny-app/ /app/

# Copy entrypoint script
COPY docker/entrypoint.sh /usr/bin/entrypoint.sh
RUN chmod +x /usr/bin/entrypoint.sh

# Expose Shiny port
EXPOSE 3838

# Start Shiny App
ENTRYPOINT ["/usr/bin/entrypoint.sh"]
