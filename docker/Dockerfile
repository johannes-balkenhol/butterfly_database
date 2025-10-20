# --- Butterflies Shiny App Dockerfile (with working leaflet setup) ---
FROM rocker/shiny:4.3.2

# Install comprehensive system dependencies for Shiny + Leaflet + tidyverse tools
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libgit2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    make \
    g++ \
    git \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install all CRAN packages needed by the app
RUN R -e "install.packages(c( \
  'remotes', \
  'RSQLite', \
  'xml2', 'httr', 'jsonlite', 'htmltools', \
  'shiny', 'shinydashboard', 'shinyWidgets', 'shinyjs', \
  'shinycssloaders', 'shinyalert', 'DT', 'readxl', \
  'data.table', 'dplyr', 'tidyr', 'igraph', 'ggraph', 'plotly', \
  'htmlwidgets' \
), repos='https://cran.rstudio.com/')"


# Install leaflet from GitHub using remotes (stable + tested)
RUN R -e "remotes::install_github('rstudio/leaflet', upgrade='never', dependencies=TRUE)"

# Copy the Shiny app into the container
WORKDIR /app
COPY shiny-app/ /app/

# Copy entrypoint script
COPY docker/entrypoint.sh /usr/bin/entrypoint.sh
RUN chmod +x /usr/bin/entrypoint.sh

# Expose default Shiny port
EXPOSE 3838

# Start the app
ENTRYPOINT ["/usr/bin/entrypoint.sh"]
