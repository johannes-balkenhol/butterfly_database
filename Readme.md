🦋 Butterfly Database — FAIR Digitalization of a Historical Lepidoptera Collection

Authors: Felix Weber, Johannes Balkenhol, Mirko Wölfling, Aman Akash & Thomas Dandekar
Affiliation: Department of Bioinformatics, University of Würzburg
License: MIT (code) | CC-BY 4.0 (data)
Publication: A Butterfly (Lepidoptera) Databank with Digitalization Package Models Adaptation (Butterfly Euphydryas aurinia) to Temperature
Repository: https://github.com/johannes-balkenhol/butterfly_database

🧭 Project Overview

This repository contains the FAIR-compliant R/Shiny application and database infrastructure for the Arthur Bott Butterfly Collection — 5,974 specimens (1900–2002).
The project provides an interactive digital archive for research and teaching, demonstrating scalable digitization workflows for natural history collections.

Core features:

🗺️ Interactive map and table interface for exploring specimens

🌈 Color brightness vs. elevation analysis

🔍 13+ filters for species, traits, location, and conservation status

🔗 Integration with GBIF, BOLD, EOL, and Wikipedia

📦 FAIR data export (ABCD/Darwin Core XML)

🧩 Repository Structure
code/
├── shiny-app/               # Main R/Shiny application
│   ├── data/                # Reference data (ignored by Git)
│   ├── database_build/      # Scripts to generate SQLite database
│   │   └── database_schema/
│   ├── server/              # Shiny server logic
│   ├── ui/                  # User interface components
│   └── www/                 # Web assets (images, icons, CSS)
│
├── docker/                  # Dockerfile and container configuration
├── leaflet/                 # Leaflet-based map component (R package)
└── scripts/                 # Utility and helper scripts


⚠️ Data note:
The full dataset and images are not stored on GitHub (see below).
They are available from archival sources (Nextcloud, GFBio, or Zenodo).

⚙️ Installation and Usage
Option 1: Run with Docker (recommended)
# Clone the repository
git clone https://github.com/johannes-balkenhol/butterfly_database.git
cd butterfly_database/code/docker

# Stop and remove existing containers (if any)
docker ps
docker stop butterfliesapp
docker rm butterfliesapp
docker image rm -f butterfliesapp

# Build and run
docker build -t butterfliesapp .
docker run -d --name butterfliesapp -p 6108:3838 butterfliesapp:latest


Then open your browser at
👉 http://localhost:6108

Option 2: Run directly in R
# From R or RStudio
setwd("code/shiny-app")
shiny::runApp()


The app will start on http://127.0.0.1:3838

🧱 Data Access

Data are not included in this GitHub repository for licensing and size reasons.
To run the app with real data:

Contact the authors or request data via GFBio/Zenodo.

Download the dataset and images into:

code/shiny-app/www/
code/shiny-app/data/


Restart the app or container.

🔧 Deployment Notes (for server use)
# Connect to remote server
ssh user@xx.xx.xx.xx

# Navigate to deployment directory
cd /var/www/Butterflies

# (Re)build and run container
docker build -t butterfliesapp .
docker run -d --name butterfliesapp -p 6108:3838 butterfliesapp:latest

# To stop or remove
docker stop butterfliesapp
docker rm butterfliesapp

Manual deployment without Docker
cd /var/www/Butterflies/shiny-app
Rscript app.R &
disown -h

🧬 FAIR Principles
Principle	Implementation
Findable	Persistent identifiers (PIDs) and structured metadata
Accessible	Web-based open interface
Interoperable	ABCD and Darwin Core XML export
Reusable	Open-source code (MIT) + open data (CC-BY 4.0)
👥 Contact

Johannes Balkenhol — johannes.balkenhol@uni-wuerzburg.de

Felix Weber — felix1997weber2@gmail.com

Prof. Dr. Thomas Dandekar — dandekar@biozentrum.uni-wuerzburg.de

🪞 Citation

Weber, F., Balkenhol, J., Akash, A., Wölfling, M., & Dandekar, T. (2025).
A Butterfly (Lepidoptera) Databank with Digitalization Package Models Adaptation (Butterfly Euphydryas aurinia) to Temperature.
University of Würzburg, Bioinformatics Department.
https://butterflies.bioinfo-wuerz.eu