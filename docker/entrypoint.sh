#!/bin/bash
set -ex

# Launch the Shiny app
R -e "shiny::runApp('/app', host='0.0.0.0', port=3838)"
