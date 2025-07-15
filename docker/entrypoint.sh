#!/bin/bash

set -ex

R -e "shiny::runApp('/app/R/R', host='0.0.0.0', port=3838)"
