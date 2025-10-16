# project is on git https://github.com/johannes-balkenhol/butterfly_database
## not the data are in R/www which are ignored by git
## thisincludes the original butterfly images and  other data
## download the pictures from database source like nextcloud, zenodo, gfbio, or write the authors



# verbindung zum server
ssh  user@xx.xx.xx.xx


# directory:
cd /var/www/Butterflies

# Install with docker
## Stop and remove the existing container:
docker container ls
docker stop <container id>
docker rm butterfliesapp

## remove image
docker image ls
docker image rm -f <image id>


## Build the new image from the Dockerfile:
cd /var/www/Butterflies
docker build -t butterfliesapp .

## Run a new container from the newly built image:
docker run -d --name butterfliesapp -p 6108:3838 butterfliesapp:latest
docker run -d --name butterfly-db -p 6108:3838 butterfly-db:latest

# open R to install packages
R

# check installed packages
ls /usr/local/lib/R/site-library


# copy files to server
scp test.png felix@xx.xxx.xx.xx:/home/felix
# aber besser mit winscp


# Install ohne Docker
## ordner fürs deployen
cd /var/www/Butterflies/shiny-app/R

## run shiny
Rscript app.R &

## keep running in Background
disown -h

## check if shiny app is running 
ps aux | grep Rscript

## check if server running
lsof -i -P | grep LISTEN | grep 88

## stop shiny
kill "id"


