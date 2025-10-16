# delete image
sudo docker container ls
sudo docker stop <container id>

sudo docker image ls
sudo docker image rm -f <image id>


# built image
sudo docker build -t butterfliesapp ./

docker run -d --name butterfliesapp -p 6108:3838 butterfliesapp:latest


