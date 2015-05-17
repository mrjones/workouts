usage="$0 [tag]"
project="mrjones/workouts"

if [ -z $1 ]
then
    echo "Please supply a tag for this image"
    echo $usage
    exit 1
fi
tag=$1

push="false"
if [[ $2 == "push" ]]
then
    push="true"
fi

echo "=== Compiling binary"
cabal install

echo "=== Creating image"
sudo docker build -t $project:$tag .

if [[ $push == "true" ]]
then
    echo "=== Pushing to docker hub"
    sudo docker push $project:$tag
else 
    echo "=== Skipping push to docker hub"
fi
