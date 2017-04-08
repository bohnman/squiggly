#!/bin/bash

version="$1"

if [ "$version" == "" ]; then
    version=$(perl -0777 -ne 'print $1 if /<artifactId>squiggly-filter-jackson<\/artifactId>.*?<version>(.*?)<\/version>/smg' README.md)
fi

echo -n "Checking Version $version: "

status=$(curl -s -o /dev/null -I -w "%{http_code}" http://central.maven.org/maven2/com/github/bohnman/squiggly-filter-jackson/${version}/squiggly-filter-jackson-${version}.jar)


if [ "$status" == "" ]; then
    echo "Error Unknown"
    exit 1
fi

if [ "$status" == "200" ]; then
    echo "Found"
    exit 0
fi

if [ "$status" == "404" ]; then
    echo "Not Found"
    exit 1
fi

echo "ERROR $status"
exit 1