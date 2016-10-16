#!/bin/bash

releaseVersion="$1"
nextVersion="$2"

if [ "$releaseVersion" == "" ]; then
    echo "Usage: ./release.sh release-version next-snapshot-version"
    exit 1
fi

if [ "$nextVersion" == "" ]; then
    echo "Usage: ./release.sh release-version next-snapshot-version"
    exit 1
fi

echo "Setting new version to ${releaseVersion}"
mvn versions:set -DnewVersion=${releaseVersion}

if [ $? -gt 0 ]; then
    exit 1;
fi

sed -i '' "s/<version>.*<\/version>/<version>$releaseVersion<\/version>/g" README.md

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "Performing release"
mvn clean deploy

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "Committing version ${releaseVersion}"
git commit -a -m "Setting version to $releaseVersion."

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "Tagging version ${releaseVersion}"
git tag ${releaseVersion}

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "Setting next snapshot version ${nextVersion}"
mvn versions:set -DnewVersion=${nextVersion}

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "Committing version ${nextVersion}"
git commit -a -m "Setting version to $nextVersion."

echo "Tagging version ${nextVersion}"
git tag ${nextVersion}

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "Pushing commits"
git push origin

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "Pushing tags"
git push origin --tags

if [ $? -gt 0 ]; then
    exit 1;
fi