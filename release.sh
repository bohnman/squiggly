#!/bin/bash

export GPG_TTY=$(tty)

# Helper script to create a new release.

releaseVersion="$1"
nextVersion="$2"

function releaseVersionReplace() {
    file=$1
    perl -i -0777 -pe "s/(<artifactId>squiggly-filter-jackson<\/artifactId>.*?<version>).*?(<\/version>)/\${1}${releaseVersion}\${2}/smg" ${file}

    if [ $? -gt 0 ]; then
        exit 1;
    fi
}

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

readmeFiles=$(find . -name README.md)

for readmeFile in ${readmeFiles}; do
    releaseVersionReplace ${readmeFile}
done

pomFiles=$(find . -name pom.xml)

for pomFile in ${pomFiles}; do
    releaseVersionReplace ${pomFile}
done

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


#echo "Pushing commits"
#git push origin
#
#if [ $? -gt 0 ]; then
#    exit 1;
#fi

echo "Pushing tags"
git push origin --tags

if [ $? -gt 0 ]; then
    exit 1;
fi

echo "done."
