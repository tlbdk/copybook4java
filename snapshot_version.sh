#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 1" >&2
  exit 1
fi

export NEWVERSION='${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}'.$1-SNAPSHOT
mvn build-helper:parse-version versions:set -DnewVersion=${NEWVERSION} versions:commit