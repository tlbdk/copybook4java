#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 1" >&2
  exit 1
fi

export CURRENT_VERSION_SNAPSHOT=$(mvn help:evaluate  -Dexpression=project.version |egrep -v "(^\[INFO\])")
export CURRENT_VERSION=$(expr "$CURRENT_VERSION_SNAPSHOT" : "\([0-9\.]*\)")
export RELEASE_VERSION=$CURRENT_VERSION.$1-SNAPSHOT
echo $RELEASE_VERSION
mvn versions:set -DnewVersion=$RELEASE_VERSION -DgenerateBackupPoms=false -DallowSnapshots=true