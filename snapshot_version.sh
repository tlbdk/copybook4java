#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 1" >&2
  exit 1
fi

export CURRENT_VERSION_SNAPSHOT=$(mvn help:evaluate  -Dexpression=project.version |egrep -v "(^\[INFO\])")
echo "org: $CURRENT_VERSION_SNAPSHOT"
export CURRENT_VERSION=$(expr "$CURRENT_VERSION_SNAPSHOT" : "\([0-9\.]*\)")
echo "cur: $CURRENT_VERSION"
export RELEASE_VERSION=$CURRENT_VERSION.$1-SNAPSHOT
echo "new: $RELEASE_VERSION"
mvn versions:set -DnewVersion=$RELEASE_VERSION -DgenerateBackupPoms=false -DallowSnapshots=true