#!/usr/bin/env bash
mvn build-helper:parse-version versions:set -DnewVersion='${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}'-SNAPSHOT -DgenerateBackupPoms=false
