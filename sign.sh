#!/bin/bash
APK_HOME=build/outputs/apk
./gradlew assembleRelease &&
cp ${APK_HOME}/*-prod-release-unsigned.apk ${APK_HOME}/c4scala-prod-release-signed.apk &&
cat pass | jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore c4-release-key.keystore ${APK_HOME}/c4scala-prod-release-signed.apk c4key &&
jarsigner -verify -verbose -certs ${APK_HOME}/c4scala-prod-release-signed.apk &&
cp ${APK_HOME}/c4scala-prod-release-signed.apk ${APK_HOME}/c4scala-prod-release-unaligned.apk &&
zipalign -v 4 ${APK_HOME}/c4scala-prod-release-unaligned.apk ${APK_HOME}/c4scala-prod-release-aligned.apk &&
cp ${APK_HOME}/c4scala-prod-release-aligned.apk ${APK_HOME}/c4scala-prod-release-final.apk &&
cp ${APK_HOME}/c4scala-prod-release-final.apk c4scala-release-final.apk