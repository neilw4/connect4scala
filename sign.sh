APK_HOME=c4scala/build/outputs/apk
gradle assembleRelease &&
cp ${APK_HOME}/c4scala-release-unsigned.apk ${APK_HOME}/c4scala-release-signed.apk &&
cat pass | jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore c4-release-key.keystore ${APK_HOME}/c4scala-release-signed.apk c4key &&
jarsigner -verify -verbose -certs ${APK_HOME}/c4scala-release-signed.apk &&
cp ${APK_HOME}/c4scala-release-signed.apk ${APK_HOME}/c4scala-release-unaligned.apk &&
zipalign -v 4 ${APK_HOME}/c4scala-release-unaligned.apk ${APK_HOME}/c4scala-release-aligned.apk &&
cp ${APK_HOME}/c4scala-release-aligned.apk ${APK_HOME}/c4scala-release-final.apk &&
cp ${APK_HOME}/c4scala-release-final.apk c4scala-release-final.apk