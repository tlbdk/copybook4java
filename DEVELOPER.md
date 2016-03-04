# Releasing a new version

```bash
mvn versions:set -DnewVersion=1.0.1
git commit -m 'Release v1.0.1' pom.xml
git tag v1.0.1
git push
git push origin v1.0.1
mvn clean deploy -P release --settings settings.xml
```