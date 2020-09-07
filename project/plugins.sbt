resolvers += Resolver.url(
  "Rally Plugin Releases",
  url("https://dl.bintray.com/rallyhealth/sbt-plugins")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.rallyhealth.sbt" %% "sbt-git-versioning" % "1.4.0")
// Not needed for Scala 2.13, but leaving this here for if we want to cross-compile to Scala 2.11
// addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.2")
