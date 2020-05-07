val username = "markblokpoel"
val repo = "probability4scala"

updateOptions := updateOptions.value.withCachedResolution(false)

lazy val commonSettings = Seq(
  name := repo,
  scalaVersion := "2.13.2",
  organization := s"com.markblokpoel",
  description := "This is an implementation of the core API for the Lanag agent-based simulation framework.",
  crossScalaVersions := Seq("2.12.8","2.12.9","2.12.10","2.13.2"),
  crossVersion := CrossVersion.binary,
//  resolvers ++= Seq(
//    "jitpack" at "https://jitpack.io"
//  ),
  libraryDependencies += Dependencies.scalatest,
  libraryDependencies ++= Seq(
  ),
  // Compile options
  updateImpactOpenBrowser := false,
  compile in Compile := (compile in Compile).dependsOn(formatAll).value,
  mainClass in assembly := Some("com.markblokpoel.probability4scala.DefaultMain"),
  test in Test := (test in Test).dependsOn(checkFormat).value,
  formatAll := {
    (scalafmt in Compile).value
    (scalafmt in Test).value
    (scalafmtSbt in Compile).value
  },
  checkFormat := {
    (scalafmtCheck in Compile).value
    (scalafmtCheck in Test).value
    (scalafmtSbtCheck in Compile).value
  }
)

lazy val root = (project in file("."))
  .settings(name := s"$repo")
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .settings(docSettings: _*)
//  .settings(skip in publish := true)
  .settings(releaseSettings: _*)
  .enablePlugins(SiteScaladocPlugin)
//  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(GhpagesPlugin)

/*
 Scaladoc settings
 Note: To compile diagrams, Graphviz must be installed in /usr/local/bin
 */
import com.typesafe.sbt.SbtGit.GitKeys._
lazy val docSettings = Seq(
  autoAPIMappings := true,
//  unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject,
  siteSourceDirectory := target.value / "api",
  git.remoteRepo := scmInfo.value.get.connection,
  envVars in ghpagesPushSite += ("SBT_GHPAGES_COMMIT_MESSAGE" -> s"Publishing Scaladoc [CI SKIP]"),
  scalacOptions in (Compile, doc) ++= Seq(
    "-groups",
    "-diagrams",
    "-implicits",
    "-doc-root-content",
    baseDirectory.value + "/overview.txt",
    "-doc-title",
    "Language Agents",
    "-diagrams-dot-path",
    "/usr/local/bin/dot"
  )
)

// Enforce source formatting before submit
lazy val formatAll = taskKey[Unit](
  "Format all the source code which includes src, test, and build files")
lazy val checkFormat = taskKey[Unit](
  "Check all the source code which includes src, test, and build files")

// Maven / Scaladex release settings
import ReleaseTransformations._

lazy val releaseSettings = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    //runClean,
    //runTest,
    setReleaseVersion,
    //commitReleaseVersion,
    //ghpagesPushSite,
    tagRelease,
    releaseStepCommandAndRemaining("publishSigned"),
    setNextVersion,
    //commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    //pushChanges
  )
)

// Github and OSS Sonatype/Maven publish settings
lazy val publishSettings = Seq(
  homepage := Some(url(s"https://github.com/$username/$repo")),
  licenses += "GPLv3" -> url(
    s"https://github.com/$username/$repo/blob/master/LICENSE"),
  scmInfo := Some(
    ScmInfo(url(s"https://github.com/$username/$repo"),
            s"git@github.com:$username/$repo.git")),
  apiURL := Some(url(s"https://$username.github.io/$repo/latest/api/")),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  developers := List(
    Developer(
      id = username,
      name = "Mark Blokpoel",
      email = "mark.blokpoel@gmail.com",
      url = new URL(s"http://github.com/$username")
    )
  ),
  useGpg := true,
  usePgpKeyHex("15B885FCC9586C56EE4587C9993E5F170C68BA83"),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := Some(
    if (isSnapshot.value) Opts.resolver.sonatypeSnapshots
    else Opts.resolver.sonatypeStaging),
//  credentials ++= (for {
//    username <- sys.env.get("SONATYPE_USERNAME")
//    password <- sys.env.get("SONATYPE_PASSWORD")
//  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq,
  // Following 2 lines need to get around https://github.com/sbt/sbt/issues/4275
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(
    true)
)
