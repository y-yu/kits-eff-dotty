val dottyVersion = "3.1.3-RC1-bin-20220322-e54a934-NIGHTLY"

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "org.halcat",
      scalaVersion := dottyVersion,
      version := "0.1",
      scalacOptions ++= Seq(
        "-explain"
      )
    )),
    name := "kits-eff-dotty"
  )
  .aggregate(core, bench)

lazy val core = (project in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "pprint" % "0.7.1",
      "dev.zio" %% "izumi-reflect" % "2.1.0-M1"
    )
  )

lazy val bench = (project in file("bench"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
