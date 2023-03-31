val dottyVersion = "3.2.2"

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
      "com.lihaoyi" %% "pprint" % "0.8.1",
      "dev.zio" %% "izumi-reflect" % "2.3.0"
    )
  )

lazy val bench = (project in file("bench"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
