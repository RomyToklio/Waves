import sbt._

object Dependencies {

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.5.20"

  def swaggerModule(module: String) = ("io.swagger.core.v3" % s"swagger-$module" % "2.0.5").exclude("com.google.guava", "guava")

  def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.1.7"

  def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.1.33.Final"

  def kamonModule(module: String, v: String) = "io.kamon" %% s"kamon-$module" % v

  def slf4j(module: String) = "org.slf4j" % module % "1.7.25"

  val JacksonVersion = "2.9.6"

  val asyncHttpClient = "org.asynchttpclient" % "async-http-client" % "2.7.0"

  lazy val itKit = Seq(
    scalatest,
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    ("com.spotify" % "docker-client" % "8.11.3").classifier("shaded").exclude("com.google.guava", "guava"),
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.6",
    asyncHttpClient.exclude("io.netty", "netty-handler")
  )

  lazy val test = Seq(
    scalatest,
    logback,
    "org.scalacheck"      %% "scalacheck"                  % "1.14.0",
    "io.github.amrhassan" %% "scalacheck-cats"             % "0.4.0",
    "org.scalatest"       %% "scalatest"                   % "3.0.5",
    "org.mockito"         % "mockito-all"                  % "1.10.19",
    "org.scalamock"       %% "scalamock-scalatest-support" % "3.6.0",
  ).map(_ % "test")

  lazy val scalatest = "org.scalatest"  %% "scalatest"      % "3.0.5"
  lazy val logback   = "ch.qos.logback" % "logback-classic" % "1.2.3"

  lazy val kindProjector = "org.spire-math" %% "kind-projector"     % "0.9.6"
  lazy val betterFor     = "com.olegpy"     %% "better-monadic-for" % "0.3.0-M4"

  lazy val common = Seq(
    "org.typelevel" %% "cats-mtl-core" % "0.3.0",
    ("org.scorexfoundation" %% "scrypto" % "2.0.4")
      .exclude("com.google.guava", "guava"),
    "org.typelevel" %% "cats-effect"         % "0.10.1",
    "org.bykn"      %% "fastparse-cats-core" % "0.1.0",
  )

  lazy val node = Seq(
    "commons-net" % "commons-net" % "3.6",
    "com.iheart"  %% "ficus"      % "1.4.2",
    slf4j("slf4j-api"),
    logback                % "runtime",
    "net.logstash.logback" % "logstash-logback-encoder" % "4.11" % "runtime",
    kamonModule("core", "1.1.3"),
    kamonModule("system-metrics", "1.0.0").exclude("io.kamon", "kamon-core_2.12"),
    kamonModule("akka-2.5", "1.1.1").exclude("io.kamon", "kamon-core_2.12"),
    kamonModule("influxdb", "1.0.2"),
    "org.influxdb"                 % "influxdb-java"         % "2.11",
    "com.google.guava"             % "guava"                 % "21.0",
    "com.google.code.findbugs"     % "jsr305"                % "3.0.2" % "compile", // javax.annotation stubs
    "com.typesafe.play"            %% "play-json"            % "2.6.10",
    "org.ethereum"                 % "leveldbjni-all"        % "1.18.3",
    "io.swagger"                   %% "swagger-scala-module" % "1.0.4",
    "com.github.swagger-akka-http" %% "swagger-akka-http"    % "1.0.0",
    "com.fasterxml.jackson.core"   % "jackson-databind"      % JacksonVersion,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % JacksonVersion,
    akkaHttpModule("akka-http"),
    "org.bitlet" % "weupnp" % "0.1.4",
    nettyModule("handler"),
    nettyModule("buffer"),
    nettyModule("codec"),
    akkaModule("persistence"),
    akkaModule("slf4j"),
    akkaModule("persistence-tck") % "test",
  ) ++
    Seq("core", "annotations", "models", "jaxrs2").map(swaggerModule) ++
    Seq("handler", "buffer").map(nettyModule) ++
    test ++
    Seq(
      akkaModule("testkit"),
      akkaHttpModule("akka-http-testkit")
    ).map(_ % "test")

  lazy val protobuf = Def.setting {
    val version = scalapb.compiler.Version.scalapbVersion
    Seq(
      // "com.google.protobuf" % "protobuf-java" % "3.4.0",
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version % "protobuf",
      "com.thesamet.scalapb" %% "scalapb-json4s" % "0.7.0"
    )
  }

  lazy val grpc = Seq(
    "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
  )
}
