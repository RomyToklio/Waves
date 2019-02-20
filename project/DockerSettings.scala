import sbt._
import sbt.Keys._
import sbtassembly.AssemblyPlugin.autoImport.assembly
import sbtdocker.DockerPlugin.autoImport._

object DockerSettings {
  val additionalFiles = SettingKey[Seq[File]]("Files")
  
  val settings = inTask(docker)(
    Seq(
      dockerfile := {
        println((Compile / resourceDirectory).value)
        val configTemplate = (Compile / resourceDirectory).value / "template.conf"
        val startWaves     = sourceDirectory.value / "container" / "start-waves.sh"

        val withAspectJ     = Option(System.getenv("WITH_ASPECTJ")).fold(false)(_.toBoolean)
        val aspectjAgentUrl = "http://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar"
        val yourKitArchive  = "YourKit-JavaProfiler-2019.1-docker.zip"

        new Dockerfile {
          from("anapsix/alpine-java:8_server-jre")
          runRaw("mkdir -p /opt/waves")

          // Install YourKit
          runRaw(s"""apk update && \\
                    |apk add --no-cache openssl ca-certificates && \\
                    |wget --quiet https://www.yourkit.com/download/docker/$yourKitArchive -P /tmp/ && \\
                    |unzip /tmp/$yourKitArchive -d /usr/local && \\
                    |rm -f /tmp/$yourKitArchive""".stripMargin)

          if (withAspectJ) run("wget", "--quiet", aspectjAgentUrl, "-O", "/opt/waves/aspectjweaver.jar")

          add((assembly in LocalProject("node")).value, "/opt/waves/waves.jar")
          add(Seq(configTemplate, startWaves), "/opt/waves/")
          runShell("chmod", "+x", "/opt/waves/start-waves.sh")
          entryPoint("/opt/waves/start-waves.sh")
          expose(10001)
        }
      },
      buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
    ))
}
