package coursierbuild.modules

import mill.*
import mill.api.*
import mill.scalalib.*

trait CoursierPublishModule extends PublishModule
    with CoursierJavaModule {
  import mill.scalalib.publish._
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "io.get-coursier",
    url = "https://github.com/coursier/coursier",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("coursier", "coursier"),
    developers = Seq(
      Developer("alexarchambault", "Alex Archambault", "https://github.com/alexarchambault")
    )
  )
  def publishVersion = "2.1.25-SNAPSHOT"

  /** Publishes locally, then drops the sub-millisecond digits of the mtime of the published files.
    *
    * Mill computes "quick" `PathRef` signatures for resolved dependencies, hashing the mtime of the
    * JAR through `FileTime#hashCode`, which takes the whole nanoseconds into account. JVMs disagree
    * on the sub-millisecond precision they read back for a given file - JDK 11 truncates Linux
    * mtimes to microseconds, later JDKs report nanoseconds - so a signature computed by a Mill
    * daemon running a recent JDK cannot be re-validated by a worker pinned to JDK 11 via `jvmId`.
    * Any build depending on these JARs then dies with `Worker wire broken, worker likely crashed`.
    *
    * JARs coming from a coursier cache are unaffected, as their mtime is that of the
    * `Last-Modified` header they were served with, and has a whole number of seconds. Publishing
    * JARs whose mtime has no sub-millisecond digits puts the ones we publish here in the same boat.
    */
  override def publishLocal(
    localIvyRepo: String = null,
    sources: Boolean = true,
    doc: Boolean = true,
    transitive: Boolean = false
  ): Task.Command[Unit] = {
    val doPublish = super.publishLocal(localIvyRepo, sources, doc, transitive)
    val publishedModules =
      if (transitive)
        (transitiveModuleDeps ++ transitiveRunModuleDeps)
          .collect { case p: PublishModule => p }
          .distinct
      else Seq(this)
    val publishedMetadata = Task.traverse(publishedModules)(_.artifactMetadata)
    Task.Command {
      doPublish()
      val repoRoot = Option(localIvyRepo)
        .map(os.Path(_, BuildCtx.workspaceRoot))
        .getOrElse(
          sys.props.get("ivy.home").map(os.Path(_)).getOrElse(os.home / ".ivy2") / "local"
        )
      for {
        artifact <- publishedMetadata()
        dir = repoRoot / artifact.group / artifact.id / artifact.version
        if os.exists(dir)
        file <- os.walk(dir)
        if os.isFile(file)
      }
        // os.mtime reads milliseconds, os.mtime.set writes them back with no sub-millisecond digits
        os.mtime.set(file, os.mtime(file))
    }
  }
}

object CoursierPublishModule {

  lazy val latestTaggedVersion = os.proc("git", "describe", "--abbrev=0", "--tags", "--match", "v*")
    .call().out
    .trim()
  private def computeBuildVersion() = {
    // FIXME Print stderr if command fails
    val gitHead = os.proc("git", "rev-parse", "HEAD")
      .call(cwd = BuildCtx.workspaceRoot, stderr = os.Pipe)
      .out.trim()
    val maybeExactTag = scala.util.Try {
      // FIXME Print stderr if command fails
      os.proc("git", "describe", "--exact-match", "--tags", "--always", gitHead)
        .call(cwd = BuildCtx.workspaceRoot, stderr = os.Pipe).out
        .trim()
        .stripPrefix("v")
    }
    maybeExactTag.toOption.getOrElse {
      // FIXME Print stderr if command fails
      val commitsSinceTaggedVersion =
        os.proc("git", "rev-list", gitHead, "--not", latestTaggedVersion, "--count")
          .call(cwd = BuildCtx.workspaceRoot, stderr = os.Pipe).out.trim()
          .toInt
      val gitHash = os.proc("git", "rev-parse", "--short", "HEAD")
        .call(cwd = BuildCtx.workspaceRoot)
        .out.trim()
      s"${latestTaggedVersion.stripPrefix("v")}-$commitsSinceTaggedVersion-$gitHash-SNAPSHOT"
    }
  }

  lazy val buildVersion = computeBuildVersion()
}
