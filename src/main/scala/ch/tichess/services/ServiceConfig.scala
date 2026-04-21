package ch.tichess.services

object ServiceConfig:
  def port(envName: String, default: Int): Int =
    sys.env.get(envName).flatMap(_.toIntOption).getOrElse(default)

  def url(envName: String, default: String): String =
    sys.env.getOrElse(envName, default).stripSuffix("/")
