package intellij.zeppelin

import com.intellij.execution.ui.ConsoleView
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.{InputValidator, Messages}

import scala.collection.mutable

class ZeppelinConnection(val project:Project) extends ProjectComponent{


  private[this] var username:String = ""
  private[this] var password:String = ""
  private[this] var hostUrl:String = ZeppelinConnection.DefaultZeppelinHost
  private[this] var proxyUrl:String = ""
  private[this] var maybeApi: Option[ZeppelinApi] = None

  val consoleViews: mutable.Map[String, ConsoleView] = mutable.Map[String, ConsoleView]()

  def getUsername:String = username
  def getPassword:String = password
  def getHostUrl:String = hostUrl
  def getProxyUrl:String = proxyUrl

  def setUsername(value:String): Unit = { username = value }
  def setPassword(value:String): Unit = { password = value }
  def setHostUrl(value:String): Unit = { hostUrl = value }
  def setProxyUrl(value:String):Unit = { proxyUrl = value }

  def promptForZeppelinHost():ZeppelinApi = {
    hostUrl = Messages.showInputDialog(
      "Please, enter your Zeppelin Notebook",
      "Zeppelin Notebook",
      null,
      hostUrl,
      new InputValidator() {
        override def checkInput(inputString: String): Boolean = true

        override def canClose(inputString: String) = true
      })
    maybeApi = Some(new ZeppelinApi(hostUrl, credentials, proxyInfo))
    maybeApi.get
  }

  private def credentials:Option[Credentials] = if (username != "" && password != "") Some(Credentials(username, password)) else None
  private def proxyInfo:Option[Proxy] = {
    if(proxyUrl!="") {
      val parts = proxyUrl.split(":")
      if (parts.length > 1) {
        Some(Proxy(parts(0), parts(1).toInt))
      } else {
        Some(Proxy(port = parts(1).toInt))
      }
    } else None
  }

  private [zeppelin] def resetApi():Unit = {
    maybeApi = Some(new ZeppelinApi(hostUrl, credentials, proxyInfo))
  }
  def api: ZeppelinApi = maybeApi.getOrElse(promptForZeppelinHost())
}

object ZeppelinConnection{
  val DefaultZeppelinHost = "http://localhost:8080"
  def connectionFor(project: Project): ZeppelinConnection = project.getComponent(classOf[ZeppelinConnection])
}
