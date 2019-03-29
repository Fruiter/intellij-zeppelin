package intellij.zeppelin

import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.{ToolWindow, ToolWindowFactory}

class ZeppelinToolWindowFactory extends ToolWindowFactory {
  override def createToolWindowContent(project: Project, toolWindow: ToolWindow): Unit = {
    //toolWindow.setAvailable(true, null)
  }
}
