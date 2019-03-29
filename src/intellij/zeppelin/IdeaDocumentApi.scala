package intellij.zeppelin

import com.intellij.execution.filters.TextConsoleBuilderFactory
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.notification.{NotificationType, Notifications}
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileEditor.ex.FileEditorManagerEx
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.openapi.wm.ToolWindowManager


sealed trait SelectionMode
object SelectedText extends SelectionMode
object SingleLine extends SelectionMode
object Block extends SelectionMode

case class CodeFragment(selectionMode: SelectionMode, content:String)

trait IdeaDocumentApi {

  def currentEditor(anActionEvent: AnActionEvent): Editor = {
    FileEditorManagerEx.getInstanceEx(anActionEvent.getProject).getSelectedTextEditor
  }

  def currentFile(anActionEvent: AnActionEvent): VirtualFile = {
    FileEditorManagerEx.getInstanceEx(anActionEvent.getProject).getCurrentFile
  }

  def invokeLater(f: => Unit): Unit = {
    ApplicationManager.getApplication.invokeLater(new Runnable {
      override def run(): Unit = f
    })
  }

  def show(message:String): Unit = invokeLater{
    val notification = new com.intellij.notification.Notification(
      "",
      "Zeppelin Idea",
      message,
      NotificationType.INFORMATION,
      null
    )
    ApplicationManager.getApplication.getMessageBus.syncPublisher(Notifications.TOPIC).notify(notification)
  }


  def replaceLine(editor: Editor, line: Int, withText:String): Unit = {
    editor.getDocument.replaceString(
      editor.getDocument.getLineStartOffset(line),
      editor.getDocument.getLineEndOffset(line),
      withText
    )
  }

  def findPreviousLineMatching(editor: Editor, lineMatching:String => Boolean): Option[Int] = {
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    val previousParagraphMarkerLine: Option[Int] = Range(currentLine, 0, -1).map { line =>
      val start = editor.getDocument.getLineStartOffset(line)
      val end = editor.getDocument.getLineEndOffset(line)
      (line, editor.getDocument.getCharsSequence.subSequence(start, end).toString)
    }.collectFirst {
      case (line, text) if lineMatching(text) => line
    }
    previousParagraphMarkerLine
  }

  def findNextLineMatching(editor: Editor, lineMatching:String => Boolean): Option[Int] = {
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    val lastLine = editor.getDocument.getLineCount
    val targetLine: Option[Int] = Range(currentLine, lastLine, 1).map { line =>
      val start = editor.getDocument.getLineStartOffset(line)
      val end = editor.getDocument.getLineEndOffset(line)
      (line, editor.getDocument.getCharsSequence.subSequence(start, end).toString)
    }.collectFirst {
      case (line, text) if lineMatching(text) => line
    }
    targetLine
  }

  def currentCodeFragment(editor: Editor): CodeFragment = {
    val text = currentSelectedText(editor)
    if (text.isEmpty) CodeFragment(Block, currentBlockText(editor)) else CodeFragment(SelectedText, text)
  }

  def currentLineText(editor: Editor):String = {
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    editor.getDocument.getCharsSequence.subSequence(
      editor.getDocument.getLineStartOffset(currentLine),
        editor.getDocument.getLineEndOffset(currentLine)
    ).toString
  }

  def currentSelectedText(editor: Editor): String = {
    val selectionModel = editor.getSelectionModel
    val blockStarts = selectionModel.getBlockSelectionStarts
    val blockEnds = selectionModel.getBlockSelectionEnds
    editor.getDocument.getCharsSequence.subSequence(blockStarts(0), blockEnds(0)).toString
  }

  def currentBlockText(editor: Editor): String = {
    val isMark: String => Boolean = line => { line.trim.startsWith("//-")}
    val markStartLine = findPreviousLineMatching(editor, isMark).getOrElse(-1)
    val markEndLine = findNextLineMatching(editor, isMark).getOrElse(editor.getDocument.getLineCount)

    val startLine = markStartLine + 1
    val endLine = markEndLine - 1

    if (endLine < startLine) {
      return ""
    }

    editor.getDocument.getCharsSequence.subSequence(
      editor.getDocument.getLineStartOffset(startLine),
      editor.getDocument.getLineEndOffset(endLine)
    ).toString
  }

  def insertAfterFragment(editor: Editor, noteId: String, text: String): Unit = {
    //editor.getDocument.insertString(lineStartOffsetAfter(editor, fragment), text)
    val connection = ZeppelinConnection.connectionFor(editor.getProject)
    val toolWindow = ToolWindowManager.getInstance(editor.getProject).getToolWindow("Zeppelin")
    val consoleView: ConsoleView = if (connection.consoleViews.contains(noteId)) {
      connection.consoleViews(noteId)
    } else {
      val consoleView = TextConsoleBuilderFactory.getInstance.createBuilder(editor.getProject).getConsole
      val content = toolWindow.getContentManager.getFactory.createContent(consoleView.getComponent, noteId, false)
      toolWindow.getContentManager.addContent(content)

      connection.consoleViews.put(noteId, consoleView)
      consoleView
    }

    toolWindow.show(null)
    consoleView.print(text, ConsoleViewContentType.NORMAL_OUTPUT)
  }

  private def lineStartOffsetAfter(editor: Editor, fragment: CodeFragment): Int = {
    fragment.selectionMode match {
      case SelectedText =>
        val currentLine = editor.getSelectionModel.getSelectionEndPosition.line
        editor.getDocument.getLineEndOffset(currentLine)
      case SingleLine =>
        val currentLine = editor.getCaretModel.getLogicalPosition.line
        editor.getDocument.getLineEndOffset(currentLine)
    }
  }

  def insertBeforeFragment(editor: Editor, fragment: CodeFragment, text: String): Unit = {
    val lineStartOffset = fragment.selectionMode match {
      case SelectedText => editor.getDocument.getLineStartOffset(editor.getSelectionModel.getSelectionStartPosition.line)
      case SingleLine => editor.getDocument.getLineStartOffset(editor.getCaretModel.getLogicalPosition.line)
    }
    editor.getDocument.insertString(lineStartOffset, text)
  }


  def currentDocument(file: VirtualFile): Document = FileDocumentManager.getInstance().getDocument(file)

  def currentFileIn(project: Project): VirtualFile = FileEditorManagerEx.getInstanceEx(project).getCurrentFile
}
