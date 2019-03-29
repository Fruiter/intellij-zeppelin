package intellij.zeppelin

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.{CommandProcessor, UndoConfirmationPolicy}
import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.openapi.util.Computable

import scala.collection.immutable

abstract class ZeppelinAction extends AnAction with IdeaDocumentApi {

  def zeppelin(anActionEvent: AnActionEvent): ZeppelinApi = {
    ZeppelinConnection.connectionFor(anActionEvent.getProject).api
  }

  def zeppelinContext(anActionEvent: AnActionEvent): Option[ZeppelinContext] = {
    val editor = currentEditor(anActionEvent)
    val url = findUrl(editor)
    if (url.isEmpty) {
      show("no url")
      None
    }

    val credentials = findCredential(editor)
    val proxy = findProxy(editor)

    val api = new ZeppelinApi(url.get.url, credentials, proxy)

    Some(ZeppelinContext(api, url.get.noteId))
  }

  def findUrl(editor: Editor): Option[Url] = {
    precedingLines(editor).flatMap(x => Url.parse(x._2)).headOption
  }

  def findCredential(editor: Editor): Option[Credentials] = {
    precedingLines(editor).flatMap(x => Credentials.parse(x._2)).headOption
  }

  def findProxy(editor: Editor): Option[Proxy] = {
    precedingLines(editor).flatMap(x => Proxy.parse(x._2)).headOption
  }

  def findNotebook(editor: Editor): Option[Notebook] = precedingLines(editor).flatMap(x => Notebook.parse(x._2)).headOption

  def findParagraph(editor: Editor): Option[Paragraph] = precedingLines(editor).flatMap(x => Paragraph.parse(x._2)).headOption

  private def precedingLines(editor: Editor): immutable.Seq[(Int, String)] = {
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    Range(currentLine, 0, -1).inclusive.map { line =>
      val start = editor.getDocument.getLineStartOffset(line)
      val end = editor.getDocument.getLineEndOffset(line)
      (line, editor.getDocument.getCharsSequence.subSequence(start, end).toString)
    }.map(x => x.copy(_2 = x._2.stripLineEnd))
  }

  /*
  def findNote(editor: Editor, line: Int): Option[Notebook] = {
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    val linesInReverse = Range(currentLine, 1, -1).map { line =>
      val start = editor.getDocument.getLineStartOffset(line - 1)
      val end = editor.getDocument.getLineStartOffset(line)
      editor.getDocument.getCharsSequence.subSequence(start, end).toString
    }.map(_.stripLineEnd)

    linesInReverse.flatMap(Notebook.parse).headOption
  }
  */

  protected def runWriteAction(anActionEvent: AnActionEvent)(f: Document => Unit): Unit = ApplicationManager.getApplication.runWriteAction{
    val document = currentDocument(currentFileIn(anActionEvent.getProject))
    new Computable[Unit] {
      override def compute(): Unit = {
        CommandProcessor.getInstance().executeCommand(
          anActionEvent.getProject,
          new Runnable {
            override def run(): Unit = {
              f(document)
            }
          },
          "Modified from Zeppelin Idea plugin",
          "ZeppelinIdea",
          UndoConfirmationPolicy.DEFAULT,
          document
        )
      }
    }
  }
}
