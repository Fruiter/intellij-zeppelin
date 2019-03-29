package intellij.zeppelin

import com.intellij.openapi.actionSystem.AnActionEvent

class ZeppelinRunParagraph extends ZeppelinAction {

  /*
  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {
    val editor = currentEditor(anActionEvent)
    val api = zeppelin(anActionEvent)
    for {
      note <- findNotebook(editor)
      paragraph <- findParagraph(editor)
    } yield {

      val codeFragment = currentCodeFragment(editor)

      (for {
          newParagraph <- api.updateParagraph(note, paragraph, codeFragment.content)
          result <- api.runParagraph(note, newParagraph)
        } yield {
          runWriteAction(anActionEvent) { _ =>
            insertAfterFragment(editor, codeFragment, result.markerText)
          }
        }).recover { case t: Throwable => show(t.toString) }
      }.getOrElse(show("No Zeppelin //Notebook: marker found."))
  }
  */

  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {
    val editor = currentEditor(anActionEvent)
    val context = zeppelinContext(anActionEvent)
    context.map({ c =>
      val api = c.api
      c.noteId.map({ noteId =>
        val notebook = Notebook(noteId)
        for {
          paragraph <- findParagraph(editor)
        } yield {
          val codeFragment = currentCodeFragment(editor)
          (for {
            newParagraph <- api.updateParagraph(notebook, paragraph, codeFragment.content)
            result <- api.runParagraph(notebook, newParagraph)
          } yield {
            runWriteAction(anActionEvent) { _ =>
              //insertAfterFragment(editor, codeFragment, result.markerText)
              insertAfterFragment(editor, c.noteId.get, result.results.mkString(""))
            }
          }).recover { case t: Throwable => show(t.toString) }
        }
      })
    }).getOrElse(show("No Zeppelin NoteId found."))
  }
}


