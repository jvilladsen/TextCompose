package writesetter.startup

import scala.collection.JavaConverters._
import javax.swing.SwingUtilities
import com.apple.eawt.{ AppEvent, OpenFilesHandler, AboutHandler, PreferencesHandler, QuitHandler, QuitResponse }
import writesetter.{ core, editor, modals, storage }

object SpecialitiesMacOSX {

  def prepare() {

    com.apple.eawt.Application.getApplication.setOpenFileHandler(
      new OpenFilesHandler {
        def openFiles(e: AppEvent.OpenFilesEvent) {
          val files = e.getFiles().asScala
          for (file <- files) {
            val fullFileName = file.getAbsolutePath();
            writesetter.editor.CompileOrGUI.handleOpenFile(fullFileName);
          }
        }
      })

    com.apple.eawt.Application.getApplication.setPreferencesHandler(
      new PreferencesHandler {
        def handlePreferences(e: AppEvent.PreferencesEvent) {
          SwingUtilities.invokeLater(new Runnable() {
            def run() {
              new modals.Preferences(false)
            }
          })
        }
      })

    com.apple.eawt.Application.getApplication.setAboutHandler(
      new AboutHandler {
        def handleAbout(e: AppEvent.AboutEvent) {
          SwingUtilities.invokeLater(new Runnable() {
            def run() {
              editor.DialogBox.about
            }
          })
        }
      })
  }
  
  def prepareQuit(workspaceTabs: writesetter.editor.WorkspaceTabs) {
    
    com.apple.eawt.Application.getApplication.setQuitHandler(
      new QuitHandler {
        def handleQuitRequestWith(e: AppEvent.QuitEvent, response: QuitResponse) {
          SwingUtilities.invokeLater(new Runnable() {
            def run() {
              if (workspaceTabs.quitHandleDirtyFile()) {
                response.performQuit()
              } else {
                response.cancelQuit()
              }
            }
          })
        }
      })
  }
}