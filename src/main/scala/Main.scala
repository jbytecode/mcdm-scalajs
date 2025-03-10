import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement


val document = dom.document
val window = dom.window
val status: HTMLDivElement = document.getElementById("status").asInstanceOf[HTMLDivElement]

def status(msg: String): Unit = 
  status.textContent = msg

@main def hello(): Unit =
  val txtname = document.getElementById("txtname").asInstanceOf[HTMLInputElement]
  txtname.oninput = (e: dom.Event) => {
    status("Hello, " + txtname.value)
  }
  status("Ready")


