package interactivevis

import org.scalajs.dom
import dom._
import org.scalajs.dom.html._
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import scalatags.JsDom.short.*
import window.{ setTimeout, clearTimeout }

trait StringTo[T] {
  def fromString(string: String): T
}

object StringTo {
  implicit object StringToString extends StringTo[String] {
    override def fromString(string: String): String = string
  }

  implicit object StringToInt extends StringTo[Int] {
    override def fromString(string: String): Int = string.toInt
  }
}

class InputBox[T : StringTo](placeholder: String, header: String, shouldRenderOnDelay: Boolean = false, delay: Int = 500) {
  val i: Input = input(*.placeholder:=placeholder).render
  val visContainer: Div = div.render
  val container: Div = div.render
  var renderCallBack: T => Unit  = (n: T) => {}
  val debouncer: () => Unit = getDebouncer(onKeyUp)
  var updateQ: List[TypedTag[Div]] = List[TypedTag[Div]]()


  document.body.appendChild(
    container.appendChild(
      div(
        h1(header),
        div(i),
        visContainer
      ).render
    )
  )

  def formatInputData(s: String): T = implicitly[StringTo[T]].fromString(s)

  def onKeyUp(): Unit = {
    val v: String = i.value
    renderCallBack(formatInputData(v))
    i.value = ""
  }

  def getDebouncer(F: () => Unit): () => Unit = {
    var timeout = 0

    def debounce(): Unit = {
      clearTimeout(timeout)
      timeout = setTimeout(() => F(), delay)
    }

    debounce
  }

  i.onkeyup = (e: dom.Event) => debouncer()

  def setRenderCallBack(cb: T => Unit): Unit = renderCallBack = cb

  def updateVis(newVisData: TypedTag[Div]): Unit = {

    if (shouldRenderOnDelay) {

      updateQ = updateQ ++ List(newVisData)

      if (updateQ.length == 1) {
        //run
        renderOnDelay
      }
    }
    else {
      visContainer.innerHTML = ""
      visContainer.appendChild(div(newVisData).render)
    }

  }

  def renderOnDelay: Unit = {
    setTimeout(() => {
      val newVisData = updateQ.head
      updateQ = updateQ.tail

      visContainer.innerHTML = ""
      visContainer.appendChild(div(newVisData).render)

      if (updateQ.length > 0) {
        renderOnDelay
      }
    }, delay)
  }

}
