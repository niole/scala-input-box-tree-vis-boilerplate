package interactivevis

import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom
import dom._
import dom.html._
import window.setTimeout


trait BinaryNode[T] {
  val value: T
  val left: scala.Option[BinaryNode[T]]
  val right: scala.Option[BinaryNode[T]]
}

class InteractiveTreeVis[T : StringTo, B <: BinaryNode[T]](
  header: String,
  placeholder: String,
  onClick: (T) => Unit,
  renderOnDelay: Boolean = false,
  delay: Int = 500
  ) {

    val ChildNodeClass: String = "child"
    val inputBox: InputBox[T] = new InputBox[T](placeholder, header, renderOnDelay, delay)

    def getElement(data: T): TypedTag[Div] = {
      div(
        onclick:= { () => onClick(data) },
        cls:="letter-node",
        s"$data"
      )
    }

    def getAllElements(node: BinaryNode[T], nodeType: String = ""): TypedTag[Div] = {
      div(
        cls:=s"tree-box $nodeType",
        node.left.map(getAllElements(_, ChildNodeClass)).getOrElse(div()),
        getElement(node.value),
        node.right.map(getAllElements(_, ChildNodeClass)).getOrElse(div())
      )
    }

    def midUpdateCallBack(x: scala.Option[B]): scala.Option[B] = {

      x.map(node => {
        inputBox.updateVis(
          getAllElements(node)
        )
        node
      })

    }

}
