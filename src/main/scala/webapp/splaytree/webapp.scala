package splaytree


import scala.scalajs.js.JSApp
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom
import dom._
import dom.html._
import window.setTimeout

object SplayTreeVis extends JSApp {
  def main(): Unit = {

    def getNumberElement(node: SplayNode, onClickHandler: (Int) => Unit): TypedTag[Div] = {
      val data = node.value
      div(
        onclick:= { () => onClickHandler(data) },
        cls:="letter-node",
        s"$data"
      )
    }

    def getAllNumbers(node: SplayNode, formattingCB: (SplayNode, (Int) => Unit) => TypedTag[Div], onClickHandler: (Int) => Unit, nodeType: String = ""): TypedTag[Div] = {
      div(
        cls:=s"tree-box $nodeType",
        node.left.map(getAllNumbers(_, formattingCB, onClickHandler, "child")).getOrElse(div()),
        formattingCB(node, onClickHandler),
        node.right.map(getAllNumbers(_, formattingCB, onClickHandler, "child")).getOrElse(div())
      )
    }

    val inputBox: InputBox = new InputBox("enter a number", "Add a Node to the Splay Tree", true)

    var st = new SplayTree()

    st.setRenderCallback((x: scala.Option[SplayNode]) => {

      x.map(node => {
        inputBox.updateVis(
          getAllNumbers(node, getNumberElement, st.find)
        )
        node
      })

    })


    inputBox.setRenderCallBack((number: Int) => {
      st.addNode(number)
    })

  }
}
