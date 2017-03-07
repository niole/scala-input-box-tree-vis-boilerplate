package interactivevis

import scala.scalajs.js.JSApp


object Vis extends JSApp {
  def main(): Unit = {

    var st = new SplayTree()

    val iv = new InteractiveTreeVis[Int, SplayNode]("A Tree Vis", "type a number", (x: Int) => st.find(x), true)

    st.setRenderCallback(iv.midUpdateCallBack)

    iv.inputBox.setRenderCallBack((number: Int) => {
      st.addNode(number)
    })

  }
}
