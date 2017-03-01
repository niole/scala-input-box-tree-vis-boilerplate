package splaytree

import org.scalatest.FlatSpec

class SplayTreeTest extends FlatSpec {

   "A SplayTree" should "initialize to an empty tree" in {
    val st = new SplayTree()

    assert(st.getTree === None)
  }

   it should "maintain the BST ordering property" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(13)
    st.addNode(9)

    val root = st.getTree

    assert(root.get !== None)
    assert(root.get.value === 5)
    assert(root.get.left.get.value === 3)
    assert(root.get.right.get.value === 13)
    assert(root.get.right.get.left.get.value === 9)
  }

  "calls to find" should "bring fetched element to root of tree, immediate child" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(13)
    st.addNode(9)

    st.find(13)

    assert(st.getTree.get.value === 13)
  }

  it should "bring fetched node to root of tree, right left, not immediate child" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(13)
    st.addNode(9)

    st.find(9)
    assert(st.getTree.get.value === 9)
  }

  it should "rotate entire tree to bring subnode to root, right left" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(13)
    st.addNode(9)
    st.addNode(10)
    st.addNode(7)

    st.find(9)

    val root = st.getTree.get

    assert(root.value === 9)
    assert(root.left.get.value === 5)
    assert(root.left.get.left.get.value === 3)
    assert(root.left.get.right.get.value === 7)
    assert(root.right.get.right === None)
    assert(root.right.get.left.get.value === 10)
  }

  it should "flip entire tree to bring subnode to root, left leaning tree, bottom node" in {
    val st = new SplayTree()

    st.addNode(3)
    st.addNode(2)
    st.addNode(1)

    var root = st.getTree.get

    assert(root.value === 3)
    assert(root.left.get.value === 2)
    assert(root.left.get.left.get.value === 1)

    st.find(1)

    root = st.getTree.get

    assert(root.value === 1)
    assert(root.right.get.value === 2)
    assert(root.right.get.right.get.value === 3)
  }

  it should "flip entire tree to bring subnode to root, right leaning tree, bottom node" in {
    val st = new SplayTree()

    st.addNode(1)
    st.addNode(2)
    st.addNode(3)

    var root = st.getTree.get

    assert(root.value === 1)
    assert(root.right.get.value === 2)
    assert(root.right.get.right.get.value === 3)

    st.find(3)

    root = st.getTree.get

    assert(root.value === 3)
    assert(root.left.get.value === 2)
    assert(root.left.get.left.get.value === 1)

  }

  it should "flip entire tree to bring subnode to root, bottom node, left right" in {
    val st = new SplayTree()

    st.addNode(3)
    st.addNode(1)
    st.addNode(2)

    var root = st.getTree.get

    assert(root.value === 3)
    assert(root.left.get.value === 1)
    assert(root.left.get.right.get.value === 2)

    st.find(2)

    root = st.getTree.get

    assert(root.value === 2)
    assert(root.left.get.value === 1)
    assert(root.right.get.value === 3)

  }

  it should "return the specified node, immediate child" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(13)
    st.addNode(9)

    val found = st.find(13)
    assert(found.get.value === 13)
  }

  it should "return the specified node, not immediate child, righ left" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(13)
    st.addNode(9)

    val found = st.find(9)
    assert(found.get.value === 9)
  }

  it should "leave tree unsplayed and return None if node not found in the tree" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(4)
    st.addNode(13)
    st.addNode(9)

    var root = st.getTree

    assert(root.get.value === 5)
    assert(root.get.left.get.value === 3)
    assert(root.get.left.get.right.get.value === 4)
    assert(root.get.left.get.left === None)
    assert(root.get.right.get.left.get.value === 9)
    assert(root.get.right.get.right === None)

    val found = st.find(1)

    assert(found === None)

    root = st.getTree

    assert(root.get.value === 5)
    assert(root.get.left.get.value === 3)
    assert(root.get.left.get.right.get.value === 4)
    assert(root.get.left.get.left === None)
    assert(root.get.right.get.left.get.value === 9)
    assert(root.get.right.get.right === None)

  }

}
