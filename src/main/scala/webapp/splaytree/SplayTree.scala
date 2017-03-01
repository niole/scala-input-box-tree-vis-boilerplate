package splaytree

/**
 * zig and zag are just large scale tree rotations that result in the element
 * searched for ending up at the root of the tree
 *
 * This splay tree's splay operations occur after a user looks for a node in the tree.
 * If the node exists, the splay tree rotates the root node and its
 * immediate children, choosing left or right or compound rotations
 * depending on the next node's location in the path to the target node.
 * When the target node has reached the root, the splay tree returns its root node.
 **/


 class SplayTree {

   private var splayRenderCallback: (Option[SplayNode]) => Option[SplayNode] = (x: Option[SplayNode]) => x

   private var tree: Option[SplayNode] = None

   def setRenderCallback(cb: (Option[SplayNode]) => Option[SplayNode]): Unit = splayRenderCallback = cb

   def getTree: Option[SplayNode] = tree

   override def toString: String = {
     val root = tree.map(_.toString)
     s"SplayTree(root: $root)"
   }

   private[this] def compare(
     root: Option[SplayNode], otherValue: Int, comparator: (Int, Int) => Boolean
   ): Boolean = root.map(r => comparator(r.value, otherValue)).getOrElse(false)

   private[this] def init(value: Int): Option[SplayNode] = Some(new SplayNode(value, None, None))

   private[this] def add(newValue: Int): Option[SplayNode] = tree.map(root => root.add(newValue)).orElse(init(newValue))

   /**
    * newParent displaces oldParent
    * oldParent becomes left child of newParent
    * leaves are reallocated
    * all done while maintaining BST invariant
    **/
   private[this] def rotateLeft(newParent: Option[SplayNode], oldParent: Option[SplayNode]): Option[SplayNode] = {
     val newRoot = newParent.map(np =>
       new SplayNode(
         np.value,
         oldParent.map(op => new SplayNode(op.value, op.left, np.left)),
         np.right
       )
     )

     splayRenderCallback(newRoot)
   }

   /**
    * newParent displaces oldParent
    * oldParent becomes right child of newParent
    * leaves are reallocated
    * all done while maintaining BST invariant
    **/
   private[this] def rotateRight(newParent: Option[SplayNode], oldParent: Option[SplayNode]): Option[SplayNode] = {
     val newRoot = newParent.map(np =>
       new SplayNode(
         np.value,
         np.left,
         oldParent.map(op => new SplayNode(op.value, np.right, op.right))
       )
     )

     splayRenderCallback(newRoot)
   }

   /**
    * In order to maintain the BST invariant,
    * rotates right grandchild to the left,
    * then rotates subtree to the right such that
    * the right child of the old parent's left child is the new root
    **/
   private[this] def rotateLeftRight(oldParent: Option[SplayNode]): Option[SplayNode] = {
     val newParent = oldParent.flatMap(
       _.left.flatMap(l => {
           l.right.map(r => {
               new SplayNode(
                 r.value,
                 Some(
                   new SplayNode(
                     l.value,
                     l.left,
                     r.left
                   )
                 ),
                 r.right
               )
             })
         })
     )

     splayRenderCallback(Some(new SplayNode(oldParent.get.value, newParent, oldParent.get.right)))

     rotateRight(newParent, oldParent)

  }

   /**
    * In order to maintain the BST invariant,
    * rotates left grandchild to the right,
    * then rotates subtree to the left such that
    * the left child of the old parent's right child is the new root
    **/
   private[this] def rotateRightLeft(oldParent: Option[SplayNode]): Option[SplayNode] = {
       val newParent = oldParent.flatMap(
         _.right.flatMap(r => {
           r.left.map(l => {
               new SplayNode(
                 l.value,
                 l.left,
                 Some(
                   new SplayNode(
                     r.value,
                     l.right,
                     r.right
                   )
                )
             )
           })
       })
      )

     splayRenderCallback(Some(new SplayNode(oldParent.get.value, oldParent.get.left, newParent)))

     rotateLeft(newParent, oldParent)
   }

   /**
    * rotates subtree at root such that
    * target node comes close and eventually is
    * the root
    **/
   private[this] def splay(targetValue: Int, root: Option[SplayNode]): Option[SplayNode] = {


     if (compare(root, targetValue, (x, y) => x == y)) root //done
     else if (compare(root, targetValue, (x, y) => x > y)) {
       //will ultimately rotate right
       //left child is new parent
       val newRoot = root.flatMap(_.left)

       if (compare(newRoot, targetValue, (a, b) => a < b)) {
         //left right rotation
         splay(
           targetValue,
           rotateLeftRight(root)
         )

       }
       else {
         //rotate root right
         splay(
           targetValue,
           rotateRight(newRoot, root)
         )
       }

     }
     else if (compare(root, targetValue, (x, y) => x < y)) {
       //ultimately rotate left, root less than target
       val newRoot = root.flatMap(_.right)

       if (compare(newRoot, targetValue, (a, b) => a > b)) {
         //newRoot's value is greater than the target
         //right left rotation
         splay(
           targetValue,
           rotateRightLeft(root)
         )

       }
       else {
         //rotate root left
         splay(
           targetValue,
           rotateLeft(newRoot, root)
         )
       }

     }
     else root //you messed up somewhere

   }

   /**
    * public alias for add method
    **/
   def addNode(newValue: Int): Unit = tree = splayRenderCallback(add(newValue))

   /**
    * retrieves specified node and splays tree if node contained in tree
    **/
   def find(newValue: Int): Option[SplayNode] = {
     val found = tree.map(_.find(newValue))
     tree = found.flatMap(_ => splay(newValue, tree)).orElse(tree)
     found.flatten
   }

 }

 class SplayNode(v: Int, l: Option[SplayNode], r: Option[SplayNode]) {
   val value: Int = v
   val left: Option[SplayNode] = l
   val right: Option[SplayNode] = r

   override def toString: String = {
     val leftChild = left.map(_.toString)
     val rightChild = right.map(_.toString)
     s"SplayNode(value: $value, left: $leftChild, right: $rightChild)"
   }

   /**
    * adds a new node to the tree such that the BST invariant is maintained
    * makes sure to not add duplicate values
    **/
   def add(newValue: Int): SplayNode = {
     if (newValue < value) {

        new SplayNode(
          value,
          left.map(_.add(newValue)).orElse(Some(new SplayNode(newValue, None, None))),
          right
        )

     }
     else if (newValue > value) {
       new SplayNode(
         value,
         left,
         right.map(_.add(newValue)).orElse(Some(new SplayNode(newValue, None, None)))
       )
     }
     else this
  }

  /**
   * once find correct node, zig and zag it back up to the root
   * and return the root
   * if node doesn't exist in tree, returns None
   **/
  def find(withValue: Int): Option[SplayNode] = {
    if (withValue < value) {
      left.map(_.find(withValue)).getOrElse(None)
    }
    else if (withValue > value) {
      right.map(_.find(withValue)).getOrElse(None)
    }
    else if (withValue == value) Some(this) //found it
    else None
  }

 }
