package trees
package cgtree

class CGTreeBuffer:
  private val buffer = scala.collection.mutable.ListBuffer[CGTree]()

  def add_tree(tree: CGTree): Unit =
    buffer += tree
  end add_tree

  def add_trees(trees: CGTrees): Unit =
    buffer ++= trees
  end add_trees

  def get_trees(): CGTrees = buffer.toList
end CGTreeBuffer
