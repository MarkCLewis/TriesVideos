import collection.mutable

class TrieSet {
  
}

object TrieSet {
  private class Node(var keyNode: Boolean) {
    private val children = mutable.Map.empty[Char, Node]
  }
}