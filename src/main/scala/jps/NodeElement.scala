package jps

/**
  * This class defines an element which is going to be stored in a node of BTree.
  * Each Node of BTree consists of NodeElements.
  * NodeElement is a key-value pair.
  * Each value is kept "under" its key.
  * Keys are unique i.e. if inserting at a key which already exists then the value is being replaced.
  *
  * @param key key at which the value is going to be stored
  * @param value value to be stored
  * @tparam K type for key
  * @tparam V type for value
  */
case class NodeElement[K, V](key: K, value: V)
