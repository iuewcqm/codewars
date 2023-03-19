# https://www.codewars.com/kata/52bef5e3588c56132c0003bc

class Node:
    def __init__(self, L, R, n):
        self.left = L
        self.right = R
        self.value = n

def tree_by_levels(tree):
    queue = [tree]
    sorted_tree = []
    while queue:
        node = queue.pop(0)
        if node:
            queue += [node.left, node.right]
            sorted_tree.append(node.value)
    return sorted_tree

if __name__ == '__main__':
    test_cases = [
            (tree_by_levels(Node(Node(None, Node(None, None, 4), 2), Node(Node(None, None, 5), Node(None, None, 6), 3), 1)), [1, 2, 3, 4, 5, 6]), 
            (tree_by_levels(Node(Node(None, Node(None, None, 4), 2), Node(Node(None, None, 5), Node(None, None, 6), 3), 1)), [1, 2, 3, 4, 5, 6]), 
            ]
    for actual, expected in test_cases:
        if actual != expected:
            print(f"expected: {expected}, but was: {actual}")
