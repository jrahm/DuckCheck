class LinkedList:
    def __init__(self):
        self.next = None
        self.data = None

    def append(self, d):
        self.data = d
        self.next = LinkedList()
        return self.next

def add_two(x):
    return x + 2

head = LinkedList()
cursor = head
cursor = cursor.append(1)
cursor = cursor.append(2)
add_two(cursor)
print(head.next.next.data)
print(head.next.next.q)
