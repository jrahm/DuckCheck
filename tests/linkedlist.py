class LinkedList:

    def __init__(self):
        self.next = None

    def add(self):
        self.next = LinkedList()

print (LinkedList().next.next.data)
