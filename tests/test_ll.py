class LinkedList:

    def __init__(self, data):
        self.data = data

    def append(self, data):
        self.next = LinkedList(data)

test = LinkedList(5)
test.next.data
