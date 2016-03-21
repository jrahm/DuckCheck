class LinkedList:

    def __init__(self):
        self.data = 5

    def append(self, data):
        self.next = LinkedList(data)

test = LinkedList(5)
test.next.next.next.data
test.next.next.next.does_not_exist # Error
