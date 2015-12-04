class Duck:
    def __init__(self):
        self.x = 3
        print("I love ducks!")

    def walk(self):
        print(self.x)
        print("I walk like a duck")

    def quack(self):
        print("I say quack")

    def feathers(self):
        print("A duck has feathers")

class Person:
    def __init__(self):
        print("I am a person")
    def walk(self):
        print("I walk like a person")
    def quack(self):
        print("I immitate a 'quack'")

def ruffle(duck):
    duck.feathers()

def test_feathers(duck):
    duck.walk()
    duck.quack()
    ruffle(duck)

my_duck = Duck()
my_person = Person()
test_feathers(my_duck)
test_feathers(my_person)
