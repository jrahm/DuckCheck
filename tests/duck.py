class Duck:
    def __init__(self):
        print("I love ducks!")
    def walk(self):
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
duck = Duck()
person = Person()
test_feathers(duck)
test_feathers(person)
