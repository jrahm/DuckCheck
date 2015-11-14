class Person:
    def quack(self):
        print("I say 'Quack'")
    def walk(self):
        print("Person walking")

class Duck:
    def quack(self):
        print("Quack!")
    def walk(self):
        print("Duck walk")
    def feathers(self):
        print("Ducks have feathers")

def InTheForest(duck):
    duck.quack()
    duck.walk()

duck = Duck()
person = Person()

InTheForest(duck)
InTheForest(person)
