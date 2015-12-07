class Person:
    def walk(self):
        print("I walk")
    def talk(self):
        print("I talk")
    def think(self):
        print("I think")

class Duck:
    def walk(self):
        print("Waddle")
    def talk(self):
        print("Quack!")
    def swim(self):
        print("I swim")

def tryToSwim(duck):
    duck.swim()

def doStuff(duck):
    duck.walk()
    duck.talk()
    tryToSwim(duck)

duck = Duck()
person = Person()

doStuff(duck)
doStuff(person)
