def test_walk(duck):
    duck.walk()

def ruffle(duck):
    duck.feathers()

def test_feathers(duck):
    duck.quack()
    ruffle(duck)

duck = Duck()
person = Person()
test_walk(duck)
test_walk(person)
test_feathers(duck)
test_feathers(person)
