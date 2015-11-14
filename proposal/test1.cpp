#include <stdio.h>
using namespace std;
class Person {
public:
    void quack() {
        printf("I Say 'Quack'\n");
    }
    void walk() {
        printf("Person Walking\n");
    }
};
class Duck {
public:
    void quack() {
        printf("Quack!\n");
    }
    void walk() {
        printf("Duck Walk\n");
    }
    void feathers() {
        printf("Ducks have feathers\n");
    }
};
template <class D>
void InTheForest(D* duck) {
    duck->quack();
    duck->walk();
}
int main(int argc, char** argv) {
    auto duck = new Duck();
    auto person = new Person();

    InTheForest(duck);
    InTheForest(person);
}
