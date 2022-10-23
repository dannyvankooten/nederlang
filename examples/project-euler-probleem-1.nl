// https://projecteuler.net/problem=1

functie probleem_1() {
    stel som = 0

    voor elke i in 1..1000 {
        als i % 3 == 0 of i % 5 == 0 {
            som += i
        }
    } 

    antwoord som
}

stel a = probleem_1()
print(a)