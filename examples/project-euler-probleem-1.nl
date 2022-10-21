// Output: 233168
// https://projecteuler.net/problem=1

functie probleem_1() {
    stel som = 0

    voor elke i in rij(1, 1000) {
        als i % 3 == 0 of i % 5 == 0 {
            som += i
        }
    } 

    antwoord som
}

print probleem_1()