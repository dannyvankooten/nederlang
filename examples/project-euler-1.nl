// https://projecteuler.net/problem=1

functie probleem_1() {
    stel som = 0
    stel i = 1

    zolang i < 1000 {
        als i % 3 == 0 || i % 5 == 0 {
            som += i
        }

        i += 1
    }

    antwoord som
}

probleem_1()
