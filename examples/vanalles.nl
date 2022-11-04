functie kwadraat(x) {
    x * x
}

functie optellen(a, b) {
    a + b
}

functie aftrekken(a, b) {
    a - b
}

functie delen(a, b) {
    a / b
}

functie vermenigvuldigen(a, b) {
    a * b
}

// Geeft het overblijfsel wanneer a door b gedeeld wordt
functie overblijfsel(a, b) {
    a % b
}

functie is_even(a) {
    als overblijfsel(a, 2) == 0 {
        ja
    } anders {
        nee
    }
}

functie is_oneven(a) {
    antwoord ! is_even(a)
}

stel a = 10
stel b = 20
stel resultaat = kwadraat(a) + kwadraat(b) - 100 / 5;

// Output: 480
// print(resultaat)

// While expression
stel i = 10
zolang i > 0 {
    i = i - 1
}

// Output: 0
// print(resultaat)

// Array
// stel getallen = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
// getallen[0] = 10;

// Output: 10
// print(getallen[0])

stel fib = functie(n) {
    als n < 2 {
        antwoord n
    } 

    fib(n - 1) + fib(n - 2)
}
fib(4)