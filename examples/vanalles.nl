functie fib(n) {
    als n < 2 {
        n
    } anders {
        fib(n - 1) + fib(n - 2)
    }    
}

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
    ! is_even(a)
}

stel a = 10
stel b = 20
stel resultaat = kwadraat(a) + kwadraat(b) - 100 / 5;

// Output: 480
resultaat

// Calculate 35th fibonacci number
fib(35)


