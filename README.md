# NederLang 

NederLang is an interpreted programming language where all keywords are Dutch words. It is dynamically typed and inspired by the likes of JavaScript, Python and C.

Here's a sample program in NederLang:

```
functie fib(n) {
    als n < 2 {
        antwoord n
    } 

    fib(n - 1) + fib(n - 2)
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

// While expression
zolang resultaat > 0 {
    resultaat = resultaat - 1
}

// Output: 0
resultaat

// Array
stel getallen = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

// Output: 1
getallen[0]

// Calculate 35th fibonacci number
fib(15)
```


### License

GPLv3, for now.