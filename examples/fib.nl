functie fib(n) {
    als n < 2 {
        antwoord n
    } 

    antwoord fib(n - 1) + fib(n - 2)
}