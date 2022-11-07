stel fib = functie(n) {
    als n < 2 {
        antwoord n
    } 
    
    fib(n - 1) + fib(n - 2)
}

fib(24)