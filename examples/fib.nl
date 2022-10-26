functie fib(n) {
    als n < 2 {
        antwoord n
    } 
    
    fib(n - 1) + fib(n - 2)
}

fib(35)