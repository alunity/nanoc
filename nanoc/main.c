int factorial_one(int n){
    if (n == 0){
        return(1);
    }
    return(n * factorial_one(n-1))
}

int factorial_two(int n){
    int res = 1;
    while (n > 0){
        res *= n;
        n = n - 1;
    }
    return res;
}

int main(){
    outInt(factorial_one(5));
    outInt(factorial_two(5));
    return(0);
}