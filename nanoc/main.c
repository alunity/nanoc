int add(int x, int y){
    return x + y;
}

int main(){
    int x = 12;
    int y = 43;

    int res1 = add(x, y);
    int res2 = add(49, y);
    int res3 = add(x, 948);

    int resa = add(res1, res2);
    int res = add(resa, res3);

    outInt(res); 
    // Hard code output functions for now, since we don't even have strings to hardcode printf into
    // In codegen we can implement using printf

    return(0); // Define return as a built in function :)
}
