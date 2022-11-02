double Square(double x) => x * x;
double Cube(double x) => x * x * x;
double Average(double a, double b) => (a + b)/2;

double Sqrt(double x)
{
    double guess = 1.0;
    
    while (Math.Abs(x - Square(guess)) > x * 0.001)
        guess = Average(guess, x / guess);

    return guess;
}

double CubeRoot(double x)
{
    double guess = 1.0;
    
    while (Math.Abs(x - Cube(guess)) > Math.Abs(x * 0.001))
        guess = Average(guess, x / Square(guess));    

    return guess;
}

Square(2)
Cube(2)
Average(Square(2), Cube(2))
Sqrt(4.0)
Sqrt(2.0)
CubeRoot(8)
CubeRoot(2)
