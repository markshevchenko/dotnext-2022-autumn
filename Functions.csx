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

double Power(double x, uint n)
{
    double result = 1.0;
    
    for (uint i = 0; i < n; i++)
        result *= x;

    return result;
}

Power(2, 2)

double Root(double x, uint n)
{
    double guess = 1.0;
    
    while (Math.Abs(x - Power(guess, n)) > Math.Abs(x * 0.001))
        guess = Average(guess, x / Power(guess, n - 1));    

    return guess;
}

Root(4, 2)
Power(2, 3)
Root(8, 3)
Power(2, 4)
Root(16, 4)
Power(2, 5)
Root(32, 5)
