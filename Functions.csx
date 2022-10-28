double Square(double x) => x * x;
double Cube(double x) => x * x * x;
double Average(double a, double b) => (a + b)/2;

double Sqrt(double x)
{
    double guess = 1.0;
    
    while (Math.Abs(x - guess * guess) > x * 0.001)
        guess = (guess + x / guess) / 2;

    return guess;
}

double CubeRoot(double x)
{
    double guess = 1.0;
    
    while (Math.Abs(x - guess * guess * guess) > Math.Abs(x * 0.001))
        guess = (guess + x / (guess * guess)) / 2;    

    return guess;
}

Square(2)
Cube(2)
Average(Square(2), Cube(2))
Sqrt(4.0)
Sqrt(2.0)
CubeRoot(8)
CubeRoot(2)
