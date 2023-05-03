double Square(double x) => x * x;
double Average(double a, double b) => (a + b)/2;

Square(2)
Average(3, 5)

double Sqrt(double z)
{
    double guess = 1.0;
    
    while (Math.Abs(z - Square(guess)) > Math.Abs(z * 0.001))
        guess = Average(guess, z / guess);

    return guess;
}

Sqrt(4.0)
Sqrt(2.0)

//

double Cube(double x) => x * x * x;

double CubeRoot(double z)
{
    double guess = 1.0;
    
    while (Math.Abs(z - Cube(guess)) > Math.Abs(z * 0.001))
        guess = Average(guess, z / Square(guess));    

    return guess;
}

Cube(2)
CubeRoot(8)
CubeRoot(2)

//

double Power(double x, uint k)
{
    double result = 1.0;
    
    for (uint i = 0; i < k; i++)
        result *= x;

    return result;
}

Power(2, 2)

double Root(double z, uint k)
{
    double guess = 1.0;
    
    while (Math.Abs(z - Power(guess, k)) > Math.Abs(z * 0.001))
        guess = Average(guess, z / Power(guess, k - 1));    

    return guess;
}

Root(4, 2)
Power(2, 3)
Root(8, 3)
Power(2, 4)
Root(16, 4)
Power(2, 5)
Root(32, 5)
