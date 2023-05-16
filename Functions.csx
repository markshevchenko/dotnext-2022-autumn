double Square(double x)
{
    return x * x;
}

double Average(double a, double b)
{
    return (a + b)/2;
}

Square(2)
Average(3, 5)

double Sqrt(double z)
{
    double x = 1.0;
    
    while (Math.Abs(z - Square(x)) > Math.Abs(0.001 * z))
        x = Average(x, z / x);

    return x;
}

Sqrt(4.0)
Sqrt(2.0)

//

double Cube(double x)
{
     return x * x * x;
}

double CubeRoot(double z)
{
    double x = 1.0;
    
    while (Math.Abs(z - Cube(x)) > Math.Abs(0.001 * z))
        x = Average(x, z / Square(x));    

    return x;
}

Cube(2)
CubeRoot(8)
CubeRoot(2)

//

double Power(double x, uint k)
{
    double power = 1.0;
    
    for (uint i = 0; i < k; i++)
        power *= x;

    return power;
}

Power(2, 2)

double Root(double z, uint k)
{
    double x = 1.0;
    
    while (Math.Abs(z - Power(x, k)) > Math.Abs(0.001 * z))
        x = Average(x, z / Power(x, k - 1));    

    return x;
}

Root(4, 2)
Power(2, 3)
Root(8, 3)
Power(2, 4)
Root(16, 4)

//