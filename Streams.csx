#r "System.Numerics"

using System.Numerics;

IEnumerable<BigInteger> Integers()
{
    var i = BigInteger.One;

    while (true)
        yield return i++;
}

string Format<T>(IEnumerable<T> source, int n = 10)
{
    return string.Join(", ", source.Take(n).Select(x => x?.ToString()));
}

Format(Integers())

IEnumerable<BigInteger> Fibs()
{
    var a = BigInteger.Zero;
    var b = BigInteger.One;

    while (true)
    {
        yield return a;

        var t = b;
        b = a + b;
        a = t;
    }
}

Format(Fibs())

BigInteger Sqrt(BigInteger n)
{
    if (n < 0)
        throw new ArgumentException(nameof(n));

    if (n < 2)
        return n;

    var shift = 2;
    var nShifted = n >> shift;

    while (nShifted != 0 && nShifted != n)
    {
        shift += 2;
        nShifted = n >> shift;
    }

    var result = BigInteger.Zero;
    while (shift >= 0)
    {
        result = result << 1;
        var candidateResult = result + 1;
        if (candidateResult * candidateResult <= (n >> shift))
        {
            result = candidateResult;
        }

        shift -= 2;
    }

    return result;
}

bool IsPrime(BigInteger n)
{
    if (n <= 1)
        return false;

    if (n == 2)
        return true;

    if (n % 2 == 0)
        return false;

    for (BigInteger i = 3; i <= Sqrt(n); i++)
        if (n % i == 0)
            return false;

    return true;
}

IEnumerable<BigInteger> Primes()
{
    return Integers().Where(IsPrime);
}

Format(Primes())

IEnumerable<T> AsEnumerable<T>(IEnumerator<T> e)
{
    while (e.MoveNext())
        yield return e.Current;

    e.Dispose();
}

(T head, IEnumerable<T> tail) HeadTail<T>(IEnumerable<T> s)
{
    var e = s.GetEnumerator();
    if (!e.MoveNext())
        throw new InvalidOperationException();

    return (e.Current, AsEnumerable(e));
}

IEnumerable<BigInteger> PrimesRecursive(IEnumerable<BigInteger> s)
{
    var (nextPrime, tailPrimes) = HeadTail(s);
    yield return nextPrime;

    var filteredTail = PrimesRecursive(tailPrimes.Where(i => i % nextPrime != BigInteger.Zero));

    foreach (var prime in filteredTail)
        yield return prime;
}

IEnumerable<BigInteger> Primes()
{
    return PrimesRecursive(Integers().Skip(1));
}

Format(Primes())

IEnumerable<BigInteger> Ones()
{
    while (true)
        yield return BigInteger.One;
}

Format(Ones())

IEnumerable<BigInteger> Integers()
{
    yield return BigInteger.One;

    foreach (var s in Ones().Zip(Integers(), (a, b) => a + b))
        yield return s;
}

Format(Integers())

IEnumerable<BigInteger> Fibonacci()
{
    yield return BigInteger.Zero;
    yield return BigInteger.One;

    foreach (var s in Fibonacci().Zip(Fibonacci().Skip(1), (a, b) => a + b))
        yield return s;
}

Format(Fibonacci())

IEnumerable<BigInteger> TwoPowers()
{
    yield return BigInteger.One;

    foreach (var p in TwoPowers().Select(i => 2 * i))
        yield return p;
}

Format(TwoPowers())

IEnumerable<double> SqrtStream(double x)
{
    yield return 1.0;

    foreach (double n in SqrtStream(x).Select(guess => (guess + x / guess) / 2))
        yield return n;
}

Format(SqrtStream(2))

IEnumerable<BigInteger> PartialSums(IEnumerable<BigInteger> s)
{
    BigInteger sum = 0;

    foreach (var next in s)
    {
        sum += next;
        yield return sum;
    }
}

Format(PartialSums(Integers()))

IEnumerable<double> PartialSums(IEnumerable<double> s)
{
    double sum = 0;

    foreach (var next in s)
    {
        sum += next;
        yield return sum;
    }
}

IEnumerable<double> PiSummands()
{
    double n = 1;
    var sign = -1;
    yield return 1 / n;

    while (true)
    {
        n += 2;
        yield return sign / n;
        sign = 0 - sign;
    }
}

Format(PiSummands())

IEnumerable<double> PiStream() => PartialSums(PiSummands()).Select(x => 4 * x);

Format(PiStream())

IEnumerable<double> EulerTransform(IEnumerable<double> s)
{
    using var e = s.GetEnumerator();
    if (!e.MoveNext())
        yield break;

    double s1 = e.Current;

    if (!e.MoveNext())
        yield break;

    double s2 = e.Current;

    while (e.MoveNext())
    {
        double s0 = s1;
        s1 = s2;
        s2 = e.Current;

        yield return s2 - Math.Pow(s2 - s1, 2) / (s0 - 2 * s1 + s2);
    }
}

Format(EulerTransform(PiStream()))

IEnumerable<IEnumerable<T>> MakeTableau<T>(
    Func<IEnumerable<T>, IEnumerable<T>> transform,
    Func<IEnumerable<T>> generate)
{
    yield return generate();

    Func<IEnumerable<T>, IEnumerable<T>> next = transform;

    while (true)
    {
        yield return next(generate());

        var previous = next;
        next = s => transform(previous(s));
    }
}

IEnumerable<T> Accelerate<T>(
    Func<IEnumerable<T>, IEnumerable<T>> transform,
    Func<IEnumerable<T>> generate)
{
    var tableau = MakeTableau(transform, generate);

    foreach (var sequence in tableau)
    {
        yield return sequence.First();
    }
}

Format(Accelerate(EulerTransform, PiStream))

IEnumerable<double> LnSummands(double x)
{
    var power = x - 1.0;
    var denominator = 1.0;

    while (true)
    {
        yield return power / denominator;

        power *= -(x - 1.0);
        denominator += 1.0;
    }
}

Format(LnSummands(2.0))

IEnumerable<double> Ln2Stream() => PartialSums(LnSummands(2.0));

Format(Ln2Stream())

Format(Accelerate(EulerTransform, Ln2Stream))
